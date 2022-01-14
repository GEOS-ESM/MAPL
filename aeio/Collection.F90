#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module CollectionMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldRegistryMod
   use GroupMod
   use GroupRegistryMod
   use TemplateMod
   use FrequencyMod

   implicit none
   private

   public Collection

   character(*), parameter :: template_key  = 'template'
   character(*), parameter :: frequency_key = 'frequency'
   character(*), parameter :: groups_key    = 'groups'
   character(*), parameter :: grid_key    = 'output_grid'

   type :: Collection
      private
      character(:), allocatable :: name

      type(Template)  :: template
      type(hinterval) :: coll_frequency
      character(:), allocatable :: output_grid

      type(StringVector) :: groups

      class(Group), allocatable :: fields
   contains
      procedure :: get_name
      procedure :: set_name
      procedure :: get_template_object
      procedure :: get_file_template
      procedure :: get_frequency
      procedure :: get_grid
      procedure :: get_groups
      procedure :: set_groups
      procedure :: get_fields
      procedure :: set_fields
      procedure :: initialize_frequency
      procedure :: is_time_to_write

      procedure :: register

      procedure :: import_collection
      procedure :: import_groups

      procedure :: fill_groups
      procedure :: fill_bundle
      procedure :: get_field_names
   end type Collection
contains
   function get_name(this) result(collection_name)
      character(:), allocatable :: collection_name
      class(Collection), intent(in) :: this

      collection_name = this%name
   end function get_name

   subroutine set_name(this, name, unusable, rc)
      class(Collection),                intent(inout) :: this
      character(*),                     intent(in   ) :: name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (allocated(this%name)) then
         status = 1
      else
         this%name = name
      end if

      if (present(rc)) rc = status
   end subroutine set_name

   function get_template_object(this) result(tmplt)
      type(Template) :: tmplt
      class(Collection), intent(in) :: this

      tmplt = this%template
   end function get_template_object

   function get_file_template(this) result(file_template)
      type(Template) :: tmplt
      class(Collection), intent(in) :: this

      character(len=:), allocatable :: file_template

      tmplt = this%template
      file_template = tmplt%get_template()

   end function get_file_template

   function get_frequency(this) result(freq)
      type(hinterval) :: freq
      class(Collection), intent(in) :: this

      freq = this%coll_frequency
   end function get_frequency

   function get_grid(this) result(grid_name)
      character(len=:), allocatable :: grid_name
      class(Collection), intent(in) :: this
      
      if (allocated(this%output_grid)) then
         grid_name=this%output_grid
      else
         grid_name='native'
      end if
   end function get_grid

   subroutine initialize_frequency(this,clock,rc)
      class(Collection), intent(inout) :: this
      type(ESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc

      integer :: status

      call this%coll_frequency%set_alarm(clock,_RC)
      _RETURN(_SUCCESS)

   end subroutine initialize_frequency

   function is_time_to_write(this,rc) result(time_to_write)
      class(collection), intent(inout) :: this
      integer, intent(out), optional :: rc
      logical :: time_to_write
      integer :: status

      time_to_write = this%coll_frequency%check_if_ringing(_RC)
      _RETURN(_SUCCESS)

   end function is_time_to_write

   function get_groups(this) result(gps)
      type(StringVector) :: gps
      class(Collection), intent(in) :: this

      gps = this%groups
   end function get_groups

   subroutine set_groups(this, groups)
      class(Collection),  intent(inout) :: this
      type(StringVector), intent(in   ) :: groups

      character(:), allocatable  :: group_name
      type(StringVectorIterator) :: iter

      iter = groups%begin()
      do while(iter /= groups%end())
         group_name = iter%get()

         call this%groups%push_back(group_name)
         call iter%next()
      end do
   end subroutine set_groups

   function get_fields(this) result(fields)
      class(Group), allocatable :: fields
      class(Collection), intent(in) :: this

      fields = this%fields
   end function get_fields

   subroutine set_fields(this, fields, unusable, rc)
      class(Collection),                intent(inout) :: this
      class(Group),                     intent(in   ) :: fields
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (allocated(this%fields)) then
         status = 1
      else
         this%fields = fields
      end if

      if(present(rc)) rc = status
   end subroutine set_fields

   subroutine register(this, field_registry)
      class(Collection),   intent(inout) :: this
      type(FieldRegistry), intent(inout) :: field_registry

      call this%fields%register(field_registry)
   end subroutine register

   subroutine import_collection(this, name, config, unusable, rc)
      class(Collection),                intent(inout) :: this
      character(*),                     intent(in   ) :: name
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable   :: config_value
      character(:), allocatable   :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      this%name = name

      if (.not. allocated(this%fields)) allocate(this%fields)

      ! Import the fields
      call this%fields%import_group(config, __RC__)

      ! Import everything else
      iter = config%begin()
      do while(iter /= config%end())
         call iter%get_key(key)

         select case (key)
         case (template_key)
            call iter%get_value(config_value)
            call this%template%initialize(config_value)
         case (frequency_key)
            call iter%get_value(config_value)
            call this%coll_frequency%initialize(config_value)
         case (grid_key)
            call iter%get_value(config_value)
            this%output_grid = config_value
         case (groups_key)
            call iter%get_value(sub_config)
            call this%import_groups(sub_config)
         end select

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_collection

   subroutine import_groups(this, config, unusable, rc)
      class(Collection),                intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable   :: group_name
      !type(ConfigurationIterator) :: iter
      integer :: i

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (config%is_sequence()) then
         !iter = config%begin()
         !do while(iter /= config%end())
         do i= 1, config%size()
            !group_name = iter%get()
            group_name = config%of(i)
            call this%groups%push_back(group_name)

            !call iter%next()
         end do

         status = 0
      else
         status = 1
      end if

      if (present(rc)) rc = status
   end subroutine import_groups

   subroutine fill_groups(this, group_registry, unusable, rc)
      class(Collection),                intent(inout) :: this
      type(GroupRegistry),              intent(in   ) :: group_registry
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer      :: group_name
      class(Group), pointer      :: group_entry
      type(StringVectorIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      iter = this%groups%begin()
      do while(iter /= this%groups%end())
         group_name => iter%get()

         if (group_registry%count(group_name) > 0) then
            group_entry => group_registry%at(group_name)

            call this%fields%union(group_entry, __RC__)
         else
            status = 1
            exit
         end if
         call iter%next()
      end do

      if (present(rc)) rc = status
   end subroutine fill_groups

   function get_field_names(this,rc) result(field_names)
      class(Collection),                intent(inout) :: this
      integer,                optional, intent(  out) :: rc

      type(StringVector) :: field_names
      call this%fields%append_field_names(field_names)
      

   end function get_field_names
       

   subroutine fill_bundle(this,state,bundle,unusable,grid,rc)
      class(Collection),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      type(ESMF_FieldBundle),           intent(inout) :: bundle
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      type(ESMF_Grid), optional,        intent(in   ) :: grid
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%fields%fill_bundle(state,bundle, grid=grid, __RC__)

      _RETURN(_SUCCESS)

   end subroutine fill_bundle

end module CollectionMod

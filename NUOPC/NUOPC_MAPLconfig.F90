#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_MAPLfieldConfig
   use ESMF
   use MAPL_Mod
   use yaFyaml

   implicit none
   private

   public FieldConfig
   public create_field_config

   character(*), parameter :: NUOPCname               = 'NUOPCname'
   character(*), parameter :: TransferOfferGeomObject = 'TransferOfferGeomObject'
   character(*), parameter :: SharePolicyField        = 'SharePolicyField'
   character(*), parameter :: SharePolicyGeomObject   = 'SharePolicyGeomObject'

   character(*), parameter :: default_TransferOfferGeomObject = 'will provide'
   character(*), parameter :: default_SharePolicyField        = 'not share'

   type :: FieldConfig
      character(:), allocatable :: name
      character(:), allocatable :: NUOPCname
      character(:), allocatable :: TransferOfferGeomObject
      character(:), allocatable :: SharePolicyField
      character(:), allocatable :: SharePolicyGeomObject
   contains
      procedure :: fill_defaults
      procedure :: read_field_config
   end type FieldConfig

contains
   subroutine fill_defaults(this)
      class(FieldConfig), intent(inout) :: this

      if (.not. allocated(this%NUOPCname))               this%NUOPCname               = this%name
      if (.not. allocated(this%TransferOfferGeomObject)) this%TransferOfferGeomObject = default_TransferOfferGeomObject
      if (.not. allocated(this%SharePolicyField))        this%SharePolicyField        = default_SharePolicyField
      if (.not. allocated(this%SharePolicyGeomObject))   this%SharePolicyGeomObject   = this%SharePolicyField
   end subroutine fill_defaults

   subroutine read_field_config(this, name, config)
      class(FieldConfig),  intent(inout) :: this
      character(*),        intent(in   ) :: name
      type(Configuration), intent(inout) :: config

      type(ConfigurationIterator) :: iter
      character(:), pointer       :: key

      this%name = name

      iter = config%begin()
      do while (iter /= config%end())
         key => iter%key()

         select case(key)
         case(NUOPCname)
            this%NUOPCname = iter%value()
         case(TransferOfferGeomObject)
            this%TransferOfferGeomObject = iter%value()
         case(SharePolicyField)
            this%SharePolicyField = iter%value()
         case(SharePolicyGeomObject)
            this%SharePolicyGeomObject = iter%value()
         end select

         call iter%next()
      end do

      call this%fill_defaults()
   end subroutine read_field_config

   function create_field_config(name, config) result(field_config)
      type(FieldConfig) :: field_config
      character(*),        intent(in   ) :: name
      type(Configuration), intent(inout) :: config

      call field_config%read_field_config(name, config)
   end function create_field_config
end module NUOPC_MAPLfieldConfig

module NUOPC_MAPLfieldConfigMap
   use NUOPC_MAPLfieldConfig

#include "types/key_deferredLengthString.inc"
#define _value type(FieldConfig)

#define _map FieldConfigMap
#define _iterator FieldConfigMapIterator
#define _alt
#include "templates/map.inc"
end module NUOPC_MAPLfieldConfigMap

module NUOPC_MAPLconfigMod
   use ESMF
   use MAPL_Mod
   use yaFyaml

   use NUOPC_MAPLfieldConfig
   use NUOPC_MAPLfieldConfigMap

   implicit none
   private

   public NUOPC_MAPLconfig
   public create_NUOPC_MAPLconfig

   character(*), parameter :: rc_label = 'NUOPC_config:'

   character(*), parameter :: NUOPC_imports = 'NUOPC_imports'
   character(*), parameter :: NUOPC_exports = 'NUOPC_exports'

   type :: NUOPC_MAPLconfig
      type(FieldConfigMap) :: imports
      type(FieldConfigMap) :: exports
   contains
      procedure, nopass :: read_filename_from_config
      procedure, nopass :: read_from_config
      procedure         :: read_config
   end type NUOPC_MAPLconfig

contains
   subroutine read_filename_from_config(config, filename, rc)
      type(ESMF_Config),         intent(inout) :: config
      character(:), allocatable, intent(  out) :: filename
      integer, optional,         intent(  out) :: rc

      logical                    :: present
      character(len=ESMF_MaxStr) :: value
      integer                    :: status

      call ESMF_ConfigFindLabel(config, isPresent=present, label=rc_label, __RC__)

      if (present) then
         call ESMF_ConfigGetAttribute(config, value=value, label=rc_label, __RC__)
         filename = trim(value)
      end if

      _RETURN(_SUCCESS)
   end subroutine read_filename_from_config

   function read_from_config(config) result(field_config_map)
      type(FieldConfigMap)               :: field_config_map
      type(Configuration), intent(inout) :: config

      type(Configuration)         :: sub_config
      type(ConfigurationIterator) :: iter

      character(:), pointer :: name

      iter = config%begin()
      do while(iter /= config%end())
         name       => iter%key()
         sub_config =  iter%value()

         call field_config_map%insert(name, create_field_config(name, sub_config))
         call iter%next()
      end do
   end function read_from_config

   subroutine read_config(this, filename)
      class(NUOPC_MAPLconfig), intent(inout) :: this
      character(*),            intent(in   ) :: filename

      type(Parser)                :: p
      type(FileStream)            :: file_stream
      type(Configuration)         :: config, sub_config
      type(ConfigurationIterator) :: iter

      character(:), pointer :: key

      p           = Parser('core')
      file_stream = FileStream(filename)
      config      = p%load(file_stream)

      iter = config%begin()
      do while (iter /= config%end())
         key => iter%key()

         select case(key)
         case (NUOPC_imports)
            sub_config = iter%value()
            this%imports = this%read_from_config(sub_config)
         case (NUOPC_exports)
            sub_config = iter%value()
            this%exports= this%read_from_config(sub_config)
         end select

         call iter%next()
      end do

      call file_stream%close()
   end subroutine read_config

   function create_NUOPC_MAPLconfig(config, rc) result(MAPL_config)
      type(NUOPC_MAPLconfig) :: MAPL_config
      type(ESMF_Config), intent(inout) :: config
      integer, optional, intent(  out) :: rc

      character(:), allocatable :: filename
      integer                   :: status

      call MAPL_config%read_filename_from_config(config, filename, __RC__)

      if (allocated(filename)) then
         call MAPL_config%read_config(filename)
      end if

      _RETURN(_SUCCESS)
   end function create_NUOPC_MAPLconfig
end module NUOPC_MAPLconfigMod

#include "MAPL_Generic.h"

module mapl3g_SimpleConnection
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_HierarchicalRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: SimpleConnection
   public :: is_valid

!!$   public :: can_share_pointer

   type, extends(Connection) :: SimpleConnection
      private
      type(ConnectionPt) :: source
      type(ConnectionPt) :: destination
   contains
      procedure :: is_export_to_import
      procedure :: is_export_to_export
      procedure :: is_valid
      procedure :: is_sibling

      procedure :: get_source
      procedure :: get_destination
      procedure :: connect
      procedure :: connect_sibling
   end type SimpleConnection

   interface SimpleConnection
      module procedure :: new_SimpleConnection
   end interface SimpleConnection

contains

   function new_SimpleConnection(source, destination) result(this)
      type(SimpleConnection) :: this
      type(ConnectionPt), intent(in) :: source
      type(ConnectionPt), intent(in) :: destination

      this%source = source
      this%destination = destination

   end function new_SimpleConnection

   logical function is_export_to_import(this)
      class(SimpleConnection), intent(in) :: this

      is_export_to_import = ( &
           this%source%get_state_intent() == 'export' .and. &
           this%destination%get_state_intent() == 'import' )

   end function is_export_to_import

   ! NOTE: We include a src that is internal as also being an export
   ! in this case.
   logical function is_export_to_export(this)
      class(SimpleConnection), intent(in) :: this

      is_export_to_export = ( &
           any(this%source%get_state_intent() == ['export  ', 'internal']) .and. &
           this%destination%get_state_intent() == 'export' )

   end function is_export_to_export

   ! Only certain combinations of state intents are supported by MAPL.
   ! separate check must be performed elsewhere to ensure the
   ! connections are either sibling to sibling or parent to child, as
   ! component relationships are not available at this level.

   logical function is_valid(this)
      class(SimpleConnection), intent(in) :: this

      associate (intents => [character(len=len('internal')) :: this%source%get_state_intent(), this%destination%get_state_intent()])
        
        is_valid = any( [ &
             all( intents == ['export  ', 'import  '] ), &    ! E2I
             all( intents == ['export  ', 'export  '] ), &    ! E2E
             all( intents == ['internal', 'export  '] ), &    ! Z2E
             all( intents == ['import  ', 'import  '] )  &    ! I2I
             ])

      end associate
   end function is_valid

   ! Only sibling connections trigger allocation of exports.
   logical function is_sibling(this)
      class(SimpleConnection), intent(in) :: this

      character(:), allocatable :: src_intent, dst_intent

      src_intent = this%source%get_state_intent()
      dst_intent = this%destination%get_state_intent()
      is_sibling = (src_intent == 'export' .and. dst_intent == 'import')

   end function is_sibling

   function get_source(this) result(source)
      type(ConnectionPt) :: source
      class(SimpleConnection), intent(in) :: this
      source = this%source
   end function get_source

   function get_destination(this) result(destination)
      type(ConnectionPt) :: destination
      class(SimpleConnection), intent(in) :: this
      destination = this%destination
   end function get_destination

   recursive subroutine connect(this, registry, rc)
      class(SimpleConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      type(HierarchicalRegistry), pointer :: src_registry, dst_registry
      integer :: status
      type(VirtualConnectionPt) :: s_v_pt
      type(VirtualConnectionPt), pointer :: d_v_pt
      type(ConnectionPt) :: s_pt,d_pt
      type(ActualPtVec_MapIterator) :: iter

      associate( &
           src_pt => this%get_source(), &
           dst_pt => this%get_destination() &
           )
        dst_registry => registry%get_subregistry(dst_pt)
        
        ! TODO: Move this into a separate procedure, or introduce
        ! a 2nd type of connection
        if (dst_pt%get_esmf_name() == '*') then
           associate (range => dst_registry%get_range())
             iter = range(1)
             do while (iter /= range(2))
                d_v_pt => iter%first()
                if (d_v_pt%get_state_intent() /= 'import') cycle
                s_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, &
                     d_v_pt%get_esmf_name(), &
                     comp_name=d_v_pt%get_comp_name())
                s_pt = ConnectionPt(src_pt%component_name, s_v_pt)
                d_pt = ConnectionPt(dst_pt%component_name, d_v_pt)
                call registry%add_connection(SimpleConnection(s_pt, d_pt), _RC)
                call iter%next()
             end do
           end associate
           _RETURN(_SUCCESS)
        end if
        
        src_registry => registry%get_subregistry(src_pt)
        
        _ASSERT(associated(src_registry), 'Unknown source registry')
        _ASSERT(associated(dst_registry), 'Unknown destination registry')
        
        if (this%is_sibling()) then
           ! TODO: do not need to send src_registry, as it can be derived from connection again.
           call this%connect_sibling(dst_registry, src_registry, _RC)
           _RETURN(_SUCCESS)
        end if
        
      end associate
      
      _RETURN(_SUCCESS)
   end subroutine connect


   subroutine connect_sibling(this, dst_registry, src_registry, unusable, rc)
      class(SimpleConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(in) :: dst_registry
      type(HierarchicalRegistry), target, intent(inout) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr), allocatable :: export_specs(:), import_specs(:)
      class(AbstractStateItemSpec), pointer :: export_spec, import_spec
      integer :: i, j
      logical :: satisfied
      integer :: status

      associate (src_pt => this%get_source(), dst_pt => this%get_destination())

        import_specs = dst_registry%get_actual_pt_SpecPtrs(dst_pt%v_pt, _RC)
        export_specs = src_registry%get_actual_pt_SpecPtrs(src_pt%v_pt, _RC)
          
        do i = 1, size(import_specs)
           import_spec => import_specs(i)%ptr
           satisfied = .false.
           
           find_source: do j = 1, size(export_specs)
              export_spec => export_specs(j)%ptr
              
              if (import_spec%can_connect_to(export_spec)) then
                 call export_spec%set_active()
                 call import_spec%set_active()
                 
                 if (import_spec%requires_extension(export_spec)) then
                    call src_registry%extend(src_pt%v_pt, import_spec, _RC)
                 else
                    call import_spec%connect_to(export_spec, _RC)
                 end if
                 
                 
                 satisfied = .true.
                 exit find_source
              end if
           end do find_source
           
           _ASSERT(satisfied,'no matching actual export spec found')
        end do
      end associate

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

 end module mapl3g_SimpleConnection

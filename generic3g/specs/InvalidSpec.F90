#include "MAPL_Generic.h"

module mapl3g_InvalidSpec

   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_StateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_ActualPtVector
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_NullAction
   use mapl3g_VerticalGrid
   use esmf, only: ESMF_FieldBundle
   use esmf, only: ESMF_Geom
   use esmf, only: ESMF_State
   use esmf, only: ESMF_SUCCESS
   use esmf, only: ESMF_TimeInterval

   implicit none
   private
  
   public :: InvalidSpec
  
   type, extends(StateItemSpec) :: InvalidSpec
     private
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      
      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: set_geometry => set_geometry

      procedure :: write_formatted
   end type InvalidSpec

contains

   subroutine create(this, rc)
      class(InvalidSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(this)
   end subroutine create

   subroutine destroy(this, rc)
      class(InvalidSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _UNUSED_DUMMY(this)
   end subroutine destroy

   subroutine allocate(this, rc)
      class(InvalidSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _UNUSED_DUMMY(this)
   end subroutine allocate

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(InvalidSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src_spec)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to

   logical function can_connect_to(this, src_spec, rc)
      class(InvalidSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use invalid spec')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src_spec)
   end function can_connect_to

   logical function requires_extension(this, src_spec)
      class(InvalidSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec

      requires_extension = .false.
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src_spec)
   end function requires_extension

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(InvalidSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use invalid spec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(multi_state)
      _UNUSED_DUMMY(actual_pt)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(InvalidSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use item of type InvalidSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(bundle)
   end subroutine add_to_bundle

   subroutine set_geometry(this, geom, vertical_grid, timestep, rc)
      class(InvalidSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(ESMF_TimeInterval), optional, intent(in) :: timestep
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to initialize item of type InvalidSpec')
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(vertical_grid)
      _UNUSED_DUMMY(timestep)
   end subroutine set_geometry

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(InvalidSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "InvalidSpec()"
   end subroutine write_formatted

end module mapl3g_InvalidSpec

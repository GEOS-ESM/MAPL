#include "MAPL_Generic.h"

module mapl3g_InvalidSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt

   use mapl3g_ActualPtVector
   use mapl3g_ActualPtSpecPtrMap
   use esmf, only: ESMF_FieldBundle
   use esmf, only: ESMF_Geom
   use esmf, only: ESMF_State
   use esmf, only: ESMF_SUCCESS
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private
  
   public :: InvalidSpec
  
   type, extends(AbstractStateItemSpec) :: InvalidSpec
     private
   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_dependencies
      
      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: make_extension
      procedure :: extension_cost
   end type InvalidSpec


contains
  


   subroutine create(this, dependency_specs, rc)
      class(InvalidSpec), intent(inout) :: this
      type(StateItemSpecPtr), intent(in) :: dependency_specs(:)
      integer, optional, intent(out) :: rc

      integer :: status
      
      _FAIL('Attempt to use invalid spec')

      _RETURN(ESMF_SUCCESS)
   end subroutine create


   subroutine destroy(this, rc)
      class(InvalidSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   subroutine allocate(this, rc)
      class(InvalidSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


   function get_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(InvalidSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      dependencies = ActualPtVector()

      _RETURN(_SUCCESS)
   end function get_dependencies

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(InvalidSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _RETURN(ESMF_SUCCESS)
   end subroutine connect_to


   logical function can_connect_to(this, src_spec)
      class(InvalidSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      can_connect_to = .false.

   end function can_connect_to


   logical function requires_extension(this, src_spec)
      class(InvalidSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      requires_extension = .false.

   end function requires_extension


   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(InvalidSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use invalid spec')

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(InvalidSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use item of type InvalidSpec')

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   function make_extension(this, dst_spec, rc) result(extension)
      class(AbstractStateItemSpec), allocatable :: extension
      class(InvalidSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status
      _FAIL('Attempt to use item of type InvalidSpec')

   end function make_extension

   integer function extension_cost(this, src_spec, rc) result(cost)
      class(InvalidSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status
      _FAIL('Attempt to use item of type InvalidSpec')

   end function extension_cost

end module mapl3g_InvalidSpec

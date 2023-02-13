#include "MAPL_Generic.h"

module mapl3g_InvalidSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_VariableSpec, only: VariableSpec
   use esmf, only: ESMF_GeomBase
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
      procedure :: initialize
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: make_extension
      procedure :: add_to_state
   end type InvalidSpec


contains
  
   subroutine initialize(this, geom_base, var_spec, unusable, rc)
      class(InvalidSpec), intent(inout) :: this
      type(ESMF_GeomBase), intent(in) :: geom_base
      type(VariableSpec), intent(in) :: var_spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize



   subroutine create(this, rc)
      class(InvalidSpec), intent(inout) :: this
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


   subroutine connect_to(this, src_spec, rc)
      class(InvalidSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
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


   subroutine add_to_state(this, state, short_name, rc)
      class(InvalidSpec), intent(in) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      _FAIL('Attempt to use invalid spec')

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   function make_extension(this, src_spec, rc) result(action_spec)
      class(AbstractActionSpec), allocatable :: action_spec
      class(InvalidSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _FAIL('Attempt to use invalid spec')

      _RETURN(_SUCCESS)
   end function make_extension


end module mapl3g_InvalidSpec

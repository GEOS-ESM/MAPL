#include "MAPL_Generic.h"

module mapl3g_VerticalRegridActionNew

   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_CSR_SparseMatrix
   use esmf

   implicit none
   private

   public :: VerticalRegridAction
   public :: Vertical_RegridMethod_Flag
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type :: Vertical_RegridMethod_Flag
      private
      integer :: id = -1
   end type Vertical_RegridMethod_Flag

   interface operator(==)
      procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

   type(Vertical_RegridMethod_Flag), parameter :: VERTICAL_REGRID_UNKNOWN = Vertical_RegridMethod_Flag(-1)
   type(Vertical_RegridMethod_Flag), parameter :: VERTICAL_REGRID_LINEAR = Vertical_RegridMethod_Flag(1)
   type(Vertical_RegridMethod_Flag), parameter :: VERTICAL_REGRID_CONSERVATIVE = Vertical_RegridMethod_Flag(2)

   type, extends(ExtensionAction) :: VerticalRegridAction
      real(ESMF_KIND_R4), allocatable :: src_vertical_coord(:)
      real(ESMF_KIND_R4), allocatable :: dst_vertical_coord(:)
      type(Vertical_RegridMethod_Flag) :: regrid_method
      type(CSR_SparseMatrix_sp), allocatable :: weights(:) ! size of horz dims
   contains
      procedure :: initialize
      procedure :: run
      procedure, private :: compute_weights_
   end type VerticalRegridAction

   interface VerticalRegridAction
      procedure :: new_VerticalRegridAction
   end interface VerticalRegridAction

contains

   function new_VerticalRegridAction(src_vertical_coord, dst_vertical_coord, regrid_method) result(action)
      type(VerticalRegridAction) :: action
      real(ESMF_KIND_R4), intent(in) :: src_vertical_coord(:)
      real(ESMF_KIND_R4), intent(in) :: dst_vertical_coord(:)
      type(Vertical_RegridMethod_Flag), intent(in) :: regrid_method

      action%src_vertical_coord = src_vertical_coord
      action%dst_vertical_coord = dst_vertical_coord

      action%regrid_method = regrid_method
   end function new_VerticalRegridAction

   subroutine initialize(this, importState, exportState, clock, rc)
      class(VerticalRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock      
      integer, optional, intent(out) :: rc

      call this%compute_weights_()

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, rc)
      class(VerticalRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock      
      integer, optional, intent(out) :: rc

      ! call use_weights_to_compute_f_out_from_f_in()

      _RETURN(_SUCCESS)
   end subroutine run

   subroutine compute_weights_(this)
      class(VerticalRegridAction), intent(inout) :: this
      ! this%weights = ...
   end subroutine compute_weights_

   pure logical function equal_to(a, b)
      type(Vertical_RegridMethod_Flag), intent(in) :: a, b
      equal_to = (a%id == b%id)
   end function equal_to

   pure logical function not_equal_to(a, b)
      type(Vertical_RegridMethod_Flag), intent(in) :: a, b
      not_equal_to = .not. (a==b)
   end function not_equal_to

end module mapl3g_VerticalRegridActionNew

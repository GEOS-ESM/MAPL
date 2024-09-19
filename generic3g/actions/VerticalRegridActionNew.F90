#include "MAPL_Generic.h"

module mapl3g_VerticalRegridActionNew

   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_VerticalRegridMethod, only: VerticalRegridMethod_Flag
   use mapl3g_CSR_SparseMatrix
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: VerticalRegridAction

   type, extends(ExtensionAction) :: VerticalRegridAction
      real(REAL32), allocatable :: src_vertical_coord(:)
      real(REAL32), allocatable :: dst_vertical_coord(:)
      type(VerticalRegridMethod_Flag) :: regrid_method
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
      real(REAL32), intent(in) :: src_vertical_coord(:)
      real(REAL32), intent(in) :: dst_vertical_coord(:)
      type(VerticalRegridMethod_Flag), intent(in) :: regrid_method

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

end module mapl3g_VerticalRegridActionNew

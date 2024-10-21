#include "MAPL_Generic.h"

module mapl3g_VerticalRegridAction

   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_GriddedComponentDriver
   use mapl3g_CouplerPhases
   use mapl3g_VerticalRegridMethod
   use esmf

   implicit none
   private

   public :: VerticalRegridAction
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type, extends(ExtensionAction) :: VerticalRegridAction
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(GriddedComponentDriver), pointer :: v_in_coupler => null()
      type(GriddedComponentDriver), pointer :: v_out_coupler => null()
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_UNKNOWN
   contains
      procedure :: initialize
      procedure :: run
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VerticalRegridAction

   interface VerticalRegridAction
      procedure :: new_VerticalRegridAction
   end interface VerticalRegridAction

contains

   function new_VerticalRegridAction(v_in_coord, v_in_coupler, v_out_coord, v_out_coupler, method) result(action)
      type(VerticalRegridAction) :: action
      type(ESMF_Field), intent(in) :: v_in_coord
      type(GriddedComponentDriver), pointer, intent(in) :: v_in_coupler
      type(ESMF_Field), intent(in) :: v_out_coord
      type(GriddedComponentDriver), pointer, intent(in) :: v_out_coupler
      type(VerticalRegridMethod), intent(in) :: method

      _HERE
      action%v_in_coord = v_in_coord
      action%v_out_coord = v_out_coord

      action%v_in_coupler => v_in_coupler
      action%v_out_coupler => v_out_coupler

      action%method = method
   end function new_VerticalRegridAction

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(VerticalRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      if (associated(this%v_in_coupler)) then
         call this%v_in_coupler%initialize(_RC)
      end if

      if (associated(this%v_out_coupler)) then
         call this%v_out_coupler%initialize(_RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine run(this, importState, exportState, clock, rc)
      use esmf
      class(VerticalRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out


      real(ESMF_KIND_R4), pointer :: x_in(:,:,:)
      real(ESMF_KIND_R4), pointer :: x_out(:,:,:)

      real(ESMF_KIND_R4), pointer :: v_in(:)
      real(ESMF_KIND_R4), pointer :: v_out(:)

      integer :: i, j, k
      integer, parameter :: IM = 2, JM = 2, LM = 2

      if (associated(this%v_in_coupler)) then
         call this%v_in_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if

      if (associated(this%v_out_coupler)) then
         call this%v_out_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)

      call ESMF_FieldGet(f_in, fArrayPtr=x_in, _RC)
      call ESMF_FieldGet(f_out, fArrayPtr=x_out, _RC)

      ! do concurrent (i=1:IM, j=1:JM)
      !    do k = 1, LM
      !       x_out(i,j,k) = x_in(i,j,k)*(v_out(i,j,k)-v_in(i,j,k))
      !    end do
      ! end do

      _RETURN(_SUCCESS)
   end subroutine run

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(VerticalRegridAction), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      real(ESMF_KIND_R4), pointer :: v_in(:), v_out(:)
      integer :: rc, status

      call ESMF_FieldGet(this%v_in_coord, fArrayPtr=v_in, _RC)
      call ESMF_FieldGet(this%v_out_coord, fArrayPtr=v_out, _RC)

      write(unit, "(a, a, 4x, a, l1, a, 4x, a, l1, a, 4x, a, *(g0, 1x), a, 1x, a)", iostat=iostat, iomsg=iomsg) &
           "VerticalRegridAction(", new_line("a"), &
           "v_in_coupler: ", associated(this%v_in_coupler), new_line("a"), &
           "v_out_coupler: ", associated(this%v_out_coupler), new_line("a"), &
           "v_in_coord: ", v_in, new_line("a") !, &
      if (iostat /= 0) return
      ! We need a separate call to write since unlimited format iterm can be specified only once
      write(unit, "(3x, a, *(g0, 1x), a, 1x, a)", iostat=iostat, iomsg=iomsg) &
           "v_out_coord: ", v_out, new_line("a"), &
           ")"
      if (iostat /= 0) return

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

end module mapl3g_VerticalRegridAction

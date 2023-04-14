module mapl3g_RegridAction

   type, extends(AbstractAction) :: ScalarRegridAction
      class(AbstractRegridder), pointer :: regridder
      type(ESMF_Field) :: f_in, f_out
!!$      character(:), allocatable :: fname_in, fname_out
   contains
      procedure :: run
   end type ScalarRegridAction

   type, extends(AbstractAction) :: VectorRegridAction
      class(AbstractRegridder), pointer :: regridder
      character(:), allocatable :: fname_in(2), fname_out(2)
   contains
      procedure :: run
   end type VectorRegridAction

   interface RegridAction
      module procedure :: new_RegridAction_scalar
      module procedure :: new_RegridAction_vector
      module procedure :: new_RegridAction_bundle
   end interface RegridAction
      
contains

   function new_RegridAction_scalar(f_in, f_out) then (action)
      use mapl_RegridderManager

      type(ESMF_Grid) :: grid_in, grid_out

      call ESMF_FieldGet(f_in, grid=grid_in, _RC)
      call ESMF_FieldGet(f_out, grid=grid_out, _RC)

      action%regridder => regridder_manager%get_regridder(grid_in, grid_out)

      action%f_in = f_in
      action%f_out = f_out
      
   end function new_RegridAction_scalar

   
   subroutine run_scalar(this)
      type(ESMF_Field) :: f_in, f_out

      call get_field(importState, fname_in, f_in)
      call get_field(exportState, fname_out, f_out)

      call regridder%regrid(this%f_in, this%f_out, _RC)
   end subroutine run_scalar

   subroutine run_vector(this, importState, exporState)

      call get_pointer(importState, fname_in_u, f_in(1))
      call get_pointer(importState, fname_in_v, f_in(2)
      call get_pointer(exportState, fname_out_u, f_out(1))
      call get_pointer(exportState, fname_out_v, f_out(2))

      call regridder%regrid(f_in(:), f_out(:), _RC)

   end subroutine run

   subroutine run_bundle(this)

      call this%regridder%regrid(this%b_in, this%b_out, _RC)

   end subroutine run

end module mapl3g_RegridAction
      

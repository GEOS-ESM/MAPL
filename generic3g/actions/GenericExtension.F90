module mapl3g_GenericExtension

   type :: Extension  ! per field
      class(AbstractAction), allocatable :: action  ! regrid
      character(:), allocatable :: fname_in, fname_out
   contains
      procedure :: run => run_extension
   end type Extension

   type :: PrivateState
      type(ExtensionVector) :: extensions
   end type PrivateState

contains


   subroutine run(this, rc)

      integer :: i

      private_state => get_private_state(this, _RC)

      do i = 1, size(private_state%extensions)

         extension => private_state%extensions%of(i)
         call extension%run(_RC)
         
      end do

   end subroutine run

end module mapl3g_GenericExtension


subroutine extension_run(this, importState, exportState)
   call this%action%run(importState, exportState, 
end subroutine extension_run


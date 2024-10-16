module mapl3g_ApplicationMode
   implicit none (type, external)
   private

   public :: ApplicationMode

   type, abstract :: ApplicationMode
   contains
      procedure(I_Run), deferred :: run
   end type ApplicationMode

   interface
      subroutine I_Run(this, config, rc)
         use esmf
         import :: ApplicationMode
          class(ApplicationMode), intent(inout) :: this
          type(ESMF_HConfig), intent(in) :: config
          integer, optional, intent(out) :: rc
        end subroutine I_Run
     end subroutine I_Run
  end interface

end module mapl3g_ApplicationMode



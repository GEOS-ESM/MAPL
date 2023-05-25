
program

   interface write(formatted)
      subroutine write_state_formatted(state, ...)
         type(ESMF_State), intent(in) :: state

         type(ESMF_State) :: use_state
         use_state = state
         
      end subroutine write_state_formatted

   end interface write(formatted)


contains

   subroutine write...
   end subroutine write

end program


print*, my_state

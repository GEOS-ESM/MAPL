#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module TemplateMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public :: Template

   type :: Template
      private
      character(:), allocatable :: template
   contains
      procedure :: initialize
      procedure :: get_template
   end type Template
contains
   subroutine initialize(this, tmplt)
      class(Template), intent(inout) :: this
      character(*),    intent(in   ) :: tmplt

      this%template = tmplt
   end subroutine initialize

   function get_template(this) result(tmplt)
      character(:), allocatable :: tmplt
      class(Template), intent(inout) :: this

      tmplt = this%template
   end function get_template
end module TemplateMod

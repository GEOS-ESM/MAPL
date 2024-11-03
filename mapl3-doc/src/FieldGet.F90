#include "MAPL_Generic.h"

module mapl3g_FieldGet
   use mapl3g_FieldInfo
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none (type, external)
   private

   public :: MAPL_FieldGet

   interface MAPL_FieldGet
      procedure field_get
   end interface MAPL_FieldGet

contains

!#   subroutine field_get (field, unusable, &
!#        ! pass thru to ESMF
!#        status, geomtype, geom, typekind,  rank, dimCount, staggerloc, name, vm, &
!#        ! allocatable in MAPL
!#        minIndex,  maxIndex, elementCount, &
!#        localMinIndex, localMaxIndex, &
!#        ! MAPL specific
!#        units, standard_name, long_name, &
!#        rc)
!#
!#   end subroutine field_get

   subroutine field_get(field, unusable, &
        units, &
        rc)

      type(ESMF_Field), intent(in) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(len=:), optional, allocatable, intent(out) :: units
      integer, optional, intent(out) :: rc

      integer :: status

      if (present(units)) then
         call MAPL_FieldInfoGetInternal(field, units=units, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine field_get
      

end module mapl3g_FieldGet
        
        
   

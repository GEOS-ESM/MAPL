#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AttributeMod

! limit the dimesion to 0 or 1
!
   use pFIO_UnlimitedEntityMod
   use MAPL_ExceptionHandling

   implicit none
   private

   public :: Attribute
   public :: StringWrap
   public :: Attribute_deserialize

   type,extends(UnlimitedEntity) :: Attribute
   contains
      generic :: operator(==) => equal_attr
      generic :: operator(/=) => not_equal_attr
      procedure :: equal_attr
      procedure :: not_equal_attr
   end type Attribute

   interface Attribute
      module procedure new_Attribute_empty ! HUGE or undef
      module procedure new_Attribute_0d ! scalar constructor
      module procedure new_Attribute_1d ! vector constructor
   end interface Attribute

contains

   function new_Attribute_empty() result(attr)
      type (Attribute) :: attr
      attr%UnlimitedEntity = UnlimitedEntity()
   end function new_Attribute_empty
   

   function new_Attribute_0d(value, rc) result(attr)
      type (Attribute) :: attr
      integer, optional, intent(out) :: rc
      integer :: status
      class (*), intent(in) :: value
      attr%UnlimitedEntity = UnlimitedEntity(value, status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end function new_Attribute_0d
   
   function new_Attribute_1d(values, rc) result(attr)
      type (Attribute) :: attr
      integer, optional, intent(out) :: rc
      integer :: status
      class (*), intent(in) :: values(:)

      attr%UnlimitedEntity = UnlimitedEntity(values,status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   end function new_Attribute_1d

   logical function equal_attr(a, b)
      class (Attribute), target, intent(in) :: a
      type (Attribute),  target, intent(in) :: b
    
      equal_attr = (a%UnlimitedEntity == b%UnlimitedEntity)

   end function equal_attr

   logical function not_equal_attr(a, b)

      class (Attribute), target, intent(in) :: a
      type (Attribute),  target, intent(in) :: b
    
      not_equal_attr = .not. (a == b)

   end function not_equal_attr

   subroutine Attribute_deserialize(buffer, this, rc)
      integer, intent(in) :: buffer(:)
      type (Attribute), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call UnlimitedEntity_deserialize(buffer, this%UnlimitedEntity, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)
   end subroutine Attribute_deserialize

end module pFIO_AttributeMod

! The following module defines an FTL map (associative array) with keys that are deferred
! length strings and values that are Attributes.


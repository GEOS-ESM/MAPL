#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

!
! Regular file metadata is not quite enough to uniquely determine
! grids.  In particular, lat-lon grids depend on the actual
! coordinates provided; not just the number of lats/lons.  (Consider
! grids that start at the international dateline vs Greenwich.)
!
! At the same time, it is impractical to load all of the values into
! the pFIO metadata represention.   Esp. when one considers large data
! sets that will not fit into memory of a single node.
!
! Our compromise is to consider the values of coordinate variables to
! be metedata, but not values for other variables.  I.e, if "M" is a
! dimension, and there is also a variable "M" associated with that
! dimension, then it should be respresented as a CoordinateVariable from
! the class implemented here.
!



module pFIO_CoordinateVariableMod
   use, intrinsic :: iso_fortran_env, only: INT32
   use, intrinsic :: iso_fortran_env, only: INT64
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   use pFIO_ConstantsMod
   use MAPL_ExceptionHandling
   use pFIO_UtilitiesMod
   use pFIO_VariableMod
   use pFIO_KeywordEnforcerMod
   implicit none
   private

   public :: CoordinateVariable
   public :: CoordinateVariable_deserialize
   public :: Coord_SERIALIZE_TYPE
   integer, parameter :: Coord_SERIALIZE_TYPE = 200

   type, extends(Variable) :: CoordinateVariable
      private
      class(*), allocatable :: coordinate_data(:)
   contains
      procedure :: set_coordinate_data
      procedure :: get_coordinate_data
      procedure :: replace_coordinate_data
      generic :: get_data => get_real32, get_real64, get_int32, get_int64
      procedure :: get_real32
      procedure :: get_real64
      procedure :: get_int32
      procedure :: get_int64
      procedure :: serialize

      generic :: operator(==) => equal_c
      generic :: operator(/=) => not_equal_c
      procedure :: equal_c
      procedure :: not_equal_c

   end type CoordinateVariable


   interface CoordinateVariable
      module procedure new_CoordinateVariable
   end interface CoordinateVariable


contains


   function new_CoordinateVariable(var, coordinate_data, rc) result(coordinate_variable)
      type (CoordinateVariable) :: coordinate_variable
      type (Variable), intent(in) :: var
      class(*), intent(in) :: coordinate_data(:)
      integer, optional, intent(out) :: rc

      coordinate_variable%Variable = var
      call coordinate_variable%set_coordinate_data(coordinate_data)
      _RETURN(_SUCCESS)
   end function new_CoordinateVariable

   subroutine set_coordinate_data(this, coordinate_data, rc)
      class (CoordinateVariable), intent(inout) :: this
      class (*), intent(in) :: coordinate_data(:)
      integer, optional, intent(out) :: rc
     
      _ASSERT(.not. allocated(this%coordinate_data), "use replace_coordinate_data") 
      allocate(this%coordinate_data, source=coordinate_data)
      _RETURN(_SUCCESS)
   end subroutine set_coordinate_data

   subroutine replace_coordinate_data(this, coordinate_data,rc)
      class (CoordinateVariable), intent(inout) :: this
      class (*), intent(in) :: coordinate_data(:)
      integer, optional, intent(out) :: rc

      if( allocated(this%coordinate_data)) deallocate(this%coordinate_data)
      allocate(this%coordinate_data, source=coordinate_data)
      _RETURN(_SUCCESS)
   end subroutine replace_coordinate_data

   function get_coordinate_data(this, rc) result(coordinate_data)
      class (*), pointer :: coordinate_data(:)
      class (CoordinateVariable), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      coordinate_data => this%coordinate_data
      _RETURN(_SUCCESS)
   end function get_coordinate_data

   subroutine get_real32(this, coordinate_data, unusable, rc)
      class (CoordinateVariable), target, intent(in) :: this
      real(kind=REAL32), pointer, intent(out) :: coordinate_data(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%coordinate_data), "not allocated coordinate data")
      select type (q => this%coordinate_data)
      type is (real(kind=REAL32))
         coordinate_data => q
      class default ! wrong type
         _ASSERT(.false., "wrong type")
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_real32

   subroutine get_real64(this, coordinate_data, unusable, rc)
      class (CoordinateVariable), target, intent(in) :: this
      real(kind=REAL64), pointer, intent(out) :: coordinate_data(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%coordinate_data), "not allocated coordinate data")
      select type (q => this%coordinate_data)
      type is (real(kind=REAL64))
         coordinate_data => q
      class default ! wrong type
         _ASSERT(.false., 'wrong type')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine get_real64

   subroutine get_int32(this, coordinate_data, unusable, rc)
      class (CoordinateVariable), target, intent(in) :: this
      integer(kind=INT32), pointer, intent(out) :: coordinate_data(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%coordinate_data), "not allocated coordinate data")
      select type (q => this%coordinate_data)
      type is (integer(kind=INT32))
         coordinate_data => q
      class default ! wrong type
         _ASSERT(.false., 'wrong type')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine get_int32

   subroutine get_int64(this, coordinate_data, unusable, rc)
      class (CoordinateVariable), target, intent(in) :: this
      integer(kind=INT64), pointer, intent(out) :: coordinate_data(:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%coordinate_data),"not allocated coordinate data")
      select type (q => this%coordinate_data)
      type is (integer(kind=INT64))
         coordinate_data => q
      class default ! wrong type
         _ASSERT(.false., 'wrong type')
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine get_int64

       
  subroutine serialize(this, buffer, rc)
      class (CoordinateVariable), intent(in) :: this
      integer, allocatable,intent(inout) :: buffer(:)
      integer, optional, intent(out) :: rc

      integer, allocatable :: tmp_buffer(:)
      integer :: length,type_kind
      integer :: status     
 
      if(allocated(buffer)) deallocate(buffer)

      call this%Variable%serialize(tmp_buffer, status)
      _VERIFY(status)
      select type (coord=>this%coordinate_data(:))
      type is (integer(INT32))
         type_kind = pFIO_INT32
         buffer =[tmp_buffer, serialize_intrinsic(type_kind),serialize_intrinsic(coord)]
      type is (integer(INT64))
         type_kind = pFIO_INT64
         buffer =[tmp_buffer, serialize_intrinsic(type_kind),serialize_intrinsic(coord)]
      type is (real(real32))
         type_kind = pFIO_REAL32
         buffer =[tmp_buffer, serialize_intrinsic(type_kind),serialize_intrinsic(coord)]
      type is (real(real64))
         type_kind = pFIO_REAL64
         buffer =[tmp_buffer, serialize_intrinsic(type_kind),serialize_intrinsic(coord)]
      class default
         _ASSERT(.false.,"not support coord type")
      end select 
      length = serialize_buffer_length(length)+ serialize_buffer_length(Coord_SERIALIZE_TYPE) + size(buffer)
      buffer = [serialize_intrinsic(length), serialize_intrinsic(Coord_SERIALIZE_TYPE), buffer]
      _RETURN(_SUCCESS)
   end subroutine

   subroutine CoordinateVariable_deserialize(buffer, cv, rc)
      integer, intent(in) :: buffer(:)
      type (CoordinateVariable), intent(inout) :: cv
      integer, optional, intent(out) :: rc
      integer :: status

      call deserialize(cv, buffer, rc=status)
      _VERIFY(status)
      _RETURN(_SUCCESS)

   contains  
      
      subroutine deserialize(this, buffer, rc)
         class (CoordinateVariable), intent(inout) :: this
         integer,intent(in) :: buffer(:)
         integer, optional, intent(out) :: rc

         integer :: n,length,type_kind, v_type
         integer :: status

         integer(KIND=INT32), allocatable :: values_int32(:)
         real(KIND=REAL32),   allocatable :: values_real32(:)
         integer(KIND=INT64), allocatable :: values_int64(:)
         real(KIND=REAL64),   allocatable :: values_real64(:)


         n = 1
         call deserialize_intrinsic(buffer(n:),length)
         _ASSERT(length == size(buffer), "size not match")

         length = serialize_buffer_length(length)
         n = n + length
         call deserialize_intrinsic(buffer(n:),v_type)
         length = serialize_buffer_length(v_type)
         n = n+length
         call deserialize_intrinsic(buffer(n:),length)
         call Variable_deserialize(buffer(n:n+length-1),this%variable, status)
         _VERIFY(status)
         n = n + length
         call deserialize_intrinsic(buffer(n:),type_kind)
         length = serialize_buffer_length(type_kind)
         n = n + length
         if(allocated(this%coordinate_data)) deallocate(this%coordinate_data)
         select case (type_kind)
         case (pFIO_INT32)
            call deserialize_intrinsic(buffer(n:),values_int32)
            allocate(this%coordinate_data, source = values_int32) 
         case (pFIO_INT64)
            call deserialize_intrinsic(buffer(n:),values_int64)
            allocate(this%coordinate_data, source = values_int64) 
         case (pFIO_REAL32)
            call deserialize_intrinsic(buffer(n:),values_REAL32)
            allocate(this%coordinate_data, source = values_real32) 
         case (pFIO_REAL64)
            call deserialize_intrinsic(buffer(n:),values_REAL64)
            allocate(this%coordinate_data, source = values_real64)
         case default
            _ASSERT(.false., "not supportted type")
         end select
         _RETURN(_SUCCESS)
      end subroutine deserialize
   end subroutine CoordinateVariable_deserialize

   logical function equal_c(a, b) 
      class (CoordinateVariable), target, intent(in) :: a
      type (CoordinateVariable), target, intent(in) :: b

      equal_c = (a%Variable == b%Variable)
      if( .not. equal_c) return
       
      if( .not. allocated(a%coordinate_data) .and. .not. allocated(b%coordinate_data) ) then
         return
      endif      

      if( allocated(a%coordinate_data) .and. allocated(b%coordinate_data) ) then
         select type (coords_a=>a%coordinate_data)
         type is (integer(INT32))
            select type (coords_b=>b%coordinate_data)
            type is (integer(INT32))
               equal_c = all(coords_a == coords_b)
            end select
         type is (integer(INT64))
            select type (coords_b=>b%coordinate_data)
            type is (integer(INT64))
               equal_c = all(coords_a == coords_b)
            end select
         type is (real(real32))
            select type (coords_b=>b%coordinate_data)
            type is (real(real32))
               equal_c = all(nearlyEqual(coords_a, coords_b))
            end select
         type is (real(real64))
            select type (coords_b=>b%coordinate_data)
            type is (real(real64))
               equal_c = all(nearlyEqual(coords_a, coords_b))
            end select
         class default
            equal_c = .false.
         end select
      else
         ! one if allocated, the other is not
         equal_c = .false.
      end if

   end function equal_c

   logical function not_equal_c(a, b) 
      class (CoordinateVariable), target, intent(in) :: a
      type (CoordinateVariable), target, intent(in) :: b
      not_equal_c = .not. (a == b)
   end function not_equal_c

end module pFIO_CoordinateVariableMod

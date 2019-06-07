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
   use pFIO_ConstantsMod
   use pFIO_UtilitiesMod
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use pFIO_VariableMod
   implicit none
   private

   public :: CoordinateVariable

   type, extends(Variable) :: CoordinateVariable
      private
      class(*), allocatable :: coordinate_data(:)
   contains
      procedure :: set_coordinate_data
      procedure :: get_coordinate_data
      
      procedure :: serialize
      procedure :: deserialize

   end type CoordinateVariable


   interface CoordinateVariable
      module procedure new_CoordinateVariable
   end interface CoordinateVariable


contains


   function new_CoordinateVariable(var, coordinate_data) result(coordinate_variable)
      type (CoordinateVariable) :: coordinate_variable
      type (Variable), intent(in) :: var
      class(*), intent(in) :: coordinate_data(:)

      coordinate_variable%Variable = var
      call coordinate_variable%set_coordinate_data(coordinate_data)

   end function new_CoordinateVariable

   subroutine set_coordinate_data(this, coordinate_data)
      class (CoordinateVariable), intent(inout) :: this
      class (*), intent(in) :: coordinate_data(:)

      allocate(this%coordinate_data, source=coordinate_data)
      
   end subroutine set_coordinate_data


   function get_coordinate_data(this) result(coordinate_data)
      class (*), pointer :: coordinate_data(:)
      class (CoordinateVariable), target, intent(in) :: this

      coordinate_data => this%coordinate_data

   end function get_coordinate_data

  subroutine serialize(this, buffer)
      class (CoordinateVariable), intent(in) :: this
      integer, allocatable,intent(inout) :: buffer(:)
      integer, allocatable :: tmp_buffer(:)
      integer :: length,type_kind
      
      if(allocated(buffer)) deallocate(buffer)

      call this%Variable%serialize(tmp_buffer)
      select type (coord=>this%coordinate_data)
      type is (integer(kind=INT32))
         type_kind = pFIO_INT32
         buffer =[tmp_buffer, serialize_intrinsic(type_kind),serialize_intrinsic(coord)]
      type is (real(kind=real32))
         type_kind = pFIO_REAL32
         buffer =[tmp_buffer, serialize_intrinsic(type_kind),serialize_intrinsic(coord)]
      class default
         stop "not support coord type"
      end select 
      length = serialize_buffer_length(length) + size(buffer)
      buffer = [serialize_intrinsic(length),buffer]

   end subroutine

   subroutine deserialize(this, buffer)
      class (CoordinateVariable), intent(inout) :: this
      integer,intent(in) :: buffer(:)
      integer :: n,length,type_kind
      integer(KIND=INT32), allocatable :: values_int32(:)
      real(KIND=REAL32), allocatable :: values_real32(:)

      n = 1
      call deserialize_intrinsic(buffer(n:),length)
      n = n + length
      call this%Variable%deserialize(buffer(n:))
      call deserialize_intrinsic(buffer(n:),length)
      n = n + length
      call deserialize_intrinsic(buffer(n:),type_kind)
      length = serialize_buffer_length(type_kind)
      n = n + length
      select case (type_kind)
      case (pFIO_INT32)
         call deserialize_intrinsic(buffer(n:),values_int32)
         allocate(this%coordinate_data, source = values_int32) 
      case (pFIO_REAL32)
         call deserialize_intrinsic(buffer(n:),values_REAL32)
         allocate(this%coordinate_data, source = values_real32) 
      end select
  
   end subroutine deserialize

end module pFIO_CoordinateVariableMod

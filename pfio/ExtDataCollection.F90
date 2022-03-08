#include "MAPL_ErrLog.h"

module pFIO_ExtDataCollectionMod
  use gFTL_StringIntegerMap
  use pFIO_NetCDF4_FileFormatterMod
  use pFIO_FormatterVectorMod
  use pFIO_ConstantsMod
  use MAPL_ExceptionHandling
  implicit none
  private

  public :: ExtDataCollection
  public :: new_ExtDataCollection

  type :: ExtDataCollection
    character(len=:), allocatable :: template
    type (FormatterVector) :: formatters
    type (StringIntegerMap) :: file_ids

  contains
    procedure :: find_formatter
  end type ExtDataCollection

  interface ExtDataCollection
    module procedure new_ExtDataCollection
  end interface ExtDataCollection


  integer, parameter :: MAX_FORMATTERS = 2

contains


  function new_ExtDataCollection(template) result(collection)
    type (ExtDataCollection) :: collection
    character(len=*), intent(in) :: template

    collection%template = template

  end function new_ExtDataCollection



  function find_formatter(this, file_name, rc) result(formatter)
    type (NetCDF4_FileFormatter), pointer :: formatter
    class (ExtDataCollection), intent(inout) :: this
    character(len=*), intent(in) :: file_name
    integer, optional, intent(out) :: rc

    integer, pointer :: file_id
    type (StringIntegerMapIterator) :: iter
    integer :: status
    type (FormatterVectorIterator) :: f_iter_tmp
    type (NetCDF4_FileFormatter) :: formatter_tmp


    file_id => this%file_ids%at(file_name)
    if (associated(file_id)) then
       formatter => this%formatters%at(file_id)
    else
       if (this%formatters%size() >= MAX_FORMATTERS) then
          formatter => this%formatters%front()
          call formatter%close(rc=status)
          _VERIFY(status)
          f_iter_tmp = this%formatters%erase(this%formatters%begin())
          !deallocate(formatter)
          nullify(formatter)

          iter = this%file_ids%begin()
          do while (iter /= this%file_ids%end())
             file_id => iter%value()
             if (file_id == 1) then
                call this%file_ids%erase(iter)
                exit
             end if
             call iter%next()
          end do

          ! Fix the old file_id's accordingly
          iter = this%file_ids%begin()
          do while (iter /= this%file_ids%end())
             file_id => iter%value()
             file_id = file_id -1
             call iter%next()
          end do

       end if

       call formatter_tmp%open(file_name, pFIO_READ, _RC)
       call this%formatters%push_back(formatter_tmp)
       formatter => this%formatters%back()
       ! size() returns 64-bit integer;  cast to 32 bit for this usage.
       call this%file_ids%insert(file_name, int(this%formatters%size()))
    end if
    _RETURN(_SUCCESS)
  end function find_formatter

end module pFIO_ExtDataCollectionMod


module pFIO_ExtdataCollectionVectorMod
   use pFIO_ExtDataCollectionMod

   ! Create a map (associative array) between names and pFIO_Attributes.

#define T ExtDataCollection 
#define Vector ExtDataCollectionVector
#define VectorIterator ExtDataCollectionVectorIterator
#define VectorRIterator ExtDataCollectionRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T

end module pFIO_ExtdataCollectionVectorMod


module pFIO_ExtDataCollectionMod
  use pFIO_StringIntegerMapMod
  use pFIO_NetCDF4_FileFormatterMod
  use pFIO_FormatterPtrVectorMod
  use pFIO_ConstantsMod
  implicit none
  private

  public :: ExtDataCollection
  public :: new_ExtDataCollection

  type :: ExtDataCollection
    character(len=:), allocatable :: template
    type (FormatterPtrVector) :: formatters
    type (StringIntegerMap) :: file_ids

    type (NetCDF4_FileFormatter), pointer :: formatter => null()
  contains
    procedure :: find
    procedure :: unfind
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



  function find(this, file_name) result(formatter)
    type (NetCDF4_FileFormatter), pointer :: formatter
    class (ExtDataCollection), intent(inout) :: this
    character(len=*), intent(in) :: file_name

    integer, pointer :: file_id
    type (StringIntegerMapIterator) :: iter


    file_id => this%file_ids%at(file_name)
    if (associated(file_id)) then
       formatter => this%formatters%at(file_id)
    else
       if (this%formatters%size() >= MAX_FORMATTERS) then
          formatter => this%formatters%front()
          call formatter%close()
          call this%formatters%erase(this%formatters%begin())
          deallocate(formatter)
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

       allocate(formatter)
       
       call formatter%open(file_name, pFIO_READ)
       call this%formatters%push_back(formatter)
       ! size() returns 64-bit integer;  cast to 32 bit for this usage.
       call this%file_ids%insert(file_name, int(this%formatters%size()))
    end if

  end function find

  subroutine unfind(this)
    class (ExtDataCollection), intent(inout) :: this

    call this%formatter%close()
    deallocate(this%formatter)
    nullify(this%formatter)
    
  end subroutine unfind

end module pFIO_ExtDataCollectionMod


module pFIO_CollectionVectorMod
   use pFIO_ThrowMod
   use pFIO_ExtDataCollectionMod
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (ExtDataCollection)
#define _vector CollectionVector
#define _iterator CollectionVectorIterator

#define _FTL_THROW pFIO_throw_exception

#include "templates/vector.inc"
   
end module pFIO_CollectionVectorMod


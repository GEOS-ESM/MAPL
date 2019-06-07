module ESMF_CFIOCollectionMod
  use pFIO_StringIntegerMapMod
  use ESMF_CFIOMod
  use ESMF_CFIOUtilMod
  use ESMF_CFIOFileMod
  use ESMF_CFIOPtrVectorMod
  use pFIO_ConstantsMod
  implicit none
  private

  public :: CFIOCollection
  public :: new_CFIOCollection

  type :: CFIOCollection
    character(len=:), allocatable :: template
    type (ESMF_CFIOPtrVector) :: formatters
    type (StringIntegerMap) :: file_ids

    type (ESMF_CFIO), pointer :: formatter => null()
  contains
    procedure :: find
    procedure :: unfind
  end type CFIOCollection

  interface CFIOCollection
    module procedure new_CFIOCollection
  end interface CFIOCollection


  integer, parameter :: MAX_FORMATTERS = 2

contains


  function new_CFIOCollection(template) result(collection)
    type (CFIOCollection) :: collection
    character(len=*), intent(in) :: template

    collection%template = template 

  end function new_CFIOCollection



  function find(this, file_name) result(formatter)
    type (ESMF_CFIO), pointer :: formatter
    class (CFIOCollection), target, intent(inout) :: this
    character(len=*), intent(in) :: file_name

    integer, pointer :: file_id
    type (StringIntegerMapIterator) :: iter

    file_id => this%file_ids%at(trim(file_name))
    if (associated(file_id)) then
       formatter => this%formatters%at(file_id)
    else
       if (this%formatters%size() >= MAX_FORMATTERS) then
          formatter => this%formatters%front()
          call ESMF_CFIODestroy(formatter)
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
   
       formatter = ESMF_CFIOCreate() 
       call ESMF_CFIOSet(formatter,fname=trim(file_name),format='SDF')
       call ESMF_CFIOFileOpen(formatter, fmode=1)
       call this%formatters%push_back(formatter)
       ! size() returns 64-bit integer;  cast to 32 bit for this usage.
       call this%file_ids%insert(trim(file_name), int(this%formatters%size()))
    end if

  end function find

  subroutine unfind(this)
    class (CFIOCollection), intent(inout) :: this

    call ESMF_CFIODestroy(this%formatter)
    deallocate(this%formatter)
    nullify(this%formatter)
    
  end subroutine unfind

end module ESMF_CFIOCollectionMod


module ESMF_CFIOCollectionVectorMod
   use pFIO_ThrowMod
   use ESMF_CFIOCollectionMod
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (CFIOCollection)
#define _vector CFIOCollectionVector
#define _iterator CFIOCollectionVectorIterator

#define _FTL_THROW pFIO_throw_exception

#include "templates/vector.inc"
   
end module ESMF_CFIOCollectionVectorMod


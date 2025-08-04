#include "MAPL_ErrLog.h"

module mapl3g_DataCollection
  use pFIO
  use MAPL_FileMetadataUtilsVectorMod
  use MAPL_FileMetadataUtilsMod
  use MAPL_GridManagerMod
  use MAPL_AbstractGridFactoryMod
  use gFTL2_StringIntegerMap
  use esmf
  use mapl_ErrorHandlingMod
  implicit none
  private

  public :: DataCollection
  public :: new_DataCollection

  type :: DataCollection
    character(len=:), allocatable :: template
    type (FileMetadataUtilsVector) :: metadatas
    type (StringIntegerMap) :: file_ids
  contains
    procedure :: find => find_
  end type DataCollection

  interface DataCollection
    module procedure new_DataCollection
  end interface DataCollection


  integer, parameter :: MAX_FORMATTERS = 3

contains


  function new_DataCollection(template) result(collection)
    type (DataCollection) :: collection
    character(len=*), intent(in) :: template

    collection%template = template 
  end function new_DataCollection



  function find_(this, file_name, rc) result(metadata)
    type (FileMetadataUtils), pointer :: metadata
    class (DataCollection), target, intent(inout) :: this
    character(len=*), intent(in) :: file_name
    integer, optional, intent(out) :: rc

    type (NetCDF4_FileFormatter) :: formatter
    type (FileMetadata) :: basic_metadata
    integer, pointer :: file_id
    type (StringIntegerMapIterator) :: iter
    integer :: status


    file_id => this%file_ids%at(file_name)
    if (associated(file_id)) then
       metadata => this%metadatas%at(file_id)
    else
       if (this%metadatas%size() >= MAX_FORMATTERS) then
          metadata => this%metadatas%front()
          call this%metadatas%erase(this%metadatas%begin())
          nullify(metadata)

          iter = this%file_ids%begin()
          do while (iter /= this%file_ids%end())
             file_id => iter%second()
             if (file_id == 1) then
                iter = this%file_ids%erase(iter)
                exit
             end if
             call iter%next()
          end do

          ! Fix the old file_id's accordingly
          iter = this%file_ids%begin()
          do while (iter /= this%file_ids%end())
             file_id => iter%second()
             file_id = file_id -1
             call iter%next()
          end do
          
       end if

       allocate(metadata)
       call formatter%open(file_name, pFIO_READ,rc=status)
       _VERIFY(status)
       basic_metadata = formatter%read(_RC)
       call formatter%close(rc=status)
       _VERIFY(status)
       call metadata%create(basic_metadata,file_name)
       call this%metadatas%push_back(metadata)
       deallocate(metadata)
       metadata => this%metadatas%back()
       ! size() returns 64-bit integer;  cast to 32 bit for this usage.
       call this%file_ids%insert(file_name, int(this%metadatas%size()))
    end if
    _RETURN(_SUCCESS)
  end function find_

end module mapl3g_DataCollection



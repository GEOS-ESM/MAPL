#include "MAPL_ErrLog.h"

module MAPL_ExtDataCollectionMod
  use pFIO
  use MAPL_FileMetadataUtilsVectorMod
  use MAPL_FileMetadataUtilsMod
  use MAPL_GridManagerMod
  use MAPL_AbstractGridFactoryMod
  use gFTL_StringIntegerMap
  implicit none
  private

  public :: MAPLExtDataCollection
  public :: new_MAPLExtDataCollection

  type :: MAPLExtDataCollection
    character(len=:), allocatable :: template
    type (FileMetadataUtilsVector) :: metadatas
    type (StringIntegerMap) :: file_ids
    type(ESMF_Grid), allocatable :: src_grid
  contains
    procedure :: find
  end type MAPLExtDataCollection

  interface MAPLExtDataCollection
    module procedure new_MAPLExtDataCollection
  end interface MAPLExtDataCollection


  integer, parameter :: MAX_FORMATTERS = 2

contains


  function new_MAPLExtDataCollection(template) result(collection)
    type (MAPLExtDataCollection) :: collection
    character(len=*), intent(in) :: template

    collection%template = template 

  end function new_MAPLExtDataCollection



  function find(this, file_name, rc) result(metadata)
    type (FileMetadataUtils), pointer :: metadata
    class (MAPLExtDataCollection), intent(inout) :: this
    character(len=*), intent(in) :: file_name
    integer, optional, intent(out) :: rc

    type (NetCDF4_FileFormatter) :: formatter
    type (FileMetadata) :: basic_metadata
    integer, pointer :: file_id
    type (StringIntegerMapIterator) :: iter
    class (AbstractGridFactory), allocatable :: factory
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

       allocate(metadata)
       call formatter%open(file_name, pFIO_READ,rc=status)
       _VERIFY(status)
       basic_metadata = formatter%read(rc=status)
       _VERIFY(status)
       call formatter%close(rc=status)
       _VERIFY(status)
       call metadata%create(basic_metadata,file_name)
       call this%metadatas%push_back(metadata)
       deallocate(metadata)
       metadata => this%metadatas%back()
       if (.not. allocated(this%src_grid)) then
          allocate(factory, source=grid_manager%make_factory(trim(file_name)))
          this%src_grid = grid_manager%make_grid(factory)
       end if
       ! size() returns 64-bit integer;  cast to 32 bit for this usage.
       call this%file_ids%insert(file_name, int(this%metadatas%size()))
    end if

  end function find

end module MAPL_ExtDataCollectionMod


module MAPL_CollectionVectorMod
   use pFIO
   use MAPL_ExtDataCollectionMod
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (MAPLExtDataCollection)
#define _vector MAPLCollectionVector
#define _iterator MAPLCollectionVectorIterator

#define _FTL_THROW pFIO_throw_exception

#include "templates/vector.inc"
   
end module MAPL_CollectionVectorMod


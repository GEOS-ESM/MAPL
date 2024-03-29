#include "MAPL_ErrLog.h"

module MAPL_DataCollectionMod
  use pFIO
  use MAPL_FileMetadataUtilsVectorMod
  use MAPL_FileMetadataUtilsMod
  use MAPL_GridManagerMod
  use MAPL_AbstractGridFactoryMod
  use gFTL_StringIntegerMap
  implicit none
  private

  public :: MAPLDataCollection
  public :: new_MAPLDataCollection

  type :: MAPLDataCollection
    character(len=:), allocatable :: template
    logical :: use_file_coords
    type (FileMetadataUtilsVector) :: metadatas
    type (StringIntegerMap) :: file_ids
    type(ESMF_Grid), allocatable :: src_grid
  contains
    procedure :: find
  end type MAPLDataCollection

  interface MAPLDataCollection
    module procedure new_MAPLDataCollection
  end interface MAPLDataCollection


  integer, parameter :: MAX_FORMATTERS = 2

contains


  function new_MAPLDataCollection(template,use_file_coords) result(collection)
    type (MAPLDataCollection) :: collection
    character(len=*), intent(in) :: template
    logical, optional, intent(in) :: use_file_coords

    collection%template = template 
    if (present(use_file_coords)) then
       collection%use_file_coords=use_file_coords
    else
       collection%use_file_coords=.false.
    end if

  end function new_MAPLDataCollection



  function find(this, file_name, rc) result(metadata)
    type (FileMetadataUtils), pointer :: metadata
    class (MAPLDataCollection), target, intent(inout) :: this
    character(len=*), intent(in) :: file_name
    integer, optional, intent(out) :: rc

    type (NetCDF4_FileFormatter) :: formatter
    type (FileMetadata) :: basic_metadata
    integer, pointer :: file_id
    type (StringIntegerMapIterator) :: iter
    class (AbstractGridFactory), allocatable :: factory
    integer :: status
    type(StringIntegerMap), pointer :: dimensions
    integer, pointer :: tile_size
    logical :: skip_grid


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
       dimensions => metadata%get_dimensions()
       tile_size => dimensions%at("tile_index") 
       skip_grid = associated(tile_size)   
 
       if ( (.not. allocated(this%src_grid)) .and. (.not. skip_grid)) then
          allocate(factory, source=grid_manager%make_factory(trim(file_name),force_file_coordinates=this%use_file_coords))
          this%src_grid = grid_manager%make_grid(factory)
       end if
       ! size() returns 64-bit integer;  cast to 32 bit for this usage.
       call this%file_ids%insert(file_name, int(this%metadatas%size()))
    end if
    _RETURN(_SUCCESS)
  end function find

end module MAPL_DataCollectionMod


module MAPL_CollectionVectorMod
   use pFIO
   use MAPL_DataCollectionMod
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (MAPLDataCollection)
#define _vector MAPLCollectionVector
#define _iterator MAPLCollectionVectorIterator

#define _FTL_THROW pFIO_throw_exception

#include "templates/vector.inc"
   
end module MAPL_CollectionVectorMod


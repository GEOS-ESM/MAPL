module MAPL_DataCollectionManagerMod
use MAPL_CollectionVectorMod
use MAPL_DataCollectionMod
implicit none
private

type(MAPLCollectionVector) :: DataCollections

public DataCollections
public MAPL_DataAddCollection

contains

  function MAPL_DataAddCollection(template,use_file_coords) result(id)
     character(len=*), intent(in) :: template
     logical, optional, intent(in) :: use_file_coords
      integer :: n
      logical :: found
      type (MAPLCollectionVectorIterator) :: iter
      type (MAPLDataCollection), pointer :: collection
      type (MAPLDataCollection) :: c
      integer :: id

      iter = Datacollections%begin()
      n = 1

      ! Is it a new collection?
      found = .false.
      do while (iter /= Datacollections%end())
         collection => iter%get()
         if (template == collection%template) then
            found = .true.
            exit
         end if
         n = n + 1
         call iter%next()
      end do

      if (.not. found) then
         c = MAPLDataCollection(template,use_file_coords=use_file_coords)
         call Datacollections%push_back(c)
      end if

      id = n

   end function MAPL_DataAddCollection

end module 

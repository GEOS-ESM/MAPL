module MAPL_ExtDataCollectionManagerMod
use MAPL_CollectionVectorMod
use MAPL_ExtDataCollectionMod
implicit none
private

type(MAPLCollectionVector) :: ExtDataCollections

public ExtDataCollections
public MAPL_ExtDataAddCollection

contains

  function MAPL_ExtDataAddCollection(template,use_file_coords) result(id)
     character(len=*), intent(in) :: template
     logical, optional, intent(in) :: use_file_coords
      integer :: n
      logical :: found
      type (MAPLCollectionVectorIterator) :: iter
      type (MAPLExtDataCollection), pointer :: collection
      type (MAPLExtDataCollection) :: c
      integer :: id

      iter = ExtDatacollections%begin()
      n = 1

      ! Is it a new collection?
      found = .false.
      do while (iter /= ExtDatacollections%end())
         collection => iter%get()
         if (template == collection%template) then
            found = .true.
            exit
         end if
         n = n + 1
         call iter%next()
      end do

      if (.not. found) then
         c = MAPLExtDataCollection(template,use_file_coords=use_file_coords)
         call ExtDatacollections%push_back(c)
      end if

      id = n

   end function MAPL_ExtDataAddCollection

end module 

module mapl3g_DataCollectionManager
use mapl3g_DataCollectionVector
use mapl3g_DataCollection
implicit none
private

type(DataCollectionVector), target :: DataCollections

public DataCollections
public mapl3g_AddDataCollection

contains

  function mapl3g_AddDataCollection(template) result(id)
     character(len=*), intent(in) :: template
      integer :: n
      logical :: found
      type (DataCollectionVectorIterator) :: iter
      type (DataCollection), pointer :: collection
      type (DataCollection) :: c
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
         c = DataCollection(template)
         call Datacollections%push_back(c)
      end if

      id = n

   end function mapl3g_AddDataCollection

end module 

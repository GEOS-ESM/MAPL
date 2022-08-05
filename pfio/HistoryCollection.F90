#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_HistoryCollectionMod
  use MAPL_ExceptionHandling
  use gFTL_StringIntegerMap
  use pFIO_NetCDF4_FileFormatterMod
  use pFIO_StringNetCDF4_FileFormatterMapMod
  use pFIO_FileMetadataMod
  use pFIO_StringVariableMapMod
  use pFIO_ConstantsMod
  use gFTL_StringVector
  use NetCDF
  implicit none
  private

  public :: HistoryCollection
  public :: new_HistoryCollection

  type :: HistoryCollection
    type (Filemetadata) :: fmd
    type (StringVector) :: files_created
    type (StringNetCDF4_FileFormatterMap) :: formatters

  contains
    procedure :: find
    procedure :: ModifyMetadata
    procedure :: clear
    procedure :: check_if_i_created
  end type HistoryCollection

  interface HistoryCollection
    module procedure new_HistoryCollection
  end interface HistoryCollection

contains

  function new_HistoryCollection(fmd) result(collection)
    type (HistoryCollection) :: collection
    type (FilemetaData), intent(in) :: fmd

    collection%fmd = fmd
    collection%formatters = StringNetCDF4_FileFormatterMap() 

  end function new_HistoryCollection

  function find(this, file_name,rc) result(formatter)
    class (HistoryCollection), intent(inout) :: this
    character(len=*), intent(in) :: file_name
    integer,optional,intent(out) :: rc 

    type (NetCDF4_FileFormatter), pointer :: formatter
    type (NetCDF4_FileFormatter) :: fm

    type(StringNetCDF4_FileFormatterMapIterator) :: iter
    integer :: status
    character(len=*), parameter :: Iam = "HistoryCollection::find()"
    logical :: f_exist, i_created

    iter = this%formatters%find(trim(file_name))
    if (iter == this%formatters%end()) then
       inquire(file=file_name, exist=f_exist)
       if(.not. f_exist) then
         call fm%create(trim(file_name),rc=status)
         _VERIFY(status)
         call fm%write(this%fmd, rc=status)
         _VERIFY(status)
         call this%files_created%push_back(file_name)
       else
          i_created = this%check_if_i_created(file_name)
          if (i_created) then
             call fm%open(trim(file_name), pFIO_WRITE)
          else
             call fm%create(trim(file_name),mode=NF90_CLOBBER,rc=status)
             _VERIFY(status)
             call fm%write(this%fmd, rc=status)
             _VERIFY(status)
             call this%files_created%push_back(file_name)
          end if
       endif
       call this%formatters%insert( trim(file_name),fm)
       iter = this%formatters%find(trim(file_name))
    end if
    formatter => iter%value()
    _RETURN(_SUCCESS)
  end function find

  subroutine  ModifyMetadata(this,var_map,rc)
    class (HistoryCollection), intent(inout) :: this
    type (StringVariableMap), intent(in) :: var_map
    integer, optional, intent(out) :: rc 

    type(StringVariableMapIterator) :: iter
    integer :: status
    character(len=*), parameter :: Iam = "HistoryCollection::ModifyMetadata()"

    iter = var_map%begin()
    do while (iter /= var_map%end()) 
       call this%fmd%modify_variable(iter%key(), iter%value(), rc=status)
       _VERIFY(status)
       call iter%next()
    enddo

    _RETURN(_SUCCESS)
  end subroutine ModifyMetadata

  subroutine clear(this, rc)
    class (HistoryCollection), intent(inout) :: this
    integer, optional, intent(out) :: rc 

    type(NetCDF4_FileFormatter), pointer :: f_ptr
    type(StringNetCDF4_FileFormatterMapIterator) :: iter
    character(:),pointer :: file_name
    integer :: status

    iter = this%formatters%begin()
    do while (iter /= this%formatters%end())
      file_name => iter%key()
      f_ptr => this%formatters%at(file_name)
      call f_ptr%close(rc=status)
      _VERIFY(status)
      ! remove the files
      call this%formatters%erase(iter)
      iter = this%formatters%begin()
    enddo
    _RETURN(_SUCCESS)
  end subroutine clear

  function check_if_i_created(this,input_file,rc) result(i_created)
    logical :: i_created
    class (HistoryCollection), intent(inout) :: this
    character(len=*), intent(in) :: input_file
    integer, optional, intent(out) :: rc 

    integer :: status
    character(len=:), pointer :: file_name
    type(StringVectorIterator) :: iter

    i_created = .false.
    iter = this%files_created%begin()
    do while (iter /= this%files_created%end()) 
       file_name => iter%get()
       if (file_name == input_file) i_created = .true.
       call iter%next()
    enddo

    _RETURN(_SUCCESS)

  end function

end module pFIO_HistoryCollectionMod


module pFIO_HistoryCollectionVectorMod
   use pFIO_HistoryCollectionMod
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (HistoryCollection)
#define _vector HistoryCollectionVector
#define _iterator HistoryCollectionVectorIterator

#include "templates/vector.inc"
   
end module pFIO_HistoryCollectionVectorMod

module pFIO_HistoryCollectionVectorUtilMod
   use pFIO_FileMetadataMod
   use pFIO_HistoryCollectionMod
   use pFIO_HistoryCollectionVectorMod
   use pFIO_UtilitiesMod
   implicit none
   private

   public:: HistoryCollectionVector_serialize
   public:: HistoryCollectionVector_deserialize

contains

  subroutine HistoryCollectionVector_serialize(histVec,buffer)
     type (HistoryCollectionVector),intent(in) :: histVec
     integer, allocatable,intent(inout) :: buffer(:)
     integer, allocatable :: tmp(:)
     type (HistoryCollection),pointer :: hist_ptr
     integer :: n, i

     if (allocated(buffer)) deallocate(buffer)
     allocate(buffer(0))
     
     n = histVec%size()
     do i = 1, n
        hist_ptr=>histVec%at(i)
        call hist_ptr%fmd%serialize(tmp) 
        buffer = [buffer,tmp]
     enddo

  end subroutine

  subroutine HistoryCollectionVector_deserialize(buffer, histVec)
     type (HistoryCollectionVector),intent(inout) :: histVec
     integer, intent(in) :: buffer(:)
     type (HistoryCollection) :: hist
     type (FileMetadata) :: fmd
     integer :: n, length, fmd_len

     length = size(buffer)
     n=1
     fmd = FileMetadata()
     histVec = HistoryCollectionVector()
     do while (n < length)
       hist = HistoryCollection(fmd)
       call FileMetadata_deserialize(buffer(n:), hist%fmd)
       call histVec%push_back(hist)
       call deserialize_intrinsic(buffer(n:),fmd_len)
       n = n + fmd_len 
     enddo
  end subroutine

end module pFIO_HistoryCollectionVectorUtilMod

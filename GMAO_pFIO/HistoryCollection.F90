#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_HistoryCollectionMod
  use pFIO_ErrorHandlingMod
  use pFIO_StringIntegerMapMod
  use pFIO_NetCDF4_FileFormatterMod
  use pFIO_StringNetCDF4_FileFormatterMapMod
  use pFIO_FileMetadataMod
  use pFIO_StringVariableMapMod
  use pFIO_ConstantsMod
  implicit none
  private

  public :: HistoryCollection
  public :: new_HistoryCollection

  type :: HistoryCollection
    type (Filemetadata) :: fmd
    type (StringNetCDF4_FileFormatterMap) :: formatters

  contains
    procedure :: find
    procedure :: ModifyMetadata
    procedure :: clear
  end type HistoryCollection

  interface HistoryCollection
    module procedure new_HistoryCollection
  end interface HistoryCollection

contains

  function new_HistoryCollection(fmd) result(collection)
    type (HistoryCollection) :: collection
    type (FilemetaData), intent(in) :: fmd

    collection%fmd = fmd

  end function new_HistoryCollection

  function find(this, file_name,rc) result(formatter)
    class (HistoryCollection), intent(inout) :: this
    character(len=*), intent(in) :: file_name
    integer,optional,intent(out) :: rc 

    type (NetCDF4_FileFormatter), pointer :: formatter

    type(StringNetCDF4_FileFormatterMapIterator) :: iter
    integer :: status
    character(len=*), parameter :: Iam = "HistoryCollection::find()"
    logical :: f_exist

    iter = this%formatters%find(trim(file_name))
    if (iter /= this%formatters%end()) then
       formatter => this%formatters%at(trim(file_name))
    else
       allocate(formatter)
       inquire(file=file_name, exist=f_exist)
       if(.not. f_exist) then 
         call formatter%create(trim(file_name),rc=status)
         _VERIFY(status)
         call formatter%write(this%fmd, rc=status)
         _VERIFY(status)
       else
          call formatter%open(trim(file_name), pFIO_WRITE)
       endif
       call this%formatters%insert( trim(file_name),formatter)
    end if
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

    iter = this%formatters%begin()
    do while (iter /= this%formatters%end())
      file_name => iter%key()
      f_ptr => this%formatters%at(file_name)
      call f_ptr%close()
      call iter%next()
    enddo
    call this%formatters%clear()
    _RETURN(_SUCCESS)
  end subroutine clear

end module pFIO_HistoryCollectionMod


module pFIO_HistoryCollectionVectorMod
   use pFIO_ThrowMod
   use pFIO_HistoryCollectionMod
   
   ! Create a map (associative array) between names and pFIO_Attributes.
   
#define _type type (HistoryCollection)
#define _vector HistoryCollectionVector
#define _iterator HistoryCollectionVectorIterator

#define _FTL_THROW pFIO_throw_exception

#include "templates/vector.inc"
   
end module pFIO_HistoryCollectionVectorMod


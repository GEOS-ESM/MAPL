#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_AbstractCollectionMod

   implicit none
   private

   public :: AbstractCollection

   type, abstract :: AbstractCollection
   contains
      procedure(find), deferred :: find   
   end type AbstractCollection

   abstract interface

      function find(this, file_name, rc) result(formatter)
         use pFIO_NetCDF4_FileFormatterMod, only: NetCDF4_FileFormatter
         import AbstractCollection
         class(AbstractCollection), intent(inout) :: this
         character(len=*), intent(in) :: file_name
         integer, optional, intent(out) :: rc
         type(NetCDF4_FileFormatter), pointer :: formatter
      end function find

   end interface

end module pFIO_AbstractCollectionMod

#include "MAPL_Generic.h"

module MAPL_newCFIOitemMod
  use ESMF
  use, intrinsic :: ISO_C_BINDING
  implicit none
  
  private

  public :: ItemTypeScalar
  public :: ItemTypeVector

  enum,bind(c)
     enumerator :: ItemTypeScalar = 0
     enumerator :: ItemTypeVector = 1
  end enum

  type, public :: newCFIOitem
     integer :: itemType
     character(len=ESMF_MAXSTR) :: xname, yname
     type(ESMF_Field) :: xfield, yfield
     type(ESMF_Field) :: xfield_out,yfield_out
  end type newCFIOitem

end module MAPL_newCFIOitemMod

module MAPL_newCFIOitemVectorMod
  use MAPL_newCFIOitemMod

#define _type type(newCFIOitem)
#define _allocatable
#define _vector newCFIOitemVector
#define _iterator newCFIOitemVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _type_type

end module MAPL_newCFIOitemVectorMod

#include "MAPL_Generic.h"

module MAPL_GriddedIOitemMod
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

  type, public :: GriddedIOitem
     integer :: itemType
     character(len=ESMF_MAXSTR) :: xname, yname
     type(ESMF_Field) :: xfield, yfield
     type(ESMF_Field) :: xfield_out,yfield_out
  end type GriddedIOitem

end module MAPL_GriddedIOitemMod

module MAPL_GriddedIOitemVectorMod
  use MAPL_GriddedIOitemMod

#define T GriddedIOitem 
#define T_polymorphic
#define Vector GriddedIOitemVector
#define VectorIterator GriddedIOitemVectorIterator
#define VectorRIterator GriddedIOitemVectorRIterator
#include "vector/template.inc"
#undef VectorRIterator
#undef VectorIterator
#undef Vector
#undef T_polymorphic
#undef T

end module MAPL_GriddedIOitemVectorMod

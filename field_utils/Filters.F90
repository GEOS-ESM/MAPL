type :: Filter
contains
   generic :: filter => filterR4, filterI4
   procedure :: filterR4
   procedure :: filterI4
end type Filter

contains

   logical function filterR4(this, u, rc)
      class(Filter), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: u
      integer, optional, intent(out) :: rc
      
      if(present(rc)) rc = -1
      filterR4 = .FALSE.

   end function filterR4

   logical function filterI4(this, u, rc)
      class(Filter), intent(in) :: this
      integer(kind=ESMF_KIND_I4), intent(in) :: u
      integer, optional, intent(out) :: rc
      
      if(present(rc)) rc = -1
      filterI4 = .FALSE.

   end function filterI4

end type Filter

type, extends(Filter)
abstract interface
   logical function CompositeFilter(filters)
      class(Filter), intent(in) :: filters
   end function CompositeFilter
end interface

type :: CompositeFilter
   class(Filter), 

type, abstract :: SimpleFilter
contains
   generic :: filter => filterR4, filterI4!, filterR8, filterI8 
   procedure(R4Filter), deferred :: filterR4
!   procedure(R8Filter), deferred :: filterR8
   procedure(I4Filter), deferred :: filterI4
!   procedure(I8Filter), deferred :: filterI8
end type SimpleFilter

abstract interface
   logical function R4Filter(val)
      real(kind=ESMF_KIND_R4), intent(in) :: val
   end function R4Filter
   logical function I4Filter(val)
      integer(kind=ESMF_KIND_I4), intent(in) :: val
   end function I4Filter
end interface

type, abstract :: BinaryFilter
contains
   generic :: filter => filterR4
end type BinaryFilter

logical, parameter :: AND = .TRUE.
logical, parameter :: OR = .FALSE.

type, abstract :: FuncObj(nargs)
contains
   procedure(), deferred :: apply
type, abstract :: HomogeneousFunctObj(k)
   integer, kind :: k

type, abstract :: Filter(k, n)
   integer, kind :: k = ESMF_KIND_R4
   integer, len  :: n = 1
contains
   generic :: filter => filterR4, filterI4
   procedure(FilterR4), deferred :: R4Filter
   procedure(FilterI4), deferred :: I4Filter
end type Filter

abstract interface

   elemental function BinaryR4(a, b) result(c)
      real(kind=ESMF_KIND_R4) :: c
      real(kind=ESMF_KIND_R4), intent(in) :: a, b
   end function BinaryR4

   elemental function UnaryR4(a) result(b)
      real(kind=ESMF_KIND_R4) :: b
      real(kind=ESMF_KIND_R4), intent(in) :: a
   end function UnaryR4
      

      
   logical function FilterESMFTypeKind(this, r4vals, i4vals, rc)
      class(Filter), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: r4values(:)
      integer(kind=ESMF_KIND_I4), intent(in) :: i4values(:)
   end function FilterESMFTypeKind

   logical function FilterR4(this, values, rc)
      class(Filter), intent(in) :: this
      real(kind-ESMF_KIND_R4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc
   end function FilterR4
   logical function FilterI4(this, values, rc)
      class(Filter), intent(in) :: this
      real(kind-ESMF_KIND_I4), intent(in) :: values(:)
      integer, optional, intent(out) :: rc
   end function FilterI4
end interface

type, extends(Filter) :: CompositeFilter
   class(Filter), allocatable :: left
   class(Filter), allocatable :: right
   logical :: join = AND
contains
   procedure :: R4Filter => composite_filterR4
   procedure :: I4Filter => composite_filterI4
end type CompositeFilter

type, extends(Filter) :: 
logical function composite_filterR4(values
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

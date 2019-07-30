#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"
! A RegridderSpec is used to indicate which subclass of regridder will be used.
module MAPL_RegridderSpecMod
   use MAPL_BaseMod, only: MAPL_UNDEF
   use MAPL_KeywordEnforcerMod
   use ESMF
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

      
   public :: RegridderSpec
   public :: RegridderTypeSpec 

   public :: REGRID_METHOD_BILINEAR
   public :: REGRID_METHOD_BILINEAR_ROTATE
   public :: REGRID_METHOD_CONSERVE
   public :: REGRID_METHOD_VOTE
   public :: REGRID_METHOD_FRACTION

   public :: TILING_METHODS

   enum, bind(c)
      enumerator :: REGRID_METHOD_BILINEAR
      enumerator :: REGRID_METHOD_BILINEAR_ROTATE
      enumerator :: REGRID_METHOD_CONSERVE
      enumerator :: REGRID_METHOD_VOTE
      enumerator :: REGRID_METHOD_FRACTION
      enumerator :: UNSPECIFIED_REGRID_METHOD = -1
   end enum
   integer, parameter :: TILING_METHODS(3) = [REGRID_METHOD_CONSERVE,REGRID_METHOD_VOTE,REGRID_METHOD_FRACTION]


   type :: RegridderTypeSpec
      character(len=:), allocatable :: grid_type_in
      character(len=:), allocatable :: grid_type_out
      integer :: regrid_method = UNSPECIFIED_REGRID_METHOD
   contains
      procedure :: less_than
      generic :: operator (<) => less_Than
   end type RegridderTypeSpec

   type :: RegridderSpec
      type (ESMF_Grid) :: grid_in
      type (ESMF_Grid) :: grid_out
      integer :: regrid_method = UNSPECIFIED_REGRID_METHOD
      integer :: hints = 0
   contains
      procedure :: equals
      procedure :: get_grid_type
      generic :: operator (==) => equals
      procedure :: spec_less_than
      generic :: operator (<) => spec_less_than
   end type RegridderSpec
   

   interface RegridderTypeSpec
      module procedure new_RegridderTypeSpec
   end interface RegridderTypeSpec

   interface RegridderSpec
      module procedure newRegridderSpec
   end interface RegridderSpec

   character(len=*), parameter :: MOD_NAME = 'MAPL_RegridderSpec_private::'

contains


   function new_RegridderTypeSpec(grid_type_in, grid_type_out, regrid_method, unusable, rc) result(spec)
      type (RegridderTypeSpec) :: spec
      character(len=*), intent(in) :: grid_type_in
      character(len=*), intent(in) :: grid_type_out
      integer, intent(in) :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      spec%grid_type_in = grid_type_in
      spec%grid_type_out = grid_type_out
      spec%regrid_method = regrid_method

   end function new_RegridderTypeSpec

   logical function less_than(a, b)
      class (RegridderTypeSpec), intent(in) :: a
      type (RegridderTypeSpec), intent(in) :: b
      logical :: greater_than

      ! Compare methods

      if (a%regrid_method /= b%regrid_method) then
         less_than = (a%regrid_method < b%regrid_method) ! strictly less than
         return
      end if ! else tie
         
      ! To get here, methods are the same for a and b.
      if (any(a%regrid_method == TILING_METHODS)) then
         less_than = .false.
         ! do not care about grid types in this case
         return
      end if

      ! Compare grid types
      if (a%grid_type_in /= b%grid_type_in) then
         less_than = (a%grid_type_in < b%grid_type_in) ! strictly less than
         return
      end if ! else tie

      ! Compare out grid types
      less_than = (a%grid_type_out < b%grid_type_out)
      return
      
   end function less_than

   function newRegridderSpec(grid_in, grid_out, regrid_method, unusable, hints, rc) result(spec)
      type (RegridderSpec) :: spec
      type (ESMF_Grid), intent(in) :: grid_in
      type (ESMF_Grid), intent(in) :: grid_out
      integer :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: hints
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      spec%grid_in = grid_in
      spec%grid_out = grid_out
      spec%regrid_method = regrid_method

      if (present(hints)) spec%hints = hints

   end function newRegridderSpec

   !---------------
   ! Two regridders are equivalent if their specs are identical.  This
   ! can be used to check if a given regridder is already contained in
   ! the regridder_manager without instantiating the regridder.
   ! Currently the implementation assumes that two regridders are identical
   ! if the corresponding grids are the same and the same method is in use.
   ! This will need to be changed to incorporate LocStreams.
   !---------------
   logical function equals(a, b)
      use MAPL_GridManagerMod, only: get_factory_id
      class (RegridderSpec), intent(in) :: a
      type (RegridderSpec), intent(in) :: b

      equals = &
           & (get_factory_id(a%grid_in) == get_factory_id(b%grid_in)) .and. &
           & (get_factory_id(a%grid_out) == get_factory_id(b%grid_out)) .and. &
           & (a%regrid_method == b%regrid_method) .and. &
           & (a%hints == b%hints)

   end function equals

   subroutine get_grid_type(this,unusable,InputGridType,OutputGridType,rc)
      class (RegridderSpec), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(out) :: InputGridType
      character(len=*), optional, intent(out) :: OutputGridType
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME//'get_grid_type'

      if (present(InputGridType)) then
         call ESMF_AttributeGet(this%grid_in,'GridType',InputGridType,rc=status)
         _VERIFY(status)
      end if
      if (present(OutputGridType)) then
         call ESMF_AttributeGet(this%grid_out,'GridType',OutputGridType,rc=status)
         _VERIFY(status)
      end if
      _RETURN(_SUCCESS)

   end subroutine get_grid_type

   logical function spec_less_than(a, b)
      use MAPL_GridManagerMod, only: get_factory_id
      class (RegridderSpec), intent(in) :: a
      type (RegridderSpec), intent(in) :: b
      integer (kind=INT64) :: a_in_id, b_in_id
      integer (kind=INT64) :: a_out_id, b_out_id

      integer :: a_esmf_method, b_esmf_method
      
      select case (a%regrid_method)
      case (REGRID_METHOD_CONSERVE, REGRID_METHOD_VOTE, REGRID_METHOD_FRACTION)
         a_esmf_method = REGRID_METHOD_CONSERVE
      case default
         a_esmf_method = a%regrid_method
      end select

      select case (b%regrid_method)
      case (REGRID_METHOD_CONSERVE, REGRID_METHOD_VOTE, REGRID_METHOD_FRACTION)
         b_esmf_method = REGRID_METHOD_CONSERVE
      case default
         b_esmf_method = b%regrid_method
      end select

      if (a_esmf_method > b_esmf_method) then
         spec_less_than = .false.
         return
      elseif (a_esmf_method < b_esmf_method) then
         spec_less_than = .true.
         return
      end if

      a_in_id = get_factory_id(a%grid_in)
      b_in_id = get_factory_id(b%grid_in)
      if (a_in_id > b_in_id) then
         spec_less_than = .false.
         return
      elseif (a_in_id < b_in_id) then
         spec_less_than = .true.
         return
      end if
         
      a_out_id = get_factory_id(a%grid_out)
      b_out_id = get_factory_id(b%grid_out)
      if (a_out_id > b_out_id) then
         spec_less_than = .false.
         return
      elseif (a_out_id < b_out_id) then
         spec_less_than = .true.
         return
      end if

      spec_less_than = .false.
      return
         
   end function spec_less_than

end module MAPL_RegridderSpecMod
#undef _UNUSED_DUMMY

module MAPL_RegridderSpecRouteHandleMapMod
   use MAPL_RegridderSpecMod
   use ESMF
   
#define _key type(RegridderSpec)
#define _key_less_than_defined   
#define _value type (ESMF_RouteHandle)

#define _map RegridderSpecRouteHandleMap
#define _iterator RegridderSpecRouteHandleMapIterator
#define _alt
#include "templates/map.inc"
   
end module MAPL_RegridderSpecRouteHandleMapMod


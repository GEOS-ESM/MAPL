#include "MAPL_Generic.h"

module MAPL_RegridderManager_private
   use MAPL_GridManagerMod
   use MAPL_RegridderTypeSpec
   use MAPL_RegridderSpec
   use MAPL_RegridderVectorMod
   use MAPL_RegridderTypeSpecRegridderMapMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_AbstractRegridderMod
   use MAPL_IdentityRegridderMod
   use ESMF
   use MAPL_EsmfRegridderMod
   use mapl_RegridMethods
   implicit none
   private

   public :: RegridderManager

   ! Private set of pre-computed route-handles.
   ! Convervative, Tiling, and Voting can all use the same route-handle.
   ! Despite being treated as "different" grids.

   type :: RegridderManager
      private
      logical :: initialized = .false.
      type (RegridderTypeSpecRegridderMap) :: prototypes
      type (RegridderVector) :: regridders
   contains
      procedure :: init
      procedure :: add_prototype_by_spec
      procedure :: add_prototype_by_grid_types
      generic :: add_prototype => add_prototype_by_spec
      generic :: add_prototype => add_prototype_by_grid_types
!!$      procedure :: make_regridder_from_fields
      procedure :: make_regridder_from_grids
      generic :: make_regridder => make_regridder_from_grids
!!$      generic :: make_regridder => make_regridder_from_fields
      procedure :: delete_regridder
   end type RegridderManager


   character(len=*), parameter :: MOD_NAME = 'MAPL_RegridderManager_private::'

contains


   subroutine add_prototype_by_spec(this, regridder_type, prototype)
      class (RegridderManager), intent(inout) :: this
      type (RegridderTypeSpec), intent(in) :: regridder_type
      class (AbstractRegridder), intent(in) :: prototype

      call this%prototypes%insert(regridder_type, prototype)

   end subroutine add_prototype_by_spec

   subroutine add_prototype_by_grid_types(this, grid_type_in, grid_type_out, regrid_method, prototype)
      class (RegridderManager), intent(inout) :: this
      character(len=*), intent(in) :: grid_type_in
      character(len=*), intent(in) :: grid_type_out
      integer, intent(in) :: regrid_method
      class (AbstractRegridder), intent(in) :: prototype

      type (RegridderTypeSpec) :: regridder_type

      regridder_type = RegridderTypeSpec(grid_type_in, grid_type_out, regrid_method)
      call this%add_prototype(regridder_type, prototype)

   end subroutine add_prototype_by_grid_types

   subroutine delete_regridder(this, regridder)
      class (RegridderManager), intent(inout) :: this
      class (AbstractRegridder), pointer :: regridder

      type (RegridderVectorIterator) :: iter
      class (AbstractRegridder), pointer :: match

      iter = this%regridders%begin()
      do while (iter /= this%regridders%end())
         match => iter%get()
         if (associated(match, regridder)) then
            call this%regridders%erase(iter)
            exit
         end if
         call iter%next()
      end do

   end subroutine delete_regridder

   function make_regridder_from_grids(this, grid_in, grid_out, regrid_method, unusable, hints, rc) result(regridder)
      class (AbstractRegridder), pointer :: regridder
      class (RegridderManager), intent(inout) :: this
      type (ESMF_Grid), intent(in) :: grid_in
      type (ESMF_Grid), intent(in) :: grid_out
      integer, intent(in) :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: hints
      integer, optional,  intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_regridder_from_grids'
      type (RegridderSpec) :: spec
      integer (kind=ESMF_KIND_I8) :: id_in, id_out

      character(len=:), allocatable :: grid_type_in, grid_type_out
      type (RegridderTypeSpec) :: type_spec
      class (AbstractRegridder), pointer :: prototype

      !---------------
      ! Note:
      ! We need to add LatLon prototype somewhere, and MAPL does not have
      ! a natural initialization.  Other grids can be added during
      ! setServices or initialize of the component that defines the grid.
      !---------------

      _UNUSED_DUMMY(unusable)

      if (.not. this%initialized) then
        call this%init()
      end if

      ! Special case if two grids are the same
      id_in = get_factory_id(grid_in,rc=status)
      _VERIFY(status)
      id_out = get_factory_id(grid_out,rc=status)
      _VERIFY(status)
      if (id_in==id_out) then
         regridder => identity_regridder()
         _RETURN(_SUCCESS)
      end if
      
      ! If manager already has suitable regridder, use it.
      spec = RegridderSpec(grid_in, grid_out, regrid_method, hints=hints)
      regridder => find(this%regridders, spec)
      if (associated(regridder)) then
         _RETURN(_SUCCESS)
      end if


      ! Else, if we have a prototype, clone it and configure
      grid_type_in = get_grid_type(grid_in, rc=status)
      _VERIFY(status)
      grid_type_out = get_grid_type(grid_out, rc=status)
      _VERIFY(status)
      
      type_spec = RegridderTypeSpec(grid_type_in, grid_type_out, regrid_method)

      prototype => this%prototypes%at(type_spec)
     
      if (associated(prototype)) then
        call this%regridders%push_back(prototype%clone())
        regridder => this%regridders%back()
        call regridder%initialize(spec, rc=status)
        _VERIFY(status)
        _RETURN(_SUCCESS)
      end if
      
      print*,__FILE__,__LINE__,'I cannot create this regridder. types are <',&
           & grid_type_in,',',grid_type_out,'>'

      ! Do not know how to make this type of regridder
      _RETURN(_FAILURE)

   contains

      function find(vector, spec) result(match)
         class (AbstractRegridder), pointer :: match
         type (RegridderVector), intent(in) :: vector
         type (RegridderSpec), intent(in) :: spec

         type (RegridderVectorIterator) :: iter

         iter = this%regridders%begin()
         do while (iter /= this%regridders%end())
            match => iter%get()
            if (match%get_spec() == spec) return
           call iter%next()
        end do

        match => null()

      end function find


      function get_grid_type(grid, unusable, rc) result(grid_type)
         character(len=:), allocatable :: grid_type
         type (ESMF_Grid), intent(in) :: grid
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         character(len=ESMF_MAXSTR) :: buffer

         call ESMF_AttributeGet(grid, 'GridType', buffer, rc=status)
         _VERIFY(status)

         grid_type = trim(buffer)

         _RETURN(_SUCCESS)

      end function get_grid_type

   end function make_regridder_from_grids


   subroutine init(this)
     use MAPL_LatLonToLatLonRegridderMod
     use MAPL_ConservativeRegridderMod
     use MAPL_VotingRegridderMod
     use MAPL_FractionalRegridderMod
     class (RegridderManager), intent(inout) :: this

     type (ConservativeRegridder) :: regridder1
     type (LatLonToLatLonRegridder) :: regridder2
     type (VotingRegridder) :: regridder3
     type (FractionalRegridder) :: regridder4
        
     call this%add_prototype('LatLon', 'LatLon', REGRID_METHOD_CONSERVE, regridder1)
     call this%add_prototype('LatLon', 'LatLon', REGRID_METHOD_BILINEAR, regridder2)
     call this%add_prototype('LatLon', 'LatLon', REGRID_METHOD_VOTE, regridder3)
     call this%add_prototype('LatLon', 'LatLon', REGRID_METHOD_FRACTION, regridder4)
     this%initialized = .true.

   end subroutine init

end module MAPL_RegridderManager_private



module MAPL_RegridderManagerMod
   use MAPL_RegridderManager_private
   implicit none
   private

   public :: RegridderManager
   public :: regridder_manager

   type (RegridderManager), target, save :: regridder_manager

end module MAPL_RegridderManagerMod

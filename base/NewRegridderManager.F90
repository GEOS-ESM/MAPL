#include "MAPL_Generic.h"

module mapl_NewRegridderManager_private
   use MAPL_GridManagerMod
   use MAPL_RegridderSpec
   use MAPL_RegridderVectorMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_AbstractRegridderMod
   use MAPL_IdentityRegridderMod
   use ESMF
   use mapl_HorizontalFluxRegridder
   use MAPL_EsmfRegridderMod
   use mapl_RegridMethods
   implicit none
   private

   public :: NewRegridderManager

   ! Private set of pre-computed route-handles.
   ! Convervative, Tiling, and Voting can all use the same route-handle.
   ! Despite being treated as "different" grids.

   type :: NewRegridderManager
      private
      logical :: initialized = .false.
      type(RegridderVector) :: prototypes
      type(RegridderVector) :: regridders
   contains
      procedure :: add_prototype
      procedure :: find_prototype
      procedure :: make_regridder_
      generic :: make_regridder => make_regridder_
      procedure :: is_initialized
      procedure :: initialize
   end type NewRegridderManager

   character(len=*), parameter :: MOD_NAME = 'mapl_NewRegridderManager_private::'

contains

   subroutine add_prototype(this, prototype)
      class(NewRegridderManager), intent(inout) :: this
      class(AbstractRegridder), intent(in) :: prototype

      call this%prototypes%push_back(prototype)

   end subroutine add_prototype


   function make_regridder_(this, grid_in, grid_out, regrid_method, unusable, hints, rc) result(regridder)
      class (AbstractRegridder), pointer :: regridder
      class (NewRegridderManager), target, intent(inout) :: this
      type (ESMF_Grid), intent(in) :: grid_in
      type (ESMF_Grid), intent(in) :: grid_out
      integer, intent(in) :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: hints
      integer, optional,  intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam= MOD_NAME // 'make_regridder_from_grids'
      type (RegridderSpec) :: spec
      integer(ESMF_KIND_I8) :: id_in, id_out

      class(AbstractRegridder), pointer :: prototype

      _UNUSED_DUMMY(unusable)

      if (.not. this%is_initialized()) call this%initialize()

      id_in = get_factory_id(grid_in,rc=status)
      _VERIFY(status)
      id_out = get_factory_id(grid_out,rc=status)
      _VERIFY(status)
      ! Special case if two grids are the same
      if (id_in == id_out) then
         regridder => identity_regridder()
         _RETURN(_SUCCESS)
      end if

      ! If manager already has suitable regridder, use it.
      spec = RegridderSpec(grid_in, grid_out, regrid_method, hints=hints)

      regridder => find(this%regridders, spec)
      if (associated(regridder)) then
         _RETURN(_SUCCESS)
      end if

      ! Else build from prototype
      prototype => this%find_prototype(spec, rc=status)
      _VERIFY(status)
      
      call this%regridders%push_back(prototype)
      regridder => this%regridders%back()
      call regridder%initialize(spec, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   contains

      function find(vector, spec) result(match)
         class (AbstractRegridder), pointer :: match
         type (RegridderVector), target :: vector
         type (RegridderSpec), intent(in) :: spec

         type (RegridderVectorIterator) :: iter

         iter = vector%begin()
         do while (iter /= vector%end())
            match => iter%get()
            if (match%get_spec() == spec) return
           call iter%next()
        end do

        match => null()

     end function find

   end function make_regridder_

   function find_prototype(this, spec, unusable, rc) result(prototype)
      class(AbstractRegridder), pointer :: prototype
      class(NewRegridderManager), target, intent(in) :: this
      type(RegridderSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: supports
      type(RegridderVectorIterator) :: iter

      _UNUSED_DUMMY(unusable)

      iter = this%prototypes%begin()
      do while (iter /= this%prototypes%end())
         prototype => iter%get()
         supports = prototype%supports(spec,__RC__)
         if (supports) then
            _RETURN(_SUCCESS)
         end if
         call iter%next()
      end do

      ! not found
      _RETURN(_FAILURE)
   end function find_prototype

   logical function is_initialized(this)
      class(NewRegridderManager), intent(in) :: this
      is_initialized = this%initialized
   end function is_initialized

   subroutine initialize(this)
      class(NewRegridderManager), intent(inout) :: this

      type(EsmfRegridder) :: esmf_regridder
      type(HorizontalFluxRegridder) :: horizontal_flux_regridder

      if (this%is_initialized()) return
      
      call this%add_prototype(esmf_regridder)
      call this%add_prototype(horizontal_flux_regridder)
      
      this%initialized = .true.
   end subroutine initialize

end module mapl_NewRegridderManager_private



module mapl_NewRegridderManager
   use mapl_NewRegridderManager_private
   implicit none
   private

   public :: new_regridder_manager

   type (NewRegridderManager), target, save :: new_regridder_manager

end module mapl_NewRegridderManager

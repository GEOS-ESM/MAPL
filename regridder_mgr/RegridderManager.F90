#include "MAPL_Generic.h"
module mapl3g_RegridderManager
   use mapl3g_Geom_API, only: GeomManager, get_geom_manager
   use mapl3g_RegridderSpec
   use mapl3g_Regridder
   use mapl3g_NullRegridder
   use mapl3g_RegridderFactory

   use mapl3g_RegridderFactoryVector
   use mapl3g_RegridderSpecVector
   use mapl3g_RegridderVector
   use mapl3g_EsmfRegridderFactory

   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: RegridderManager
   public :: regridder_manager ! singleton
   public :: get_regridder_manager

   type :: RegridderManager
      private
      type(RegridderFactoryVector) :: factories
      ! Next two vectors grow together
      type(RegridderSpecVector) :: specs
      type(RegridderVector) :: regridders
      type(GeomManager), pointer :: geom_manager => null()
   contains
      procedure :: get_regridder
      procedure :: add_factory
      procedure :: make_regridder
      procedure :: add_regridder
      procedure :: delete_regridder
   end type RegridderManager

   interface RegridderManager
      procedure new_RegridderManager
   end interface RegridderManager

   type(RegridderManager), target, protected :: regridder_manager

contains

   function new_RegridderManager(geom_manager) result(mgr)
      type(RegridderManager) :: mgr
      type(GeomManager), target, optional, intent(in) :: geom_manager

      ! Load default factories

      mgr%geom_manager => get_geom_manager()
      if (present(geom_manager)) then
         mgr%geom_manager => geom_manager
      end if
      
      call mgr%add_factory(EsmfRegridderFactory())
!!$      call mgr%add_factory(horzHorzFluxRegridderFactory())

   end function new_RegridderManager


   ! TODO - do we need an RC here for duplicate name?
   subroutine add_factory(this, factory)
      class(RegridderManager), intent(inout) :: this
      class(RegridderFactory), intent(in) :: factory
      call this%factories%push_back(factory)
   end subroutine add_factory


   subroutine add_regridder(this, spec, regriddr)
      class(RegridderManager), intent(inout) :: this
      class(RegridderSpec), intent(in) :: spec
      class(Regridder), intent(in) :: regriddr

      call this%specs%push_back(spec)
      call this%regridders%push_back(regriddr)
     
   end subroutine add_regridder

   subroutine delete_regridder(this, spec, rc)
      class(RegridderManager), target, intent(inout) :: this
      class(RegridderSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(RegridderSpecVectorIterator) :: spec_iter
      type(RegridderVectorIterator) :: regridder_iter

      associate (specs => this%specs, regridders => this%regridders)
        associate (b => specs%begin(), e => specs%end())

          spec_iter = find(b, e, spec)
          _ASSERT(spec_iter /= e, 'spec not found in RegridderManager.')

          regridder_iter = regridders%begin() + (spec_iter - b)
          regridder_iter = regridders%erase(regridder_iter)

          spec_iter = specs%erase(spec_iter)

        end associate
      end associate

      _RETURN(_SUCCESS)
   end subroutine delete_regridder

   function get_regridder(this, spec, rc) result(regriddr)
      class(Regridder), pointer :: regriddr
      class(RegridderManager), target, intent(inout) :: this
      class(RegridderSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      class(Regridder), allocatable :: tmp_regridder

      associate (b => this%specs%begin(), e => this%specs%end())
        associate (iter => find(b, e, spec))
          if (iter /= e) then
             regriddr => this%regridders%of((iter-b+1))
             _RETURN(_SUCCESS)
          end if

          tmp_regridder = this%make_regridder(spec, _RC)
          call this%add_regridder(spec, tmp_regridder)
          regriddr => this%regridders%back()

        end associate
      end associate

      _RETURN(_SUCCESS)
   end function get_regridder

   function make_regridder(this, spec, rc) result(regriddr)
      class(Regridder), allocatable :: regriddr
      class(RegridderManager), target, intent(in) :: this
      class(RegridderSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      class(RegridderFactory), pointer :: factory

      regriddr = NULL_REGRIDDER
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         if (factory%supports(spec%get_param())) then
            deallocate(regriddr) ! workaround for gfortran 12.3
            regriddr = factory%make_regridder(spec, _RC)
            call regriddr%set_geom_manager(this%geom_manager)
            _RETURN(_SUCCESS)
         end if
      end do

      _FAIL('No factory found to make regridder for spec.')
   end function make_regridder

   function get_regridder_manager() result(regridder_mgr)
      type(RegridderManager), pointer :: regridder_mgr
      logical :: init = .false.

      if (.not. init) then
         regridder_manager = RegridderManager()
         init = .true.
      end if

      regridder_mgr => regridder_manager
         
   end function get_regridder_manager

end module mapl3g_RegridderManager

#include "MAPL_Generic.h"

module mapl3g_AspectCollection
   use mapl3g_StateItemAspect

   use mapl3g_GeomAspect
   use mapl3g_UnitsAspect

   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private

   public :: AspectCollection

   type AspectCollection
      private
      type(GeomAspect), allocatable :: geom_aspect
      type(UnitsAspect), allocatable :: units_aspect
   contains
      procedure :: get_aspect ! polymorphic
      procedure :: has_aspect ! polymorphic
      procedure :: set_aspect ! polymorphic

      procedure :: get_geom_aspect
      procedure :: set_geom_aspect
      procedure :: get_units_aspect
      procedure :: set_units_aspect
      
   end type AspectCollection

   interface AspectCollection
      procedure :: new_AspectCollection
   end interface AspectCollection
   
contains

   function new_AspectCollection( unusable, &
        geom_aspect &
        ) result(collection)
      type(AspectCollection) :: collection
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(GeomAspect), optional, intent(in) :: geom_aspect

      if (present(geom_aspect)) then
         collection%geom_aspect = geom_aspect
      end if

      _UNUSED_DUMMY(unusable)
   end function new_AspectCollection

   function get_aspect(this, name, rc) result(aspect)
      class(StateItemAspect), pointer :: aspect
      class(AspectCollection), target :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      aspect => null()
      select case (name)
      case ('GEOM')
         aspect => this%get_geom_aspect()
      case ('UNITS')
         aspect => this%get_units_aspect()
      case default
         _FAIL('unknown aspect type: '//name)
      end select

      _RETURN(_SUCCESS)
   end function get_aspect

   logical function has_aspect(this, name)
      class(AspectCollection), target :: this
      character(*), intent(in) :: name

      select case (name)
      case ('GEOM', 'UNITS')
         has_aspect = .true.
      case default
         has_aspect = .false.
      end select

   end function has_aspect

   subroutine set_aspect(this, aspect, rc)
      class(AspectCollection) :: this
      class(StateItemAspect), target, intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type (aspect)
      type is (GeomAspect)
         this%geom_aspect = aspect
      type is (UnitsAspect)
         this%units_aspect = aspect
      class default
         _FAIL('unsupported aspect type: ')
      end select
         
      _RETURN(_SUCCESS)
   end subroutine set_aspect

   function get_geom_aspect(this) result(geom_aspect)
      type(GeomAspect), pointer :: geom_aspect
      class(AspectCollection), target, intent(in) :: this
      geom_aspect => null()
      if (allocated(this%geom_aspect)) then
         geom_aspect => this%geom_aspect
      end if
   end function get_geom_aspect

   subroutine set_geom_aspect(this, geom_aspect)
      class(AspectCollection), intent(inout) :: this
      type(GeomAspect), intent(in) :: geom_aspect
      this%geom_aspect = geom_aspect
   end subroutine set_geom_aspect

   function get_units_aspect(this) result(units_aspect)
      type(UnitsAspect), pointer :: units_aspect
      class(AspectCollection), target, intent(in) :: this
      units_aspect => null()
      if (allocated(this%units_aspect)) then
         units_aspect => this%units_aspect
      end if
   end function get_units_aspect

   subroutine set_units_aspect(this, units_aspect)
      class(AspectCollection), intent(inout) :: this
      type(UnitsAspect), intent(in) :: units_aspect
      this%units_aspect = units_aspect
   end subroutine set_units_aspect

end module mapl3g_AspectCollection


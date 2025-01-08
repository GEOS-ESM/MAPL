#include "MAPL_Generic.h"

module mapl3g_AspectCollection
   use mapl3g_StateItemAspect

   use mapl3g_GeomAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_FrequencyAspect
   use mapl3g_UngriddedDimsAspect

   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   implicit none
   private

   public :: AspectCollection

   type AspectCollection
      private
      type(GeomAspect), allocatable :: geom_aspect
      type(UnitsAspect), allocatable :: units_aspect
      type(TypekindAspect), allocatable :: typekind_aspect
      type(UngriddedDimsAspect), allocatable :: ungridded_dims_aspect
      type(FrequencyAspect), allocatable :: frequency_aspect
   contains
      procedure :: get_aspect ! polymorphic
      procedure :: has_aspect ! polymorphic
      procedure :: set_aspect ! polymorphic

      procedure :: get_geom_aspect
      procedure :: set_geom_aspect

      procedure :: get_units_aspect
      procedure :: set_units_aspect

      procedure :: get_typekind_aspect
      procedure :: set_typekind_aspect

      procedure :: get_ungridded_dims_aspect
      procedure :: set_ungridded_dims_aspect
      
      procedure :: get_frequency_aspect
      procedure :: set_frequency_aspect
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
      case ('TYPEKIND')
         aspect => this%get_typekind_aspect()
      case ('UNGRIDDED_DIMS')
         aspect => this%get_ungridded_dims_aspect()
      case ('FREQUENCY')
         aspect => this%get_frequency_aspect()
      case default
         _FAIL('unknown aspect type: '//name)
      end select

      _RETURN(_SUCCESS)
   end function get_aspect

   logical function has_aspect(this, name)
      class(AspectCollection), target :: this
      character(*), intent(in) :: name

      select case (name)
      case ('GEOM', 'UNITS', 'UNGRIDDED_DIMS', 'FREQUENCY')
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
      type is (TypekindAspect)
         this%typekind_aspect = aspect
      type is (UngriddedDimsAspect)
         this%ungridded_dims_aspect = aspect
      type is (FrequencyAspect)
         this%frequency_aspect = aspect
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

   function get_typekind_aspect(this) result(typekind_aspect)
      type(TypekindAspect), pointer :: typekind_aspect
      class(AspectCollection), target, intent(in) :: this

      typekind_aspect => null()
      if (allocated(this%typekind_aspect)) then
         typekind_aspect => this%typekind_aspect
      end if
   end function get_typekind_aspect

   subroutine set_typekind_aspect(this, typekind_aspect)
      class(AspectCollection), intent(inout) :: this
      type(TypekindAspect), intent(in) :: typekind_aspect
      this%typekind_aspect = typekind_aspect
   end subroutine set_typekind_aspect

   function get_ungridded_dims_aspect(this) result(ungridded_dims_aspect)
      type(UngriddedDimsAspect), pointer :: ungridded_dims_aspect
      class(AspectCollection), target, intent(in) :: this
      ungridded_dims_aspect => null()
      if (allocated(this%ungridded_dims_aspect)) then
         ungridded_dims_aspect => this%ungridded_dims_aspect
      end if
   end function get_ungridded_dims_aspect

   subroutine set_ungridded_dims_aspect(this, ungridded_dims_aspect)
      class(AspectCollection), intent(inout) :: this
      type(UngriddedDimsAspect), intent(in) :: ungridded_dims_aspect
      this%ungridded_dims_aspect = ungridded_dims_aspect
   end subroutine set_ungridded_dims_aspect

   function get_frequency_aspect(this) result(frequency_aspect)
      type(FrequencyAspect), pointer :: frequency_aspect
      class(AspectCollection), intent(inout) :: this
      frequency_aspect => null()
      if(allocated(this%frequency_aspect)) then
         frequency_aspect => this%frequency_aspect
      end if
   end function get_frequency_aspect

   subroutine set_frequency_aspect(this, frequency_aspect)
      class(AspectCollection), intent(inout) :: this
      type(FrequencyAspect), intent(in) :: frequency_aspect
      this%frequency_aspect = frequency_aspect
   end subroutine set_frequency_aspect

end module mapl3g_AspectCollection


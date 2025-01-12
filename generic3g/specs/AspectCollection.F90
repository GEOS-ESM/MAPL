#include "MAPL_Generic.h"

module mapl3g_AspectCollection
   use mapl3g_StateItemAspect
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none
   private

   public :: AspectCollection

   type AspectCollection
      private
      type(GeomAspect), allocatable :: geom_aspect
      type(VerticalGridAspect), allocatable :: vertical_grid_aspect
      type(UnitsAspect), allocatable :: units_aspect
      type(TypekindAspect), allocatable :: typekind_aspect
      type(UngriddedDimsAspect), allocatable :: ungridded_dims_aspect
   contains
      procedure :: get_aspect ! polymorphic
      procedure :: has_aspect ! polymorphic
      procedure :: set_aspect ! polymorphic

      procedure :: get_geom_aspect
      procedure :: set_geom_aspect

      procedure :: get_vertical_grid_aspect
      procedure :: set_vertical_grid_aspect

      procedure :: get_units_aspect
      procedure :: set_units_aspect

      procedure :: get_typekind_aspect
      procedure :: set_typekind_aspect

      procedure :: get_ungridded_dims_aspect
      procedure :: set_ungridded_dims_aspect

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
      
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
      case ('VERTICAL')
         aspect => this%get_vertical_grid_aspect()
      case ('UNITS')
         aspect => this%get_units_aspect()
      case ('TYPEKIND')
         aspect => this%get_typekind_aspect()
      case ('UNGRIDDED_DIMS')
         aspect => this%get_ungridded_dims_aspect()
      case default
         _FAIL('unknown aspect type: '//name)
      end select

      _RETURN(_SUCCESS)
   end function get_aspect

   logical function has_aspect(this, name)
      class(AspectCollection), target :: this
      character(*), intent(in) :: name

      select case (name)
      case ('GEOM')
         has_aspect = allocated(this%geom_aspect)
      case ('VERTICAL')
         has_aspect = allocated(this%vertical_grid_aspect)
      case ('UNITS')
         has_aspect = allocated(this%units_aspect)
      case ('TYPEKIND')
         has_aspect = allocated(this%typekind_aspect)
      case ('UNGRIDDED_DIMS')
         has_aspect = allocated(this%ungridded_dims_aspect)
      case default
         has_aspect = .false.
      end select

   end function has_aspect

   subroutine set_aspect(this, aspect, rc)
      class(AspectCollection), target :: this
      class(StateItemAspect), target, intent(in) :: aspect
      integer, optional, intent(out) :: rc

      type(ESMF_Geom) :: geom
      type(ESMF_Typekind_Flag) :: typekind
      integer :: status

      select type (aspect)
      type is (GeomAspect)
         this%geom_aspect = aspect
         ! aux vertical
         if (allocated( this%vertical_grid_aspect)) then
            geom = aspect%get_geom()
            call this%vertical_grid_aspect%set_geom(geom)
         end if
      type is (VerticalGridAspect)
         if (allocated(this%vertical_grid_aspect)) then
            if (allocated(this%vertical_grid_aspect%vertical_grid)) then
            end if
         end if
         this%vertical_grid_aspect = aspect
         if (allocated(this%vertical_grid_aspect%vertical_grid)) then
         end if
      type is (UnitsAspect)
         this%units_aspect = aspect
      type is (TypekindAspect)
         this%typekind_aspect = aspect
         ! aux vertical
         typekind = aspect%get_typekind()
         if (allocated( this%vertical_grid_aspect)) then
            call this%vertical_grid_aspect%set_typekind(typekind)
         end if
      type is (UngriddedDimsAspect)
         this%ungridded_dims_aspect = aspect
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

   function get_vertical_grid_aspect(this) result(vertical_grid_aspect)
      type(VerticalGridAspect), pointer :: vertical_grid_aspect
      class(AspectCollection), target, intent(in) :: this
      vertical_grid_aspect => null()
      if (allocated(this%vertical_grid_aspect)) then
         vertical_grid_aspect => this%vertical_grid_aspect
      end if
   end function get_vertical_grid_aspect

   subroutine set_vertical_grid_aspect(this, vertical_grid_aspect)
      class(AspectCollection), intent(inout) :: this
      type(VerticalGridAspect), intent(in) :: vertical_grid_aspect
      this%vertical_grid_aspect = vertical_grid_aspect
   end subroutine set_vertical_grid_aspect

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


   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(AspectCollection), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      iostat = 0

      call handle('TYPEKIND', iostat=iostat, iomsg=iomsg)
      call handle('GEOM', iostat=iostat, iomsg=iomsg)
      call handle('VERTICAL', iostat=iostat, iomsg=iomsg)
      call handle('UNITS', iostat=iostat, iomsg=iomsg)
      call handle('UNGRIDDED_DIMS', iostat=iostat, iomsg=iomsg)
      
      _UNUSED_DUMMY(v_list)
      _UNUSED_DUMMY(iotype)

   contains

      subroutine handle(name, iostat, iomsg)
         character(*), intent(in) :: name
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(10) :: fmt
         class(StateItemAspect), pointer :: aspect

         ! v_list(1) is indent
         if (size(v_list) > 0) then
            write(fmt,'("(",i0,"x)")', iostat=iostat) v_list(1)
            if (iostat /= 0) return
            write(unit,trim(fmt), iostat=iostat, iomsg=iomsg)
            if (iostat /= 0) return
         end if

         aspect => this%get_aspect(name)
         if (associated(aspect)) then
            write(unit,'(a12,":",1x)',iostat=iostat,iomsg=iomsg) name 
            if (iostat /= 0) return

!#            write(unit, '(DT,a)', iostat=iostat, iomsg=iomsg) aspect, new_line('a')
            write(unit, '(a,a)', iostat=iostat, iomsg=iomsg) aspect%get_description(), new_line('a')
            return
         end if

         write(unit, '(a, a)', iostat=iostat, iomsg=iomsg) '< not allocated >', new_line('a')
      end subroutine handle

   end subroutine write_formatted
   
end module mapl3g_AspectCollection


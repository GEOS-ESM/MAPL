#include "MAPL.h"

module mapl_GeometrySpec_mod
   use mapl_geom_api, only: mapl_GeomSpec
   use mapl_VerticalGrid_mod
   implicit none
   private

   public :: GeometrySpec

   public :: GEOMETRY_NONE
   public :: GEOMETRY_PROVIDER
   public :: GEOMETRY_FROM_PARENT
   public :: GEOMETRY_FROM_CHILD

   enum, bind(c)
      enumerator :: GEOMETRY_NONE
      enumerator :: GEOMETRY_PROVIDER
      enumerator :: GEOMETRY_FROM_PARENT ! MAPL Default
      enumerator :: GEOMETRY_FROM_CHILD
   end enum

   type GeometrySpec
      integer :: kind= GEOMETRY_FROM_PARENT
      character(len=:), allocatable :: provider ! name of child
      class(mapl_GeomSpec), allocatable :: geom_spec
      class(VerticalGrid), allocatable :: vertical_grid
   end type GeometrySpec


   interface GeometrySpec
      module procedure new_GeometrySpecSimple
      module procedure new_GeometryFromChild
      module procedure new_GeometryProvider
   end interface GeometrySpec


contains

   function new_GeometrySpecSimple(kind) result(spec)
      type(GeometrySpec) :: spec
      integer, intent(in) :: kind
      spec%kind = kind
   end function new_GeometrySpecSimple

   function new_GeometryFromChild(provider) result(spec)
      type(GeometrySpec) :: spec
      character(*), intent(in) :: provider
      spec%kind = GEOMETRY_FROM_CHILD
      spec%provider = provider
   end function new_GeometryFromChild

   function new_GeometryProvider(geom_spec, vertical_grid) result(spec)
      type(GeometrySpec) :: spec
      class(mapl_GeomSpec), optional, intent(in) :: geom_spec
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      spec%kind = GEOMETRY_PROVIDER
      if (present(geom_spec)) spec%geom_spec = geom_spec
      if (present(vertical_grid)) spec%vertical_grid = vertical_grid
   end function new_GeometryProvider



end module mapl_GeometrySpec_mod

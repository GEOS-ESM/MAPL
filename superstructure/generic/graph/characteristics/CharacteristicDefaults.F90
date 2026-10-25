#include "MAPL.h"

!------------------------------------------------------------------------------
! CharacteristicDefaults: the values a component supplies after ADVERTISE
! (at GEOM_B) to the item characteristics that were DEFERRED. Each
! characteristic takes what it needs through
! StateItemCharacteristic%resolve_deferred; callers never name a kind.
!
! Graph layer only: holds plain values, no OuterMetaComponent. A new kind
! with a component-supplied value adds its component here.
!------------------------------------------------------------------------------
module mapl_CharacteristicDefaults_mod
   use ESMF, only: ESMF_Geom
   use mapl_GeomId_mod, only: GeomId
   use mapl_VerticalGrid_mod, only: VerticalGrid
   implicit none(type, external)
   private

   public :: CharacteristicDefaults

   type :: CharacteristicDefaults
      private
      logical :: has_geom_value = .false.
      type(ESMF_Geom) :: geom_value
      type(GeomId) :: geom_id_value
      ! Held by value until a vertical grid registry lets nodes share one.
      class(VerticalGrid), allocatable :: vgrid_value
   contains
      procedure :: has_geom => defaults_has_geom
      procedure :: get_geom => defaults_get_geom
      procedure :: get_geom_id => defaults_get_geom_id
      procedure :: has_vgrid => defaults_has_vgrid
      procedure :: get_vgrid => defaults_get_vgrid
      procedure :: set_vgrid => defaults_set_vgrid
   end type CharacteristicDefaults

   interface CharacteristicDefaults
      module procedure new_CharacteristicDefaults
   end interface CharacteristicDefaults

contains

   function new_CharacteristicDefaults(geom, geom_id, vgrid) result(defaults)
      type(ESMF_Geom), optional, intent(in) :: geom
      type(GeomId), optional, intent(in) :: geom_id
      class(VerticalGrid), optional, intent(in) :: vgrid
      type(CharacteristicDefaults) :: defaults

      if (present(geom)) then
         defaults%geom_value = geom
         defaults%has_geom_value = .true.
      end if
      if (present(geom_id)) defaults%geom_id_value = geom_id
      if (present(vgrid)) allocate(defaults%vgrid_value, source=vgrid)
   end function new_CharacteristicDefaults

   logical function defaults_has_geom(this) result(has)
      class(CharacteristicDefaults), intent(in) :: this

      has = this%has_geom_value
   end function defaults_has_geom

   function defaults_get_geom(this) result(geom)
      class(CharacteristicDefaults), intent(in) :: this
      type(ESMF_Geom) :: geom

      geom = this%geom_value
   end function defaults_get_geom

   function defaults_get_geom_id(this) result(geom_id)
      class(CharacteristicDefaults), intent(in) :: this
      type(GeomId) :: geom_id

      geom_id = this%geom_id_value
   end function defaults_get_geom_id

   logical function defaults_has_vgrid(this) result(has)
      class(CharacteristicDefaults), intent(in) :: this

      has = allocated(this%vgrid_value)
   end function defaults_has_vgrid

   subroutine defaults_get_vgrid(this, vgrid)
      class(CharacteristicDefaults), intent(in) :: this
      class(VerticalGrid), allocatable, intent(out) :: vgrid

      if (allocated(this%vgrid_value)) allocate(vgrid, source=this%vgrid_value)
   end subroutine defaults_get_vgrid

   subroutine defaults_set_vgrid(this, vgrid)
      class(CharacteristicDefaults), intent(inout) :: this
      class(VerticalGrid), intent(in) :: vgrid

      if (allocated(this%vgrid_value)) deallocate(this%vgrid_value)
      allocate(this%vgrid_value, source=vgrid)
   end subroutine defaults_set_vgrid

end module mapl_CharacteristicDefaults_mod

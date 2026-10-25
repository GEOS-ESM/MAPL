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
   implicit none(type, external)
   private

   public :: CharacteristicDefaults

   type :: CharacteristicDefaults
      private
      logical :: has_geom_value = .false.
      type(ESMF_Geom) :: geom_value
      type(GeomId) :: geom_id_value
   contains
      procedure :: has_geom => defaults_has_geom
      procedure :: get_geom => defaults_get_geom
      procedure :: get_geom_id => defaults_get_geom_id
   end type CharacteristicDefaults

   interface CharacteristicDefaults
      module procedure new_CharacteristicDefaults
   end interface CharacteristicDefaults

contains

   function new_CharacteristicDefaults(geom, geom_id) result(defaults)
      type(ESMF_Geom), optional, intent(in) :: geom
      type(GeomId), optional, intent(in) :: geom_id
      type(CharacteristicDefaults) :: defaults

      if (present(geom)) then
         defaults%geom_value = geom
         defaults%has_geom_value = .true.
      end if
      if (present(geom_id)) defaults%geom_id_value = geom_id
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

end module mapl_CharacteristicDefaults_mod

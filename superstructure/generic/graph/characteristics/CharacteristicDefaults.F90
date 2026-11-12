#include "MAPL.h"

!------------------------------------------------------------------------------
! CharacteristicDefaults: the values a component supplies after ADVERTISE
! (at GEOM_B) to the item characteristics that were DEFERRED. Each
! characteristic takes what it needs through
! StateItemCharacteristic%resolve_deferred; callers never name a kind.
!
! Graph layer only: holds plain values, no OuterMetaComponent. A new kind
! with a component-supplied value adds its component here.
!
! component_scope marks the GEOM_B defaults, where an absent vertical grid
! means the component has none. A set holding only what one realized field
! showed (harvest) is not component scope: an absent grid there is unknown.
!------------------------------------------------------------------------------
module mapl_CharacteristicDefaults_mod
   use ESMF, only: ESMF_Geom
   use mapl_GeomId_mod, only: GeomId
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_QuantityType_mod, only: QuantityType, MixingRatioBasis
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
      logical :: component_scope_value = .false.
      type(VerticalStaggerLoc), allocatable :: vert_stagger_value
      type(QuantityType), allocatable :: quantity_type_value
      type(MixingRatioBasis), allocatable :: basis_value
   contains
      procedure :: has_geom => defaults_has_geom
      procedure :: get_geom => defaults_get_geom
      procedure :: get_geom_id => defaults_get_geom_id
      procedure :: has_vgrid => defaults_has_vgrid
      procedure :: get_vgrid => defaults_get_vgrid
      procedure :: set_vgrid => defaults_set_vgrid
      procedure :: is_component_scope => defaults_is_component_scope
      procedure :: set_component_scope => defaults_set_component_scope
      procedure :: has_vert_stagger => defaults_has_vert_stagger
      procedure :: get_vert_stagger => defaults_get_vert_stagger
      procedure :: set_vert_stagger => defaults_set_vert_stagger
      procedure :: has_quantity_type => defaults_has_quantity_type
      procedure :: get_quantity_type => defaults_get_quantity_type
      procedure :: get_basis => defaults_get_basis
      procedure :: set_quantity_type => defaults_set_quantity_type
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

   logical function defaults_is_component_scope(this) result(is_scope)
      class(CharacteristicDefaults), intent(in) :: this

      is_scope = this%component_scope_value
   end function defaults_is_component_scope

   subroutine defaults_set_component_scope(this)
      class(CharacteristicDefaults), intent(inout) :: this

      this%component_scope_value = .true.
   end subroutine defaults_set_component_scope

   logical function defaults_has_vert_stagger(this) result(has)
      class(CharacteristicDefaults), intent(in) :: this

      has = allocated(this%vert_stagger_value)
   end function defaults_has_vert_stagger

   function defaults_get_vert_stagger(this) result(vert_stagger)
      class(CharacteristicDefaults), intent(in) :: this
      type(VerticalStaggerLoc) :: vert_stagger

      if (allocated(this%vert_stagger_value)) vert_stagger = this%vert_stagger_value
   end function defaults_get_vert_stagger

   subroutine defaults_set_vert_stagger(this, vert_stagger)
      class(CharacteristicDefaults), intent(inout) :: this
      type(VerticalStaggerLoc), intent(in) :: vert_stagger

      if (allocated(this%vert_stagger_value)) deallocate(this%vert_stagger_value)
      allocate(this%vert_stagger_value, source=vert_stagger)
   end subroutine defaults_set_vert_stagger

   logical function defaults_has_quantity_type(this) result(has)
      class(CharacteristicDefaults), intent(in) :: this

      has = allocated(this%quantity_type_value)
   end function defaults_has_quantity_type

   function defaults_get_quantity_type(this) result(quantity_type_)
      class(CharacteristicDefaults), intent(in) :: this
      type(QuantityType) :: quantity_type_

      if (allocated(this%quantity_type_value)) quantity_type_ = this%quantity_type_value
   end function defaults_get_quantity_type

   function defaults_get_basis(this) result(basis)
      class(CharacteristicDefaults), intent(in) :: this
      type(MixingRatioBasis) :: basis

      if (allocated(this%basis_value)) basis = this%basis_value
   end function defaults_get_basis

   subroutine defaults_set_quantity_type(this, quantity_type_, basis)
      class(CharacteristicDefaults), intent(inout) :: this
      type(QuantityType), intent(in) :: quantity_type_
      type(MixingRatioBasis), intent(in) :: basis

      if (allocated(this%quantity_type_value)) deallocate(this%quantity_type_value)
      if (allocated(this%basis_value)) deallocate(this%basis_value)
      allocate(this%quantity_type_value, source=quantity_type_)
      allocate(this%basis_value, source=basis)
   end subroutine defaults_set_quantity_type

end module mapl_CharacteristicDefaults_mod

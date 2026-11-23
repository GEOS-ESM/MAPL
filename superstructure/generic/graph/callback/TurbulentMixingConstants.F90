!------------------------------------------------------------------------------
! mapl_TurbulentMixingConstants_mod: named-string constants for the
! turbulent_mixing callback interface (callback_interfaces_geos.yaml, in
! this directory), so a Handler or Invoker implementing it references a
! declared name, or one of mix_like's or bottom_boundary's allowed
! values, through a constant instead of a hand-typed string literal that
! could typo against the YAML. turbulent_mixing declares no call scalars,
! so this module declares none either.
!------------------------------------------------------------------------------
module mapl_TurbulentMixingConstants_mod
   implicit none(type, external)
   private

   public :: TURBULENT_MIXING_INTERFACE_NAME

   public :: TURBULENT_MIXING_ARGUMENT_MIXED_QUANTITY
   public :: TURBULENT_MIXING_ARGUMENT_AIR_MASS
   public :: TURBULENT_MIXING_ARGUMENT_DRY_DEPOSITION_VELOCITY

   public :: TURBULENT_MIXING_METHOD_GET
   public :: TURBULENT_MIXING_METHOD_PUT

   public :: TURBULENT_MIXING_ATTRIBUTE_MIX_LIKE
   public :: TURBULENT_MIXING_ATTRIBUTE_BOTTOM_BOUNDARY

   public :: TURBULENT_MIXING_MIX_LIKE_HEAT
   public :: TURBULENT_MIXING_MIX_LIKE_MOISTURE
   public :: TURBULENT_MIXING_MIX_LIKE_MOMENTUM

   public :: TURBULENT_MIXING_BOTTOM_BOUNDARY_NEUMANN_HOMOGENEOUS
   public :: TURBULENT_MIXING_BOTTOM_BOUNDARY_ROBIN

   character(*), parameter :: TURBULENT_MIXING_INTERFACE_NAME = &
        'turbulent_mixing'

   character(*), parameter :: TURBULENT_MIXING_ARGUMENT_MIXED_QUANTITY = &
        'mixed_quantity'
   character(*), parameter :: TURBULENT_MIXING_ARGUMENT_AIR_MASS = &
        'air_mass'
   character(*), parameter :: &
        TURBULENT_MIXING_ARGUMENT_DRY_DEPOSITION_VELOCITY = &
        'dry_deposition_velocity'

   character(*), parameter :: TURBULENT_MIXING_METHOD_GET = 'get'
   character(*), parameter :: TURBULENT_MIXING_METHOD_PUT = 'put'

   character(*), parameter :: TURBULENT_MIXING_ATTRIBUTE_MIX_LIKE = &
        'mix_like'
   character(*), parameter :: TURBULENT_MIXING_ATTRIBUTE_BOTTOM_BOUNDARY = &
        'bottom_boundary'

   ! mix_like's allowed values.
   character(*), parameter :: TURBULENT_MIXING_MIX_LIKE_HEAT = 'heat'
   character(*), parameter :: TURBULENT_MIXING_MIX_LIKE_MOISTURE = &
        'moisture'
   character(*), parameter :: TURBULENT_MIXING_MIX_LIKE_MOMENTUM = &
        'momentum'

   ! bottom_boundary's allowed values. neumann_homogeneous is also its
   ! default value.
   character(*), parameter :: &
        TURBULENT_MIXING_BOTTOM_BOUNDARY_NEUMANN_HOMOGENEOUS = &
        'neumann_homogeneous'
   character(*), parameter :: TURBULENT_MIXING_BOTTOM_BOUNDARY_ROBIN = &
        'robin'

end module mapl_TurbulentMixingConstants_mod

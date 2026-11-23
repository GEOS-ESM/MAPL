!------------------------------------------------------------------------------
! mapl_ToyScalingConstants_mod: named-string constants for the toy_scaling
! callback interface (callback_interfaces_test.yaml, in this directory), so
! a Handler or Invoker implementing it references a declared name, or one
! of operation's allowed values, through a constant instead of a
! hand-typed string literal that could typo against the YAML. toy_scaling
! declares no call scalars, so this module declares none either.
!------------------------------------------------------------------------------
module mapl_ToyScalingConstants_mod
   implicit none(type, external)
   private

   public :: TOY_SCALING_INTERFACE_NAME

   public :: TOY_SCALING_ARGUMENT_QUANTITY
   public :: TOY_SCALING_ARGUMENT_OFFSET_VALUE

   public :: TOY_SCALING_METHOD_GET
   public :: TOY_SCALING_METHOD_PUT

   public :: TOY_SCALING_ATTRIBUTE_OPERATION
   public :: TOY_SCALING_ATTRIBUTE_FACTOR

   public :: TOY_SCALING_OPERATION_SCALE
   public :: TOY_SCALING_OPERATION_OFFSET

   character(*), parameter :: TOY_SCALING_INTERFACE_NAME = 'toy_scaling'

   character(*), parameter :: TOY_SCALING_ARGUMENT_QUANTITY = 'quantity'
   character(*), parameter :: TOY_SCALING_ARGUMENT_OFFSET_VALUE = &
        'offset_value'

   character(*), parameter :: TOY_SCALING_METHOD_GET = 'get'
   character(*), parameter :: TOY_SCALING_METHOD_PUT = 'put'

   character(*), parameter :: TOY_SCALING_ATTRIBUTE_OPERATION = 'operation'
   character(*), parameter :: TOY_SCALING_ATTRIBUTE_FACTOR = 'factor'

   ! operation's allowed values.
   character(*), parameter :: TOY_SCALING_OPERATION_SCALE = 'scale'
   character(*), parameter :: TOY_SCALING_OPERATION_OFFSET = 'offset'

end module mapl_ToyScalingConstants_mod

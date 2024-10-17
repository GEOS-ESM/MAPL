module mapl3_FieldMask
   use :: esmf, only: ESMF_Field
   implicit none
   private

   public :: FieldMask

   type :: FieldMask
   contains
      generic :: mask => maskField, maskLRFields, maskComposite, maskNegation
      procedure(MaskFieldFunction), deferred :: maskField
      procedure(MaskLRFieldFunction), deferred :: maskLRFields
      procedure(MaskNegationFunction), deferred :: maskNegation
      procedure(MaskCompositeFunction), deferred :: maskComposite
   end type FieldMask
   
   abstract interface

      function MaskFieldFunction(this, f) result(mask)
         type(ESMF_Field) :: mask
         class(FieldMask), intent(in) :: this
         type(ESMF_Field), intent(inout) :: f
      end function MaskFieldFunction
         
      function MaskLRFieldFunction(this, left, right) result(mask)
         type(ESMF_Field) :: mask
         class(FieldMask), intent(in) :: this
         type(ESMF_Field, intent(inout) :: left, right
      end function MaskLRFieldFunction

      function MaskNegationFunction(this, nf)
         type(ESMF_Field) :: mask
         class(FieldMask), intent(in) :: this
         class(FieldMask), intent(in) :: nf
      end function MaskNegationFunction
         
      function MaskCompositeFunction(this, left, right)
         type(ESMF_Field) :: mask
         class(FieldMask), intent(in) :: this
         class(FieldMask), intent(in) :: left, right
      end function MaskCompositeFunction

   end interface

end module mapl3_FieldMask

module mapl_VarConnType
   use mapl_VarConnPoint
   implicit none
   private

   public :: VarConnType

   type VarConnType
!!$      private
      type (VarConnPoint)                 :: FROM
      type (VarConnPoint)                 :: TO
      logical                                  :: used = .false.
      logical                                  :: notRequired = .false.
   end type VarConnType
   
end module mapl_VarConnType

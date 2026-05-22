module mapl_MeterNodePtr_mod
   use mapl_AbstractMeterNode_mod
   private
   public :: MeterNodePtr

   type :: MeterNodePtr
      class(AbstractMeterNode), pointer :: ptr
   end type MeterNodePtr

end module mapl_MeterNodePtr_mod

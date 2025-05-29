module mapl_MeterNodePtr
   use mapl_AbstractMeterNode
   private
   public :: MeterNodePtr

   type :: MeterNodePtr
      class(AbstractMeterNode), pointer :: ptr
   end type MeterNodePtr

end module mapl_MeterNodePtr

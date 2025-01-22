   type :: StateClassAspect
      type(StateRegistry) :: registry
      type(ActualPtStateItemSpecMap) :: items
   end type StateClassAspect

   logical function matches(src, dst)

      ! every item in dst matches src
      ! extra items in src is not a problem
      
   end function matches

   function make_action2(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(StateClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      

      ! dst must also be State

      action = StateAction(src, dst)

      _RETURN(_SUCCESS)
   end function make_action2


   type :: StateAction
   contains
      procedure :: update
   end type StateAction

   

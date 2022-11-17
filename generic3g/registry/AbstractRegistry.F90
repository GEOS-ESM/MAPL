module mapl3g_AbstractRegistry
   use mapl3g_ConnectionPoint
   use mapl3g_ConnectionSpec
   use mapl_KeywordEnforcer
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr
   implicit none
   private
  
   public :: AbstractRegistry
  
   type, abstract :: AbstractRegistry
     private
   contains
      procedure(I_get_item_spec_ptr), deferred :: get_item_spec_ptr
      procedure(I_get_item_spec), deferred :: get_item_spec
      procedure(I_add_item), deferred :: add_item_spec
      procedure(I_has_item_spec), deferred :: has_item_spec
      procedure(I_set_active), deferred :: set_active
      procedure(I_connect), deferred :: connect_sibling
      procedure(I_connect), deferred :: propagate_ptr
   end type AbstractRegistry
  

   abstract interface

      function I_get_item_spec_ptr(this, conn_pt) result(spec_ptr)
         import AbstractRegistry
         import AbstractStateItemSpec
         import StateItemSpecPtr
         import ConnectionPoint
         class(StateItemSpecPtr), pointer :: spec_ptr
         class(AbstractRegistry), intent(in) :: this
         type(ConnectionPoint), intent(in) :: conn_pt
      end function I_get_item_spec_ptr

      function I_get_item_spec(this, conn_pt) result(spec)
         import AbstractRegistry
         import AbstractStateItemSpec
         import ConnectionPoint
         class(AbstractStateItemSpec), pointer :: spec
         class(AbstractRegistry), intent(in) :: this
         type(ConnectionPoint), intent(in) :: conn_pt
      end function I_get_item_spec

      subroutine I_add_item(this, conn_pt, spec, rc)
         import AbstractRegistry
         import AbstractStateItemSpec
         import ConnectionPoint
         class(AbstractRegistry), intent(inout) :: this
         type(ConnectionPoint), intent(in) :: conn_pt
         class(AbstractStateItemSpec), target, intent(in) :: spec
         integer, optional, intent(out) :: rc
      end subroutine I_add_item

      logical function I_has_item_spec(this, conn_pt)
         import AbstractRegistry
         import AbstractStateItemSpec
         import ConnectionPoint
         class(AbstractRegistry), intent(in) :: this
         type(ConnectionPoint), intent(in) :: conn_pt
      end function I_has_item_spec

      subroutine I_set_active(this, conn_pt, unusable, require_inactive, rc)
         import AbstractRegistry
         import ConnectionPoint
         import KeywordEnforcer
         class(AbstractRegistry), intent(inout) :: this
         class(ConnectionPoint), intent(in) :: conn_pt
         class(KeywordEnforcer), optional, intent(in) :: unusable
         logical, optional, intent(in) :: require_inactive
         integer, optional, intent(out) :: rc
      end subroutine I_set_active


      subroutine I_connect(this, src_registry, connection, unusable, rc)
         import AbstractRegistry
         import ConnectionSpec
         import KeywordEnforcer
         class(AbstractRegistry), intent(in) :: this
         class(AbstractRegistry), intent(in) :: src_registry
         type(ConnectionSpec), intent(in) :: connection
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine I_connect
      
   end interface

end module mapl3g_AbstractRegistry

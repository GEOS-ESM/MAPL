module mapl3g_AbstractRegistry
   use mapl3g_ConnectionPt
   use mapl3g_newActualConnectionPt
   use mapl3g_ActualPtVector
   use mapl3g_newVirtualConnectionPt
   use mapl3g_ConnectionSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_StateItemSpecPtr
   use mapl3g_StateItemSpecPtr
   use mapl_KeywordEnforcer
   implicit none
   private
  
   public :: AbstractRegistry
  
   type, abstract :: AbstractRegistry
     private
   contains
      ! The interfaces that are needed on subregistries:
!!$      procedure(I_connect), deferred :: connect_sibling
!!$      procedure(I_set_active), deferred :: set_active
!!$      procedure(I_get_actual_pts), deferred :: get_actual_pts
!!$      procedure(I_get_actual_pt_SpecPtrs), deferred :: get_actual_pt_SpecPtrs
!!$      procedure(I_get_item_spec), deferred :: get_item_spec

   end type AbstractRegistry
  

   abstract interface

      function I_get_item_SpecPtr(this, actual_pt, rc) result(spec_ptr)
         import AbstractRegistry
         import AbstractStateItemSpec
         import StateItemSpecPtr
         import newActualConnectionPt
         class(StateItemSpecPtr), pointer :: spec_ptr
         class(AbstractRegistry), intent(in) :: this
         class(newActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end function I_get_item_SpecPtr

      function I_get_item_spec(this, actual_pt, rc) result(spec)
         import AbstractRegistry
         import AbstractStateItemSpec
         import newActualConnectionPt
         class(AbstractStateItemSpec), pointer :: spec
         class(AbstractRegistry), target, intent(in) :: this
         class(newActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end function I_get_item_spec

      subroutine I_add_item_spec_virtual(this, virtual_pt, spec, rc)
         import AbstractRegistry
         import AbstractStateItemSpec
         import newVirtualConnectionPt
         class(AbstractRegistry), intent(inout) :: this
         type(newVirtualConnectionPt), intent(in) :: virtual_pt
         class(AbstractStateItemSpec), target, intent(in) :: spec
         integer, optional, intent(out) :: rc
      end subroutine I_add_item_spec_virtual

      subroutine I_add_item_spec_virtual_override(this, virtual_pt, spec, actual_pt, rc)
         import AbstractRegistry
         import AbstractStateItemSpec
         import newVirtualConnectionPt
         import newActualConnectionPt
         class(AbstractRegistry), intent(inout) :: this
         type(newVirtualConnectionPt), intent(in) :: virtual_pt
         class(AbstractStateItemSpec), target, intent(in) :: spec
         type(newActualConnectionPt), intent(in) :: actual_pt
         integer, optional, intent(out) :: rc
      end subroutine I_add_item_spec_virtual_override

      subroutine I_add_item_spec_actual(this, actual_pt, spec, rc)
         import AbstractRegistry
         import AbstractStateItemSpec
         import newActualConnectionPt
         class(AbstractRegistry), intent(inout) :: this
         type(newActualConnectionPt), intent(in) :: actual_pt
         class(AbstractStateItemSpec), target, intent(in) :: spec
         integer, optional, intent(out) :: rc
      end subroutine I_add_item_spec_actual

      logical function I_has_item_spec(this, actual_pt)
         import AbstractRegistry
         import AbstractStateItemSpec
         import newActualConnectionPt
         class(AbstractRegistry), intent(in) :: this
         type(newActualConnectionPt), intent(in) :: actual_pt
      end function I_has_item_spec

      subroutine I_set_active(this, actual_pt, unusable, require_inactive, rc)
         import AbstractRegistry
         import newActualConnectionPt
         import KeywordEnforcer
         class(AbstractRegistry), intent(inout) :: this
         class(newActualConnectionPt), intent(in) :: actual_pt
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
      
      function I_get_actual_pt_SpecPtrs(this, virtual_pt, rc) result(specs)
         import AbstractRegistry
         import newVirtualConnectionPt
         import StateItemSpecPtr
         type(StateItemSpecPtr), allocatable :: specs(:)
         class(AbstractRegistry), intent(in) :: this
         type(newVirtualConnectionPt), intent(in) :: virtual_pt
         integer, optional, intent(out) :: rc
      end function I_get_actual_pt_SpecPtrs


      function I_get_actual_pts(this, virtual_pt) result(actual_pts)
         import AbstractRegistry
         import newVirtualConnectionPt
         import ActualPtVector
         type(ActualPtVector), pointer :: actual_pts
         class(AbstractRegistry), target, intent(in) :: this
         type(newVirtualConnectionPt), intent(in) :: virtual_pt
      end function I_get_actual_pts

   end interface

end module mapl3g_AbstractRegistry

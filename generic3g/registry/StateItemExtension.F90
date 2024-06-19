#include "MAPL_Generic.h"

module mapl3g_StateItemExtension
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector
   implicit none
   private

   public :: StateItemExtension
   public :: StateItemExtensionPtr

   ! A StateItemExtension "owns" the spec and associated export
   ! couplers.   The import couplers are pointers back to
   ! other export couplers.

   type StateItemExtension
      private
      class(StateItemSpec), allocatable :: spec
      type(ComponentDriverVector) :: export_couplers ! invalidate()
      type(ComponentDriverPtrVector) :: import_couplers ! update()
   contains
      procedure :: add_export_coupler
      procedure :: add_import_coupler
      procedure :: get_spec
      procedure :: get_export_couplers
      procedure :: get_import_couplers
   end type StateItemExtension

   type :: StateItemExtensionPtr
      type(StateItemExtension), pointer :: ptr => null()
   end type StateItemExtensionPtr

   interface StateItemExtension
      procedure :: new_StateItemExtension_spec
   end interface StateItemExtension

contains

   function new_StateItemExtension_spec(spec) result(ext)
      type(StateItemExtension) :: ext
      class(StateItemSpec), intent(in) :: spec
      ext%spec = spec
   end function new_StateItemExtension_spec

   subroutine add_export_coupler(this, coupler)
      class(StateItemExtension), intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: coupler
      call this%export_couplers%push_back(coupler)
   end subroutine add_export_coupler

   subroutine add_import_coupler(this, coupler)
      class(StateItemExtension), intent(inout) :: this
      type(GriddedComponentDriver), pointer :: coupler
      type(ComponentDriverPtr) :: wrapper

      wrapper%ptr => coupler
      call this%import_couplers%push_back(wrapper)
   end subroutine add_import_coupler

   function get_spec(this) result(spec)
      class(StateItemExtension), target, intent(in) :: this
      class(StateItemSpec), pointer :: spec
      spec => this%spec
   end function get_spec

   function get_export_couplers(this) result(couplers)
      class(StateItemExtension), target, intent(in) :: this
      type(ComponentDriverVector), pointer :: couplers
      couplers => this%export_couplers
   end function get_export_couplers

   function get_import_couplers(this) result(couplers)
      class(StateItemExtension), target, intent(in) :: this
      type(ComponentDriverPtrVector), pointer :: couplers
      couplers => this%import_couplers
   end function get_import_couplers
   
end module mapl3g_StateItemExtension

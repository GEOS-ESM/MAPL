#include "MAPL_Generic.h"

module mapl3g_StateItemExtension
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_ExtensionAction
   use mapl3g_GenericCoupler
   use mapl_ErrorHandling
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
      procedure :: make_extension
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

   ! Creation of an extension requires a new coupler that transforms
   ! from source (this) spec to dest (extension) spec. This new coupler
   ! is added to the export specs of source (this), and the new extension
   ! gains it as a reference (pointer).

   function make_extension(this, goal, rc) result(extension)
      type(StateItemExtension) :: extension
      class(StateItemExtension), target, intent(inout) :: this
      class(StateItemSpec), target, intent(in) :: goal
      integer, intent(out) :: rc

      integer :: status
!#      class(StateItemSpec), allocatable :: new_spec
!#      class(ExtensionAction), allocatable :: action
!#      type(GriddedComponentDriver) :: new_coupler
!#      
!#      new_spec = this%spec%make_extension(goal, _RC)
!#      call new_spec%set_active()
!#      call this%spec%set_active
!#
!#      action = this%spec%make_action(new_spec, _RC)
!#      new_coupler = make_driver(action, _RC)
!#      call this%add_export_coupler(new_coupler)
!#
!#      extension = StateItemExtension(new_spec)
!#      call extension%add_import_coupler(this%export_couplers%back())

      _RETURN(_SUCCESS)
   end function make_extension
end module mapl3g_StateItemExtension

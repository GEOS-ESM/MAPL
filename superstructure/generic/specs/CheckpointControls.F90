#include "MAPL.h"

module mapl_CheckpointControls_mod

   implicit none
   private

   public :: CheckpointControls

   type :: CheckpointControls
      private
      logical :: import = .false.
      logical :: export = .false.
      logical :: internal = .false.
      logical :: bootstrap = .false.
   contains
      procedure :: get_import
      procedure :: get_export
      procedure :: get_internal
      procedure :: get_bootstrap
      procedure :: set_import
      procedure :: set_export
      procedure :: set_internal
      procedure :: set_bootstrap
   end type CheckpointControls

   interface CheckpointControls
      module procedure new_CheckpointControls
   end interface CheckpointControls

contains

   function new_CheckpointControls(import, export, internal, bootstrap) result(controls)
      type(CheckpointControls) :: controls
      logical, optional, intent(in) :: import
      logical, optional, intent(in) :: export
      logical, optional, intent(in) :: internal
      logical, optional, intent(in) :: bootstrap

      if (present(import)) controls%import = import
      if (present(export)) controls%export = export
      if (present(internal)) controls%internal = internal
      if (present(bootstrap)) controls%bootstrap = bootstrap

   end function new_CheckpointControls

   ! Getter methods
   logical function get_import(this)
      class(CheckpointControls), intent(in) :: this
      get_import = this%import
   end function get_import

   logical function get_export(this)
      class(CheckpointControls), intent(in) :: this
      get_export = this%export
   end function get_export

   logical function get_internal(this)
      class(CheckpointControls), intent(in) :: this
      get_internal = this%internal
   end function get_internal

   logical function get_bootstrap(this)
      class(CheckpointControls), intent(in) :: this
      get_bootstrap = this%bootstrap
   end function get_bootstrap

   ! Setter methods
   subroutine set_import(this, value)
      class(CheckpointControls), intent(inout) :: this
      logical, intent(in) :: value
      this%import = value
   end subroutine set_import

   subroutine set_export(this, value)
      class(CheckpointControls), intent(inout) :: this
      logical, intent(in) :: value
      this%export = value
   end subroutine set_export

   subroutine set_internal(this, value)
      class(CheckpointControls), intent(inout) :: this
      logical, intent(in) :: value
      this%internal = value
   end subroutine set_internal

   subroutine set_bootstrap(this, value)
      class(CheckpointControls), intent(inout) :: this
      logical, intent(in) :: value
      this%bootstrap = value
   end subroutine set_bootstrap

end module mapl_CheckpointControls_mod

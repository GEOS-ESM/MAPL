#include "MAPL.h"
module mapl_ComponentInterface_mod
   use mapl_ItemSpec_mod, only: ItemSpec
   use mapl_VirtualConnectionPt_mod, only: VirtualConnectionPt
   use mapl_PortId_mod, only: PortId, PortIdGenerator, INVALID_PortId
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   implicit none(type, external)
   private

   public :: ComponentInterface, PublicPort
   public :: PORT_IMPORT, PORT_EXPORT, PORT_INTERNAL

   integer, parameter :: PORT_IMPORT = 1
   integer, parameter :: PORT_EXPORT = 2
   integer, parameter :: PORT_INTERNAL = 3

   ! Imports consume a representation, exports produce one, and internal
   ! points are component-local. Only imports and exports cross this interface.

   type :: PublicPort
      private
      type(PortId) :: id_ = INVALID_PortId
      type(VirtualConnectionPt) :: connection_point_
      type(ItemSpec) :: spec_
   contains
      procedure :: id
      procedure :: name
      procedure :: connection_point
      procedure :: item_spec
      procedure :: direction
      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal
   end type PublicPort

   type :: ComponentInterface
      private
      type(PublicPort), allocatable :: imports_(:)
      type(PublicPort), allocatable :: exports_(:)
      type(PortIdGenerator) :: id_generator_
   contains
      procedure :: add_import
      procedure :: add_export
      procedure :: find_import
      procedure :: find_export
      procedure :: get_import
      procedure :: get_export
      procedure :: get_import_port
      procedure :: get_export_port
      procedure :: import_count
      procedure :: export_count
   end type ComponentInterface

contains

   function add_import(this, name, spec, unusable, rc) result(port_id)
      class(ComponentInterface), intent(inout) :: this
      character(*), intent(in) :: name
      type(ItemSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(PortId) :: port_id, existing
      integer :: status
      type(PublicPort) :: port

      existing = this%find_import(name)
      _ASSERT(.not. existing%is_valid(), 'Component import already declared.')
      port_id = this%id_generator_%next(_RC)
      port%id_ = port_id
      port%connection_point_ = VirtualConnectionPt(state_intent='import', short_name=name)
      port%spec_ = spec
      call append_port(this%imports_, port)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_import

   function add_export(this, name, spec, unusable, rc) result(port_id)
      class(ComponentInterface), intent(inout) :: this
      character(*), intent(in) :: name
      type(ItemSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(PortId) :: port_id, existing
      integer :: status
      type(PublicPort) :: port

      existing = this%find_export(name)
      _ASSERT(.not. existing%is_valid(), 'Component export already declared.')
      port_id = this%id_generator_%next(_RC)
      port%id_ = port_id
      port%connection_point_ = VirtualConnectionPt(state_intent='export', short_name=name)
      port%spec_ = spec
      call append_port(this%exports_, port)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_export

   function find_import(this, name) result(port_id)
      class(ComponentInterface), intent(in) :: this
      character(*), intent(in) :: name
      integer :: i
      type(PortId) :: port_id

      port_id = INVALID_PortId
      do i = 1, this%import_count()
         if (this%imports_(i)%name() == name) then
            port_id = this%imports_(i)%id_
            return
         end if
      end do
   end function find_import

   function find_export(this, name) result(port_id)
      class(ComponentInterface), intent(in) :: this
      character(*), intent(in) :: name
      integer :: i
      type(PortId) :: port_id

      port_id = INVALID_PortId
      do i = 1, this%export_count()
         if (this%exports_(i)%name() == name) then
            port_id = this%exports_(i)%id_
            return
         end if
      end do
   end function find_export

   subroutine get_import(this, name, port_id, spec, rc)
      class(ComponentInterface), intent(in) :: this
      character(*), intent(in) :: name
      type(PortId), intent(out) :: port_id
      type(ItemSpec), intent(out) :: spec
      integer, optional, intent(out) :: rc

      port_id = this%find_import(name)
      _ASSERT(port_id%is_valid(), 'Component import is unknown.')
      call get_port_data(this%imports_, name, port_id, spec)
      _RETURN(_SUCCESS)
   end subroutine get_import

   function get_import_port(this, name, rc) result(port)
      class(ComponentInterface), intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PublicPort) :: port
      type(PortId) :: port_id

      port_id = this%find_import(name)
      _ASSERT(port_id%is_valid(), 'Component import is unknown.')
      port = this%imports_(find_port_index(this%imports_, name))
      _RETURN(_SUCCESS)
   end function get_import_port

   subroutine get_export(this, name, port_id, spec, rc)
      class(ComponentInterface), intent(in) :: this
      character(*), intent(in) :: name
      type(PortId), intent(out) :: port_id
      type(ItemSpec), intent(out) :: spec
      integer, optional, intent(out) :: rc
      integer :: index
      logical :: found

      index = 0
      found = .false.
      do while (index < this%export_count())
         index = index + 1
         if (this%exports_(index)%name() == name) then
            found = .true.
            exit
         end if
      end do
      _ASSERT(found, 'Component export is unknown.')
      port_id = this%exports_(index)%id_
      spec = this%exports_(index)%spec_

      _RETURN(_SUCCESS)
   end subroutine get_export

   function get_export_port(this, name, rc) result(port)
      class(ComponentInterface), intent(in) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(PublicPort) :: port
      type(PortId) :: port_id

      port_id = this%find_export(name)
      _ASSERT(port_id%is_valid(), 'Component export is unknown.')
      port = this%exports_(find_port_index(this%exports_, name))
      _RETURN(_SUCCESS)
   end function get_export_port

   integer function export_count(this)
      class(ComponentInterface), intent(in) :: this

      if (allocated(this%exports_)) then
         export_count = size(this%exports_)
      else
         export_count = 0
      end if
   end function export_count

   integer function import_count(this)
      class(ComponentInterface), intent(in) :: this

      if (allocated(this%imports_)) then
         import_count = size(this%imports_)
      else
         import_count = 0
      end if
   end function import_count

   pure function id(this) result(port_id)
      class(PublicPort), intent(in) :: this
      type(PortId) :: port_id
      port_id = this%id_
   end function id

   function name(this) result(value)
      class(PublicPort), intent(in) :: this
      character(:), allocatable :: value
      value = this%connection_point_%get_esmf_name()
   end function name

   function connection_point(this) result(value)
      class(PublicPort), intent(in) :: this
      type(VirtualConnectionPt) :: value
      value = this%connection_point_
   end function connection_point

   function item_spec(this) result(spec)
      class(PublicPort), intent(in) :: this
      type(ItemSpec) :: spec
      spec = this%spec_
   end function item_spec

   integer function direction(this)
      class(PublicPort), intent(in) :: this
      if (this%connection_point_%is_import()) then
         direction = PORT_IMPORT
      else if (this%connection_point_%is_export()) then
         direction = PORT_EXPORT
      else if (this%connection_point_%is_internal()) then
         direction = PORT_INTERNAL
      else
         direction = 0
      end if
   end function direction

   logical function is_import(this)
      class(PublicPort), intent(in) :: this
      is_import = this%connection_point_%is_import()
   end function is_import

   logical function is_export(this)
      class(PublicPort), intent(in) :: this
      is_export = this%connection_point_%is_export()
   end function is_export

   logical function is_internal(this)
      class(PublicPort), intent(in) :: this
      is_internal = this%connection_point_%is_internal()
   end function is_internal

   integer function find_port_index(ports, name) result(index)
      type(PublicPort), intent(in) :: ports(:)
      character(*), intent(in) :: name

      index = 0
      do index = 1, size(ports)
         if (ports(index)%name() == name) return
      end do
      index = 0
   end function find_port_index

   subroutine get_port_data(ports, name, port_id, spec)
      type(PublicPort), intent(in) :: ports(:)
      character(*), intent(in) :: name
      type(PortId), intent(out) :: port_id
      type(ItemSpec), intent(out) :: spec
      integer :: i

      do i = 1, size(ports)
         if (ports(i)%name() == name) then
            port_id = ports(i)%id_
            spec = ports(i)%spec_
            return
         end if
      end do
   end subroutine get_port_data

   subroutine append_port(ports, port)
      type(PublicPort), allocatable, intent(inout) :: ports(:)
      type(PublicPort), intent(in) :: port

      if (allocated(ports)) then
         ports = [ports, port]
      else
         ports = [port]
      end if
   end subroutine append_port

end module mapl_ComponentInterface_mod

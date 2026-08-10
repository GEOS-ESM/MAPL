#include "MAPL.h"
module mapl_ComponentInterface_mod
   use mapl_ItemSpec_mod, only: ItemSpec
   use mapl_PortId_mod, only: PortId, PortIdGenerator, INVALID_PortId
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod, only: KeywordEnforcer
   implicit none(type, external)
   private

   public :: ComponentInterface
   public :: PORT_IMPORT, PORT_EXPORT

   integer, parameter :: PORT_IMPORT = 1
   integer, parameter :: PORT_EXPORT = 2

   type :: PublicPort
      private
       type(PortId) :: id_ = INVALID_PortId
      character(:), allocatable :: name_
      type(ItemSpec) :: spec_
      integer :: direction_ = 0
   contains
      procedure :: id
      procedure :: name
      procedure :: item_spec
      procedure :: direction
   end type PublicPort

   type :: ComponentInterface
      private
       type(PublicPort), allocatable :: exports_(:)
       type(PortIdGenerator) :: id_generator_
   contains
      procedure :: add_export
      procedure :: find_export
      procedure :: get_export
      procedure :: export_count
   end type ComponentInterface

contains

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
      port%name_ = name
      port%spec_ = spec
      port%direction_ = PORT_EXPORT
      call append_port(this%exports_, port)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function add_export

    function find_export(this, name) result(port_id)
       class(ComponentInterface), intent(in) :: this
       character(*), intent(in) :: name
       integer :: i
       type(PortId) :: port_id

        port_id = INVALID_PortId
       do i = 1, this%export_count()
          if (this%exports_(i)%name_ == name) then
             port_id = this%exports_(i)%id_
            return
         end if
      end do
   end function find_export

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
          if (this%exports_(index)%name_ == name) then
             found = .true.
             exit
          end if
       end do
       _ASSERT(found, 'Component export is unknown.')
      port_id = this%exports_(index)%id_
      spec = this%exports_(index)%spec_

      _RETURN(_SUCCESS)
   end subroutine get_export

   integer function export_count(this)
      class(ComponentInterface), intent(in) :: this

      if (allocated(this%exports_)) then
         export_count = size(this%exports_)
      else
         export_count = 0
      end if
   end function export_count

    pure function id(this) result(port_id)
      class(PublicPort), intent(in) :: this
      type(PortId) :: port_id
      port_id = this%id_
   end function id

   function name(this) result(value)
      class(PublicPort), intent(in) :: this
      character(:), allocatable :: value
      value = this%name_
   end function name

   function item_spec(this) result(spec)
      class(PublicPort), intent(in) :: this
      type(ItemSpec) :: spec
      spec = this%spec_
   end function item_spec

   pure integer function direction(this)
      class(PublicPort), intent(in) :: this
      direction = this%direction_
   end function direction

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

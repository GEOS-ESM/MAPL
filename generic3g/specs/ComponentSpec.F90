#include "MAPL_Generic.h"

module mapl3g_ComponentSpec
   use mapl3g_Connection
   use mapl3g_SimpleConnection
   use mapl3g_ReexportConnection
   use mapl3g_ConnectionVector
   use mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_VariableSpec
   use mapl3g_VariableSpecVector
   use mapl3g_ChildSpecMap
   use mapl3g_GeometrySpec
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl_stringutilities
   use gftl2_StringVector
   use ESMF
   implicit none
   private

   public :: ComponentSpec

   type :: ComponentSpec
      !!$      private
      type(GeometrySpec) :: geometry_spec
      type(VariableSpecVector) :: var_specs
      type(ConnectionVector) :: connections
      type(ChildSpecMap) :: children
      type(ESMF_HConfig), allocatable :: geom_hconfig ! optional
      logical :: activate_all_exports = .false. ! used for testing in isolation
      logical :: activate_all_imports = .false. ! used for testing in isolation

   contains
      procedure :: has_geom_hconfig
      procedure :: add_var_spec
      procedure :: add_connection_conn
      generic :: add_connection => add_connection_conn
      procedure :: add_connectivity
      procedure :: reexport
   end type ComponentSpec

   interface ComponentSpec
      module procedure new_ComponentSpec
   end interface ComponentSpec

contains

   function new_ComponentSpec(var_specs, connections) result(spec)
      type(ComponentSpec) :: spec
      type(VariableSpecVector), optional, intent(in) :: var_specs
      type(ConnectionVector), optional, intent(in) :: connections

      if (present(var_specs)) spec%var_specs = var_specs
      if (present(connections)) spec%connections = connections

   end function new_ComponentSpec

   logical function has_geom_hconfig(this)
      class(ComponentSpec), intent(in) :: this
      has_geom_hconfig = allocated(this%geom_hconfig)
   end function has_geom_hconfig

   subroutine add_var_spec(this, var_spec)
      class(ComponentSpec), intent(inout) :: this
      class(VariableSpec), intent(in) :: var_spec
      call this%var_specs%push_back(var_spec)
   end subroutine add_var_spec

   subroutine add_connection_conn(this, conn)
      class(ComponentSpec), intent(inout) :: this
      class(Connection), intent(in) :: conn
      call this%connections%push_back(conn)
   end subroutine add_connection_conn

   subroutine add_connectivity(this, unusable, src_comp, src_names, dst_comp, dst_names, rc)
      class(ComponentSpec), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: src_names
      character(*), intent(in) :: dst_comp
      character(*), optional, intent(in) :: dst_names
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dst_names_
      type(ConnectionPt) :: src_pt, dst_pt
      type(SimpleConnection) :: conn
      type(StringVector) :: srcs, dsts
      integer :: i

      dst_names_ = src_names ! default
      if (present(dst_names)) dst_names_ = dst_names

      srcs = split(src_names)
      dsts = split(dst_names_)
      _ASSERT(srcs%size() == dsts%size(), 'Number of src_names does not match number of dst_names.')

      do i = 1, srcs%size()
         src_pt = ConnectionPt(src_comp, VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, srcs%of(i)))
         dst_pt = ConnectionPt(dst_comp, VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, dsts%of(i)))
         conn = SimpleConnection(src_pt, dst_pt)
         call this%add_connection(conn)
      end do

      _RETURN(_SUCCESS)
   end subroutine add_connectivity

   subroutine reexport(this, unusable, src_comp, src_name, src_intent, new_name, rc)
      class(ComponentSpec), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: src_name
      character(*), optional, intent(in) :: src_intent
      character(*), optional, intent(in) :: new_name ! default is src_name
      integer, optional, intent(out) :: rc


      integer :: status
      character(:), allocatable :: new_name_
      type(ConnectionPt) :: src_pt, dst_pt
      type(ReexportConnection) :: conn
      type(ESMF_STATEINTENT_FLAG) :: src_intent_

      new_name_ = src_name
      if (present(new_name)) new_name_ = new_name

      src_intent_ = ESMF_STATEINTENT_EXPORT
      if (present(src_intent)) then
         select case (to_lower(src_intent))
         case ('export')
            src_intent_ = ESMF_STATEINTENT_INTERNAL
         case ('internal')
            src_intent_ = ESMF_STATEINTENT_INTERNAL
         case default
            _FAIL('Cannot reexport intent='//src_intent)
         end select
      end if

      src_pt = ConnectionPt(src_comp, VirtualConnectionPt(src_intent_, src_name))
      dst_pt = ConnectionPt('<self>', VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, new_name_))
      conn = ReexportConnection(src_pt, dst_pt)
      call this%add_connection(conn)

      _RETURN(_SUCCESS)
   end subroutine reexport

end module mapl3g_ComponentSpec

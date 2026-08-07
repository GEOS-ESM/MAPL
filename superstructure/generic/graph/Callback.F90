module mapl_CallbackTypes

   use ESMF, only: ESMF_STATEITEM_FIELD

   use mapl_AccessSpec
   use mapl_NodeId_mod, only: NodeId
   use mapl_DependencyNetworkId_mod, only: DependencyNetworkId
   use mapl_PortId_mod, only: PortId

   ! Generated gFTL containers, shown with illustrative names.
   use mapl_String_CallbackArgumentSpecMap
   use mapl_String_CallbackArgumentUseMap
   use mapl_String_CallbackMethodSpecMap
   use mapl_String_CallbackMethodAttachmentMap
   use mapl_String_NodeIdMap
   use mapl_String_CallbackMethodItemBindingMap
   use mapl_String_CallbackMethodBindingMap

   implicit none(type, external)
   private

   public :: CallbackArgumentSpec
   public :: CallbackArgumentUse
   public :: CallbackMethodSpec
   public :: CallbackInterface
   public :: CallbackMethodAttachment
   public :: CallbackStateBinding
   public :: CallbackMethodItemBinding
   public :: CallbackMethodBinding
   public :: CallbackEndpointRef
   public :: CallbackConnection

   public :: CALLBACK_ACCESS_INPUT
   public :: CALLBACK_ACCESS_OUTPUT
   public :: CALLBACK_ACCESS_INOUT

   !--------------------------------------------------------------------
   ! Method-relative access to a callback-state argument.
   !
   ! The same argument can have different access in different methods.
   ! For example, a PassiveTracer argument can be output from get and
   ! input to put.
   !--------------------------------------------------------------------

   integer, parameter :: CALLBACK_ACCESS_INPUT  = 1
   integer, parameter :: CALLBACK_ACCESS_OUTPUT = 2
   integer, parameter :: CALLBACK_ACCESS_INOUT  = 3

   !--------------------------------------------------------------------
   ! Declaration of one argument in a reusable callback interface.
   !
   ! The argument name is the key in:
   !
   !   String_CallbackArgumentSpecMap
   !
   ! and therefore is not duplicated here.
   !
   ! Detailed MAPL StateItem specifications and shared-attribute
   ! requirements should remain in the existing specification types.
   !--------------------------------------------------------------------

   type :: CallbackArgumentSpec

      ! Uses ESMF constants such as:
      !
      !   ESMF_STATEITEM_FIELD
      !   ESMF_STATEITEM_FIELDBUNDLE
      !   ESMF_STATEITEM_STATE
      integer :: expected_item_kind = ESMF_STATEITEM_FIELD

      logical :: required = .true.

   end type CallbackArgumentSpec

   !--------------------------------------------------------------------
   ! Use of one declared argument by one callback method.
   !
   ! The argument name is the key in the method's argument-use map.
   !--------------------------------------------------------------------

   type :: CallbackArgumentUse

      integer :: access = CALLBACK_ACCESS_INPUT

   end type CallbackArgumentUse

   !--------------------------------------------------------------------
   ! Shared specification of one callback method.
   !
   ! The method name, such as get or put, is the key in:
   !
   !   CallbackInterface%methods
   !
   ! This map identifies which interface arguments participate in the
   ! method and their method-relative access.
   !--------------------------------------------------------------------

   type :: CallbackMethodSpec
      type(AccessSpecMap) :: access_by_argument
   end type CallbackMethodSpec

   !--------------------------------------------------------------------
   ! Reusable callback contract.
   !
   ! The service/interface name is the key in the registry containing
   ! this object. It is not duplicated in CallbackInterface.
   !
   ! Argument names and method names are likewise keys in their maps.
   !--------------------------------------------------------------------

   type :: CallbackInterface

      ! All arguments declared by this callback interface.
      type(String_CallbackArgumentSpecMap) :: arguments

      ! All methods declared by this callback interface.
      type(String_CallbackMethodSpecMap) :: methods

   end type CallbackInterface

   !--------------------------------------------------------------------
   ! Concrete attachment of one interface method to an ESMF State.
   !
   ! The method name is the key in the enclosing attachment map and is
   ! therefore absent here.
   !
   ! The ESMF State itself owns the actual ESMF method attachment.
   ! This object records only MAPL bookkeeping that cannot be recovered
   ! conveniently from that State.
   !--------------------------------------------------------------------

   type :: CallbackMethodAttachment

      logical :: attached = .false.

      ! Additional implementation-selection information can be added
      ! later if MAPL must distinguish framework-provided and
      ! component-provided methods.

   end type CallbackMethodAttachment

   !--------------------------------------------------------------------
   ! Binding of the callback interface identified by the enclosing map
   ! key to one callback state.
   !
   ! The service/interface name should be the key in the component's
   ! callback-binding map, so it is not duplicated here.
   !
   ! state_node_id identifies the ValueGraphNode whose GraphValue wraps
   ! the callback ESMF State.
   !
   ! items maps interface argument names to graph nodes representing the
   ! corresponding actual members of that ESMF State.
   !
   ! methods maps interface method names to concrete ESMF attachments.
   !--------------------------------------------------------------------

   type :: CallbackStateBinding

      type(NodeId) :: state_node_id

      type(String_NodeIdMap) :: items

      type(String_CallbackMethodAttachmentMap) :: methods

   end type CallbackStateBinding

   !--------------------------------------------------------------------
   ! Resolved graph wiring for one argument of one callback method.
   !
   ! The argument name is the key in:
   !
   !   CallbackMethodBinding%items
   !
   ! source_node_id and target_node_id identify the method-relative data
   ! direction.
   !
   ! trigger_node_id identifies the node whose update initiates the
   ! resolved operation. It is normally the target node.
   !
   ! network_id selects the acyclic DependencyNetwork in which that
   ! operation is evaluated.
   !
   ! In the direct-alias case, source_node_id and target_node_id may be
   ! equal and no TransformGraphNode is required.
   !--------------------------------------------------------------------

   type :: CallbackMethodItemBinding

      type(NodeId) :: source_node_id
      type(NodeId) :: target_node_id
      type(NodeId) :: trigger_node_id

      type(DependencyNetworkId) :: network_id

   end type CallbackMethodItemBinding

   !--------------------------------------------------------------------
   ! Resolved graph wiring for one callback method.
   !
   ! The method name is the key in the enclosing method-binding map.
   ! Each item entry is keyed by the interface argument name.
   !--------------------------------------------------------------------

   type :: CallbackMethodBinding

      type(String_CallbackMethodItemBindingMap) :: items

   end type CallbackMethodBinding

   !--------------------------------------------------------------------
   ! Reference to a published callback endpoint at a component boundary.
   !
   ! A parent graph connects through a published PortId and does not
   ! obtain arbitrary NodeIds from a child graph.
   !
   ! The exact convention for a local endpoint versus a child endpoint
   ! may be refined by OuterComponent. Zero is tentatively reserved for
   ! a local endpoint.
   !--------------------------------------------------------------------

   type :: CallbackEndpointRef

      integer :: child_index = 0
      type(PortId) :: port_id

   end type CallbackEndpointRef

   !--------------------------------------------------------------------
   ! Composite connection between two published callback endpoints.
   !
   ! The interface/service name should be the key in the container that
   ! owns this connection, rather than a component stored here.
   !
   ! The methods map is keyed by method name, such as get or put.
   !
   ! CallbackConnection is not a GraphNode. It orchestrates ordinary
   ! graph nodes, TransformGraphNodes, and DependencyNetworks.
   !--------------------------------------------------------------------

   type :: CallbackConnection

      type(CallbackEndpointRef) :: provider
      type(CallbackEndpointRef) :: consumer

      type(String_CallbackMethodBindingMap) :: methods

   end type CallbackConnection

end module mapl_CallbackTypes

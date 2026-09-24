target_sources(MAPL.generic PRIVATE
  NodeId.F90
  DependencyNetworkId.F90
  PortId.F90
  CallbackInterfaceId.F90

  GraphNode.F90
  BaseGraphNode.F90
  OperationGraphNode.F90
  NodeRevision.F90
  MAPLStateItemFlag.F90
  StateItemVariantInfo.F90

  containers/StateItemMemberMap.F90
  GraphStateItem.F90
  StateItemNode.F90

  PortSpec.F90
  containers/PortSpecMap.F90
  containers/PortNameRevisionMap.F90
  Transform.F90
  TransformGraphNode.F90

  AccessSpec.F90
  ArgumentSpec.F90
  containers/ArgumentSpecMap.F90
  MethodInvocationAdapter.F90
  GridCompMethodInvocation.F90
  GridCompDriverPhaseInvoker.F90
  StateMethodInvocation.F90
  MethodGraphNode.F90
  MethodInvocation.F90

  PortBindingKey.F90
  containers/PortBindingKeyPortMap.F90
  PortBindingTable.F90

  containers/NodeIdSet.F90
  containers/NodeId_NodeIdSet_Map.F90
  containers/NodeIdGraphNodeMap.F90
  containers/PortIdNodeIdMap.F90

  NodeLabel.F90
  containers/NodeIdLabelMap.F90

  containers/DependencyNetworkIdSet.F90
  DependencyNetwork.F90
  containers/DependencyNetworkIdNetworkMap.F90

  ComponentGraph.F90
  ComponentGraph_DemandDrivenUpdate.F90
  GraphExport.F90

  CharacteristicId.F90
  Characteristic.F90
  UnitsCharacteristic.F90
  VerticalGridCharacteristic.F90
  UnitsConverterTransform.F90
  ExtensionResolution.F90
  ExtensionMaterialization.F90

  CompositeStateMaterialization.F90

  CallbackArgumentSpec.F90
  containers/CallbackArgumentSpecMap.F90
  containers/AccessSpecMap.F90
  CallbackMethodSpec.F90
  containers/CallbackMethodSpecMap.F90
  CallbackInterface.F90
  containers/CallbackInterfaceIdInterfaceMap.F90
  containers/CallbackServiceNameIdMap.F90
  CallbackInterfaceRegistry.F90
  containers/CallbackMethodAttachmentMap.F90
  CallbackStateBinding.F90
  CallbackMethodBinding.F90
)

target_include_directories(MAPL.generic PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

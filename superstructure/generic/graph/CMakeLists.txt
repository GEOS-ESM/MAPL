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
)

target_include_directories(MAPL.generic PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)

if (PFUNIT_FOUND)
  add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

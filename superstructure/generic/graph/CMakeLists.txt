target_sources(MAPL.generic PRIVATE
  AccessSpec.F90
  NodeId.F90
  NodeRevision.F90
  PortId.F90
  EdgeId.F90
  DependencyNetworkId.F90

  GraphValue.F90
  GraphValueSpec.F90
  ItemSpec.F90
  ComponentInterface.F90
  GraphAssemblyStatus.F90
  GraphNode.F90
  GraphEdge.F90

  containers/NodeIdSet.F90
  containers/EdgeIdSet.F90
  containers/GraphNodeMap.F90
  containers/GraphEdgeMap.F90
  containers/NodeId_EdgeIdSetMap.F90
  containers/DependencyNetworkMap.F90

  DependencyNetwork.F90
  GraphTransform.F90
  FieldGraphValue.F90
  GeomGraphValue.F90
  ValueGraphNode.F90
  TransformGraphNode.F90
  ComponentGraph.F90
  GraphAssembly.F90
  GraphBuilder.F90

  # Callback*.F90 sources are not yet ready for integration.
  )

target_include_directories(MAPL.generic PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/include)

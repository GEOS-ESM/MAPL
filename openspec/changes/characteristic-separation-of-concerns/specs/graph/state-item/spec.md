## REMOVED Requirements

### Requirement: GraphStateItem carries a sparse characteristics map
**Reason**: The characteristics map moves to `StateItemDescriptor`, owned
by `StateItemNode` (`graph/state-item-descriptor`, "StateItemNode owns a
descriptor separate from its ESMF handle"). `GraphStateItem` becomes the
ESMF handle only.

**Migration**: Callers of `GraphStateItem%has_characteristic`,
`get_characteristic`, `set_characteristic`, and `get_characteristics`
use the same methods on the node's descriptor. The sparse-map semantics
(absence distinct from invalid status, key/kind agreement) carry over to
the descriptor unchanged.

### Requirement: GraphStateItem exposes its characteristic-ordering strategy
**Reason**: Ordering is owned by the chain tip's shape
(`ItemShapeCharacteristic%get_extension_order`, see
`graph/state-item-characteristics`), with per-shape lists derived from
legacy `get_aspect_order`. The variant-keyed `characteristic_ordering_table`
had no legacy basis.

**Migration**: `GraphStateItem%ordering` and
`characteristic_ordering_table` are deleted. Tests of `ordering` are
replaced by golden tests of `get_extension_order` per shape.

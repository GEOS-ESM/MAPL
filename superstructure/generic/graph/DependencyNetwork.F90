#include "MAPL.h"

!------------------------------------------------------------------------------
! DependencyNetwork: graph-neutral dependency wiring with direct
! predecessor/successor adjacency, keyed by NodeId (spec/06-dependency-
! network.md REQ-DEP-001/002). No first-class edge objects: a dependency is
! addressed only by its (source, target) NodeId pair.
!
! Two synchronized gFTL maps (NodeId -> NodeIdSet) hold successor and
! predecessor adjacency. Ownership/classification of NodeIds is not known to
! this type (that is ComponentGraph's job, REQ-CG-002 layering) - callers
! that need ownership or producer-count validation pass the relevant NodeId
! sets into add_dependency()/validate() as optional arguments; if omitted,
! those specific checks are skipped, which is how the standalone synthetic
! tests exercise adjacency/cycle/self-edge behavior without a real
! ComponentGraph.
!
! Iteration convention: every gFTL walk here uses ftn_begin()/ftn_end() with
! the %next() call at the top of the loop body (iter = c%ftn_begin() sits
! one position before the first element; next() must run before the first
! use of iter).
!------------------------------------------------------------------------------
module mapl_DependencyNetwork_mod
   use mapl_NodeId_mod
   use mapl_NodeIdSet_mod
   use mapl_NodeId_NodeIdSet_Map_mod
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: DependencyNetwork

   type :: DependencyNetwork
      private
      type(NodeId_NodeIdSet_Map) :: predecessors_map
      type(NodeId_NodeIdSet_Map) :: successors_map
      logical :: frozen = .false.
   contains
      procedure :: add_dependency => network_add_dependency
      procedure :: remove_dependency => network_remove_dependency
      procedure :: contains_dependency => network_contains_dependency
      procedure :: get_predecessors => network_get_predecessors
      procedure :: get_successors => network_get_successors
      procedure :: has_predecessors => network_has_predecessors
      procedure :: has_successors => network_has_successors
      procedure :: would_create_cycle => network_would_create_cycle
      procedure :: validate => network_validate
      procedure :: freeze => network_freeze
      procedure :: is_frozen => network_is_frozen
      procedure :: clear => network_clear
   end type DependencyNetwork

contains

   ! -- mutators ---------------------------------------------------------

   ! REQ-DEP-005/006: validates before mutating either map; on any
   ! rejection (frozen, invalid id, self-dependency, foreign id, cycle),
   ! no adjacency changes.
   subroutine network_add_dependency(this, source, target, rc, valid_ids)
      class(DependencyNetwork), intent(inout) :: this
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target
      integer, optional, intent(out) :: rc
      type(NodeIdSet), optional, intent(in) :: valid_ids

      type(NodeIdSet), pointer :: successors, predecessors

      _ASSERT(.not. this%frozen, 'DependencyNetwork: add_dependency called on a frozen network')
      _ASSERT(source%is_valid(), 'DependencyNetwork: add_dependency source NodeId is not valid')
      _ASSERT(target%is_valid(), 'DependencyNetwork: add_dependency target NodeId is not valid')
      _ASSERT(source /= target, 'DependencyNetwork: add_dependency rejects self-dependency')
      if (present(valid_ids)) then
         _ASSERT(valid_ids%count(source) > 0, 'DependencyNetwork: add_dependency source NodeId is foreign to this network')
         _ASSERT(valid_ids%count(target) > 0, 'DependencyNetwork: add_dependency target NodeId is foreign to this network')
      end if
      _ASSERT(.not. this%would_create_cycle(source, target), 'DependencyNetwork: add_dependency would create a cycle')

      call ensure_entry(this%successors_map, source)
      call ensure_entry(this%predecessors_map, target)

      successors => this%successors_map%at(source)
      call successors%insert(target)

      predecessors => this%predecessors_map%at(target)
      call predecessors%insert(source)

      _RETURN(_SUCCESS)
   end subroutine network_add_dependency

   ! REQ-DEP-005: removes the pair if present; no-op (success) if absent.
   subroutine network_remove_dependency(this, source, target, rc)
      class(DependencyNetwork), intent(inout) :: this
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target
      integer, optional, intent(out) :: rc

      type(NodeIdSet), pointer :: successors, predecessors
      integer :: n_removed

      _ASSERT(.not. this%frozen, 'DependencyNetwork: remove_dependency called on a frozen network')

      successors => this%successors_map%at(source)
      if (associated(successors)) n_removed = int(successors%erase(target))

      predecessors => this%predecessors_map%at(target)
      if (associated(predecessors)) n_removed = int(predecessors%erase(source))

      _RETURN(_SUCCESS)
   end subroutine network_remove_dependency

   subroutine network_clear(this, rc)
      class(DependencyNetwork), intent(inout) :: this
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%frozen, 'DependencyNetwork: clear called on a frozen network')

      call this%predecessors_map%clear()
      call this%successors_map%clear()

      _RETURN(_SUCCESS)
   end subroutine network_clear

   ! REQ-DEP-005: irreversible; no failure path defined by the spec.
   subroutine network_freeze(this)
      class(DependencyNetwork), intent(inout) :: this

      this%frozen = .true.
   end subroutine network_freeze

   ! -- queries (read-only, remain available after freeze) ---------------

   logical function network_is_frozen(this) result(frozen)
      class(DependencyNetwork), intent(in) :: this

      frozen = this%frozen
   end function network_is_frozen

   logical function network_contains_dependency(this, source, target) result(found)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target

      type(NodeIdSet), pointer :: successors

      found = .false.
      successors => this%successors_map%at(source)
      if (associated(successors)) found = successors%count(target) > 0
   end function network_contains_dependency

   function network_get_predecessors(this, node) result(predecessors)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: node
      type(NodeIdSet) :: predecessors

      type(NodeIdSet), pointer :: found

      found => this%predecessors_map%at(node)
      if (associated(found)) predecessors = found
   end function network_get_predecessors

   function network_get_successors(this, node) result(successors)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: node
      type(NodeIdSet) :: successors

      type(NodeIdSet), pointer :: found

      found => this%successors_map%at(node)
      if (associated(found)) successors = found
   end function network_get_successors

   logical function network_has_predecessors(this, node) result(has)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: node

      type(NodeIdSet), pointer :: found

      found => this%predecessors_map%at(node)
      has = associated(found)
      if (has) has = .not. found%empty()
   end function network_has_predecessors

   logical function network_has_successors(this, node) result(has)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: node

      type(NodeIdSet), pointer :: found

      found => this%successors_map%at(node)
      has = associated(found)
      if (has) has = .not. found%empty()
   end function network_has_successors

   ! REQ-DEP-006: pure query - no side effects. Searches downstream from
   ! `target`; if `source` is reachable, adding source->target would close
   ! a cycle.
   logical function network_would_create_cycle(this, source, target) result(cyclic)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: source
      type(NodeId), intent(in) :: target

      type(NodeIdSet) :: visited

      cyclic = node_reachable(this, target, source, visited)
   end function network_would_create_cycle

   ! -- validation ---------------------------------------------------------

   ! REQ-DEP-007: full structural validation. `valid_ids`, `producer_ids`,
   ! and `state_item_ids` are optional NodeId classification sets, normally
   ! supplied by the owning ComponentGraph (REQ-CG-002 keeps this type
   ! itself graph-neutral - it does not know about GraphNode subclasses).
   ! Omitting them skips only the checks that need that classification
   ! (ownership, one-producer-per-state-item); symmetry, self-dependency,
   ! and acyclicity are always checked.
   subroutine network_validate(this, rc, valid_ids, producer_ids, state_item_ids)
      class(DependencyNetwork), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      type(NodeIdSet), optional, target, intent(in) :: valid_ids
      type(NodeIdSet), optional, target, intent(in) :: producer_ids
      type(NodeIdSet), optional, target, intent(in) :: state_item_ids

      type(NodeIdSet), target :: all_ids
      type(NodeIdSet), target :: visiting, visited
      type(NodeId_NodeIdSet_MapIterator) :: map_iter
      type(NodeIdSetIterator) :: set_iter
      type(NodeId) :: node, other
      type(NodeIdSet), pointer :: neighbor_set, reverse_set

      _HERE
      all_ids = collect_referenced_ids(this)
      _HERE
      ! REQ-DEP-007 item 2: all referenced ids are owned by the graph.
      if (present(valid_ids)) then
         set_iter = all_ids%ftn_begin()
         do while (set_iter /= all_ids%ftn_end())
            call set_iter%next()
            node = set_iter%of()
            _ASSERT(valid_ids%count(node) > 0, 'DependencyNetwork: validate found a NodeId not owned by the graph')
         end do
      end if
      _HERE

      ! REQ-DEP-007 item 3: no self-dependencies.
      map_iter = this%successors_map%ftn_begin()
      do while (map_iter /= this%successors_map%ftn_end())
         call map_iter%next()
         node = map_iter%first()
         neighbor_set => map_iter%second()
         _ASSERT(neighbor_set%count(node) == 0, 'DependencyNetwork: validate found a self-dependency')
      end do

      _HERE
      ! REQ-DEP-007 item 1: predecessor/successor symmetry, both directions.
      map_iter = this%successors_map%ftn_begin()
      do while (map_iter /= this%successors_map%ftn_end())
         call map_iter%next()
         node = map_iter%first()
         neighbor_set => map_iter%second()
         set_iter = neighbor_set%ftn_begin()
         do while (set_iter /= neighbor_set%ftn_end())
            call set_iter%next()
            other = set_iter%of()
            reverse_set => this%predecessors_map%at(other)
            _ASSERT(associated(reverse_set), 'DependencyNetwork: validate found asymmetric adjacency (missing predecessor entry)')
            _ASSERT(reverse_set%count(node) > 0, 'DependencyNetwork: validate found asymmetric adjacency (predecessor set missing source)')
         end do
      end do

      _HERE
      map_iter = this%predecessors_map%ftn_begin()
      do while (map_iter /= this%predecessors_map%ftn_end())
         call map_iter%next()
         node = map_iter%first()
         neighbor_set => map_iter%second()
         set_iter = neighbor_set%ftn_begin()
         do while (set_iter /= neighbor_set%ftn_end())
            call set_iter%next()
            other = set_iter%of()
            reverse_set => this%successors_map%at(other)
            _ASSERT(associated(reverse_set), 'DependencyNetwork: validate found asymmetric adjacency (missing successor entry)')
            _ASSERT(reverse_set%count(node) > 0, 'DependencyNetwork: validate found asymmetric adjacency (successor set missing target)')
         end do
      end do

      _HERE
      ! REQ-DEP-007 item 4: acyclicity of the whole network.
      set_iter = all_ids%ftn_begin()
      do while (set_iter /= all_ids%ftn_end())
         call set_iter%next()
         node = set_iter%of()
         if (visited%count(node) == 0) then
            _ASSERT(.not. has_cycle_from(this, node, visiting, visited), 'DependencyNetwork: validate found a cycle')
         end if
      end do

      _HERE, present(producer_ids), present(state_item_ids)
      ! REQ-DEP-007 item 5 / REQ-DEP-008: at most one producer per state item.
      if (present(producer_ids) .and. present(state_item_ids)) then
         call validate_producer_counts(this, producer_ids, state_item_ids, rc=rc)
         if (present(rc)) then
            if (rc /= _SUCCESS) return
         end if
      end if
      _HERE

      _RETURN(_SUCCESS)
   end subroutine network_validate

   subroutine validate_producer_counts(this, producer_ids, state_item_ids, rc)
      class(DependencyNetwork), target, intent(in) :: this
      type(NodeIdSet), target, intent(in) :: producer_ids
      type(NodeIdSet), target, intent(in) :: state_item_ids
      integer, optional, intent(out) :: rc

      type(NodeIdSetIterator) :: item_iter, pred_iter
      type(NodeId) :: item, pred
      type(NodeIdSet), pointer :: predecessors
      integer :: producer_count

      item_iter = state_item_ids%ftn_begin()
      do while (item_iter /= state_item_ids%ftn_end())
         call item_iter%next()
         item = item_iter%of()
         predecessors => this%predecessors_map%at(item)
         if (associated(predecessors)) then
            producer_count = 0
            pred_iter = predecessors%ftn_begin()
            do while (pred_iter /= predecessors%ftn_end())
               call pred_iter%next()
               pred = pred_iter%of()
               if (producer_ids%count(pred) > 0) producer_count = producer_count + 1
            end do
            _ASSERT(producer_count <= 1, 'DependencyNetwork: validate found more than one producer for a state item')
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine validate_producer_counts

   ! -- private helpers ----------------------------------------------------

   ! Ensures a (possibly empty) NodeIdSet entry exists for `key` so that a
   ! subsequent %at(key) call returns an associated pointer.
   subroutine ensure_entry(map, key)
      type(NodeId_NodeIdSet_Map), intent(inout) :: map
      type(NodeId), intent(in) :: key

      type(NodeIdSet) :: empty_set

      if (map%count(key) == 0) call map%insert(key, empty_set)
   end subroutine ensure_entry

   ! Is `needle` reachable from `start` by following successor adjacency?
   recursive function node_reachable(this, start, needle, visited) result(found)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: start
      type(NodeId), intent(in) :: needle
      type(NodeIdSet), intent(inout) :: visited
      logical :: found

      type(NodeIdSet), pointer :: successors
      type(NodeIdSetIterator) :: iter
      type(NodeId) :: next_node

      found = .false.
      if (start == needle) then
         found = .true.
         return
      end if
      if (visited%count(start) > 0) return
      call visited%insert(start)

      successors => this%successors_map%at(start)
      if (.not. associated(successors)) return

      iter = successors%ftn_begin()
      do while (iter /= successors%ftn_end())
         call iter%next()
         next_node = iter%of()
         if (node_reachable(this, next_node, needle, visited)) then
            found = .true.
            return
         end if
      end do
   end function node_reachable

   ! Classic white/gray/black DFS cycle detection: `visiting` holds nodes
   ! currently on the recursion stack, `visited` holds fully-explored nodes.
   recursive function has_cycle_from(this, node, visiting, visited) result(found)
      class(DependencyNetwork), intent(in) :: this
      type(NodeId), intent(in) :: node
      type(NodeIdSet), intent(inout) :: visiting
      type(NodeIdSet), intent(inout) :: visited
      logical :: found

      type(NodeIdSet), pointer :: successors
      type(NodeIdSetIterator) :: iter
      type(NodeId) :: next_node
      integer :: n_removed

      if (visiting%count(node) > 0) then
         found = .true.
         return
      end if
      if (visited%count(node) > 0) then
         found = .false.
         return
      end if

      call visiting%insert(node)
      found = .false.

      successors => this%successors_map%at(node)
      if (associated(successors)) then
         iter = successors%ftn_begin()
         do while (iter /= successors%ftn_end())
            call iter%next()
            next_node = iter%of()
            if (has_cycle_from(this, next_node, visiting, visited)) then
               found = .true.
               exit
            end if
         end do
      end if

      n_removed = int(visiting%erase(node))
      call visited%insert(node)
   end function has_cycle_from

   ! All NodeIds referenced anywhere in either adjacency map, as keys or
   ! as set members.
   function collect_referenced_ids(this) result(ids)
      class(DependencyNetwork), target, intent(in) :: this
      type(NodeIdSet) :: ids

      type(NodeId_NodeIdSet_MapIterator) :: map_iter
      type(NodeIdSetIterator) :: set_iter
      type(NodeIdSet), pointer :: neighbor_set
      type(NodeId) :: node, member

      map_iter = this%successors_map%ftn_begin()
      do while (map_iter /= this%successors_map%ftn_end())
         call map_iter%next()
         node = map_iter%first()
         call ids%insert(node)
         neighbor_set => map_iter%second()
         set_iter = neighbor_set%ftn_begin()
         do while (set_iter /= neighbor_set%ftn_end())
            call set_iter%next()
            member = set_iter%of()
            call ids%insert(member)
         end do
      end do

      map_iter = this%predecessors_map%ftn_begin()
      do while (map_iter /= this%predecessors_map%ftn_end())
         call map_iter%next()
         node = map_iter%first()
         call ids%insert(node)
         neighbor_set => map_iter%second()
         set_iter = neighbor_set%ftn_begin()
         do while (set_iter /= neighbor_set%ftn_end())
            call set_iter%next()
            member = set_iter%of()
            call ids%insert(member)
         end do
      end do
   end function collect_referenced_ids

end module mapl_DependencyNetwork_mod

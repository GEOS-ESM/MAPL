#include "MAPL_ErrLog.h"

submodule (mapl3g_MeshGeomFactory) MeshGeomFactory_smod

   use mapl3g_GeomSpec
   use mapl3g_MeshGeomSpec
   use mapl3g_MeshDecomposition
   use mapl_ErrorHandlingMod
   use mapl_Constants, only: MAPL_PI_R8
   use pfio
   use gftl2_StringVector
   use mapl3g_StringDictionary
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer

   implicit none

contains

   ! Make GeomSpec from HConfig
   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(MeshGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)

      geom_spec = make_MeshGeomSpec(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig

   ! Make GeomSpec from FileMetadata
   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(MeshGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)

      geom_spec = make_MeshGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata

   ! Check if factory supports this GeomSpec type
   module logical function supports_spec(this, geom_spec) result(supports)
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(MeshGeomSpec) :: reference

      _UNUSED_DUMMY(this)

      supports = same_type_as(geom_spec, reference)

   end function supports_spec

   ! Check if factory supports this HConfig
   module logical function supports_hconfig(this, hconfig, rc) result(supports)
      class(MeshGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(MeshGeomSpec) :: spec
      type(MeshDecomposition) :: decomp

      _UNUSED_DUMMY(this)

      ! Create dummy spec to test support
      decomp = MeshDecomposition([1])
      spec = MeshGeomSpec(1, 1, decomp)
      supports = spec%supports(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function supports_hconfig

   ! Check if factory supports this FileMetadata
   module logical function supports_metadata(this, file_metadata, rc) result(supports)
      class(MeshGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(MeshGeomSpec) :: spec
      type(MeshDecomposition) :: decomp

      _UNUSED_DUMMY(this)

      ! Create dummy spec to test support
      decomp = MeshDecomposition([1])
      spec = MeshGeomSpec(1, 1, decomp)
      supports = spec%supports(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function supports_metadata

   ! Create ESMF_Geom from GeomSpec
   module function make_geom(this, geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)

      select type (geom_spec)
      type is (MeshGeomSpec)
         geom = typesafe_make_geom(geom_spec, _RC)
      class default
         _FAIL("geom_spec type not supported")
      end select

      _RETURN(_SUCCESS)
   end function make_geom

   ! Type-safe helper for creating ESMF_Geom from MeshGeomSpec
   function typesafe_make_geom(spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(MeshGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Mesh) :: mesh

      mesh = create_esmf_mesh(spec, _RC)
      geom = ESMF_GeomCreate(mesh=mesh, _RC)

      _RETURN(_SUCCESS)
   end function typesafe_make_geom

   ! Create ESMF_Mesh from MeshGeomSpec
   function create_esmf_mesh(spec, rc) result(mesh)
      type(ESMF_Mesh) :: mesh
      type(MeshGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: nnodes, nelements
      integer :: mask_status  ! For optional elementMask read
      real(kind=ESMF_KIND_R8), pointer :: node_coords(:,:)
      integer, pointer :: element_conn(:)
      integer, pointer :: num_element_conn(:)
      integer, pointer :: element_mask(:)
      character(len=:), allocatable :: filename
      
        ! For file reading
        type(NetCDF4_FileFormatter) :: file_formatter
        real(kind=ESMF_KIND_R8), allocatable :: node_coords_file(:,:)
        integer, allocatable :: element_conn_file(:)
        integer, allocatable :: num_element_conn_file(:)
        integer, allocatable :: element_mask_file(:)
        integer :: coord_dim = 0, node_count = 0, conn_count = 0, element_count = 0  ! Dimension sizes from file
      
       ! Local node data for this PE
       integer :: local_node_count
       integer :: i_node_start, i_node_end
       integer, allocatable :: nodeIds(:)
       integer, allocatable :: nodeOwners(:)
       real(kind=ESMF_KIND_R8), allocatable :: nodeCoords(:)
       logical, allocatable :: is_ghost_node(:)
       integer, allocatable :: ghost_node_ids(:)
       integer :: num_ghost_nodes, ghost_idx
       logical :: use_phase3_distribution
       integer, allocatable :: node_dist(:)
      
      ! Local element data for this PE
      integer :: local_element_count
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementMask(:)
      
       type(MeshDecomposition) :: decomp
       type(ESMF_VM) :: vm
       integer :: localPet, petCount
       integer :: i, j, elem_idx, conn_idx, conn_start, conn_end, np, node_id
       integer :: i_elem_start, i_elem_end
       integer :: n_start, n_end, n_id
       integer, allocatable :: node_to_pe(:)  ! Maps node ID to owning PE

      ! Get mesh data from spec
      nnodes = spec%get_nnodes()
      nelements = spec%get_nelements()
      
      ! Check if spec already has data loaded
      call spec%get_node_coords(node_coords)
      call spec%get_connectivity(element_conn)
      call spec%get_num_element_conn(num_element_conn)
      call spec%get_element_mask(element_mask)
      
       ! If data not loaded, read from file
       if (.not. associated(node_coords)) then
          filename = spec%get_filename()
          _ASSERT(allocated(filename), 'Neither mesh data nor filename provided in MeshGeomSpec')
          
          ! Read mesh data from file
          call file_formatter%open(filename, pFIO_READ, _RC)
          
          ! Query dimensions before allocating arrays
          coord_dim = file_formatter%inq_dim('coordDim', _RC)
          node_count = file_formatter%inq_dim('nodeCount', _RC)
          conn_count = file_formatter%inq_dim('connectionCount', _RC)
          element_count = file_formatter%inq_dim('elementCount', _RC)
          
          ! Allocate arrays with correct sizes
          ! nodeCoords has dimensions (nodeCount, coordDim) in NetCDF, which is (coordDim, nodeCount) in Fortran
          allocate(node_coords_file(coord_dim, node_count))
          allocate(element_conn_file(conn_count))
          allocate(num_element_conn_file(element_count))
          
          ! Read node coordinates (2, nodeCount)
          call file_formatter%get_var('nodeCoords', node_coords_file, _RC)
          
          ! Read element connectivity
          call file_formatter%get_var('elementConn', element_conn_file, _RC)
          
          ! Read number of nodes per element
          call file_formatter%get_var('numElementConn', num_element_conn_file, _RC)
          
          ! Read element mask (optional) - try to read, ignore if not present
          ! Allocate elementMask array
          allocate(element_mask_file(element_count))
          call file_formatter%get_var('elementMask', element_mask_file, rc=mask_status)
          ! If mask_status /= 0, elementMask not present - that's ok, it's optional
          
          call file_formatter%close(_RC)
         
         ! Point to file-read data
         allocate(node_coords(size(node_coords_file,1), size(node_coords_file,2)))
         node_coords = node_coords_file
         
         allocate(element_conn(size(element_conn_file)))
         element_conn = element_conn_file
         
         allocate(num_element_conn(size(num_element_conn_file)))
         num_element_conn = num_element_conn_file
         
         if (allocated(element_mask_file)) then
            allocate(element_mask(size(element_mask_file)))
            element_mask = element_mask_file
         end if
      end if

      _ASSERT(associated(node_coords), 'Node coordinates not set in MeshGeomSpec')
      _ASSERT(associated(element_conn), 'Element connectivity not set in MeshGeomSpec')
      _ASSERT(associated(num_element_conn), 'Num element connections not set in MeshGeomSpec')

       ! Get decomposition info
       decomp = spec%get_decomposition()
       call ESMF_VMGetCurrent(vm, _RC)
       call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, _RC)

        ! Check if Phase 3 node distribution is enabled
        node_dist = decomp%get_node_distribution()
        use_phase3_distribution = allocated(node_dist)

        ! Get element distribution (elements are distributed across PEs)
        call decomp%get_local_indices(localPet, i_elem_start, i_elem_end)
        local_element_count = i_elem_end - i_elem_start + 1
        
        if (use_phase3_distribution) then
           ! Phase 3: Elements are distributed, nodes are shared among PEs
           ! Each PE owns a subset of elements, but nodes can be shared with neighboring PEs
           
           ! First, identify all nodes needed by this PE's local elements
           allocate(is_ghost_node(nnodes))
           is_ghost_node = .false.
           
           ! Mark nodes needed by local elements
           conn_start = 1
           do elem_idx = 1, i_elem_start - 1
              conn_start = conn_start + num_element_conn(elem_idx)
           end do
           
           do elem_idx = i_elem_start, i_elem_end
              np = num_element_conn(elem_idx)
              conn_end = conn_start + np - 1
              do j = conn_start, conn_end
                 node_id = element_conn(j)
                 is_ghost_node(node_id) = .true.  ! Mark as needed
              end do
              conn_start = conn_end + 1
           end do
           
            ! Build node ownership map: assign ownership based on first element using each node
            allocate(node_to_pe(nnodes))
            node_to_pe = -1  ! Initialize as unowned
            
            ! Process all elements to assign node ownership
            conn_start = 1
            do elem_idx = 1, nelements
               ! Find which PE owns this element
               do i = 0, petCount - 1
                  call decomp%get_local_indices(i, n_start, n_end)
                  if (elem_idx >= n_start .and. elem_idx <= n_end) then
                     ! PE i owns element elem_idx - assign unassigned nodes to PE i
                     np = num_element_conn(elem_idx)
                     conn_end = conn_start + np - 1
                     do j = conn_start, conn_end
                        node_id = element_conn(j)
                        if (node_to_pe(node_id) == -1) then
                           node_to_pe(node_id) = i
                        end if
                     end do
                     exit
                  end if
               end do
               ! Move to next element's connectivity
               conn_start = conn_start + num_element_conn(elem_idx)
            end do
            
            ! Count nodes needed by this PE
            local_node_count = count(is_ghost_node)
        else
           ! Phase 2: All nodes on all PEs (backward compatibility)
           i_node_start = 1
           i_node_end = nnodes
           local_node_count = nnodes
        end if

      ! Create mesh with parametric and spatial dimensions
      mesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
                             coordSys=ESMF_COORDSYS_SPH_DEG, _RC)

       ! Add nodes (Phase 2: all nodes, Phase 3: nodes needed by local elements with proper ownership)
        allocate(nodeIds(local_node_count))
        allocate(nodeCoords(2*local_node_count))
        
        if (use_phase3_distribution) then
           ! Phase 3: Add only nodes needed by local elements, with proper ownership
           ! Nodes are shared among PEs - each PE adds nodes needed by its elements
           allocate(nodeOwners(local_node_count))
           
           ! Add all nodes needed by this PE's elements
           ghost_idx = 1
           do i = 1, nnodes
              if (is_ghost_node(i)) then
                 nodeIds(ghost_idx) = i
                 nodeOwners(ghost_idx) = node_to_pe(i)  ! Owner PE (first PE that uses it)
                 nodeCoords(2*(ghost_idx-1)+1) = node_coords(1, i)  ! longitude
                 nodeCoords(2*(ghost_idx-1)+2) = node_coords(2, i)  ! latitude
                 ghost_idx = ghost_idx + 1
              end if
           end do
           
           call ESMF_MeshAddNodes(mesh, nodeIds, nodeCoords, nodeOwners=nodeOwners, _RC)
           deallocate(nodeOwners)
        else
           ! Phase 2: All nodes on all PEs (no ownership)
           do i = 1, local_node_count
              nodeIds(i) = i
              nodeCoords(2*(i-1)+1) = node_coords(1, i)  ! longitude
              nodeCoords(2*(i-1)+2) = node_coords(2, i)  ! latitude
           end do
           
           call ESMF_MeshAddNodes(mesh, nodeIds, nodeCoords, _RC)
        end if
       
       deallocate(nodeIds, nodeCoords)
       if (use_phase3_distribution) then
          deallocate(is_ghost_node, node_to_pe)
       end if

      ! Add elements (distributed by decomposition)
      allocate(elementIds(local_element_count))
      allocate(elementTypes(local_element_count))
      
      ! Calculate total connectivity size for local elements
      conn_idx = 0
      do elem_idx = i_elem_start, i_elem_end
         np = num_element_conn(elem_idx)
         conn_idx = conn_idx + np
      end do
      
      allocate(elementConn(conn_idx))
      
      if (associated(element_mask)) then
         allocate(elementMask(local_element_count))
      end if

      ! Fill element data
      conn_idx = 1
      conn_start = 1
      do elem_idx = 1, i_elem_start - 1
         conn_start = conn_start + num_element_conn(elem_idx)
      end do
      
      do i = 1, local_element_count
         elem_idx = i_elem_start + i - 1
         elementIds(i) = elem_idx
         
         np = num_element_conn(elem_idx)
         elementTypes(i) = np  ! Number of nodes = element type for ESMF
         
         ! Copy connectivity for this element
         conn_end = conn_start + np - 1
         elementConn(conn_idx:conn_idx+np-1) = element_conn(conn_start:conn_end)
         conn_idx = conn_idx + np
         conn_start = conn_end + 1
         
         if (associated(element_mask)) then
            elementMask(i) = element_mask(elem_idx)
         end if
      end do

      ! Add elements to mesh
      if (associated(element_mask)) then
         call ESMF_MeshAddElements(mesh, elementIds, elementTypes, elementConn, &
                                    elementMask=elementMask, _RC)
         deallocate(elementMask)
      else
         call ESMF_MeshAddElements(mesh, elementIds, elementTypes, elementConn, _RC)
      end if
      
      deallocate(elementIds, elementTypes, elementConn)
      
      ! Clean up temporary data if we read from file
      if (allocated(node_coords_file)) then
         deallocate(node_coords, element_conn, num_element_conn)
         if (allocated(element_mask_file) .and. associated(element_mask)) deallocate(element_mask)
      end if

      _RETURN(_SUCCESS)
   end function create_esmf_mesh

   ! Generate file metadata for mesh
   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)

      select type (geom_spec)
      type is (MeshGeomSpec)
         file_metadata = typesafe_make_file_metadata(geom_spec, _RC)
      class default
         _FAIL('geom_spec is not of dynamic type MeshGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_file_metadata

   ! Type-safe helper for creating FileMetadata from MeshGeomSpec
   function typesafe_make_file_metadata(geom_spec, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(MeshGeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: nnodes, nelements
      integer, pointer :: element_conn(:), num_element_conn(:)
      integer :: total_conn
      type(Variable) :: var

      file_metadata = FileMetadata()

      nnodes = geom_spec%get_nnodes()
      nelements = geom_spec%get_nelements()
      call geom_spec%get_connectivity(element_conn)
      call geom_spec%get_num_element_conn(num_element_conn)

      ! Calculate total connectivity count
      if (associated(num_element_conn)) then
         total_conn = sum(num_element_conn)
      else
         total_conn = 0
      end if

      ! Add dimensions
      call file_metadata%add_dimension('nodeCount', nnodes, _RC)
      call file_metadata%add_dimension('elementCount', nelements, _RC)
      call file_metadata%add_dimension('coordDim', 2, _RC)
      if (total_conn > 0) then
         call file_metadata%add_dimension('connectionCount', total_conn, _RC)
      end if

      ! Add node coordinate variable
      var = Variable(type=PFIO_REAL64, dimensions='coordDim,nodeCount')
      call var%add_attribute('units', Attribute('degrees'))
      call var%add_attribute('long_name', Attribute('Node coordinates (longitude, latitude)'))
      call file_metadata%add_variable('nodeCoords', var)

      ! Add connectivity variable
      if (total_conn > 0) then
         var = Variable(type=PFIO_INT32, dimensions='connectionCount')
         call var%add_attribute('long_name', &
            Attribute('Node indices that define the element connectivity'))
         call var%add_attribute('_FillValue', Attribute(-1))
         call var%add_attribute('start_index', Attribute(1))
         call file_metadata%add_variable('elementConn', var)

         ! Add number of nodes per element
         var = Variable(type=PFIO_INT32, dimensions='elementCount')
         call var%add_attribute('long_name', Attribute('Number of nodes per element'))
         call file_metadata%add_variable('numElementConn', var)
      end if

      ! Add element mask variable (optional)
      var = Variable(type=PFIO_INT32, dimensions='elementCount')
      call var%add_attribute('long_name', &
         Attribute('Element mask for surface types'))
      call file_metadata%add_variable('elementMask', var)

      ! Add global attributes
      call file_metadata%add_attribute('gridType', Attribute('unstructured'))
      call file_metadata%add_attribute('version', Attribute('0.9'))
      call file_metadata%add_attribute('convention', Attribute('ESMF'))

      _RETURN(_SUCCESS)
   end function typesafe_make_file_metadata

   ! Return gridded dimensions for mesh
   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)

      select type (geom_spec)
      type is (MeshGeomSpec)
         ! For unstructured mesh, use element count as the gridded dimension
         call gridded_dims%push_back("ncells")
      class default
         _FAIL('geom_spec is not of dynamic type MeshGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_gridded_dims

   ! Return variable attributes for mesh
   module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)

      variable_attributes = StringDictionary()

      select type (geom_spec)
      type is (MeshGeomSpec)
         ! CF-compliant attributes for unstructured mesh
         call variable_attributes%put('location', 'face')
         call variable_attributes%put('mesh', 'mesh_topology')
      class default
         _FAIL('geom_spec is not of dynamic type MeshGeomSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_variable_attributes

end submodule MeshGeomFactory_smod

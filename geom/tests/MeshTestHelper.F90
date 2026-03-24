#include "MAPL_ErrLog.h"

module mapl3g_MeshTestHelper
   use pfio
   use mapl_ErrorHandlingMod
   use, intrinsic :: iso_fortran_env, only: REAL64, INT32
   implicit none
   private

   public :: create_simple_quad_file
   public :: create_simple_triangle_file
   public :: create_four_quad_mesh_file
   public :: create_eight_element_mesh_file
   public :: create_mixed_element_mesh_file
   public :: cleanup_test_file

contains

   ! Create a simple quad mesh file for testing
   ! Unit square: nodes at (0,0), (1,0), (1,1), (0,1)
   subroutine create_simple_quad_file(filename, rc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable) :: var
      
      ! Mesh dimensions
      integer, parameter :: n_nodes = 4
      integer, parameter :: n_elements = 1
      integer, parameter :: n_conn = 4
      
      ! Mesh data
      real(kind=REAL64) :: node_coords(2, n_nodes)
      integer(kind=INT32) :: element_conn(n_conn)
      integer(kind=INT32) :: num_element_conn(n_elements)
      integer(kind=INT32) :: element_mask(n_elements)

      ! Define unit square nodes
      node_coords(1, :) = [0.0d0, 1.0d0, 1.0d0, 0.0d0]  ! longitudes
      node_coords(2, :) = [0.0d0, 0.0d0, 1.0d0, 1.0d0]  ! latitudes

      ! Define quad element connectivity (counter-clockwise)
      element_conn = [1, 2, 3, 4]
      num_element_conn = [4]
      element_mask = [1]  ! Ocean

      ! Create file metadata
      file_metadata = FileMetadata()
      
      ! Add dimensions
      call file_metadata%add_dimension('nodeCount', n_nodes, rc=status)
      call file_metadata%add_dimension('elementCount', n_elements, rc=status)
      call file_metadata%add_dimension('coordDim', 2, rc=status)
      call file_metadata%add_dimension('connectionCount', n_conn, rc=status)

      ! Add nodeCoords variable
      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', rc=status)
      call var%add_attribute('units', Attribute('degrees'))
      call var%add_attribute('long_name', Attribute('Node coordinates (longitude, latitude)'))
      call file_metadata%add_variable('nodeCoords', var, rc=status)

      ! Add elementConn variable
      var = Variable(type=pFIO_INT32, dimensions='connectionCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Node indices that define the element connectivity'))
      call var%add_attribute('_FillValue', Attribute(-1))
      call var%add_attribute('start_index', Attribute(1))
      call file_metadata%add_variable('elementConn', var, rc=status)

      ! Add numElementConn variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', Attribute('Number of nodes per element'))
      call file_metadata%add_variable('numElementConn', var, rc=status)

      ! Add elementMask variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Surface type: {1: Ocean, 2: Land, 3: Lake, 4: Landice}'))
      call file_metadata%add_variable('elementMask', var, rc=status)

      ! Add global attributes
      call file_metadata%add_attribute('gridType', Attribute('unstructured'))
      call file_metadata%add_attribute('version', Attribute('0.9'))
      call file_metadata%add_attribute('convention', Attribute('ESMF'))

      ! Create file and write metadata
      call formatter%create(file=filename,mode=pFIO_CLOBBER, rc=status)
      call formatter%write(file_metadata, rc=status)

      ! Write data
      call formatter%put_var('nodeCoords', node_coords, rc=status)
      call formatter%put_var('elementConn', element_conn, rc=status)
      call formatter%put_var('numElementConn', num_element_conn, rc=status)
      call formatter%put_var('elementMask', element_mask, rc=status)

      ! Close file
      call formatter%close(rc=status)

      _RETURN(_SUCCESS)
   end subroutine create_simple_quad_file

   ! Create a simple triangle mesh file for testing
   subroutine create_simple_triangle_file(filename, rc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable) :: var
      
      ! Mesh dimensions
      integer, parameter :: n_nodes = 3
      integer, parameter :: n_elements = 1
      integer, parameter :: n_conn = 3
      
      ! Mesh data
      real(kind=REAL64) :: node_coords(2, n_nodes)
      integer(kind=INT32) :: element_conn(n_conn)
      integer(kind=INT32) :: num_element_conn(n_elements)
      integer(kind=INT32) :: element_mask(n_elements)

      ! Define triangle nodes
      node_coords(1, :) = [0.0d0, 1.0d0, 0.5d0]  ! longitudes
      node_coords(2, :) = [0.0d0, 0.0d0, 1.0d0]  ! latitudes

      ! Define triangle element connectivity
      element_conn = [1, 2, 3]
      num_element_conn = [3]
      element_mask = [2]  ! Land

      ! Create file metadata
      file_metadata = FileMetadata()
      
      ! Add dimensions
      call file_metadata%add_dimension('nodeCount', n_nodes, rc=status)
      call file_metadata%add_dimension('elementCount', n_elements, rc=status)
      call file_metadata%add_dimension('coordDim', 2, rc=status)
      call file_metadata%add_dimension('connectionCount', n_conn, rc=status)

      ! Add nodeCoords variable
      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', rc=status)
      call var%add_attribute('units', Attribute('degrees'))
      call var%add_attribute('long_name', Attribute('Node coordinates (longitude, latitude)'))
      call file_metadata%add_variable('nodeCoords', var, rc=status)

      ! Add elementConn variable
      var = Variable(type=pFIO_INT32, dimensions='connectionCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Node indices that define the element connectivity'))
      call var%add_attribute('_FillValue', Attribute(-1))
      call var%add_attribute('start_index', Attribute(1))
      call file_metadata%add_variable('elementConn', var, rc=status)

      ! Add numElementConn variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', Attribute('Number of nodes per element'))
      call file_metadata%add_variable('numElementConn', var, rc=status)

      ! Add elementMask variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Surface type: {1: Ocean, 2: Land, 3: Lake, 4: Landice}'))
      call file_metadata%add_variable('elementMask', var, rc=status)

      ! Add global attributes
      call file_metadata%add_attribute('gridType', Attribute('unstructured'))
      call file_metadata%add_attribute('version', Attribute('0.9'))
      call file_metadata%add_attribute('convention', Attribute('ESMF'))

      ! Create file and write metadata
      call formatter%create(file=filename, mode=pFIO_CLOBBER,  rc=status)
      call formatter%write(file_metadata, rc=status)

      ! Write data
      call formatter%put_var('nodeCoords', node_coords, rc=status)
      call formatter%put_var('elementConn', element_conn, rc=status)
      call formatter%put_var('numElementConn', num_element_conn, rc=status)
      call formatter%put_var('elementMask', element_mask, rc=status)

      ! Close file
      call formatter%close(rc=status)

      _RETURN(_SUCCESS)
   end subroutine create_simple_triangle_file

   ! Create a mesh with 4 quad elements for 4-PE testing
   ! Grid layout: 2x2 grid of quads
   ! 9 nodes: (0,0), (1,0), (2,0), (0,1), (1,1), (2,1), (0,2), (1,2), (2,2)
   ! 4 elements: bottom-left, bottom-right, top-left, top-right
   subroutine create_four_quad_mesh_file(filename, rc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable) :: var
      
      ! Mesh dimensions
      integer, parameter :: n_nodes = 9
      integer, parameter :: n_elements = 4
      integer, parameter :: n_conn = 16  ! 4 nodes per element * 4 elements
      
      ! Mesh data
      real(kind=REAL64) :: node_coords(2, n_nodes)
      integer(kind=INT32) :: element_conn(n_conn)
      integer(kind=INT32) :: num_element_conn(n_elements)
      integer(kind=INT32) :: element_mask(n_elements)

      ! Define 2x2 grid nodes (row-major: bottom to top)
      ! Row 0 (bottom)
      node_coords(:, 1) = [0.0d0, 0.0d0]
      node_coords(:, 2) = [1.0d0, 0.0d0]
      node_coords(:, 3) = [2.0d0, 0.0d0]
      ! Row 1 (middle)
      node_coords(:, 4) = [0.0d0, 1.0d0]
      node_coords(:, 5) = [1.0d0, 1.0d0]
      node_coords(:, 6) = [2.0d0, 1.0d0]
      ! Row 2 (top)
      node_coords(:, 7) = [0.0d0, 2.0d0]
      node_coords(:, 8) = [1.0d0, 2.0d0]
      node_coords(:, 9) = [2.0d0, 2.0d0]

      ! Define element connectivity (counter-clockwise)
      ! Element 1: bottom-left quad (nodes 1,2,5,4)
      element_conn(1:4) = [1, 2, 5, 4]
      ! Element 2: bottom-right quad (nodes 2,3,6,5)
      element_conn(5:8) = [2, 3, 6, 5]
      ! Element 3: top-left quad (nodes 4,5,8,7)
      element_conn(9:12) = [4, 5, 8, 7]
      ! Element 4: top-right quad (nodes 5,6,9,8)
      element_conn(13:16) = [5, 6, 9, 8]

      num_element_conn = [4, 4, 4, 4]
      element_mask = [1, 1, 2, 2]  ! Elements 1-2: Ocean, 3-4: Land

      ! Create file metadata
      file_metadata = FileMetadata()
      
      ! Add dimensions
      call file_metadata%add_dimension('nodeCount', n_nodes, rc=status)
      call file_metadata%add_dimension('elementCount', n_elements, rc=status)
      call file_metadata%add_dimension('coordDim', 2, rc=status)
      call file_metadata%add_dimension('connectionCount', n_conn, rc=status)

      ! Add nodeCoords variable
      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', rc=status)
      call var%add_attribute('units', Attribute('degrees'))
      call var%add_attribute('long_name', Attribute('Node coordinates (longitude, latitude)'))
      call file_metadata%add_variable('nodeCoords', var, rc=status)

      ! Add elementConn variable
      var = Variable(type=pFIO_INT32, dimensions='connectionCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Node indices that define the element connectivity'))
      call var%add_attribute('_FillValue', Attribute(-1))
      call var%add_attribute('start_index', Attribute(1))
      call file_metadata%add_variable('elementConn', var, rc=status)

      ! Add numElementConn variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', Attribute('Number of nodes per element'))
      call file_metadata%add_variable('numElementConn', var, rc=status)

      ! Add elementMask variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Surface type: {1: Ocean, 2: Land, 3: Lake, 4: Landice}'))
      call file_metadata%add_variable('elementMask', var, rc=status)

      ! Add global attributes
      call file_metadata%add_attribute('gridType', Attribute('unstructured'))
      call file_metadata%add_attribute('version', Attribute('0.9'))
      call file_metadata%add_attribute('convention', Attribute('ESMF'))
      ! Create file and write metadata
      call formatter%create(file=filename,mode=PFIO_CLOBBER, rc=status)
      call formatter%write(file_metadata, rc=status)

       ! Write data
       call formatter%put_var('nodeCoords', node_coords, rc=status)
       call formatter%put_var('elementConn', element_conn, rc=status)
       call formatter%put_var('numElementConn', num_element_conn, rc=status)
       call formatter%put_var('elementMask', element_mask, rc=status)

       ! Close file
       call formatter%close(rc=status)

       _RETURN(_SUCCESS)
    end subroutine create_four_quad_mesh_file

   ! Create a mesh with 8 elements for 4-PE testing (2 elements per PE)
   ! Grid layout: 4x2 grid of quads
   subroutine create_eight_element_mesh_file(filename, rc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable) :: var
      
      ! Mesh dimensions
      integer, parameter :: n_nodes = 15
      integer, parameter :: n_elements = 8
      integer, parameter :: n_conn = 32  ! 4 nodes per element * 8 elements
      
      ! Mesh data
      real(kind=REAL64) :: node_coords(2, n_nodes)
      integer(kind=INT32) :: element_conn(n_conn)
      integer(kind=INT32) :: num_element_conn(n_elements)
      integer(kind=INT32) :: element_mask(n_elements)
      integer :: i, j, node_idx, elem_idx, conn_idx

      ! Define 5x3 grid nodes (5 columns, 3 rows)
      node_idx = 0
      do j = 0, 2  ! rows
         do i = 0, 4  ! columns
            node_idx = node_idx + 1
            node_coords(1, node_idx) = real(i, REAL64)  ! longitude
            node_coords(2, node_idx) = real(j, REAL64)  ! latitude
         end do
      end do

      ! Define element connectivity for 4x2 grid of quads
      conn_idx = 0
      do j = 0, 1  ! 2 rows of elements
         do i = 0, 3  ! 4 columns of elements
            elem_idx = j * 4 + i + 1
            ! Bottom-left, bottom-right, top-right, top-left (counter-clockwise)
            node_idx = j * 5 + i + 1
            conn_idx = conn_idx + 1
            element_conn(conn_idx) = node_idx          ! bottom-left
            conn_idx = conn_idx + 1
            element_conn(conn_idx) = node_idx + 1      ! bottom-right
            conn_idx = conn_idx + 1
            element_conn(conn_idx) = node_idx + 6      ! top-right
            conn_idx = conn_idx + 1
            element_conn(conn_idx) = node_idx + 5      ! top-left
            num_element_conn(elem_idx) = 4
         end do
      end do

      ! Alternate masks: odd elements Ocean, even elements Land
      do i = 1, n_elements
         if (mod(i, 2) == 1) then
            element_mask(i) = 1  ! Ocean
         else
            element_mask(i) = 2  ! Land
         end if
      end do

      ! Create file metadata
      file_metadata = FileMetadata()
      
      ! Add dimensions
      call file_metadata%add_dimension('nodeCount', n_nodes, rc=status)
      call file_metadata%add_dimension('elementCount', n_elements, rc=status)
      call file_metadata%add_dimension('coordDim', 2, rc=status)
      call file_metadata%add_dimension('connectionCount', n_conn, rc=status)

      ! Add nodeCoords variable
      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', rc=status)
      call var%add_attribute('units', Attribute('degrees'))
      call var%add_attribute('long_name', Attribute('Node coordinates (longitude, latitude)'))
      call file_metadata%add_variable('nodeCoords', var, rc=status)

      ! Add elementConn variable
      var = Variable(type=pFIO_INT32, dimensions='connectionCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Node indices that define the element connectivity'))
      call var%add_attribute('_FillValue', Attribute(-1))
      call var%add_attribute('start_index', Attribute(1))
      call file_metadata%add_variable('elementConn', var, rc=status)

      ! Add numElementConn variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', Attribute('Number of nodes per element'))
      call file_metadata%add_variable('numElementConn', var, rc=status)

      ! Add elementMask variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Surface type: {1: Ocean, 2: Land, 3: Lake, 4: Landice}'))
      call file_metadata%add_variable('elementMask', var, rc=status)

      ! Add global attributes
      call file_metadata%add_attribute('gridType', Attribute('unstructured'))
      call file_metadata%add_attribute('version', Attribute('0.9'))
      call file_metadata%add_attribute('convention', Attribute('ESMF'))

      ! Create file and write metadata
      call formatter%create(file=filename,mode=pFIO_Clobber,  rc=status)
      call formatter%write(file_metadata, rc=status)

      ! Write data
      call formatter%put_var('nodeCoords', node_coords, rc=status)
      call formatter%put_var('elementConn', element_conn, rc=status)
      call formatter%put_var('numElementConn', num_element_conn, rc=status)
      call formatter%put_var('elementMask', element_mask, rc=status)

      ! Close file
      call formatter%close(rc=status)

      _RETURN(_SUCCESS)
   end subroutine create_eight_element_mesh_file

   ! Create a mesh with mixed element types (triangles and quads) for 4-PE testing
   ! 4 elements: 2 triangles + 2 quads
   subroutine create_mixed_element_mesh_file(filename, rc)
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: file_metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(Variable) :: var
      
      ! Mesh dimensions
      integer, parameter :: n_nodes = 7
      integer, parameter :: n_elements = 4
      integer, parameter :: n_conn = 14  ! 3+3+4+4
      
      ! Mesh data
      real(kind=REAL64) :: node_coords(2, n_nodes)
      integer(kind=INT32) :: element_conn(n_conn)
      integer(kind=INT32) :: num_element_conn(n_elements)
      integer(kind=INT32) :: element_mask(n_elements)

      ! Define nodes for mixed mesh
      node_coords(:, 1) = [0.0d0, 0.0d0]   ! Bottom-left
      node_coords(:, 2) = [1.0d0, 0.0d0]   ! Bottom-center
      node_coords(:, 3) = [2.0d0, 0.0d0]   ! Bottom-right
      node_coords(:, 4) = [0.5d0, 1.0d0]   ! Middle-left
      node_coords(:, 5) = [1.5d0, 1.0d0]   ! Middle-right
      node_coords(:, 6) = [0.0d0, 2.0d0]   ! Top-left
      node_coords(:, 7) = [2.0d0, 2.0d0]   ! Top-right

      ! Element 1: Triangle (nodes 1,2,4)
      element_conn(1:3) = [1, 2, 4]
      num_element_conn(1) = 3
      
      ! Element 2: Triangle (nodes 2,3,5)
      element_conn(4:6) = [2, 3, 5]
      num_element_conn(2) = 3
      
      ! Element 3: Quad (nodes 1,4,6,1) - adjusted to be valid
      element_conn(7:10) = [1, 4, 6, 1]
      num_element_conn(3) = 4
      
      ! Element 4: Quad (nodes 3,5,7,3) - adjusted to be valid
      element_conn(11:14) = [3, 5, 7, 3]
      num_element_conn(4) = 4

      element_mask = [1, 1, 2, 2]  ! Triangles: Ocean, Quads: Land

      ! Create file metadata
      file_metadata = FileMetadata()
      
      ! Add dimensions
      call file_metadata%add_dimension('nodeCount', n_nodes, rc=status)
      call file_metadata%add_dimension('elementCount', n_elements, rc=status)
      call file_metadata%add_dimension('coordDim', 2, rc=status)
      call file_metadata%add_dimension('connectionCount', n_conn, rc=status)

      ! Add nodeCoords variable
      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', rc=status)
      call var%add_attribute('units', Attribute('degrees'))
      call var%add_attribute('long_name', Attribute('Node coordinates (longitude, latitude)'))
      call file_metadata%add_variable('nodeCoords', var, rc=status)

      ! Add elementConn variable
      var = Variable(type=pFIO_INT32, dimensions='connectionCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Node indices that define the element connectivity'))
      call var%add_attribute('_FillValue', Attribute(-1))
      call var%add_attribute('start_index', Attribute(1))
      call file_metadata%add_variable('elementConn', var, rc=status)

      ! Add numElementConn variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', Attribute('Number of nodes per element'))
      call file_metadata%add_variable('numElementConn', var, rc=status)

      ! Add elementMask variable
      var = Variable(type=pFIO_INT32, dimensions='elementCount', rc=status)
      call var%add_attribute('long_name', &
           Attribute('Surface type: {1: Ocean, 2: Land, 3: Lake, 4: Landice}'))
      call file_metadata%add_variable('elementMask', var, rc=status)

      ! Add global attributes
      call file_metadata%add_attribute('gridType', Attribute('unstructured'))
      call file_metadata%add_attribute('version', Attribute('0.9'))
      call file_metadata%add_attribute('convention', Attribute('ESMF'))

      ! Create file and write metadata
      call formatter%create(file=filename, mode=pFIO_CLOBBER,  rc=status)
      call formatter%write(file_metadata, rc=status)

      ! Write data
      call formatter%put_var('nodeCoords', node_coords, rc=status)
      call formatter%put_var('elementConn', element_conn, rc=status)
      call formatter%put_var('numElementConn', num_element_conn, rc=status)
      call formatter%put_var('elementMask', element_mask, rc=status)

print*, __LINE__
      ! Close file
      call formatter%close(rc=status)
print*, __LINE__
      _RETURN(_SUCCESS)
   end subroutine create_mixed_element_mesh_file

   ! Clean up test file
    subroutine cleanup_test_file(filename, rc)
       character(len=*), intent(in) :: filename
       integer, optional, intent(out) :: rc

       integer :: status
       logical :: file_exists

       inquire(file=filename, exist=file_exists)
       if (file_exists) then
          open(unit=999, file=filename, status='old', iostat=status)
          if (status == 0) then
             close(unit=999, status='delete', iostat=status)
          end if
       end if

       _RETURN(_SUCCESS)
    end subroutine cleanup_test_file

end module mapl3g_MeshTestHelper

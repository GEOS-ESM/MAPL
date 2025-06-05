#include "MAPL_ErrLog.h"
module sf_Mesh
   use sf_Direction
   use sf_Vertex
   use sf_VertexVector
   use sf_Element
   use sf_ElementVector
   use mapl_ErrorHandling
   use gFTL2_Integer64Vector
   use esmf
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   implicit none(type,external)
   private

   public :: mesh
   public :: initialize
   

   type :: Mesh
      integer(PIXEL_KIND), pointer :: pixels(:,:) => null()
      type(VertexVector) :: vertices
      type(ElementVector) :: elements
      real(kind=REAL64) :: longitude_range(2)
      real(kind=REAL64) :: latitude_range(2)
   contains
      procedure :: initialize
      procedure :: get_element
      procedure :: get_vertex
      procedure :: num_elements

      procedure :: add_vertex
      procedure :: get_perimeter
      procedure :: element_degree
      procedure :: connect
      procedure :: split ! as even as possible
      procedure :: split_connection
      procedure :: refine
      procedure :: refine_north_south
      procedure :: refine_east_west
      procedure :: make_esmf_mesh
   end type Mesh
      

   
contains

   function add_vertex(this, v) result(iv)
      integer(kind=INT64) :: iv
      class(Mesh), target, intent(inout) :: this
      type(Vertex), intent(in) :: v

      call this%vertices%push_back(v)
      iv = this%vertices%size()
   end function add_vertex

   subroutine connect(this, iv_1, iv_2, rc)
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: iv_1, iv_2
      integer, optional, intent(out) :: rc

      type(Vertex), pointer :: v_1, v_2
      integer :: dir
      integer :: status

      v_1 => this%get_vertex(iv_1)
      _ASSERT(associated(v_1), 'uh oh')
      v_2 => this%get_vertex(iv_2)
      _ASSERT(associated(v_2), 'uh oh')

      !print*
      !_HERE, 'connecting vertices: ', iv_1, iv_2
      !_HERE, '  v1 loc: ', v_1%loc
      !_HERE, '  v2 loc: ', v_2%loc
      !print*

      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
      call v_1%insert_connection(iv_2, dir, _RC)
      call v_2%insert_connection(iv_1, reverse(dir), _RC)

      _RETURN(_SUCCESS)

   end subroutine connect

   ! Start with 2 caps and a set of quadrants on sphere
   subroutine initialize(m, pixels, longitude_range, latitude_range, rc)
      class(Mesh), target, intent(inout) :: m
      integer(kind=PIXEL_KIND), target, intent(in) :: pixels(:,:)
      real(kind=REAL64), intent(in) :: longitude_range(2), latitude_range(2)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: ni, nj, nj_cap
      integer :: j_s ! loc of SW corner of uppermost pixel in southern cap
      integer :: j_n ! loc of SW corner of lower most pixel in northern cap

      integer(kind=INT64) :: iv_1, iv_2, iv_3, iv_4 ! southern cap
      integer(kind=INT64) :: iv_5, iv_6, iv_7, iv_8 ! norther cap

      ni = size(pixels, 1)
      nj = size(pixels, 2)
      nj_cap = nj / (180/2)  ! 88 degrees

      j_s = 1 + nj_cap
      j_n = 1 + (nj - nj_cap)
      !_HERE, 'caps: ', j_s, j_n

      m%longitude_range = longitude_range
      m%latitude_range = latitude_range
      m%pixels => pixels

      ! Polygon around south pole at 88 degrees south:
      ! 88 degrees is chosen to prevent large prime factors from happening for dn = (nj_n-1) - (nj_s+1) + 1
      ! Assuming a 10 arcsecond pixelization this provides:
      !     nj = 64800
      !     dn = 63360
      iv_1 = m%add_vertex(Vertex([1+0*ni/4,j_s]))
      iv_2 = m%add_vertex(Vertex([1+1*ni/4,j_s]))
      iv_3 = m%add_vertex(Vertex([1+2*ni/4,j_s]))
      iv_4 = m%add_vertex(Vertex([1+3*ni/4,j_s]))

      iv_5 = m%add_vertex(Vertex([1+0*ni/4,j_n]))
      iv_6 = m%add_vertex(Vertex([1+1*ni/4,j_n]))
      iv_7 = m%add_vertex(Vertex([1+2*ni/4,j_n]))
      iv_8 = m%add_vertex(Vertex([1+3*ni/4,j_n]))

      call m%connect(iv_1, iv_2, _RC)
      call m%connect(iv_1, iv_4, _RC)
      call m%connect(iv_2, iv_3, _RC)
      call m%connect(iv_3, iv_4, _RC)

      call m%connect(iv_5, iv_6, _RC)
      call m%connect(iv_5, iv_8, _RC)
      call m%connect(iv_6, iv_7, _RC)
      call m%connect(iv_7, iv_8, _RC)

      call m%connect(iv_1, iv_5, _RC)
      call m%connect(iv_2, iv_6, _RC)
      call m%connect(iv_3, iv_7, _RC)
      call m%connect(iv_4, iv_8, _RC)

      ! Quadrants
      call m%elements%push_back(Element(m%pixels(1+0*(ni/4):1*(ni/4),j_s:j_n-1), iv_1, dir=EAST))
      call m%elements%push_back(Element(m%pixels(1+1*(ni/4):2*(ni/4),j_s:j_n-1), iv_2, dir=EAST))
      call m%elements%push_back(Element(m%pixels(1+2*(ni/4):3*(ni/4),j_s:j_n-1), iv_3, dir=EAST))
      call m%elements%push_back(Element(m%pixels(1+3*(ni/4):4*(ni/4),j_s:j_n-1), iv_4, dir=EAST))

      ! Elements
      ! South cap
      call m%elements%push_back(Element(m%pixels(:,1:j_s-1), iv_1, dir=WEST))
      ! North cap - careful with orientation (corner is not SW)
      call m%elements%push_back(Element(m%pixels(:,j_n:), iv_5, dir=EAST))

      _RETURN(_SUCCESS)
   end subroutine initialize

   ! Split interval in 2 as evenly as possible
   function split(this, ivs, rc) result(new_iv)
      integer(kind=INT64) :: new_iv
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: ivs(2) ! from and 2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: d_loc(2)
      integer :: new_loc(2)
      integer :: ni, dir
      type(Vertex), pointer :: v_1, v_2, v_new

      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
!#      _HERE, 'split: ', v_1%loc, v_2%loc
      ni = size(this%pixels,1)
      d_loc = delta_loc(v_1%loc, v_2%loc, ni)

      new_loc = add_loc(v_1%loc, (d_loc+1)/2, ni)
!#      _HERE, '   new_loc: ', new_loc
      new_iv = this%add_vertex(Vertex(new_loc))

      ! v_1 and v_2 might now be dangling ...
      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
      call v_1%replace_connection(new_iv, dir=dir, _RC)
      call v_2%replace_connection(new_iv, dir=reverse(dir), _RC)

      v_new => this%get_vertex(new_iv)
      call v_new%insert_connection(ivs(1), reverse(dir), _RC)
      call v_new%insert_connection(ivs(2), dir, _RC)

      _RETURN(_SUCCESS)
   end function split

   recursive function split_connection(this, ivs, n_split, rc) result(new_ivs)
      integer, intent(in) :: n_split
      integer(kind=INT64) :: new_ivs(n_split-1)
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: ivs(2) ! from and 2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: d_loc(2)
      integer :: new_loc(2)
      integer :: ni, dir
      type(Vertex), pointer :: v_1, v_2, v_new


      _ASSERT(n_split >= 2, 'Must have n_split >= 2 for when splitting a connection')

      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)

      ni = size(this%pixels,1)
      d_loc = delta_loc(v_1%loc, v_2%loc, ni)
      _ASSERT(all(modulo(d_loc, n_split) == 0), 'expected split to be even')

      new_loc = add_loc(v_1%loc, d_loc/n_split, ni)
      new_ivs(1) = this%add_vertex(Vertex(new_loc))

      ! v_1 and v_2 might now be dangling ...
      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
      !print*
      !_HERE, new_ivs(1), ivs, 'split connection in direction: ', dir_string(dir), ' n_split:  ', n_split
      !_HERE, 'new loc: ', new_loc
      !_HERE, '  d loc: ', d_loc
      !_HERE, '  d loc: ', d_loc/n_split
      !_HERE, 'orig loc v_1 ', v_1%loc
      !_HERE, 'orig loc v_2', v_2%loc
      !_HERE, 'orig loc v_2', v_2%loc - v_1%loc
      !print*
      call v_1%replace_connection(new_ivs(1), dir=dir, _RC)
      call v_2%replace_connection(new_ivs(1), dir=reverse(dir), _RC)
      !_HERE
      v_new => this%get_vertex(new_ivs(1))
      call v_new%insert_connection(ivs(1), reverse(dir), _RC)
      call v_new%insert_connection(ivs(2), dir, _RC)

      !_HERE
      if (n_split > 2) then
         new_ivs(2:) = this%split_connection([new_ivs(1),ivs(2)], n_split-1, _RC)
      end if
      !_HERE

     
      _RETURN(_SUCCESS)
   end function split_connection

   ! Refine element e ...
   subroutine refine(this, e, rc)
      class(Mesh), target, intent(inout) :: this
      type(Element), intent(inout) :: e
      integer, optional, intent(out) :: rc

      integer :: ni, nj
      integer :: status

      ni = size(e%pixels, 1)
      nj = size(e%pixels, 2)

      _ASSERT(ni*nj > 1, 'No need to refine - should not reach here.')

      if (nj > ni) then
         call this%refine_north_south(e, _RC)
         _RETURN(_SUCCESS)
      end if

      call this%refine_east_west(e, _RC)

      _RETURN(_SUCCESS)
   end subroutine refine

   ! Splitting a cell in the North-South direction requires that each
   ! N/S edge is itself plit.  However, refinements of neighboring
   ! cells may have already split these edges, necessitating
   ! subtle logic.
   !

   ! The process involves introducing and then connecting intermediate
   ! vertices.  (See diagram below.)    Some of these vertices may have already
   ! been created by previous refinement of neighboring elements.

   !    [4] ------------------- [3]
   !     |                       |
   !     |                       |
   !     |                       |
   !     |          1            |
   !     |                       |
   !     |                       |
   !     |                       |   
   !     |                       |
   !    [1] ------------------- [2]

   !
   ! When refine by factor of 3 we get:
   !

   !    [4] ------------------- [3]
   !     |                       |
   !     |          3            |
   !    [7] ................... [6|
   !     |                       |
   !     |          2            |
   !    [8] ................... [5]
   !     |                       |
   !     |          1            |
   !    [1] ------------------- [2]

   ! The complication here is that some of the intermediate vertices
   ! {5,6,7,8} may have already been created via refinement of the
   ! neighbor.  The good news is that along an edge either all or none
   ! of the intermediates will exist.  Nothing in between.   This is true
   ! because refinement at a given level is done across all elements before
   ! proceeding to the next level.

   
   subroutine refine_north_south(this, e, rc)
      class(Mesh), target, intent(inout) :: this
      type(Element), intent(inout) :: e
      integer, optional, intent(out) :: rc

      integer :: ni, nj
      integer(kind=INT64) :: iv_sw, iv_se, iv_ne, iv_nw
      integer :: k_sw, k_se, k_ne, k_nw
      integer, dimension(2) :: sw_corner, se_corner, ne_corner, nw_corner
      integer :: k
      type(Integer64Vector), target :: vertices
      integer(kind=INT64) :: iv_east, iv_west
      integer(kind=PIXEL_KIND), pointer :: pixels(:,:)
      type(Vertex), pointer :: v
      integer :: status
      integer :: ni_glob
      integer :: j_mid
      type(Element) :: new_element

      ni = size(e%pixels, 1)
      nj = size(e%pixels, 2)
!#      call describe_element(this, e)

      ! Find corners
      v => this%get_vertex(e%iv_0)
      ni_glob = size(this%pixels,1)
      sw_corner = v%loc
      se_corner = add_loc(sw_corner, [ni, 0], ni_glob)
      ne_corner = add_loc(sw_corner, [ni,nj], ni_glob)
      nw_corner = add_loc(sw_corner, [ 0,nj], ni_glob)

      vertices = this%get_perimeter(e)
      k_sw = vertices%size() + 1
      k_se = -1 ! not found unless
      k_ne = -1
      k_nw = -1
      do k = 1, vertices%size()
         v => this%get_vertex(vertices%of(k))
         if (all(v%loc == se_corner)) then
            k_se = k
         elseif (all(v%loc == ne_corner)) then
            k_ne = k
         elseif (all(v%loc == nw_corner)) then ! done
            k_nw = k
            exit
         end if
      end do

      ! Check that all intermediates exist, or none.
      _ASSERT(any(k_ne - k_se == [1, 2]), 'mismatched refinement')
      _ASSERT(any(k_sw - k_nw == [1, 2]), 'mismatched refinement')

      !_HERE
      if (k_ne - k_se == 1) then !
         iv_se = vertices%of(k_se)
         iv_ne = vertices%of(k_ne)
!#         _HERE, 'splitting north south? ', iv_se, iv_ne
         iv_east = this%split([iv_se, iv_ne], _RC)
      else
         iv_east = vertices%of(k_se+1)
      end if

      if (k_sw - k_nw == 1) then !
         iv_sw = vertices%of(1) ! k_sw happens at both ends of polygon
         iv_nw = vertices%of(k_nw)
         iv_west = this%split([iv_sw, iv_nw], _RC)
      else
         iv_west = vertices%of(k_nw+1)
      end if

      call this%connect(iv_west, iv_east, _RC)

     ! Modify original element
      pixels => e%pixels
      j_mid = 1 + (nj-1)/2 
      e%pixels => pixels(:,:j_mid)
!#      call describe_element(this, e)

      ! Add new element
      new_element = Element(pixels(:,j_mid+1:), iv_west, dir=EAST)
!#      call describe_element(this, new_element)
      call this%elements%push_back(new_element)

      _RETURN(_SUCCESS)

   end subroutine refine_north_south

   subroutine refine_east_west(this, e, rc)
      class(Mesh), target, intent(inout) :: this
      type(Element), intent(inout) :: e
      integer, optional, intent(out) :: rc

      integer :: ni, nj
      integer :: i_mid
      integer(kind=INT64) :: iv_sw, iv_se, iv_ne, iv_nw
      integer :: k_sw, k_se, k_ne, k_nw
      integer, dimension(2) :: sw_corner, se_corner, ne_corner, nw_corner
      integer :: k
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: vertices
      integer(kind=INT64) :: iv_south, iv_north
      integer(kind=PIXEL_KIND), pointer :: pixels(:,:)
      integer :: i_0, i_1
      integer :: status
      type(Element) :: new_element
      integer :: ni_glob

      ni = size(e%pixels, 1)
      nj = size(e%pixels, 2)

      vertices = this%get_perimeter(e)

      ! Find corners include wrapping around dateline
      v => this%get_vertex(e%iv_0)
      ni_glob = size(this%pixels,1)
      sw_corner = v%loc
      se_corner = add_loc(sw_corner, [ni, 0], ni_glob)
      ne_corner = add_loc(sw_corner, [ni,nj], ni_glob)
      nw_corner = add_loc(sw_corner, [ 0,nj], ni_glob)

!#       call describe_element(this, e)
!#     _HERE, 'sw: ', sw_corner
!#      _HERE, 'se: ', se_corner
!#      _HERE, 'ne: ', ne_corner
!#      _HERE, 'nw: ', nw_corner
      k_sw = 1
      k_se = -1 ! not found unless
      k_ne = -1
      k_nw = -1

!#      _HERE, vertices%size()
      do k = 1, vertices%size()
         v => this%get_vertex(vertices%of(k))
!#         _HERE, k, v%loc
         if (all(v%loc == se_corner)) then
            k_se = k
         elseif (all(v%loc == ne_corner)) then
            k_ne = k
         elseif (all(v%loc == nw_corner)) then ! done
            k_nw = k
            exit
         end if
      end do

      ! Check that all intermediates exist, or none.
!#      _HERE, k_se, k_sw
!#      _HERE, k_nw, k_ne
      _ASSERT(any(k_se - k_sw == [1,2]), 'mismatched refinement')
      _ASSERT(any(k_nw - k_ne == [1,2]), 'mismatched refinement')
      
      if (k_se - k_sw == 1) then
         iv_se = vertices%of(k_se)
         iv_sw = vertices%of(k_sw)
         iv_south = this%split([iv_sw, iv_se], _RC)
      else
         iv_south = vertices%of(k_sw+1)
      end if

      if (k_nw - k_ne == 1) then !
         iv_ne = vertices%of(k_ne)
         iv_nw = vertices%of(k_nw)
         iv_north = this%split([iv_nw, iv_ne], _RC)
      else
         ! Careful need to step bacwwards on northern edge
         iv_north = vertices%of(k_nw-1)
      end if

      call this%connect(iv_south, iv_north, _RC)

      pixels => e%pixels
      i_mid = 1 + (ni-1)/2
      e%pixels => pixels(:i_mid,:)

      new_element = Element(pixels(i_mid+1:,:), iv_south, dir=EAST)
!#      call describe_element(this, new_element)
          
      call this%elements%push_back(new_element)

     _RETURN(_SUCCESS)

  end subroutine refine_east_west


  ! Could be slightly faster if we did not save the vertex indices as
  ! we proceed.
   integer function element_degree(this, e)
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e

      type(Integer64Vector) :: ivs

      ivs = this%get_perimeter(e)
      element_degree = ivs%size()

   end function element_degree

   function get_element(this, ith) result(e)
      type(Element), pointer :: e
      class(Mesh), target, intent(in) :: this
      integer, intent(in) :: ith

      e => this%elements%of(ith)
   end function get_element

   function get_vertex(this, ith) result(v)
      type(Vertex), pointer :: v
      class(Mesh), target,intent(in) :: this
      integer(kind=INT64), intent(in) :: ith

      v => this%vertices%of(ith)
   end function get_vertex

   function get_perimeter(this, e, rc) result(ivs)
      type(Integer64Vector) :: ivs
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e
      integer, optional, intent(out) :: rc

      integer(kind=INT64) :: iv_0, iv
      integer :: dir
      integer :: i
      type(Vertex), pointer :: v
      integer :: j
      integer, parameter :: MAX_COUNT = 1000

      iv_0 = e%iv_0
      dir = e%dir

      call ivs%push_back(iv_0)

      v => this%get_vertex(iv_0)
      iv = v%get_connection(dir)
      v => this%get_vertex(iv)

      ! At each vertex, search in CW direction starting from the
      ! direction we came _from_.
      dir = reverse(e%dir)
      j = 1
      do while (iv /= iv_0)
         j = j + 1
         _ASSERT(j < MAX_COUNT, 'large polygon?')
         call ivs%push_back(iv)
         do i = 1, N_DIR - 1
            dir = next_direction(dir)
            if (v%has_connection(dir)) then
               iv = v%get_connection(dir)
               v => this%get_vertex(iv)
               dir = reverse(dir)
               exit
            end if
         end do
      end do

      _RETURN(_SUCCESS)

   end function get_perimeter

   integer function num_elements(this)
      class(Mesh), intent(in) :: this
      num_elements = this%elements%size()
   end function num_elements

   function add_loc(loc_1, delta, ni) result(sum)
      integer :: sum(2)
      integer, intent(in) :: loc_1(2), delta(2)
      integer, intent(in) :: ni

      sum = loc_1 + delta
      sum(1) = 1 + modulo(sum(1)-1, ni) ! periodic
      
   end function add_loc

   function delta_loc(loc_1, loc_2, ni) result(delta)
      integer :: delta(2)
      integer, intent(in) :: loc_1(2), loc_2(2)
      integer, intent(in) :: ni

      delta = loc_2 - loc_1
      delta(1) = modulo(delta(1) + ni/2, ni) - ni/2 ! periodic
      
   end function delta_loc

   subroutine describe_element(this, e)
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e

      type(Vertex), pointer :: v
      type(Integer64Vector) :: nodes
      integer :: k
      
      print*
      print*,'****************************'
      _HERE, 'describe element: '
      _HERE, '            shp: ', shape(e%pixels)
      _HERE, '           iv_0: ', e%iv_0
      _HERE, '            dir: ', dir_string(e%dir)
      _HERE
      _HERE, '     root vertex: '
      v => this%get_vertex(e%iv_0)
      _HERE, '            loc: ', v%loc
      _HERE, '          conns: ', v%connections

      nodes = this%get_perimeter(e)
      _HERE
      _HERE, '      perimeter: '
      _HERE, '        # nodes: ', nodes%size()
      do k = 1, nodes%size()
         v => this%get_vertex(nodes%of(k))
         _HERE,'      corner: ', k, nodes%of(k), v%loc
      end do
      print*,'****************************'
      print*
   end subroutine describe_element

   function make_esmf_mesh(this, rc) result(msh)
      type(ESMF_Mesh), target :: msh
      class(Mesh), intent(in) :: this
      integer, optional, intent(out):: rc

      integer :: status
      integer :: n_nodes
      integer, allocatable :: nodeIds(:)
      real(kind=REAL64), allocatable :: nodeCoords(:)
      integer :: i, k, kk, i0
      integer(kind=INT64) :: k64
      integer :: np, n_elements, n_conn
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)
      integer :: ni, nj

      msh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, coordSys=ESMF_COORDSYS_SPH_DEG, _RC)

      n_nodes = this%vertices%size()
      ni = size(this%pixels,1)
      nj = size(this%pixels,2)

      allocate(nodeIds(n_nodes))
      allocate(nodeCoords(2*n_nodes))

      _HERE, this%longitude_range
      _HERE, this%latitude_range
      do k = 1, n_nodes
         k64 = k
         v => this%get_vertex(k64)
         nodeIds(k) = k
         nodeCoords(1 + (k-1)*2) = this%longitude_range(1) + (v%loc(1)-1) * (this%longitude_range(2) - this%longitude_range(1)) / ni
         nodeCoords(2 + (k-1)*2) = this%latitude_range(1) + (v%loc(2)-1) * (this%latitude_range(2) - this%latitude_range(1)) / nj
      end do

      call ESMF_MeshAddNodes(msh, nodeIds, nodeCoords, _RC)
      deallocate(nodeCoords, nodeIds)

      n_elements = this%elements%size()
      allocate(elementIds(n_elements))
      allocate(elementTypes(n_elements))

      n_conn = 0
      kk = 0
      do k = 1, n_elements
         kk = kk + 1
         e => this%get_element(k)
         p = this%get_perimeter(e)
         n_conn = n_conn + p%size()
      end do

      allocate(elementConn(n_conn))

      i0 = 0
      kk = 0
      elementIds = huge(1)
      
      do k = 1, n_elements
         kk = kk + 1
         e => this%get_element(k)
         elementIds(kk) = kk
         p = this%get_perimeter(e)
         np = p%size()
         elementTypes(kk) = np
         elementConn(i0+1:i0+np) = [(p%of(i),i=1,np)]
         i0 = i0 + np

!#         elementArea(k) = ...
      end do
         
      call ESMF_MeshAddElements(msh, &
           elementIds, elementTypes, elementConn, &
!#           elementMask, &
!#           elementArea, &
!#           elementCoords, &
!#           elementDistgrid, &
           _RC)

      deallocate(elementConn, elementTypes, elementIds)
      _RETURN(_SUCCESS)
   end function make_esmf_mesh
      


end module sf_Mesh


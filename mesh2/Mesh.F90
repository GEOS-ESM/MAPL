#include "MAPL_ErrLog.h"
module sf_Mesh
   use sf_Direction
   use sf_Vertex
   use sf_VertexVector
   use sf_Element
   use sf_ElementVector
   use mapl_ErrorHandling
   use mapl_constants, only: MAPL_DEGREES_TO_RADIANS_R8, MAPL_PI_R8
   use mapl_constants, only: MAPL_RADIANS_TO_DEGREES
   use gFTL2_Integer64Vector
   use esmf
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   implicit none(type,external)
   private

   public :: mesh
   public :: describe_element
   public :: write_to_file


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
      procedure :: split_loc_north_south
      procedure :: split_loc_east_west
      procedure :: split_north_south ! evenly by area
      procedure :: split_east_west ! easy case
      procedure :: refine
      procedure :: refine_north_south
      procedure :: refine_east_west
      procedure :: make_esmf_mesh_0
      procedure :: make_esmf_mesh
      procedure :: make_esmf_mesh2
      procedure :: make_esmf_mesh3
      procedure :: to_netcdf_0
      procedure :: to_netcdf
      procedure :: to_lon
      procedure :: to_lat
      procedure :: aspect_ratio
      procedure :: get_centroid
      procedure :: resolution
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

      ! Elements
      ! South cap
      call m%elements%push_back(Element(m%pixels(:,1:j_s-1), iv_1, dir=WEST))
      ! North cap - careful with orientation (corner is not SW)
      call m%elements%push_back(Element(m%pixels(:,j_n:), iv_5, dir=EAST))

      ! Quadrants
      call m%elements%push_back(Element(m%pixels(1+0*(ni/4):1*(ni/4),j_s:j_n-1), iv_1, dir=EAST))
      call m%elements%push_back(Element(m%pixels(1+1*(ni/4):2*(ni/4),j_s:j_n-1), iv_2, dir=EAST))
      call m%elements%push_back(Element(m%pixels(1+2*(ni/4):3*(ni/4),j_s:j_n-1), iv_3, dir=EAST))
      call m%elements%push_back(Element(m%pixels(1+3*(ni/4):4*(ni/4),j_s:j_n-1), iv_4, dir=EAST))


      _RETURN(_SUCCESS)
   end subroutine initialize

   function split_loc_east_west(this, ivs, rc) result(new_loc)
      integer :: new_loc(2)
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: ivs(2) ! from and 2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: d_loc(2)
      integer :: ni, dir
      type(Vertex), pointer :: v_1, v_2

      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
      ni = size(this%pixels,1)
      d_loc(1) = delta_loc(v_1%loc(1), v_2%loc(1), ni)
      d_loc(2) = 0

      new_loc = add_loc(v_1%loc, (d_loc+1)/2, ni)

      _RETURN(_SUCCESS)
   end function split_loc_east_west

   function split_loc_north_south(this, ivs, rc) result(new_loc)
      integer :: new_loc(2)
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: ivs(2) ! from and 2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: d_loc(2)
      integer :: dir
      type(Vertex), pointer :: v_1, v_2

      real(kind=REAL64) :: lat_1, lat_2, new_lat ! radians
      real(kind=REAL64) :: frac

      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)

      d_loc(1) = 0
      d_loc(2) = v_2%loc(2) - v_1%loc(2)

      lat_1 = MAPL_DEGREES_TO_RADIANS_R8 * this%to_lat(v_1%loc)
      lat_2 = MAPL_DEGREES_TO_RADIANS_R8 * this%to_lat(v_2%loc)

      new_lat = asin((sin(lat_1) + sin(lat_2))/2)
      frac = (new_lat-lat_1)/(lat_2-lat_1)

      new_loc(1) = v_1%loc(1)
      new_loc(2) = v_1%loc(2) + max(1, floor(frac * d_loc(2)))

      _RETURN(_SUCCESS)
   end function split_loc_north_south

   ! Split interval in 2  as evenly as possible
   function split_east_west(this, ivs, rc) result(new_iv)
      integer(kind=INT64) :: new_iv
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: ivs(2) ! from and 2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dir
      integer :: new_loc(2)
      type(Vertex), pointer :: v_1, v_2, v_new

      new_loc = this%split_loc_east_west(ivs, _RC)
      new_iv = this%add_vertex(Vertex(new_loc))

      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
      call v_1%replace_connection(new_iv, dir=dir, _RC)
      call v_2%replace_connection(new_iv, dir=reverse(dir), _RC)

      v_new => this%get_vertex(new_iv)
      call v_new%insert_connection(ivs(1), reverse(dir), _RC)
      call v_new%insert_connection(ivs(2), dir, _RC)

      _RETURN(_SUCCESS)
   end function split_east_west

   function split_north_south(this, ivs, rc) result(new_iv)
      integer(kind=INT64) :: new_iv
      class(Mesh), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: ivs(2) ! from and 2
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: new_loc(2)
      integer :: dir
      type(Vertex), pointer :: v_1, v_2, v_new

      new_loc = this%split_loc_north_south(ivs, _RC)
      new_iv = this%add_vertex(Vertex(new_loc))

      v_1 => this%vertices%of(ivs(1))
      v_2 => this%vertices%of(ivs(2))
      dir = v_1%get_direction_to(v_2, shape(this%pixels), _RC)
      call v_1%replace_connection(new_iv, dir=dir, _RC)
      call v_2%replace_connection(new_iv, dir=reverse(dir), _RC)

      v_new => this%get_vertex(new_iv)
      call v_new%insert_connection(ivs(1), reverse(dir), _RC)
      call v_new%insert_connection(ivs(2), dir, _RC)

      _RETURN(_SUCCESS)
   end function split_north_south

   ! Refine element e ...
   subroutine refine(this, e, rc)
      class(Mesh), target, intent(inout) :: this
      type(Element), intent(inout) :: e
      integer, optional, intent(out) :: rc

      integer :: ni, nj
      integer :: status

      ! It is possible to end up with region near pole that is 4x1 for
      ! pixels but still has an aspect ration < 1.
      if (aspect_ratio(this, e) < 1 .and. size(e%pixels,2) > 1) then
         call this%refine_north_south(e, _RC)
         _RETURN(_SUCCESS)
      end if

      _ASSERT(size(e%pixels,1) > 1, 'uh oh')
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
      integer :: mid_loc(2)
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

      iv_se = vertices%of(k_se)
      iv_ne = vertices%of(k_ne)
      if (k_ne - k_se == 1) then !
         iv_east = this%split_north_south([iv_se, iv_ne], _RC)
      else
         mid_loc = this%split_loc_north_south([iv_se, iv_ne], _RC)
         iv_east = -1
         do k = k_se+1, k_ne-1
            v => this%get_vertex(vertices%of(k))
            if (all(v%loc == mid_loc)) then
               iv_east = vertices%of(k)
               exit
            end if
         end do
         _ASSERT(iv_east /= -1, 'split point should exist')
      end if

      iv_sw = vertices%of(1) ! k_sw happens at both ends of polygon
      iv_nw = vertices%of(k_nw)
      if (k_sw - k_nw == 1) then !
         iv_west = this%split_north_south([iv_sw, iv_nw], _RC)
      else
         mid_loc = this%split_loc_north_south([iv_sw, iv_nw], _RC)
         iv_west = -1
         do k = k_sw - 1, k_nw + 1, -1
            v => this%get_vertex(vertices%of(k))
            if (all(v%loc == mid_loc)) then
               iv_west = vertices%of(k)
               exit
            end if
         end do
         if (iv_west == -1) then
            call describe_element(this, e)
         end if
         _ASSERT(iv_west /= -1, 'split point should exist')
      end if

      call this%connect(iv_west, iv_east, _RC)

      ! Modify original element
      pixels => e%pixels
      v => this%get_vertex(iv_east)
      j_mid = v%loc(2) - sw_corner(2)

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
      integer, dimension(2) :: sw_corner, se_corner, ne_corner, nw_corner, mid_loc
      integer :: k
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: vertices
      integer(kind=INT64) :: iv_south, iv_north
      integer(kind=PIXEL_KIND), pointer :: pixels(:,:)
      integer :: status
      type(Element) :: new_element
      integer :: ni_glob

      ni = size(e%pixels, 1)
      nj = size(e%pixels, 2)

!#      call describe_element(this, e)
      vertices = this%get_perimeter(e)

      ! Find corners include wrapping around dateline
      v => this%get_vertex(e%iv_0)
      ni_glob = size(this%pixels,1)
      sw_corner = v%loc
      se_corner = add_loc(sw_corner, [ni, 0], ni_glob)
      ne_corner = add_loc(sw_corner, [ni,nj], ni_glob)
      nw_corner = add_loc(sw_corner, [ 0,nj], ni_glob)

      k_sw = 1
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

      iv_se = vertices%of(k_se)
      iv_sw = vertices%of(k_sw)
      if (k_se - k_sw == 1) then
         iv_south = this%split_east_west([iv_sw, iv_se], _RC)
      else
         mid_loc = this%split_loc_east_west([iv_sw, iv_se], _RC)
         iv_south = -1
         do k = k_sw+1, k_se-1
            v => this%get_vertex(vertices%of(k))
            if (all(v%loc == mid_loc)) then
               iv_south = vertices%of(k)
               exit
            end if
         end do
         _ASSERT(iv_south /= -1, 'split point should exist')
      end if

      iv_ne = vertices%of(k_ne)
      iv_nw = vertices%of(k_nw)
      if (k_nw - k_ne == 1) then !
         iv_north = this%split_east_west([iv_nw, iv_ne], _RC)
      else
         mid_loc = this%split_loc_east_west([iv_nw, iv_ne], _RC)
         iv_north = -1
         do k = k_nw-1, k_ne+1, -1
            v => this%get_vertex(vertices%of(k))
            if (all(v%loc == mid_loc)) then
               iv_north = vertices%of(k)
               exit
            end if
         end do
         if (iv_north == -1) then
            call describe_element(this, e)
         end if
         _ASSERT(iv_north /= -1, 'split point should exist')
      end if

      call this%connect(iv_south, iv_north, _RC)

      pixels => e%pixels
      v => this%get_vertex(iv_south)
      i_mid = v%loc(1) - sw_corner(1)
      e%pixels => pixels(:i_mid,:)
!#      call describe_element(this, e)

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

   function delta_loc(lon_1, lon_2, ni) result(delta)
      integer :: delta
      integer, intent(in) :: lon_1, lon_2
      integer, intent(in) :: ni

      delta = lon_2 - lon_1
      delta = modulo(delta + ni/2, ni) - ni/2 ! periodic

   end function delta_loc

   subroutine describe_element(this, e)
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e

      type(Vertex), pointer :: v
      type(Integer64Vector) :: nodes
      integer :: k
      real(kind=REAL64) :: lon, lat

      print*
      print*,'****************************'
      _HERE, 'describe element: '
      _HERE, '            shp: ', shape(e%pixels)
      _HERE, '           iv_0: ', e%iv_0
      _HERE, '            dir: ', dir_string(e%dir)
      _HERE, '        min/max: ', minval(e%pixels), maxval(e%pixels)
      _HERE, '   aspect ratio: ', this%aspect_ratio(e)
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
         lon = this%to_lon(v%loc)
         lat = this%to_lat(v%loc)
         write(*,'(a,2x,3(i8.0,1x),2x,2(g15.7,1x))') '      corner: ', k, nodes%of(k), v%loc, lon, lat
      end do
      print*,'****************************'
      print*
   end subroutine describe_element

   function make_esmf_mesh_0(this, rc) result(msh)
      type(ESMF_Mesh), target :: msh
      class(Mesh), intent(in) :: this
      integer, optional, intent(out):: rc

      integer :: status
      integer :: n_nodes
      integer, allocatable :: nodeIds(:)
      real(kind=REAL64), allocatable :: nodeCoords(:)
      integer :: i, k, kk, i0
      integer(kind=INT64) :: k64
      integer :: np, n_elements, n_vertices
      integer(kind=INT64) :: n_conn
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)
      integer :: k_element, typ

      msh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, coordSys=ESMF_COORDSYS_SPH_DEG, _RC)

      ! As a workaround to ESMF issue, we create an additional node at the
      ! center of each element and perform our own triangulation.
      n_vertices = this%vertices%size()
      n_elements = this%elements%size()
      n_nodes = n_vertices

      allocate(nodeIds(n_nodes))
      allocate(nodeCoords(2*n_nodes))

      do k = 1, n_vertices
         k64 = k
         v => this%get_vertex(k64)
         nodeIds(k) = k
         nodeCoords(1 + (k-1)*2) = this%to_lon(v%loc)
         nodeCoords(2 + (k-1)*2) = this%to_lat(v%loc)
      end do

      call ESMF_MeshAddNodes(msh, nodeIds, nodeCoords, _RC)

      n_conn = 0
      _HERE, n_elements
      do k = 1, n_elements
         e => this%get_element(k)
         p = this%get_perimeter(e)
         np = p%size()
         n_conn = n_conn + np
      end do
      _HERE,'total connection :', n_conn

      deallocate(nodeCoords, nodeIds)

      allocate(elementIds(n_elements))
      allocate(elementTypes(n_elements))
      allocate(elementConn(n_conn))
!#      elementTypes = ESMF_MESHELEMTYPE_TRI

      i0 = 0
      elementIds = huge(1)

      kk = 0
      do k = 1, n_elements
         e => this%get_element(k)
!#         typ = e%get_type()
         elementIds(k) = k

         p = this%get_perimeter(e)
         np = p%size()
         elementTypes(k) = np

         do i = 1, np
            elementConn(kk+i) = p%of(i)
         end do
         kk = kk + np
      end do

      call ESMF_MeshAddElements(msh, &
           elementIds, elementTypes, elementConn, &
!#           elementArea, &
!#           elementCoords, &
!#           elementDistgrid, &
           _RC)
      _HERE
      deallocate(elementConn, elementTypes, elementIds)
      _RETURN(_SUCCESS)
   end function make_esmf_mesh_0

   ! Naive triangulation?
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
      integer :: np, n_elements, n_vertices, n_esmf_elements
      integer(kind=INT64) :: n_conn
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)
      integer, allocatable :: elementMask(:)
      integer :: k_element, typ

      msh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, coordSys=ESMF_COORDSYS_SPH_DEG, _RC)

      ! As a workaround to ESMF issue, we create an additional node at the
      ! center of each element and perform our own triangulation.
      n_vertices = this%vertices%size()
      n_elements = this%elements%size()
      n_nodes = n_vertices + n_elements

      allocate(nodeIds(n_nodes))
      allocate(nodeCoords(2*n_nodes))

      do k = 1, n_vertices
         k64 = k
         v => this%get_vertex(k64)
         nodeIds(k) = k
         nodeCoords(1 + (k-1)*2) = this%to_lon(v%loc)
         nodeCoords(2 + (k-1)*2) = this%to_lat(v%loc)
      end do


      kk = n_vertices
      do k = 1, n_elements
         kk = kk + 1
         e => this%get_element(k)
         nodeIds(kk) = kk
         nodeCoords(1 + (kk-1)*2: 2 + (kk-1)*2) = this%get_centroid(e)
      end do

      call ESMF_MeshAddNodes(msh, nodeIds, nodeCoords, _RC)
      deallocate(nodeCoords, nodeIds)

      n_conn = 0
      n_esmf_elements = 0
      do k = 1, n_elements
         e => this%get_element(k)
         p = this%get_perimeter(e)
         np = p%size()
         ! triangulate
         n_conn = n_conn + 3*np
         n_esmf_elements = n_esmf_elements + np
      end do
      _HERE,'total connection :', n_conn

      allocate(elementIds(n_esmf_elements))
      allocate(elementTypes(n_esmf_elements))
      allocate(elementConn(n_conn))
      allocate(elementMask(n_esmf_elements))
      elementMask = -1
      elementTypes = ESMF_MESHELEMTYPE_TRI

      i0 = 0
      elementIds = huge(1)

      kk = 0
      k_element = 0
      do k = 1, n_elements
         e => this%get_element(k)
         typ = e%get_type()
         p = this%get_perimeter(e)
         np = p%size()
         do i = 1, np
            elementConn(kk+1) = n_vertices + k
            elementConn(kk+2) = p%of(1+mod(i-1,np))
            elementConn(kk+3) = p%of(1 + mod(i-1+1,np))
!#            elementArea(k) = ...
            kk = kk + 3
            k_element = k_element + 1
            elementIds(k_element) = k_element
            elementMask(k_element) = typ
!#            _HERE, 'type? ', k_element, typ
         end do
      end do

      

      call ESMF_MeshAddElements(msh, &
           elementIds, elementTypes, elementConn, &
           elementMask=elementMask, &
!#           elementArea, &
!#           elementCoords, &
!#           elementDistgrid, &
           _RC)

      deallocate(elementConn, elementTypes, elementIds, elementMask)
      _RETURN(_SUCCESS)
   end function make_esmf_mesh

    function make_esmf_mesh2(this, rc) result(msh)
      type(ESMF_Mesh), target :: msh
      class(Mesh), intent(in) :: this
      integer, optional, intent(out):: rc

      integer :: status
      real(kind=REAL64), allocatable :: elementCornerCoords(:,:)
      integer :: i, j, k, n, np
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      real(kind=REAL64) :: lon, lat
      integer :: stype

      integer, parameter :: SURF_TYPES(*) = [0, 1, 190000000, 200000000]

      integer :: counts_by_type(size(SURF_TYPES))
      real(kind=REAL64) :: centroid(2)

      counts_by_type = 0
      do i = 1, this%elements%size()
         e => this%get_element(i)
         select case (e%pixels(1,1))
         case (SURF_TYPES(1)) ! ocean
            stype = 1
         case (SURF_TYPES(2)) ! land
            stype = 2
         case (SURF_TYPES(3)) ! land
            stype = 3
         case (SURF_TYPES(4)) ! land
            stype = 4
         end select
         counts_by_type(stype) = counts_by_type(stype) + 3*this%element_degree(e)
      end do

      allocate(elementCornerCoords(2,3*sum(counts_by_type)))

      n = 0
      do j = 1, 4
         do i = 1, this%elements%size()
            e => this%get_element(i)
            if (e%pixels(1,1) /= SURF_TYPES(j)) cycle


            p = this%get_perimeter(e)
            np = p%size()
            centroid = this%get_centroid(e)

            do k = 1, p%size()
               v => this%get_vertex(p%of(k))
               elementCornerCoords(:,n+1) = centroid

               v => this%get_vertex(p%of(1+mod(k-1,np)))
               lon = this%to_lon(v%loc)
               lat = this%to_lat(v%loc)
               elementCornerCoords(:,n+2) = [lon, lat]

               v => this%get_vertex(p%of(1+mod(k-1+1,np)))
               lon = this%to_lon(v%loc)
               lat = this%to_lat(v%loc)
               elementCornerCoords(:,n+3) = [lon, lat]

               n = n + 3
            end do

         end do
      end do


      _HERE, counts_by_type
      msh = ESMF_MeshCreate(parametricDim=2, coordSys=ESMF_COORDSYS_SPH_DEG, &
           elementTypes=counts_by_type, &
           elementCornerCoords=elementCornerCoords, _RC)



      _RETURN(_SUCCESS)
   end function make_esmf_mesh2


   function make_esmf_mesh3(this, rc) result(msh)
      type(ESMF_Mesh), target :: msh
      class(Mesh), intent(in) :: this
      integer, optional, intent(out):: rc

      integer :: status
      integer :: n_nodes
      integer, allocatable :: nodeIds(:)
      real(kind=REAL64), allocatable :: nodeCoords(:)
      integer :: i, j, k, kk, kk_last
      integer(kind=INT64) :: k64
      integer :: np, n_elements, n_vertices, n_esmf_elements
      integer(kind=INT64) :: n_conn
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)
      integer, parameter :: SURF_TYPES(*) = [0, 1, 190000000, 200000000]

      integer :: counts_by_type(size(SURF_TYPES))
      real(kind=REAL64) :: centroid(2)

      msh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, coordSys=ESMF_COORDSYS_SPH_DEG, _RC)

      ! As a workaround to ESMF issue, we create an additional node at the
      ! center of each element and perform our own triangulation.
      n_vertices = this%vertices%size()
      n_elements = this%elements%size()
      n_nodes = n_vertices + n_elements

      allocate(nodeIds(n_nodes))
      allocate(nodeCoords(2*n_nodes))

      do k = 1, n_vertices
         k64 = k
         v => this%get_vertex(k64)
         nodeIds(k) = k
         nodeCoords(1 + (k-1)*2) = this%to_lon(v%loc)
         nodeCoords(2 + (k-1)*2) = this%to_lat(v%loc)
      end do

      kk = n_vertices
      do k = 1, n_elements
         kk = kk + 1
         e => this%get_element(k)
         nodeIds(kk) = kk
         nodeCoords(1 + (kk-1)*2: 2 + (kk-1)*2) = this%get_centroid(e)
      end do

      call ESMF_MeshAddNodes(msh, nodeIds, nodeCoords, _RC)
      deallocate(nodeCoords, nodeIds)

      n_conn = 0
      n_esmf_elements = 0
      do k = 1, n_elements
         e => this%get_element(k)
         p = this%get_perimeter(e)
         np = p%size()
         ! triangulate
         n_conn = n_conn + (3+1)*np  ! allow for poly break
      end do
      _HERE,'total connection :', n_conn

!#      allocate(elementIds(n_esmf_elements))
!#      allocate(elementTypes(n_esmf_elements))
!#      allocate(elementConn(n_conn))
!#      elementTypes = ESMF_MESHELEMTYPE_TRI

      allocate(elementIds(4))
      allocate(elementTypes(4))
      allocate(elementConn(n_conn))
!#      elementTypes = ESMF_MESHELEMTYPE_TRI

      elementIds = [1,2,3,4]

      kk = 0
      do j = 1, 4 ! esmf elements
         kk_last = kk
         do k = 1, n_elements
            e => this%get_element(k)
            if (e%get_type() /= j) cycle
            p = this%get_perimeter(e)
            np = p%size()
            do i = 1, np
               elementConn(kk+1) = n_vertices + k
               elementConn(kk+2) = p%of(1+mod(i-1,np))
               elementConn(kk+3) = p%of(1 + mod(i-1+1,np))
               elementConn(kk+4) = ESMF_MESH_POLYBREAK
!#            elementArea(k) = ...
               kk = kk + 4
            end do
         end do
         kk = kk - 1 ! remove last polybreak
         elementTypes(j) = kk - kk_last 
      end do

      call ESMF_MeshAddElements(msh, &
           elementIds, elementTypes, elementConn(:n_conn-4), & ! ignore final POLYBREAK
!#           elementMask, &
!#           elementArea, &
!#           elementCoords, &
!#           elementDistgrid, &
           _RC)

      deallocate(elementConn, elementTypes, elementIds)
      _RETURN(_SUCCESS)
   end function make_esmf_mesh3

   function get_centroid(this, e) result(centroid)
      real(kind=REAL64) :: centroid(2)
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e

      real(kind=REAL64) :: xyz(3), lon, lat
      integer :: i
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p

      p = this%get_perimeter(e)

      xyz = 0
      do i = 1, p%size()
         v => this%get_vertex(p%of(i))
         lon = this%to_lon(v%loc) * MAPL_DEGREES_TO_RADIANS_R8
         lat = this%to_lat(v%loc) * MAPL_DEGREES_TO_RADIANS_R8

         xyz = xyz + to_cartesian(lon, lat)
      end do

      ! normalize
      xyz = xyz / sqrt(sum(xyz**2))
      centroid = to_lon_lat(xyz)

   end function get_centroid

   function to_cartesian(lon, lat) result(xyz)
      real(kind=REAL64) :: xyz(3)
      real(kind=REAL64), intent(in) :: lon, lat

      xyz(1) = cos(lon) * cos(lat)
      xyz(2) = sin(lon) * cos(lat)
      xyz(3) = sin(lat)

   end function to_cartesian

   function to_lon_lat(xyz) result(lon_lat)
      real(kind=REAL64), intent(in) :: xyz(3)
      real(kind=REAL64)  :: lon_lat(2)

      lon_lat(1) = atan2(xyz(2), xyz(1)) ! lon
      lon_lat(2) = asin(xyz(3))          ! lat

      lon_lat = lon_lat * MAPL_RADIANS_TO_DEGREES

   end function to_lon_lat

   function to_lat(this, loc) result(lat)
      real(kind=REAL64) :: lat
      class(Mesh), target, intent(in) :: this
      integer, intent(in) :: loc(2)

      integer :: nj

      nj = size(this%pixels,2)
      associate (lat_sp => this%latitude_range(1), lat_np => this%latitude_range(2))
        lat = lat_sp + loc(2) * (lat_np - lat_sp) / (nj+1)
      end associate

   end function to_lat

   function to_lon(this, loc) result(lon)
      real(kind=REAL64) :: lon
      class(Mesh), target, intent(in) :: this
      integer, intent(in) :: loc(2)

      integer :: ni

      ni = size(this%pixels,1)
      associate (lon_0 => this%longitude_range(1), lon_1 => this%longitude_range(2))
        lon = lon_0 + (loc(1)-1) * (lon_1 - lon_0) / ni
      end associate

   end function to_lon

   function aspect_ratio(this, e)
      real(kind=REAL64) :: aspect_ratio
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e

      integer :: loc_sw(2), loc_ne(2)
      integer(kind=INT64) :: iv_0
      type(Vertex), pointer :: v

      real(kind=REAL64) :: lon_sw, lat_sw
      real(kind=REAL64) :: lon_ne, lat_ne
      real(kind=REAL64) :: lat_mid, dx, dy, d_lon
      integer :: ni, nj

      ni = size(e%pixels, 1)
      nj = size(e%pixels, 2)

      iv_0 = e%iv_0
      v => this%get_vertex(iv_0)

      loc_sw = v%loc
      loc_ne = add_loc(loc_sw, [ni,nj], size(this%pixels,1))

      lon_sw = this%to_lon(loc_sw)
      lon_ne = this%to_lon(loc_ne)
      lat_sw = this%to_lat(loc_sw)
      lat_ne = this%to_lat(loc_ne)

      lat_mid = asin((sin(lat_sw) + sin(lat_ne))/2)
      d_lon = abs(lon_ne - lon_sw)
      if (d_lon > 180) d_lon = 360 - d_lon
      dx = d_lon*cos(lat_mid)
      dy = (lat_ne - lat_sw)
      aspect_ratio = dx/dy

   end function aspect_ratio

!#   function area(this, e)
!#      real(kind=REAL64) :: area
!#      class(Mesh), target, intent(in) :: this
!#      type(Element), intent(in) :: e
!#      
!#      real(kind=REAL64) :: lon_sw, lat_sw
!#      real(kind=REAL64) :: lon_ne, lat_ne
!#      real(kind=REAL64) :: lat_mid, dx, dy
!#
!#      lon_sw = this%to_lon(loc_sw)
!#      lon_ne = this%to_lon(loc_ne)
!#      lat_sw = this%to_lat(loc_sw)
!#      lat_ne = this%to_lat(loc_ne)
!#
!#      lat_mid = asin((sin(lat_sw) + sin(lat_ne))/2)
!#
!#      dx = (lon_ne - lon_sw)*cos(lat_mid)
!#      dy = (lat_ne - lat_sw)
!#      aspect_ratio = dx/dy
!#
!#   end function area

   function resolution(this, e)
      real(kind=REAL64) :: resolution
      class(Mesh), target, intent(in) :: this
      type(Element), intent(in) :: e

      integer :: loc_sw(2), loc_ne(2)
      integer(kind=INT64) :: iv_0
      type(Vertex), pointer :: v

      real(kind=REAL64) :: lon_sw, lat_sw
      real(kind=REAL64) :: lon_ne, lat_ne
      real(kind=REAL64) :: lat_mid, dx, dy, d_lon
      integer :: ni, nj

      ni = size(e%pixels, 1)
      nj = size(e%pixels, 2)

      iv_0 = e%iv_0
      v => this%get_vertex(iv_0)

      loc_sw = v%loc
      loc_ne = add_loc(loc_sw, [ni,nj], size(this%pixels,1))

      lon_sw = this%to_lon(loc_sw)
      lon_ne = this%to_lon(loc_ne)
      lat_sw = this%to_lat(loc_sw)
      lat_ne = this%to_lat(loc_ne)

      lat_mid = asin((sin(lat_sw) + sin(lat_ne))/2)
      d_lon = abs(modulo(lon_ne - lon_sw, 360.d0))
!#      d_lon = abs(lon_ne - lon_sw)
      if (d_lon > 180) d_lon = 360 - d_lon

!#      d_lon = abs(asin(sin(lon_ne-lon_sw)))

      dx = d_lon*cos(lat_mid)
      dy = (lat_ne - lat_sw)

      resolution = max(dx, dy)

   end function resolution

   subroutine write_to_file(m, filename, rc)
      use pfio
      type(ESMF_Mesh), intent(in) :: m
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status
      type(NetCDF4_FileFormatter) :: formatter
      type(FileMetadata), target :: filemd
      integer :: nodeCount, elementCount
      integer :: maxNodePElement
      real(Kind=ESMF_KIND_R8), allocatable :: nodeCoords(:)
      integer, allocatable :: elementConn(:)
      integer :: elementConnCount
      integer, allocatable :: numElementConn(:)
      integer, allocatable :: elementMask(:)
      type(Variable) :: var

      call formatter%create(file=filename, _RC)

      call ESMF_MeshGet(m, nodeCount=nodeCount, elementCount=ElementCount, _RC)
      maxNodePElement = 3

      filemd = FileMetadata()
      call filemd%add_dimension('nodeCount', nodeCount, _RC)
      call filemd%add_dimension('elementCount', elementCount, _RC)
      call filemd%add_dimension('maxNodePElement', maxNodePElement, _RC)
      call filemd%add_dimension('coordDim', 2, _RC)

      !
      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', _RC)
      call var%add_attribute('units', Attribute('degrees'), _RC)
      call filemd%add_variable('nodeCoords', var, _RC)

      var = Variable(type=pFIO_INT32, dimensions='maxNodePElement,elementCount', _RC)
      call var%add_attribute('long_name', Attribute("Node Indices that define the element connectivity"), _RC)
      call var%add_attribute('_FillValue', Attribute(-1), _RC)
      call var%add_attribute('start_index', Attribute(1), _RC)
      call filemd%add_variable('elementConn', var, _RC)

      var = Variable(type=pFIO_INT32, dimensions='elementCount', _RC)
      call var%add_attribute('long_name', Attribute("Number of nodes per elemennt"))
      call filemd%add_variable('numElementConn', var, _RC)

     var = Variable(type=pFIO_INT32, dimensions='elementCount', _RC)
      call var%add_attribute('long_name', Attribute("mask for surface types: {1: Ocean, 2: Land, 3: Lake, 4: Landice}"))
      call filemd%add_variable('elementMask', var, _RC)

!#     var = Variable(type=???, dimensions='coordDim,elementCount', _RC)
!#     call var%add_attribute('units', Attribute('degrees'), _RC)
!#     call filemd%add_variable('centerCoords' var, _RC)

!#     var = Variable(type=???, dimensions='elementCount')
!#     call var%add_attribute('units', Attribute("radians^2"), _RC)
!#     call var%add_attribute('long_name', Attribute("area weights"), _RC)
!#     call filemd%add_variable('elementArea', var, _RC)

!#     var = Variable('elementCount')
!#     call var%add_attribute('_FillValue', Attribute(-9999), _RC)
!#


      call filemd%add_attribute('gridType', Attribute('unstructured'), _RC)
      call filemd%add_attribute('version', Attribute('0.9'), _RC)

      call formatter%write(filemd, _RC)

      allocate(nodeCoords(2*nodeCount))
      call ESMF_MeshGet(m, nodeCoords=nodeCoords, _RC)
      call formatter%put_var('nodeCoords', reshape(nodeCoords, [2, nodeCount]), _RC)
      deallocate(nodeCoords)

      call ESMF_MeshGet(m, elementConnCount=elementConnCount, _RC)
      allocate(elementConn(elementConnCount))
      call ESMF_MeshGet(m, elementConn=elementConn, _RC)
      call formatter%put_var('elementConn', reshape(elementConn, [3, elementCount]), _RC)
      deallocate(elementConn)

      allocate(elementMask(elementCount))
      call ESMF_MeshGet(m, elementMask=elementMask, _RC)
      call formatter%put_var('elementMask', elementMask, _RC)
      deallocate(elementMask)

      allocate(numElementConn(elementCount))
      numElementConn = 3
      call formatter%put_var('numElementConn', numElementConn, _RC)
      deallocate(numElementConn)

      call formatter%close(_RC)

      block
        type(ESMF_Mesh) :: m2

        m2 = ESMF_MeshCreate(filename, fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
      end block

      _RETURN(_SUCCESS)
   end subroutine write_to_file


   subroutine to_netcdf_0(this, filename, rc)
      use pfio
      class(Mesh), target, intent(in) :: this
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: n_nodes
      integer, allocatable :: nodeIds(:)
      real(kind=REAL64), allocatable :: nodeCoords(:,:)
      integer :: i, j, k, kk, kk_last
      integer(kind=INT64) :: k64
      integer :: np, n_elements, n_vertices, n_esmf_elements
      integer :: nodeCount
      integer(kind=INT64) :: n_conn
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)
      integer, allocatable :: numElementConn(:)

      integer, parameter :: N_SURF_TYPES = 4
      type(FileMetadata), target :: filemd
      type(Variable) :: var
      type(NetCDF4_FileFormatter) :: formatter

      integer :: counts_by_type(N_SURF_TYPES)
      integer(kind=INT64) :: c0, c1, crate

      ! As a workaround to ESMF issue, we create an additional node at the
      ! center of each element and perform our own triangulation.
      n_vertices = this%vertices%size()
      n_elements = this%elements%size()
      n_nodes = n_vertices
      _HERE,'n_nodes: ', n_nodes

      filemd = FileMetadata()
      call filemd%add_dimension('nodeCount', n_Nodes, _RC)
      call filemd%add_dimension('elementCount', n_elements, _RC)
      call filemd%add_dimension('coordDim', 2, _RC)

      n_conn = 0
      n_esmf_elements = 0
      do k = 1, n_elements
         e => this%get_element(k)
         p = this%get_perimeter(e)
         np = p%size()
         ! triangulate
         n_conn = n_conn + np
      end do

      _HERE,'total connection :', n_conn

      call filemd%add_dimension('connectionCount', int(n_conn), _RC)

      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', _RC)
      call var%add_attribute('units', Attribute('degrees'), _RC)
      call filemd%add_variable('nodeCoords', var, _RC)

      var = Variable(type=pFIO_INT32, dimensions='connectionCount', _RC)
      call var%add_attribute('long_name', Attribute("Node Indices that define the element connectivity"), _RC)
      call var%add_attribute('_FillValue', Attribute(-1), _RC)
      call var%add_attribute('polygon_break_value', Attribute(ESMF_MESH_POLYBREAK), _RC)
      call var%add_attribute('start_index', Attribute(1), _RC)
      call filemd%add_variable('elementConn', var, _RC)

      var = Variable(type=pFIO_INT32, dimensions='elementCount', _RC)
      call var%add_attribute('long_name', Attribute("Number of nodes per elemennt"))
      call filemd%add_variable('numElementConn', var, _RC)

      call filemd%add_attribute('gridType', Attribute('unstructured'), _RC)
      call filemd%add_attribute('version', Attribute('0.9'), _RC)

      _HERE, filename
      call formatter%create(file=filename, _RC)
      call formatter%write(filemd, _RC)

      call system_clock(c0, crate)

      allocate(nodeIds(n_nodes))
      allocate(nodeCoords(2,n_nodes))

      allocate(elementIds(n_elements))
      allocate(elementTypes(n_elements))
      allocate(elementConn(n_conn))
      allocate(numElementConn(n_elements))

      do k = 1, n_vertices
         k64 = k
         v => this%get_vertex(k64)
         nodeIds(k) = k
         nodeCoords(:,k) = [this%to_lon(v%loc), this%to_lat(v%loc)]
      end do

      call formatter%put_var('nodeCoords', nodeCoords, _RC)
      deallocate(nodeCoords, nodeIds)

      call system_clock(c1)
      _HERE, real(c1-c0)/crate

      call system_clock(c0)

      elementIds = [1,2,3,4]

      kk = 0
      do k = 1, n_elements
         e => this%get_element(k)
         p = this%get_perimeter(e)
         np = p%size()
         numElementConn(k) = np
         do i = 1, np
            elementConn(kk+i) = p%of(i)
         end do
         kk = kk + np
      end do

      _HERE, n_conn, minval(elementConn), maxval(elementConn)
      call formatter%put_var('elementConn', elementConn(:n_conn), _RC)
      call formatter%put_var('numElementConn', numElementConn, _RC)
      call system_clock(c1)
      _HERE, real(c1-c0)/crate
      call system_clock(c0)
      call formatter%close(_RC)
      call system_clock(c1)
      _HERE, real(c1-c0)/crate

      deallocate(elementConn, elementTypes, elementIds)

      block
        type(ESMF_Mesh) :: m2

        _HERE, 'Creating a mesh?'
        m2 = ESMF_MeshCreate(filename, fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
        _HERE, 'done!'
      end block


      _RETURN(_SUCCESS)
   end subroutine to_netcdf_0


   subroutine to_netcdf(this, filename, rc)
      use pfio
      class(Mesh), target, intent(in) :: this
      character(*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: n_nodes
      integer, allocatable :: nodeIds(:)
      real(kind=REAL64), allocatable :: nodeCoords(:,:)
      integer :: i, j, k, kk, kk_last
      integer(kind=INT64) :: k64
      integer :: np, n_elements, n_vertices, n_esmf_elements
      integer :: nodeCount
      integer(kind=INT64) :: n_conn
      type(Element), pointer :: e
      type(Vertex), pointer :: v
      type(Integer64Vector), target :: p
      integer, allocatable :: elementConn(:)
      integer, allocatable :: elementIds(:)
      integer, allocatable :: elementTypes(:)

      integer, parameter :: N_SURF_TYPES = 4
      type(FileMetadata), target :: filemd
      type(Variable) :: var
      type(NetCDF4_FileFormatter) :: formatter

      integer :: counts_by_type(N_SURF_TYPES)
      integer(kind=INT64) :: c0, c1, crate

      ! As a workaround to ESMF issue, we create an additional node at the
      ! center of each element and perform our own triangulation.
      n_vertices = this%vertices%size()
      n_elements = this%elements%size()
      n_nodes = n_vertices + n_elements
      _HERE,'n_nodes: ', n_nodes

      filemd = FileMetadata()
      call filemd%add_dimension('nodeCount', n_Nodes, _RC)
!#      call filemd%add_dimension('elementCount', n_elements, _RC)
      call filemd%add_dimension('elementCount', 4, _RC)
      call filemd%add_dimension('coordDim', 2, _RC)

      n_conn = 0
      n_esmf_elements = 0
      do k = 1, n_elements
         if (mod(k, 10000) == 0) then
            _HERE, k
         end if
         e => this%get_element(k)
         p = this%get_perimeter(e)
         np = p%size()
         ! triangulate
         n_conn = n_conn + (3+1)*np  ! allow for poly break
      end do
      n_conn = n_conn - N_SURF_TYPES ! last poly of each type has an extra vertex
      _HERE,'total connection :', n_conn

      call filemd%add_dimension('connectionCount', int(n_conn), _RC)

      var = Variable(type=pFIO_REAL64, dimensions='coordDim,nodeCount', _RC)
      call var%add_attribute('units', Attribute('degrees'), _RC)
      call filemd%add_variable('nodeCoords', var, _RC)

      var = Variable(type=pFIO_INT32, dimensions='connectionCount', _RC)
      call var%add_attribute('long_name', Attribute("Node Indices that define the element connectivity"), _RC)
      call var%add_attribute('_FillValue', Attribute(-1), _RC)
      call var%add_attribute('polygon_break_value', Attribute(ESMF_MESH_POLYBREAK), _RC)
      call var%add_attribute('start_index', Attribute(1), _RC)
      call filemd%add_variable('elementConn', var, _RC)

      var = Variable(type=pFIO_INT32, dimensions='elementCount', _RC)
      call var%add_attribute('long_name', Attribute("Number of nodes per elemennt"))
      call filemd%add_variable('numElementConn', var, _RC)

!#     var = Variable(type=???, dimensions='coordDim,elementCount', _RC)
!#     call var%add_attribute('units', Attribute('degrees'), _RC)
!#     call filemd%add_variable('centerCoords' var, _RC)

!#     var = Variable(type=???, dimensions='elementCount')
!#     call var%add_attribute('units', Attribute("radians^2"), _RC)
!#     call var%add_attribute('long_name', Attribute("area weights"), _RC)
!#     call filemd%add_variable('elementArea', var, _RC)

!#     var = Variable('elementCount')
!#     call var%add_attribute('_FillValue', Attribute(-9999), _RC)
!#
      call filemd%add_attribute('gridType', Attribute('unstructured'), _RC)
      call filemd%add_attribute('version', Attribute('0.9'), _RC)

      call formatter%create(file=filename, _RC)
      call formatter%write(filemd, _RC)

      call system_clock(c0, crate)

      allocate(nodeIds(n_nodes))
      allocate(nodeCoords(2,n_nodes))

      allocate(elementIds(N_SURF_TYPES))
      allocate(elementTypes(N_SURF_TYPES))
      allocate(elementConn(n_conn+N_SURF_TYPES))

      do k = 1, n_vertices
         k64 = k
         v => this%get_vertex(k64)
         nodeIds(k) = k
         nodeCoords(:,k) = [this%to_lon(v%loc), this%to_lat(v%loc)]
      end do

      kk = n_vertices
      do k = 1, n_elements
         kk = kk + 1
         e => this%get_element(k)
         nodeIds(kk) = kk
         nodeCoords(:,kk) = this%get_centroid(e)
      end do

      _HERE, nodeCoords(:, [1,5,7])
      call formatter%put_var('nodeCoords', nodeCoords, _RC)
      deallocate(nodeCoords, nodeIds)

      call system_clock(c1)
      _HERE, real(c1-c0)/crate

      call system_clock(c0)

      elementIds = [1,2,3,4]

      kk = 0
      do j = 1, 4 ! esmf elements
         kk_last = kk
         do k = 1, n_elements
            e => this%get_element(k)
            if (e%get_type() /= j) cycle
            p = this%get_perimeter(e)
            np = p%size()
            do i = 1, np
               elementConn(kk+1) = n_vertices + k
               elementConn(kk+2) = p%of(1+mod(i-1,np))
               elementConn(kk+3) = p%of(1 + mod(i-1+1,np))
               elementConn(kk+4) = ESMF_MESH_POLYBREAK
!#            elementArea(k) = ...
               kk = kk + 4
            end do
         end do
         kk = kk - 1 ! remove last polybreak
         elementTypes(j) = kk - kk_last 
      end do

      call formatter%put_var('elementConn', elementConn(:n_conn), _RC)
      call formatter%put_var('numElementConn', elementTypes, _RC)
      call system_clock(c1)
      _HERE, real(c1-c0)/crate
      call system_clock(c0)
      call formatter%close(_RC)
      call system_clock(c1)
      _HERE, real(c1-c0)/crate

      deallocate(elementConn, elementTypes, elementIds)

      block
        type(ESMF_Mesh) :: m2

        m2 = ESMF_MeshCreate(filename, fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
      end block


      _RETURN(_SUCCESS)
   end subroutine to_netcdf


end module sf_Mesh


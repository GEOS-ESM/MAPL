#include "MAPL_ErrLog.h"

module mapl3g_VectorBasis
   use esmf
   use mapl_FieldBLAS
   use mapl_FieldPointerUtilities
   use mapl_ErrorHandlingMod
   use mapl_base, only: MAPL_GridGetCorners

   implicit none
   private

   public :: VectorBasis
   ! Factory functions
   public :: NS_VectorBasis
   public :: GridVectorBasis

   integer, parameter :: NI = 3 ! num dims cartesian
   integer, parameter :: NJ = 2 ! num dims tangent (u,v)

   type :: VectorBasis
      type(ESMF_Field)  :: elements(NI,NJ)
   contains
      final :: destroy_fields
   end type VectorBasis

   interface NS_VectorBasis
      module procedure new_NS_Basis
   end interface NS_VectorBasis

   interface GridVectorBasis
      module procedure new_GridVectorBasis
   end interface GridVectorBasis

   type :: Ptr_1d
      real(kind=ESMF_KIND_R8), pointer :: ptr(:)
   end type Ptr_1d

   type :: Ptr_2d
      real(kind=ESMF_KIND_R8), pointer :: ptr(:,:)
   end type Ptr_2d

   interface GridGetCoords
      module procedure grid_get_coords_1d
      module procedure grid_get_coords_2d
      module procedure grid_get_centers
   end interface GridGetCoords

  interface GridGetCorners
      module procedure grid_get_corners
   end interface GridGetCorners

contains

   function new_NS_Basis(geom, rc) result(basis)
      type(VectorBasis) :: basis
      type(ESMF_Geom), intent(inout) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:)

      call create_fields(basis%elements, geom, _RC)
      call MAPL_GeomGetCoords(geom, longitudes, latitudes, _RC)
      call fill_fields(basis, longitudes, latitudes, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine fill_fields(basis, longitudes, latitudes, rc)
         type(VectorBasis), intent(inout) :: basis
         real(kind=ESMF_KIND_R8), intent(in) :: longitudes(:)
         real(kind=ESMF_KIND_R8), intent(in) :: latitudes(:)
         integer, optional, intent(out) :: rc

         integer :: status
         type(Ptr_1d) :: x(NI,NJ)
         integer :: i, j, n
         real(kind=ESMF_KIND_R8) :: local_basis(NI,NJ)

         do j = 1, NJ
            do i = 1, NI
               call assign_fptr(basis%elements(i,j), x(i,j)%ptr, _RC)
            end do
         end do

         do n = 1, size(x(1,1)%ptr)
            local_basis = fill_element(longitudes(i), latitudes(i))

            do j = 1, NJ
               do i = 1, NI
                  x(i,j)%ptr(n) = local_basis(i,j)
               end do
            end do

         end do

         _RETURN(ESMF_SUCCESS)
      end subroutine fill_fields

      pure function fill_element(longitude, latitude) result(x)
         real(kind=ESMF_KIND_R8) :: x(NI,NJ)
         real(kind=ESMF_KIND_R8), intent(in) :: longitude
         real(kind=ESMF_KIND_R8), intent(in) :: latitude

         x(:,1) = [ -sin(longitude), cos(longitude), 0._ESMF_KIND_R8 ]
         x(:,2) = [ -sin(latitude)*cos(longitude), -sin(latitude)*sin(longitude), cos(latitude) ]

      end function fill_element

   end function new_NS_Basis

   ! Valid only for grids.
   function new_GridVectorBasis(geom, inverse, rc) result(basis)
      type(VectorBasis) :: basis
      type(ESMF_Geom), intent(inout) :: geom
      logical, optional, intent(in) :: inverse
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      type(ESMF_GeomType_Flag) :: geomtype
      logical :: inverse_
      integer :: i, j
      real(kind=ESMF_KIND_R8), allocatable :: centers(:,:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corners(:,:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lats(:,:), corner_lons(:,:)

      inverse_ = .false.
      if (present(inverse)) inverse_ = inverse

      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
      _ASSERT(geomtype == ESMF_GEOMTYPE_GRID, 'GridVectorBasis is only valid for ESMF_Grid geoms.')
      call ESMF_GeomGet(geom, grid=grid, _RC)

      call create_fields(basis%elements, geom, _RC)

      call GridGetCoords(grid, centers, _RC)
      call GridGetCorners(grid, corners, _RC)

      call fill_fields(basis, centers, corners, inverse_, _RC)

      _RETURN(ESMF_SUCCESS)
   contains

      subroutine fill_fields(basis, centers, corners, inverse, rc)
         type(VectorBasis), intent(inout) :: basis
         real(kind=ESMF_KIND_R8), intent(in) :: centers(:,:,:)
         real(kind=ESMF_KIND_R8), intent(in) :: corners(:,:,:)
         logical, intent(in) :: inverse
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: k1, k2
         integer :: im, jm
         type(Ptr_2d) :: x(NI,NJ)

         im = size(centers,1)
         jm = size(centers,2)

         do k2 = 1, NJ
            do k1 = 1, NI
               call assign_fptr(basis%elements(k1,k2), int([im,jm],kind=ESMF_KIND_I8), x(k1,k2)%ptr, _RC)
            end do
         end do

         do concurrent (i=1:im, j=1:jm)
            associate (local_basis => fill_element(centers(i,j,:), corners(i:i+1,j+j+1,:), inverse) )
              
              do k2 = 1, NJ
                 do k1 = 1, NI
                    x(k1,k2)%ptr(i,j) = local_basis(k1,k2)
                 end do
              end do
            end associate
         end do
       
         _RETURN(ESMF_SUCCESS)
      end subroutine fill_fields
      !--------------------------------------
      !
      !   ^ lat
      !   !
      !   !         x c    p4     x  d
      !   !
      !   !
      !   !         p1     C       p3
      !   !
      !   !
      !   !         x a    p2     x  b
      !   !
      !   !
      !   !------------------------------> lon
      !
      !--------------------------------------

      pure function fill_element(center, corners, inverse) result(basis)
         real(kind=ESMF_KIND_R8), intent(in) :: center(2)
         real(kind=ESMF_KIND_R8), intent(in) :: corners(2,2,2) ! last dim is lat/lon
         logical, intent(in) :: inverse
         real(kind=ESMF_KIND_R8) :: basis(NI,2)

         associate ( &
              p1 => mid_pt_sphere(corners(1,1,:),corners(1,2,:)), &
              p2 => mid_pt_sphere(corners(1,1,:),corners(2,1,:)), &
              p3 => mid_pt_sphere(corners(2,1,:),corners(2,2,:)), &
              p4 => mid_pt_sphere(corners(1,2,:),corners(2,2,:)) )

           associate ( &
                e1 => get_unit_vector(p3, center, p1), &
                e2 => get_unit_vector(p4, center, p2) )

             if (.not. inverse) then
                basis(:,1) = e1
                basis(:,2) = e2
                return
             end if

             associate (dot => dot_product(e1, e2))
               basis(:,1) = (e1 - dot*e2) / (1-dot**2)
               basis(:,2) = (e2 - dot*e1) / (1-dot**2)
             end associate

           end associate
         end associate

      end function fill_element

   end function new_GridVectorBasis

   ! Utility functions
   !------------------
   pure function get_unit_vector( p1, p2, p3 ) result(uvect)
      real(kind=ESMF_KIND_R8), intent(in):: p1(2), p2(2), p3(2) 
      real(kind=ESMF_KIND_R8) :: uvect(3) 
      real(kind=ESMF_KIND_R8) :: xyz1(3), xyz2(3), xyz3(3)
      real(kind=ESMF_KIND_R8) :: ap 

      xyz1 = latlon2xyz(p1,right_hand=.true.)
      xyz2 = latlon2xyz(p2,right_hand=.true.)
      xyz3 = latlon2xyz(p3,right_hand=.true.)
      uvect = xyz3-xyz1

      ap = dot_product(uvect,xyz2) 
      uvect = uvect - ap*xyz2
      ap = dot_product(uvect,uvect)
      uvect=uvect/sqrt(ap)

   end function get_unit_vector


   subroutine create_fields(elements, geom, rc)
      type(ESMF_Field), intent(inout) :: elements(NI,NJ)
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, j
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_LocStream) :: locstream
      type(ESMF_Mesh) :: mesh

      
      
      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)

      if (geomtype == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         do j = 1, nj
            do i = 1, ni
               elements(i,j) = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                    staggerloc=ESMF_STAGGERLOC_CENTER, _RC)
            end do
         end do
      elseif (geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom, locstream=locstream, _RC)
         do j = 1, nj
            do i = 1, ni
               elements(i,j) = ESMF_FieldCreate(locstream, typekind=ESMF_TYPEKIND_R8, _RC)
            end do
         end do
      elseif (geomtype == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomGet(geom, mesh=mesh, _RC)
         do j = 1, nj
            do i = 1, ni
               elements(i,j) = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, _RC)
            end do
         end do
      elseif (geomtype == ESMF_GEOMTYPE_XGRID) then
         _FAIL('Unsupported geomtype XGRID')
      else
         _FAIL('Unknown geomtype.')
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine create_fields



   ! Geometry utilities

   pure function mid_pt_sphere(p1, p2) result(pm)
      real(kind=ESMF_KIND_R8) , intent(in)  :: p1(2), p2(2)
      real(kind=ESMF_KIND_R8) :: pm(2)
      real(kind=ESMF_KIND_R8) :: e1(3), e2(3), e3(3),dd

      e1 = latlon2xyz(p1)
      e2 = latlon2xyz(p2)
      e3 = e1 + e2
      dd = sqrt(dot_product(e3,e3))
      e3 = e3 / dd
      pm = xyz2latlon(e3)

   end function mid_pt_sphere

   pure function latlon2xyz(sph_coord,right_hand) result(xyz_coord)
      real(kind=ESMF_KIND_R8), intent(in), dimension(2) :: sph_coord
      logical, intent(in), optional :: right_hand
      real(kind=ESMF_KIND_R8), dimension(3) :: xyz_coord

      logical :: rh_
      if (present(right_hand)) then
         rh_=right_hand
      else
         rh_=.true.
      end if
      xyz_coord(1) = cos(sph_coord(2)) * cos(sph_coord(1))
      xyz_coord(2) = cos(sph_coord(2)) * sin(sph_coord(1))
      if (rh_) then
         xyz_coord(3) = sin(sph_coord(2))
      else
         xyz_coord(3) = -sin(sph_coord(2))
      end if

   end function latlon2xyz

   pure function xyz2latlon(xyz_coord) result(sph_coord)
      use MAPL_Constants, only: PI => MAPL_PI_R8
      real(kind=ESMF_KIND_R8), intent(in):: xyz_coord(3)
      real(kind=ESMF_KIND_R8) :: sph_coord(2)
      real(kind=ESMF_KIND_R8), parameter:: esl=1.e-10
      real(kind=ESMF_KIND_R8):: p(3)
      real(kind=ESMF_KIND_R8):: dist, lat, lon
      integer k

      p = xyz_coord
      dist =sqrt( dot_product(p,p))
      do k=1,3
         p(k) = p(k) / dist
      enddo

      if ( (abs(p(1))+abs(p(2)))  < esl ) then
         lon = 0.
      else
         lon = atan2( p(2), p(1) )   ! range [-pi,pi]
      endif

      if ( lon < 0.) lon = 2.*pi + lon
      lat = asin(p(3))

      sph_coord(1) = lon
      sph_coord(2) = lat

   end function xyz2latlon

   subroutine destroy_fields(this)
      type(VectorBasis), intent(inout) :: this

      integer :: i, j

      do j = 1, size(this%elements,2)
         do i =  1, size(this%elements,1)
            call ESMF_FieldDestroy(this%elements(i,j))
         end do
      end do

   end subroutine destroy_fields


   subroutine MAPL_GeomGetCoords(geom, longitudes, latitudes, rc)
      type(ESMF_Geom), intent(in) :: geom
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
      integer, optional, intent(out) :: rc

      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_LocStream) :: locstream
      integer :: status

      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
      if (geomtype == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         call GridGetCoords(grid, longitudes, latitudes, _RC)
      else if (geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom, locstream=locstream, _RC)
         call get_locstream_coords(locstream, longitudes, latitudes, _RC)
      else if (any([geomtype==ESMF_GEOMTYPE_MESH, geomtype==ESMF_GEOMTYPE_XGRID])) then
         _FAIL("Unsupported geom type.")
      else
         _FAIL("Illeggal geom type.")
      end if
      _RETURN(ESMF_SUCCESS)

   contains

      subroutine get_locstream_coords(locstream, longitudes, latitudes, rc)
         type(ESMF_LocStream), intent(in) :: locstream
         real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
         real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
         integer, optional, intent(out) :: rc

         integer :: status

         call ESMF_LocStreamGetKey(locstream, keyName='ESMF:Lon', farray=longitudes, _RC)
         call ESMF_LocStreamGetKey(locstream, keyName='ESMF:Lat', farray=latitudes, _RC)

         _RETURN(ESMF_SUCCESS)
      end subroutine get_locstream_coords

   end subroutine MAPL_GeomGetCoords

   ! GridGetCoords - specific procedures
   subroutine grid_get_coords_1d(grid, longitudes, latitudes, rc)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_loc
      type(ESMF_Grid), intent(in) :: grid
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), dimension(:,:), pointer :: lons_2d, lats_2d
      type(c_ptr) :: loc

      call GridGetCoords(grid, lons_2d, lats_2d, _RC)

      associate (n => product(shape(lons_2d)))
        loc = c_loc(lons_2d)
        call c_f_pointer(loc, longitudes, [n])

        loc = c_loc(lats_2d)
        call c_f_pointer(loc, latitudes, [n])
      end associate

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_coords_1d

   subroutine grid_get_coords_2d(grid, longitudes, latitudes, rc)
      type(ESMF_Grid), intent(in) :: grid
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, farrayPtr=longitudes, &
           staggerloc=ESMF_STAGGERLOC_CENTER, _RC)
      call ESMF_GridGetCoord(grid, localDE=1, coordDim=2, farrayPtr=latitudes, &
           staggerloc=ESMF_STAGGERLOC_CENTER, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_coords_2d

   subroutine grid_get_centers(grid, centers, rc)
      type(ESMF_Grid), intent(in) :: grid
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: centers(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)

      call GridGetCoords(grid, longitudes, latitudes, _RC)

      allocate(centers(size(longitudes,1),size(longitudes,2),2))
      centers(:,:,1) = longitudes
      centers(:,:,2) = latitudes

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_centers

   subroutine grid_get_corners(grid, corners, rc)
      type(ESMF_Grid), intent(inout) :: grid
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: corners(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im, jm
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lons(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lats(:,:)

      call GridGetCoords(grid, longitudes, latitudes, _RC)
      im = size(longitudes,1)
      jm = size(longitudes,2)

      allocate(corner_lons(im+1,jm+1))
      allocate(corner_lats(im+1,jm+1))

      call MAPL_GridGetCorners(grid, corner_lons, corner_lats, _RC)

      allocate(corners(size(longitudes,1),size(longitudes,2),2))
      corners(:,:,1) = corner_lons
      corners(:,:,2) = corner_lats

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_corners

end module mapl3g_VectorBasis



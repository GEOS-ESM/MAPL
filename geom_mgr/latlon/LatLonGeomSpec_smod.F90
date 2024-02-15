#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonGeomSpec) LatLonGeomSpec_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
!   use hconfig3g
   use esmf
   implicit none
   
contains


   ! Basic constructor for LatLonGeomSpec
   module function new_LatLonGeomSpec(lon_axis, lat_axis, decomposition) result(spec)
      type(LatLonGeomSpec) :: spec
      type(LonAxis), intent(in) :: lon_axis
      type(LatAxis), intent(in) :: lat_axis
      type(LatLonDecomposition), intent(in) :: decomposition
      
      spec%lon_axis = lon_axis
      spec%lat_axis = lat_axis
      spec%decomposition = decomposition
      
   end function new_LatLonGeomSpec


   pure logical module function equal_to(a, b)
      class(LatLonGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      select type (b)
      type is (LatLonGeomSpec)
         equal_to = (a%lon_axis == b%lon_axis) .and. (a%lat_axis == b%lat_axis)
         if (.not. equal_to) return
         equal_to = (a%decomposition == b%decomposition)
      class default
         equal_to = .false.
      end select

   end function equal_to


   ! HConfig section
   module function make_LatLonGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      logical :: is_regional
      integer :: status

      spec%lon_axis = make_LonAxis(hconfig, _RC)
      spec%lat_axis = make_LatAxis(hconfig, _RC)
      associate (im => spec%lon_axis%get_extent(), jm => spec%lat_axis%get_extent())
        spec%decomposition = make_Decomposition(hconfig, dims=[im,jm], _RC)
      end associate

      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_hconfig

   function make_decomposition(hconfig, dims, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc
      integer, allocatable :: ims(:), jms(:)
      integer :: nx, ny

      integer :: status
      logical :: has_ims, has_jms, has_nx, has_ny

      has_ims = ESMF_HConfigIsDefined(hconfig, keystring='ims', _RC)
      has_jms = ESMF_HConfigIsDefined(hconfig, keystring='jms', _RC)
!      call MAPL_HConfigGet(hconfig, 'ims', ims, found=has_ims, _RC)
!      call MAPL_HConfigGet(hconfig, 'jms', jms, found=has_jms, _RC)
      _ASSERT(has_ims .eqv. has_jms, 'ims and jms must be both defined or both undefined')

      if (has_ims) then
         ims = ESMF_HConfigAsI4Seq(hconfig, keystring='ims', _RC) 
         jms = ESMF_HConfigAsI4Seq(hconfig, keystring='jms', _RC)
         decomp = LatLonDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      has_nx = ESMF_HConfigIsDefined(hconfig, keystring = 'nx', _RC)
      has_ny = ESMF_HConfigIsDefined(hconfig, keystring = 'ny', _RC)
!      call MAPL_HConfigGet(hconfig, 'nx', nx, found=has_nx, _RC)
!      call MAPL_HConfigGet(hconfig, 'ny', ny, found=has_ny, _RC)
      _ASSERT(has_nx .eqv. has_ny, 'nx and ny must be both defined or both undefined')

      if (has_nx) then
         nx = ESMF_HConfigAsI4(hconfig, keystring= 'nx', _RC)
         ny = ESMF_HConfigAsI4(hconfig, keystring= 'ny', _RC)
         decomp = LatLonDecomposition(dims, topology=[nx, ny])
         _RETURN(_SUCCESS)
      end if

      ! Invent a decomposition
      decomp = make_LatLonDecomposition(dims, _RC)
      
      _RETURN(_SUCCESS)
   end function make_decomposition

!#   module function get_distribution(hconfig, m_world, key_npes, key_distribution, rc) result(distribution)
!#      integer, allocatable :: distribution(:)
!#      type(ESMF_HConfig), intent(in) :: hconfig
!#      integer, intent(in) :: m_world
!#      character(len=*), intent(in) :: key_npes
!#      character(len=*), intent(in) :: key_distribution
!#      integer, optional, intent(out) :: rc
!#
!#      integer :: status
!#      integer :: nx
!#      integer, allocatable :: ims(:)
!#      logical :: has_distribution
!#
!#      call MAPL_GetResource(nx, hconfig, key_npes, _RC)
!#      _ASSERT(nx > 0, key_npes // ' must be greater than 0.')
!#
!#      has_distribution = ESMF_HConfigIsDefined(hconfig, keystring=key_distribution, _RC)
!#      if (has_distribution) then
!#         call MAPL_GetResource(ims, hconfig, key_distribution, _RC)
!#         _ASSERT(size(ims) == nx, 'inconsistent processor distribution')
!#         _ASSERT(sum(ims) == m_world, 'Requested pe distribution inconsistent with grid resolution.')
!#      else
!#         allocate(ims(nx))
!#         call MAPL_DecomposeDim(m_world, ims, nx, min_DE_extent=2)
!#      end if
!#
!#      distribution = ims
!#      
!#      _RETURN(_SUCCESS)
!#   end function get_distribution
!#
  
   ! File metadata section

   ! Unfortunately, we cannot quite compute each axis (lat - lon) independently,
   ! as the optimal decomposition depends on the ratio of the extens along each
   ! dimension.
   module function make_LatLonGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LatLonDecomposition) :: decomposition

      lon_axis = make_LonAxis(file_metadata, _RC)
      lat_axis = make_LatAxis(file_metadata, _RC)

      associate (im_world => lon_axis%get_extent(), jm_world => lat_axis%get_extent())
        decomposition = make_LatLonDecomposition([im_world, jm_world], _RC)
      end associate
      spec = LatLonGeomSpec(lon_axis, lat_axis, decomposition)
      
      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_metadata

   module function make_distribution(im, nx) result(distribution)
      integer, allocatable :: distribution(:)
      integer, intent(in) :: im, nx

      allocate(distribution(nx))
      call MAPL_DecomposeDim(im, distribution, nx, min_DE_extent=2)

   end function make_distribution



   ! Accessors
   pure module function get_lon_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LonAxis) :: axis
      axis = spec%lon_axis
   end function get_lon_axis

   pure module function get_lat_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LatAxis) :: axis
      axis = spec%lat_axis
   end function get_lat_axis


   pure module function get_decomposition(spec) result(decomposition)
      type(LatLonDecomposition) :: decomposition
      class(LatLonGeomSpec), intent(in) :: spec

      decomposition = spec%decomposition
   end function get_decomposition

   logical module function supports_hconfig(this, hconfig, rc) result(supports)
      class(LatLonGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      character(:), allocatable :: geom_schema
      logical :: has_schema

      ! Mandatory entry: "class: latlon"
      has_schema = ESMF_HConfigIsDefined(hconfig, keystring = 'schema', _RC)
      _ASSERT(has_schema, 'Keystring "schema" not found.')
!      call MAPL_HConfigGet(hconfig, 'schema', geom_schema, found=supports, _RC)
      geom_schema = ESMF_HConfigAsString(hconfig, keystring= 'schema', _RC)
      _RETURN_UNLESS(supports)

      supports = (geom_schema == 'latlon')
      _RETURN_UNLESS(supports)
      
      supports = lon_axis%supports(hconfig, _RC)
      _RETURN_UNLESS(supports)

      supports = lat_axis%supports(hconfig, _RC)
      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
   end function supports_hconfig

   logical module function supports_metadata(this, file_metadata, rc) result(supports)
      class(LatLonGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis

      supports = .false.

      supports = lon_axis%supports(file_metadata, _RC)
      _RETURN_UNLESS(supports)

      supports = lat_axis%supports(file_metadata, _RC)
      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
   end function supports_metadata

end submodule LatLonGeomSpec_smod

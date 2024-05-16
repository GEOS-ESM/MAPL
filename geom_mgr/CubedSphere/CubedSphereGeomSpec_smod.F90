#include "MAPL_ErrLog.h"

submodule (mapl3g_CubedSphereGeomSpec) CubedSphereGeomSpec_smod
   use mapl3g_CoordinateAxis
   use mapl3g_GeomSpec
   use pfio
   use MAPL_RangeMod
   use MAPLBase_Mod
   use mapl_ErrorHandling
   use mapl_Constants
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none
   real(ESMF_Kind_R8) :: undef_schmidt = 1d15
   
contains


   ! Basic constructor for CubedSphereGeomSpec
   module function new_CubedSphereGeomSpec(im_world, schmidt_parameters, decomposition) result(spec)
      type(CubedSphereGeomSpec) :: spec
      integer, intent(in) :: im_world
      type(ESMF_CubedSphereTransform_Args), intent(in) :: schmidt_parameters
      type(CubedSphereDecomposition), intent(in) :: decomposition
      
      spec%im_world = im_world
      spec%schmidt_parameters = schmidt_parameters
      spec%decomposition = decomposition
      
   end function new_CubedSphereGeomSpec


   pure logical module function equal_to(a, b)
      class(CubedSphereGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      select type (b)
      type is (CubedSphereGeomSpec)
         equal_to = (a%im_world== b%im_world)
         if (.not. equal_to) return
         equal_to = (a%decomposition == b%decomposition)
         if (.not. equal_to) return
         equal_to = equal_schmidt(a%schmidt_parameters,b%schmidt_parameters)
      class default
         equal_to = .false.
      end select

      contains
      pure logical function equal_schmidt(a,b)
         type(ESMF_CubedSphereTransform_Args), intent(in) :: a
         type(ESMF_CubedSphereTransform_Args), intent(in) :: b

         equal_schmidt = (a%target_lat /= b%target_lat) .and. &
                        (a%target_lon /= b%target_lon) .and. &
                        (a%stretch_factor /= b%stretch_factor)
      end function equal_schmidt

   end function equal_to


   ! HConfig section
   module function make_CubedSphereGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(CubedSphereGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: found

      spec%im_world = ESMF_HConfigAsI4(hconfig, keyString='im_world', asOkay=found, _RC)
      _ASSERT(found, '"im_world" not found.') 
      spec%decomposition = make_Decomposition(hconfig, cube_size=spec%im_world, _RC)
      spec%schmidt_parameters = make_SchmidtParameters_from_hconfig(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_CubedSphereGeomSpec_from_hconfig

   function make_SchmidtParameters_from_hconfig(hconfig, rc) result(schmidt_parameters)
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(out), optional :: rc

      integer :: status, ifound
      logical :: has_tlon, has_tlat, has_sfac, consistent
  
      schmidt_parameters%stretch_factor = undef_schmidt 
      schmidt_parameters%target_lon= undef_schmidt 
      schmidt_parameters%target_lat= undef_schmidt 
      ifound = 0 
      has_sfac = ESMF_HConfigIsDefined(hconfig, keystring='stretch_factor', _RC)
      if (has_sfac) then
         schmidt_parameters%stretch_factor = ESMF_HConfigAsR8(hconfig, keystring='stretch_factor' ,_RC)
         ifound = ifound + 1
      end if
      has_tlon = ESMF_HConfigIsDefined(hconfig, keystring='target_lon', _RC)
      if (has_tlon) then
         schmidt_parameters%target_lon = ESMF_HConfigAsR8(hconfig, keystring='target_lon' ,_RC)
         schmidt_parameters%target_lon = schmidt_parameters%target_lon * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if
      has_tlat = ESMF_HConfigIsDefined(hconfig, keystring='target_lat', _RC)
      if (has_tlat) then
         schmidt_parameters%target_lat = ESMF_HConfigAsR8(hconfig, keystring='target_lat' ,_RC)
         schmidt_parameters%target_lat = schmidt_parameters%target_lat * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if
      consistent = (ifound .eq. 3) .or. (ifound .eq. 0)
      _ASSERT(consistent, "specfied partial stretch parameters")
      _RETURN(_SUCCESS)

   end function make_SchmidtParameters_from_hconfig

   function make_decomposition(hconfig, cube_size, rc) result(decomp)
      type(CubedSphereDecomposition) :: decomp
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, intent(in) :: cube_size
      integer, optional, intent(out) :: rc
      integer, allocatable :: ims(:), jms(:)
      integer :: nx, ny

      integer :: status
      logical :: has_ims, has_jms, has_nx, has_ny

      has_ims = ESMF_HConfigIsDefined(hconfig, keystring='ims', _RC)
      has_jms = ESMF_HConfigIsDefined(hconfig, keystring='jms', _RC)
      _ASSERT(has_ims .eqv. has_jms, 'ims and jms must be both defined or both undefined')

      if (has_ims) then
         ims = ESMF_HConfigAsI4Seq(hconfig, keyString='ims', _RC)
         jms = ESMF_HConfigAsI4Seq(hconfig, keyString='jms', _RC)
         decomp = CubedSphereDecomposition(ims, jms)
         _RETURN(_SUCCESS)
      end if

      has_nx = ESMF_HConfigIsDefined(hconfig, keystring='nx', _RC)
      has_ny = ESMF_HConfigIsDefined(hconfig, keystring='ny', _RC)
      _ASSERT(has_nx .eqv. has_ny, 'nx and ny must be both defined or both undefined')

      if (has_nx) then
         nx = ESMF_HConfigAsI4(hconfig, keyString='nx', _RC)
         ny = ESMF_HConfigAsI4(hconfig, keyString='ny', _RC)
         decomp = CubedSphereDecomposition([cube_size,cube_size], topology=[nx, ny])
         _RETURN(_SUCCESS)
      end if

      ! Invent a decomposition
      decomp = make_CubedSphereDecomposition([cube_size,cube_size], _RC)
      
      _RETURN(_SUCCESS)
   end function make_decomposition

   module function make_CubedSphereGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(CubedSphereGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status, im_world
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(CubedSphereDecomposition) :: decomposition 

      im_world = file_metadata%get_dimension("Xdim", _RC)
      decomposition = make_CubedSphereDecomposition([im_world,im_world], _RC)
      schmidt_parameters = make_SchmidtParameters_from_metadata(file_metadata, _RC)
      spec = CubedSphereGeomSpec(im_world, schmidt_parameters, decomposition)
      
      _RETURN(_SUCCESS)
   end function make_CubedSphereGeomSpec_from_metadata

   function make_SchmidtParameters_from_metadata(file_metadata, rc) result(schmidt_parameters)
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(FileMetadata), intent(in) :: file_metadata
      integer, intent(out), optional :: rc

      integer :: status, ifound
      logical :: has_tlon, has_tlat, has_sfac, consistent
  
      schmidt_parameters%stretch_factor = undef_schmidt 
      schmidt_parameters%target_lon= undef_schmidt 
      schmidt_parameters%target_lat= undef_schmidt 
      ifound = 0 
      has_sfac = file_metadata%has_attribute('stretch_factor')
      if (has_sfac) then
         schmidt_parameters%stretch_factor = return_r8(file_metadata, 'stretch_factor', _RC)
         ifound = ifound + 1
      end if
      has_tlon = file_metadata%has_attribute('target_lon')
      if (has_tlon) then
         schmidt_parameters%target_lon = return_r8(file_metadata, 'target_lon', _RC)
         schmidt_parameters%target_lon = schmidt_parameters%target_lon * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if
      has_tlat = file_metadata%has_attribute('target_lat')
      if (has_tlat) then
         schmidt_parameters%target_lat = return_r8(file_metadata, 'target_lat', _RC)
         schmidt_parameters%target_lat = schmidt_parameters%target_lat * MAPL_DEGREES_TO_RADIANS_R8
         ifound = ifound + 1
      end if

      consistent = (ifound .eq. 3) .or. (ifound .eq. 0)
      _ASSERT(consistent, "specfied partial stretch parameters")
      _RETURN(_SUCCESS)

   end function make_SchmidtParameters_from_metadata

   function return_r8(file_metadata, attr_name, rc) result(param)
      real(kind=ESMF_KIND_R8) :: param
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), intent(in) :: attr_name
      integer, optional, intent(out) :: rc
 
      integer :: status
      class(*), pointer :: attr_val(:)
      type(Attribute), pointer :: attr

      attr => file_metadata%get_attribute(attr_name)
      attr_val => attr%get_values()
      select type(q=>attr_val)
      type is (real(kind=REAL32))
         param = q(1)
      type is (real(kind=REAL64))
         param = q(1)
      class default
         _FAIL('unsupported subclass for stretch parameters')
      end select
      _RETURN(_SUCCESS)
   end function return_r8
      

   ! Accessors
   pure module function get_decomposition(spec) result(decomposition)
      type(CubedSphereDecomposition) :: decomposition
      class(CubedSphereGeomSpec), intent(in) :: spec

      decomposition = spec%decomposition
   end function get_decomposition

   pure module function get_im_world(spec) result(im_world)
      integer :: im_world
      class(CubedSphereGeomSpec), intent(in) :: spec

      im_world = spec%im_world
   end function get_im_world

   pure module function get_schmidt_parameters(spec) result(schmidt_parameters)
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      class(CubedSphereGeomSpec), intent(in) :: spec

      schmidt_parameters = spec%schmidt_parameters
   end function get_schmidt_parameters

   logical module function supports_hconfig_(this, hconfig, rc) result(supports)
      class(CubedSphereGeomSpec), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: geom_class

      ! Mandatory entry: "class: CubedSphere"
      supports = ESMF_HConfigIsDefined(hconfig, keystring='class', _RC)
      _RETURN_UNLESS(supports)

      geom_class = ESMF_HConfigAsString(hconfig, keyString='class', _RC)
      supports = (geom_class == 'CubedSphere')
      _RETURN_UNLESS(supports)
      
      _RETURN(_SUCCESS)
   end function supports_hconfig_

   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      class(CubedSphereGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      supports = file_metadata%has_dimension("nf", _RC)

      _RETURN_UNLESS(supports)

      _RETURN(_SUCCESS)
   end function supports_metadata_

end submodule CubedSphereGeomSpec_smod

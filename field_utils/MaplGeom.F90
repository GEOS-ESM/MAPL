module mapl_MaplGeom
   implicit none
   private

   public :: MaplGeom

   ! MaplGeom encapsulates an ESMF Geom object along with various related
   ! data associated with that object that are not easily stored in ESMF
   ! info.

   type, abstract :: MaplGeom
      private
   contains
      procedure, deferred :: get_esmf_geom
      procedure, deferred :: 

      ! Geom independent logic
      procedure :: spherical_to_cartesian
      procedure :: cartesian_to_spherical
   end type MaplGeom

contains


   subroutine spherical_to_cartesian(this, uv, xyz, unusable, rc)
      type(ESMF_Field), intent(in) :: uv
      type(ESMF_Field), intent(out) :: xyz


      do i = 1, npts
        xyz = fmatmul(basis, uv)
      end do

   end subroutine spherical_to_cartesian
end module mapl_MaplGeom

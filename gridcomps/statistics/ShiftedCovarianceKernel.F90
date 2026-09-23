#include "MAPL.h"

module mapl_ShiftedCovarianceKernel_mod

   use mapl_AbstractCovarianceKernel_mod
   use MAPL
   use ESMF

   implicit none(type, external)
   private

   public :: ShiftedCovarianceKernel

   type, extends(AbstractCovarianceKernel) :: ShiftedCovarianceKernel
   contains
      procedure :: advertise
      procedure :: get_internal_field_prefixes
      procedure :: initialize
      procedure :: destroy
      procedure :: reset
      procedure :: update_r4
      procedure :: update_r8
      procedure :: compute_r4
      procedure :: compute_r8
   end type ShiftedCovarianceKernel

   ! Internal field name prefixes:
   !   kx_<name>, ky_<name>   : shift constants
   !   ex_<name>, ey_<name>   : sum of (x - kx), (y - ky)
   !   exy_<name>             : sum of (x - kx)*(y - ky)
   !
   ! Fixed positional convention (see get_internal_field_prefixes):
   !   internal_fields(1) = kx_, (2) = ky_, (3) = ex_, (4) = ey_, (5) = exy_
   !
   ! For variance, f_x = f_y so kx=ky, ex=ey, exy=ex2.
   ! Cov(X,Y) = (exy - ex*ey/n) / (n - offset)

contains

   function get_internal_field_prefixes(this) result(prefixes)
      class(ShiftedCovarianceKernel), intent(in) :: this
      character(len=16), allocatable :: prefixes(:)

      prefixes = [character(len=16) :: 'kx_', 'ky_', 'ex_', 'ey_', 'exy_']
      _UNUSED_DUMMY(this)
   end function get_internal_field_prefixes

   subroutine advertise(this, gridcomp, name, item_type, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      type(ESMF_StateItem_Flag), intent(in) :: item_type
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'kx_'//name,  fill_value=0.0, itemtype=item_type, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'ky_'//name,  fill_value=0.0, itemtype=item_type, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'ex_'//name,  fill_value=0.0, itemtype=item_type, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'ey_'//name,  fill_value=0.0, itemtype=item_type, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'exy_'//name, fill_value=0.0, itemtype=item_type, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine advertise

   subroutine initialize(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_Geom), allocatable :: geom
      type(MAPL_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units
      type(esmf_TypeKind_Flag) :: typekind
      type(MAPL_VerticalStaggerLoc) :: vstagger
      class(mapl_VerticalGrid), pointer :: vertical_grid

      call mapl_FieldGet(f_x, &
           geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)

      call mapl_FieldSet(internal_fields(1), geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)
      call mapl_FieldSet(internal_fields(2), geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)
      call mapl_FieldSet(internal_fields(3), geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)
      call mapl_FieldSet(internal_fields(4), geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)
      call mapl_FieldSet(internal_fields(5), geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(f_y)
      _UNUSED_DUMMY(counts_f)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine destroy(this, gridcomp, internal_fields, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(internal_fields)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine destroy

   subroutine reset(this, gridcomp, internal_fields, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldFill(internal_fields(1), dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(internal_fields(2), dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(internal_fields(3), dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(internal_fields(4), dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(internal_fields(5), dataFillScheme='const', const1=0.d0, _RC)

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine reset

   subroutine update_r4(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: xp(:), yp(:), kxp(:), kyp(:), exp(:), eyp(:), exyp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(internal_fields(1), kxp,  _RC)
      call MAPL_AssignFptr(internal_fields(2), kyp,  _RC)
      call MAPL_AssignFptr(internal_fields(3), exp,  _RC)
      call MAPL_AssignFptr(internal_fields(4), eyp,  _RC)
      call MAPL_AssignFptr(internal_fields(5), exyp, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      where ((xp /= MAPL_UNDEF) .and. (yp /= MAPL_UNDEF) .and. (counts == 0))
         kxp = xp
         kyp = yp
      end where

      where ((xp /= MAPL_UNDEF) .and. (yp /= MAPL_UNDEF))
         counts = counts + 1
         exp    = exp  + (xp - kxp)
         eyp    = eyp  + (yp - kyp)
         exyp   = exyp + (xp - kxp) * (yp - kyp)
      end where

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r4

   subroutine update_r8(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: xp(:), yp(:), kxp(:), kyp(:), exp(:), eyp(:), exyp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(internal_fields(1), kxp,  _RC)
      call MAPL_AssignFptr(internal_fields(2), kyp,  _RC)
      call MAPL_AssignFptr(internal_fields(3), exp,  _RC)
      call MAPL_AssignFptr(internal_fields(4), eyp,  _RC)
      call MAPL_AssignFptr(internal_fields(5), exyp, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      where ((xp /= MAPL_UNDEF) .and. (yp /= MAPL_UNDEF) .and. (counts == 0))
         kxp = xp
         kyp = yp
      end where

      where ((xp /= MAPL_UNDEF) .and. (yp /= MAPL_UNDEF))
         counts = counts + 1
         exp    = exp  + (xp - kxp)
         eyp    = eyp  + (yp - kyp)
         exyp   = exyp + (xp - kxp) * (yp - kyp)
      end where

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r8

   subroutine compute_r4(this, gridcomp, f_x, f_y, counts_f, cov_f, internal_fields, biased, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: exp(:), eyp(:), exyp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_AssignFptr(internal_fields(3), exp,  _RC)
      call MAPL_AssignFptr(internal_fields(4), eyp,  _RC)
      call MAPL_AssignFptr(internal_fields(5), exyp, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = (exyp - exp * eyp / counts) / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(f_x)
      _UNUSED_DUMMY(f_y)
      _RETURN(_SUCCESS)

   end subroutine compute_r4

   subroutine compute_r8(this, gridcomp, f_x, f_y, counts_f, cov_f, internal_fields, biased, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: exp(:), eyp(:), exyp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_AssignFptr(internal_fields(3), exp,  _RC)
      call MAPL_AssignFptr(internal_fields(4), eyp,  _RC)
      call MAPL_AssignFptr(internal_fields(5), exyp, _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = (exyp - exp * eyp / counts) / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(f_x)
      _UNUSED_DUMMY(f_y)
      _RETURN(_SUCCESS)

   end subroutine compute_r8

end module mapl_ShiftedCovarianceKernel_mod

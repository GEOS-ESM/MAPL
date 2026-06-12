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
   ! For variance, f_x = f_y so kx=ky, ex=ey, exy=ex2.
   ! Cov(X,Y) = (exy - ex*ey/n) / (n - offset)

contains

   subroutine advertise(this, gridcomp, name, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'kx_'//name,  fill_value=0.0, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'ky_'//name,  fill_value=0.0, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'ex_'//name,  fill_value=0.0, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'ey_'//name,  fill_value=0.0, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'exy_'//name, fill_value=0.0, _RC)

      _RETURN(_SUCCESS)

   end subroutine advertise

   subroutine initialize(this, gridcomp, f_x, f_y, counts_f, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: kx_f, ky_f, ex_f, ey_f, exy_f
      type(esmf_Geom), allocatable :: geom
      type(UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      class(VerticalGrid), pointer :: vertical_grid
      type(VerticalStaggerLoc) :: vstagger

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call mapl_FieldGet(f_x, &
           geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)

      call esmf_StateGet(internal_state, 'kx_'//name,  field=kx_f,  _RC)
      call mapl_FieldSet(kx_f,  geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)
      call esmf_StateGet(internal_state, 'ky_'//name,  field=ky_f,  _RC)
      call mapl_FieldSet(ky_f,  geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)
      call esmf_StateGet(internal_state, 'ex_'//name,  field=ex_f,  _RC)
      call mapl_FieldSet(ex_f,  geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)
      call esmf_StateGet(internal_state, 'ey_'//name,  field=ey_f,  _RC)
      call mapl_FieldSet(ey_f,  geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)
      call esmf_StateGet(internal_state, 'exy_'//name, field=exy_f, _RC)
      call mapl_FieldSet(exy_f, geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(f_y)
      _UNUSED_DUMMY(counts_f)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine destroy(this, gridcomp, f_x, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(f_x)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine destroy

   subroutine reset(this, gridcomp, f_x, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: kx_f, ky_f, ex_f, ey_f, exy_f
      character(:), allocatable :: name

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'kx_'//name,  field=kx_f,  _RC)
      call esmf_StateGet(internal_state, 'ky_'//name,  field=ky_f,  _RC)
      call esmf_StateGet(internal_state, 'ex_'//name,  field=ex_f,  _RC)
      call esmf_StateGet(internal_state, 'ey_'//name,  field=ey_f,  _RC)
      call esmf_StateGet(internal_state, 'exy_'//name, field=exy_f, _RC)

      call esmf_FieldFill(kx_f,  dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(ky_f,  dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(ex_f,  dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(ey_f,  dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(exy_f, dataFillScheme='const', const1=0.d0, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine reset

   subroutine update_r4(this, gridcomp, f_x, f_y, counts_f, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: kx_f, ky_f, ex_f, ey_f, exy_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: xp(:), yp(:), kxp(:), kyp(:), exp(:), eyp(:), exyp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'kx_'//name,  field=kx_f,  _RC)
      call esmf_StateGet(internal_state, 'ky_'//name,  field=ky_f,  _RC)
      call esmf_StateGet(internal_state, 'ex_'//name,  field=ex_f,  _RC)
      call esmf_StateGet(internal_state, 'ey_'//name,  field=ey_f,  _RC)
      call esmf_StateGet(internal_state, 'exy_'//name, field=exy_f, _RC)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(kx_f,     kxp,    _RC)
      call MAPL_AssignFptr(ky_f,     kyp,    _RC)
      call MAPL_AssignFptr(ex_f,     exp,    _RC)
      call MAPL_AssignFptr(ey_f,     eyp,    _RC)
      call MAPL_AssignFptr(exy_f,    exyp,   _RC)
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

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r4

   subroutine update_r8(this, gridcomp, f_x, f_y, counts_f, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: kx_f, ky_f, ex_f, ey_f, exy_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: xp(:), yp(:), kxp(:), kyp(:), exp(:), eyp(:), exyp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'kx_'//name,  field=kx_f,  _RC)
      call esmf_StateGet(internal_state, 'ky_'//name,  field=ky_f,  _RC)
      call esmf_StateGet(internal_state, 'ex_'//name,  field=ex_f,  _RC)
      call esmf_StateGet(internal_state, 'ey_'//name,  field=ey_f,  _RC)
      call esmf_StateGet(internal_state, 'exy_'//name, field=exy_f, _RC)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(kx_f,     kxp,    _RC)
      call MAPL_AssignFptr(ky_f,     kyp,    _RC)
      call MAPL_AssignFptr(ex_f,     exp,    _RC)
      call MAPL_AssignFptr(ey_f,     eyp,    _RC)
      call MAPL_AssignFptr(exy_f,    exyp,   _RC)
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

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r8

   subroutine compute_r4(this, gridcomp, f_x, f_y, counts_f, cov_f, biased, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: ex_f, ey_f, exy_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: exp(:), eyp(:), exyp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'ex_'//name,  field=ex_f,  _RC)
      call esmf_StateGet(internal_state, 'ey_'//name,  field=ey_f,  _RC)
      call esmf_StateGet(internal_state, 'exy_'//name, field=exy_f, _RC)

      call MAPL_AssignFptr(ex_f,     exp,    _RC)
      call MAPL_AssignFptr(ey_f,     eyp,    _RC)
      call MAPL_AssignFptr(exy_f,    exyp,   _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = (exyp - exp * eyp / counts) / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(f_y)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine compute_r4

   subroutine compute_r8(this, gridcomp, f_x, f_y, counts_f, cov_f, biased, rc)
      class(ShiftedCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: ex_f, ey_f, exy_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: exp(:), eyp(:), exyp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'ex_'//name,  field=ex_f,  _RC)
      call esmf_StateGet(internal_state, 'ey_'//name,  field=ey_f,  _RC)
      call esmf_StateGet(internal_state, 'exy_'//name, field=exy_f, _RC)

      call MAPL_AssignFptr(ex_f,     exp,    _RC)
      call MAPL_AssignFptr(ey_f,     eyp,    _RC)
      call MAPL_AssignFptr(exy_f,    exyp,   _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = (exyp - exp * eyp / counts) / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(f_y)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine compute_r8

end module mapl_ShiftedCovarianceKernel_mod

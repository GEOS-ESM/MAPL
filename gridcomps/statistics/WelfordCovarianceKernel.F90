#include "MAPL.h"

module mapl_WelfordCovarianceKernel_mod

   use mapl_AbstractCovarianceKernel_mod
   use MAPL
   use ESMF

   implicit none(type, external)
   private

   public :: WelfordCovarianceKernel

   type, extends(AbstractCovarianceKernel) :: WelfordCovarianceKernel
   contains
      procedure :: advertise
      procedure :: initialize
      procedure :: destroy
      procedure :: reset
      procedure :: update_r4
      procedure :: update_r8
      procedure :: compute_r4
      procedure :: compute_r8
   end type WelfordCovarianceKernel

   ! Internal field name prefixes:
   !   mux_<name>, muy_<name>  : running means of x and y
   !   c_<name>                : running cross-moment sum
   !
   ! Recurrence (n = counts after increment):
   !   dx   = x - prev_mux
   !   mux += dx / n
   !   muy += (y - muy) / n
   !   C   += dx * (y - muy)          <- uses updated muy
   !
   ! Cov(X,Y) = C / (n - offset)
   ! When x = y: reduces to standard Welford variance, C = m2.

contains

   subroutine advertise(this, gridcomp, name, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'mux_'//name, fill_value=0.0, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'muy_'//name, fill_value=0.0, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'c_'//name,   fill_value=0.0, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine advertise

   subroutine initialize(this, gridcomp, f_x, f_y, counts_f, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: mux_f, muy_f, c_f
      type(esmf_Geom), allocatable :: geom
      type(MAPL_UngriddedDims) :: ungridded_dims
      character(:), allocatable :: units, name
      type(esmf_TypeKind_Flag) :: typekind
      type(MAPL_VerticalStaggerLoc) :: vstagger
      class(mapl_VerticalGrid), pointer :: vertical_grid

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call mapl_FieldGet(f_x, &
           geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, _RC)

      call esmf_StateGet(internal_state, 'mux_'//name, field=mux_f, _RC)
      call mapl_FieldSet(mux_f, geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)
      call esmf_StateGet(internal_state, 'muy_'//name, field=muy_f, _RC)
      call mapl_FieldSet(muy_f, geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)
      call esmf_StateGet(internal_state, 'c_'//name,   field=c_f,   _RC)
      call mapl_FieldSet(c_f,   geom=geom, ungridded_dims=ungridded_dims, units=units, &
           typekind=typekind, vgrid=vertical_grid, vert_staggerloc=vstagger, &
           has_deferred_aspects=.false., _RC)

      _UNUSED_DUMMY(f_y)
      _UNUSED_DUMMY(counts_f)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine destroy(this, gridcomp, f_x, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(f_x)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine destroy

   subroutine reset(this, gridcomp, f_x, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: mux_f, muy_f, c_f
      character(:), allocatable :: name

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'mux_'//name, field=mux_f, _RC)
      call esmf_StateGet(internal_state, 'muy_'//name, field=muy_f, _RC)
      call esmf_StateGet(internal_state, 'c_'//name,   field=c_f,   _RC)

      call esmf_FieldFill(mux_f, dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(muy_f, dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(c_f,   dataFillScheme='const', const1=0.d0, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine reset

   subroutine update_r4(this, gridcomp, f_x, f_y, counts_f, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: mux_f, muy_f, c_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: xp(:), yp(:), muxp(:), muyp(:), cp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      real(kind=ESMF_KIND_R4), allocatable :: dx(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'mux_'//name, field=mux_f, _RC)
      call esmf_StateGet(internal_state, 'muy_'//name, field=muy_f, _RC)
      call esmf_StateGet(internal_state, 'c_'//name,   field=c_f,   _RC)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(mux_f,    muxp,   _RC)
      call MAPL_AssignFptr(muy_f,    muyp,   _RC)
      call MAPL_AssignFptr(c_f,      cp,     _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      allocate(dx(size(xp)))
      dx = 0.0_ESMF_KIND_R4

      where ((xp /= MAPL_UNDEF) .and. (yp /= MAPL_UNDEF))
         counts = counts + 1
         dx     = xp - muxp
         muxp   = muxp + dx / counts
         muyp   = muyp + (yp - muyp) / counts
         cp     = cp + dx * (yp - muyp)
      end where

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r4

   subroutine update_r8(this, gridcomp, f_x, f_y, counts_f, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: mux_f, muy_f, c_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: xp(:), yp(:), muxp(:), muyp(:), cp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      real(kind=ESMF_KIND_R8), allocatable :: dx(:)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'mux_'//name, field=mux_f, _RC)
      call esmf_StateGet(internal_state, 'muy_'//name, field=muy_f, _RC)
      call esmf_StateGet(internal_state, 'c_'//name,   field=c_f,   _RC)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(mux_f,    muxp,   _RC)
      call MAPL_AssignFptr(muy_f,    muyp,   _RC)
      call MAPL_AssignFptr(c_f,      cp,     _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)

      allocate(dx(size(xp)))
      dx = 0.0_ESMF_KIND_R8

      where ((xp /= MAPL_UNDEF) .and. (yp /= MAPL_UNDEF))
         counts = counts + 1
         dx     = xp - muxp
         muxp   = muxp + dx / counts
         muyp   = muyp + (yp - muyp) / counts
         cp     = cp + dx * (yp - muyp)
      end where

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r8

   subroutine compute_r4(this, gridcomp, f_x, f_y, counts_f, cov_f, biased, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: c_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R4), pointer :: cp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'c_'//name, field=c_f, _RC)

      call MAPL_AssignFptr(c_f,      cp,     _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = cp / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(f_y)
      _RETURN(_SUCCESS)

   end subroutine compute_r4

   subroutine compute_r8(this, gridcomp, f_x, f_y, counts_f, cov_f, biased, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      type(esmf_State) :: internal_state
      type(esmf_Field) :: c_f
      character(:), allocatable :: name
      real(kind=ESMF_KIND_R8), pointer :: cp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call mapl_FieldGet(f_x, short_name=name, _RC)
      call esmf_StateGet(internal_state, 'c_'//name, field=c_f, _RC)

      call MAPL_AssignFptr(c_f,      cp,     _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = cp / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(f_y)
      _RETURN(_SUCCESS)

   end subroutine compute_r8

end module mapl_WelfordCovarianceKernel_mod

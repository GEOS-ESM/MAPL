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
      procedure :: get_internal_field_prefixes
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
   ! Fixed positional convention (see get_internal_field_prefixes):
   !   internal_fields(1) = mux_, internal_fields(2) = muy_, internal_fields(3) = c_
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

   function get_internal_field_prefixes(this) result(prefixes)
      class(WelfordCovarianceKernel), intent(in) :: this
      character(len=16), allocatable :: prefixes(:)

      prefixes = [character(len=16) :: 'mux_', 'muy_', 'c_']
      _UNUSED_DUMMY(this)
   end function get_internal_field_prefixes

   subroutine advertise(this, gridcomp, name, item_type, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      character(*), intent(in) :: name
      type(ESMF_StateItem_Flag), intent(in) :: item_type
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'mux_'//name, fill_value=0.0, itemtype=item_type, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'muy_'//name, fill_value=0.0, itemtype=item_type, _RC)
      call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, 'c_'//name,   fill_value=0.0, itemtype=item_type, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine advertise

   subroutine initialize(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
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

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(f_y)
      _UNUSED_DUMMY(counts_f)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine initialize

   subroutine destroy(this, gridcomp, internal_fields, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(internal_fields)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine destroy

   subroutine reset(this, gridcomp, internal_fields, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status

      call esmf_FieldFill(internal_fields(1), dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(internal_fields(2), dataFillScheme='const', const1=0.d0, _RC)
      call esmf_FieldFill(internal_fields(3), dataFillScheme='const', const1=0.d0, _RC)

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine reset

   subroutine update_r4(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: xp(:), yp(:), muxp(:), muyp(:), cp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      real(kind=ESMF_KIND_R4), allocatable :: dx(:)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(internal_fields(1), muxp,   _RC)
      call MAPL_AssignFptr(internal_fields(2), muyp,   _RC)
      call MAPL_AssignFptr(internal_fields(3), cp,     _RC)
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

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r4

   subroutine update_r8(this, gridcomp, f_x, f_y, counts_f, internal_fields, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: xp(:), yp(:), muxp(:), muyp(:), cp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      real(kind=ESMF_KIND_R8), allocatable :: dx(:)

      call MAPL_AssignFptr(f_x,      xp,     _RC)
      call MAPL_AssignFptr(f_y,      yp,     _RC)
      call MAPL_AssignFptr(internal_fields(1), muxp,   _RC)
      call MAPL_AssignFptr(internal_fields(2), muyp,   _RC)
      call MAPL_AssignFptr(internal_fields(3), cp,     _RC)
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

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)

   end subroutine update_r8

   subroutine compute_r4(this, gridcomp, f_x, f_y, counts_f, cov_f, internal_fields, biased, rc)
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: cp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_AssignFptr(internal_fields(3), cp,     _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = cp / (counts - counts_offset)
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
      class(WelfordCovarianceKernel), intent(inout) :: this
      type(esmf_GridComp), intent(inout) :: gridcomp
      type(esmf_Field), intent(inout) :: f_x
      type(esmf_Field), intent(inout) :: f_y
      type(esmf_Field), intent(inout) :: counts_f
      type(esmf_Field), intent(inout) :: cov_f
      type(esmf_Field), intent(inout) :: internal_fields(:)
      logical, intent(in) :: biased
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: cp(:), covp(:)
      integer(kind=ESMF_KIND_I4), pointer :: counts(:)
      integer :: counts_offset

      counts_offset = 1
      if (biased) counts_offset = 0

      call MAPL_AssignFptr(internal_fields(3), cp,     _RC)
      call MAPL_AssignFptr(counts_f, counts, _RC)
      call MAPL_AssignFptr(cov_f,    covp,   _RC)

      where (counts > counts_offset)
         covp = cp / (counts - counts_offset)
      elsewhere
         covp = MAPL_UNDEF
      end where

      _UNUSED_DUMMY(gridcomp)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(f_x)
      _UNUSED_DUMMY(f_y)
      _RETURN(_SUCCESS)

   end subroutine compute_r8

end module mapl_WelfordCovarianceKernel_mod

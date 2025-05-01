! This class is to support propagation of time-dependent Field
! attributes across couplers as well as to provide guidance to the
! containt Action objects on when to recompute internal items.

#include "MAPL_Exceptions.h"
module mapl3g_FieldDelta
   use mapl3g_FieldInfo
   use mapl3g_FieldGet
   use mapl3g_VerticalStaggerLoc
   use mapl3g_InfoUtilities
   use mapl_FieldPointerUtilities
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf
   implicit none(type,external)
   private

   public :: FieldDelta

   ! Allocatable components are used to indicate that the delta involves a
   ! change in the relevant quantity.   Unallocated means unchanged.
   type :: FieldDelta
      private
      ! intrinsic
      type(ESMF_Geom), allocatable :: geom
      type(ESMF_TypeKind_Flag), allocatable :: typekind
      ! info attributes
      integer, allocatable :: num_levels
      character(:), allocatable :: units

!#      logical :: geom_coords_changed = .false.
!#      logical :: vgrid_coords_changed = .false.
   contains
      procedure :: initialize_field_delta
      procedure :: initialize_field_delta_degenerate
      generic :: initialize => initialize_field_delta
      generic :: initialize => initialize_field_delta_degenerate
      procedure :: update_field
      procedure :: update_fields
      procedure :: reallocate_field
      procedure :: reallocate_fields
   end type FieldDelta


   interface FieldDelta
      procedure new_FieldDelta
   end interface FieldDelta


contains

   function new_FieldDelta(geom, typekind, num_levels, units) result(field_delta)
      type(FieldDelta) :: field_delta

      type(ESMF_Geom), intent(in), optional :: geom
      type(ESMF_TypeKind_Flag), intent(in), optional :: typekind
      integer, intent(in), optional :: num_levels
      character(*), intent(in), optional :: units

      if (present(geom)) then
         field_delta%geom = geom
      end if

      if (present(typekind)) then
         field_delta%typekind = typekind
      end if

      if (present(num_levels)) then
         field_delta%num_levels = num_levels
      end if

      if (present(units)) then
         field_delta%units = units
      end if

   end function new_FieldDelta


   ! delta = f_b - f_a
   subroutine initialize_field_delta(this, f_a, f_b, rc) 
      class(FieldDelta), intent(out) :: this
      type(ESMF_Field), intent(in) :: f_a
      type(ESMF_Field), intent(in) :: f_b
      integer, optional, intent(out) :: rc

      integer :: status

      call compute_geom_delta(this%geom, f_a, f_b, _RC)
      call compute_typekind_delta(this%typekind, f_a, f_b, _RC)
      call compute_num_levels_delta(this%num_levels, f_a, f_b, _RC)
      call compute_units_delta(this%units, f_a, f_b, _RC)

      _RETURN(_SUCCESS)


   contains

      subroutine compute_geom_delta(geom, f_a, f_b, rc)
         type(ESMF_Geom), allocatable, intent(out) :: geom
         type(ESMF_Field), intent(in) :: f_a
         type(ESMF_Field), intent(in) :: f_b
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Geom):: geom_a, geom_b

         call ESMF_FieldGet(f_a, geom=geom_a, _RC)
         call ESMF_FieldGet(f_b, geom=geom_b, _RC)

          if (geom_a /= geom_b) then
              geom = geom_b
          end if

         _RETURN(_SUCCESS)

      end subroutine compute_geom_delta

      subroutine compute_typekind_delta(typekind, f_a, f_b, rc)
         type(ESMF_TypeKind_Flag), allocatable, intent(out) :: typekind
         type(ESMF_Field), intent(in) :: f_a
         type(ESMF_Field), intent(in) :: f_b
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_TypeKind_Flag) :: typekind_a, typekind_b

         call ESMF_FieldGet(f_a, typekind=typekind_a, _RC)
         call ESMF_FieldGet(f_b, typekind=typekind_b, _RC)

         if (typekind_a /= typekind_b) then
            typekind = typekind_b
         end if

         _RETURN(_SUCCESS)

      end subroutine compute_typekind_delta

      subroutine compute_num_levels_delta(num_levels, f_a, f_b, rc)
         integer, allocatable, intent(out) :: num_levels
         type(ESMF_Field), intent(in) :: f_a
         type(ESMF_Field), intent(in) :: f_b
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: num_levels_a, num_levels_b

         call FieldGet(f_a, num_levels=num_levels_a, _RC)
         call FieldGet(f_b, num_levels=num_levels_b, _RC)

          if (num_levels_a /= num_levels_b) then
              num_levels = num_levels_b
          end if

         _RETURN(_SUCCESS)

      end subroutine compute_num_levels_delta

      subroutine compute_units_delta(units, f_a, f_b, rc)
         character(:), allocatable, intent(out) :: units
         type(ESMF_Field), intent(in) :: f_a
         type(ESMF_Field), intent(in) :: f_b
         integer, optional, intent(out) :: rc

         integer :: status
         character(len=:), allocatable :: units_a, units_b

         call FieldGet(f_a, units=units_a, _RC)
         call FieldGet(f_b, units=units_b, _RC)

         if (units_a /= units_b) then
            allocate(character(len_trim(units_b)) :: units)
            units = units_b
         end if

         _RETURN(_SUCCESS)

      end subroutine compute_units_delta

   end subroutine initialize_field_delta

   ! delta = f
   subroutine initialize_field_delta_degenerate(this, f, rc)
      class(FieldDelta), intent(out) :: this
      type(ESMF_Field), intent(in) :: f
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind

      allocate(this%geom)
      allocate(this%typekind)
      call ESMF_FieldGet(f, geom=this%geom, typekind=typekind, _RC)

      allocate(this%num_levels)
      call FieldGet(f, num_levels=this%num_levels, units=this%units, _RC)

      _RETURN(_SUCCESS)
   end subroutine initialize_field_delta_degenerate

   


   subroutine update_field(this, field, ignore, rc)
      class(FieldDelta), intent(in) :: this
      type(ESMF_Field), intent(inout) :: field
      character(*), intent(in), optional :: ignore
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: ignore_

      ignore_ = ''
      if (present(ignore)) ignore_ = ignore

      call this%reallocate_field(field, ignore=ignore_, _RC)

      call update_num_levels(this%num_levels, field, ignore=ignore_, _RC)
      call update_units(this%units, field, ignore=ignore_, _RC)

      _RETURN(_SUCCESS)
   contains

      subroutine update_num_levels(num_levels, field, ignore, rc)
         integer, optional, intent(in) :: num_levels
         type(ESMF_Field), intent(inout) :: field
         character(*), intent(in) :: ignore
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Info) :: info

         _RETURN_UNLESS(present(num_levels))
         _RETURN_IF(ignore == 'num_levels')

         call ESMF_InfoGetFromHost(field, info, _RC)
         call FieldInfoSetInternal(info, num_levels=num_levels, _RC)

         _RETURN(_SUCCESS)
      end subroutine update_num_levels

      subroutine update_units(units, field, ignore, rc)
         character(*), optional, intent(in) :: units
         type(ESMF_Field), intent(inout) :: field
         character(*), intent(in) :: ignore
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Info) :: info

         _RETURN_UNLESS(present(units))
         _RETURN_IF(ignore == 'units')

         call ESMF_InfoGetFromHost(field, info, _RC)
         call FieldInfoSetInternal(info, units=units, _RC)

         _RETURN(_SUCCESS)
      end subroutine update_units

   end subroutine update_field

   subroutine update_fields(this, fieldList, ignore, rc)
      class(FieldDelta), intent(in) :: this
      type(ESMF_Field), intent(inout) :: fieldList(:)
      character(*), intent(in) :: ignore
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      do i = 1, size(fieldList)
         call this%update_field(fieldList(i), ignore, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine update_fields

   subroutine reallocate_field(this, field, ignore, unusable, rc)
      class(FieldDelta), intent(in) :: this
      type(ESMF_Field), intent(inout) :: field
      character(*), optional, intent(in) :: ignore
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_Geom) :: current_geom, geom
      type(ESMF_TypeKind_Flag) :: current_typekind, typekind
      
      integer :: i, rank
      integer, allocatable :: ungriddedLBound(:), ungriddedUBound(:)
      integer, allocatable :: localElementCount(:), current_ungriddedUBound(:)
      character(:), allocatable :: ignore_
      logical :: new_array
      type(ESMF_FieldStatus_Flag) :: field_status

      new_array = .false.
      ignore_ = ''
      if (present(ignore)) ignore_ = ignore


      call ESMF_FieldGet(field, status=field_status, _RC)
      _ASSERT(field_status == ESMF_FIELDSTATUS_COMPLETE, 'field must at least have a geom.')
      call ESMF_FieldGet(field, geom=current_geom, _RC)

      call ESMF_FieldGet(field, typekind=current_typekind, _RC)
      localElementCount = FieldGetLocalElementCount(field, _RC)

      call select_geom(geom, current_geom, this%geom, ignore_, new_array)
      call select_typekind(typekind, current_typekind, this%typekind, ignore_, new_array)
      call select_ungriddedUbound(ungriddedUbound, field, this%num_levels, ignore_, new_array, _RC)
      ungriddedLBound = [(1, i=1, size(ungriddedUBound))]

      _RETURN_UNLESS(new_array)

      call ESMF_FieldEmptyReset(field, status=ESMF_FIELDSTATUS_EMPTY, _RC)
      call ESMF_FieldEmptySet(field, geom, _RC)

      call ESMF_FieldEmptyComplete(field, &
           typekind=typekind, &
           ungriddedLBound=ungriddedLBound, ungriddedUbound=ungriddedUBound, &
           _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine select_geom(geom, current_geom, new_geom, ignore, new_array)
         type(ESMF_Geom), intent(out) :: geom
         type(ESMF_Geom), intent(in) :: current_geom
         type(ESMF_Geom), optional, intent(in) :: new_geom
         character(*), intent(in) :: ignore
         logical, intent(inout) :: new_array
         
         geom = current_geom

         if (ignore == 'geom') return
         if (.not. present(new_geom)) return

         new_array = new_array .or. (new_geom /= current_geom)
         geom = new_geom

      end subroutine select_geom

      subroutine select_typekind(typekind, current_typekind, new_typekind, ignore, new_array)
         type(ESMF_TypeKind_Flag), intent(out) :: typekind
         type(ESMF_TypeKind_Flag), intent(in) :: current_typekind
         type(ESMF_TypeKind_Flag), optional, intent(in) :: new_typekind
         character(*), intent(in) :: ignore
         logical, intent(inout) :: new_array
         
         typekind = current_typekind

         if (ignore == 'typekind') return
         if (.not. present(new_typekind)) return

         new_array = new_array .or. (new_typekind /= current_typekind)
         typekind = new_typekind

      end subroutine select_typekind

      subroutine select_ungriddedUbound(ungriddedUbound, field, new_num_levels, ignore, new_array, rc)
         integer, allocatable, intent(out) :: ungriddedUbound(:)
         type(ESMF_Field), intent(inout) :: field
         integer, optional, intent(in) :: new_num_levels
         character(*), intent(in) :: ignore
         logical, intent(inout) :: new_array
         integer, optional, intent(inout) :: rc

         integer :: status
         integer :: ungriddedDimCount
         integer :: rank
         integer :: current_num_levels
         integer, allocatable :: localElementCount(:)
         integer, allocatable :: current_ungriddedUBound(:)
         type(VerticalStaggerLoc) :: vert_staggerloc

         call ESMF_FieldGet(field, &
              ungriddedDimCount=ungriddedDimCount, &
              rank=rank,  _RC)
         localElementCount = FieldGetLocalElementCount(field, _RC)
         current_ungriddedUBound = localElementCount(rank-ungriddedDimCount+1:)
         ungriddedUbound = current_ungriddedUBound

         if (ignore == 'num_levels') return
         if (.not. present(new_num_levels)) return

         call FieldGet(field, vert_staggerloc=vert_staggerloc, _RC)

         ! Surface fields are not impacted by change in vertical grid
         _RETURN_IF(vert_staggerloc == VERTICAL_STAGGER_NONE)


         call FieldGet(field, num_levels=current_num_levels, _RC)
         _ASSERT(count(vert_staggerloc == [VERTICAL_STAGGER_CENTER, VERTICAL_STAGGER_EDGE]) == 1, 'unsupported vertical stagger')
         ungriddedUBound(1) = this%num_levels

         new_array = new_array .or. (this%num_levels /= current_num_levels)

         _RETURN(_SUCCESS)
      end subroutine select_ungriddedUbound
      
   end subroutine reallocate_field


   subroutine reallocate_fields(this, fieldList, ignore, rc)
      class(FieldDelta), intent(in) :: this
      type(ESMF_Field), intent(inout) :: fieldList(:)
      character(*), intent(in) :: ignore
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      do i = 1, size(fieldList)
         call this%reallocate_field(fieldList(i), ignore, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine reallocate_fields

   subroutine MAPL_EmptyField(field, rc)
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status

      field%ftypep%status = ESMF_FIELDSTATUS_GRIDSET
      call ESMF_ArrayDestroy(field%ftypep%array, _RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_EmptyField

end module mapl3g_FieldDelta

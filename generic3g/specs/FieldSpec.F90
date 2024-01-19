#include "MAPL_Generic.h"

module mapl3g_FieldSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_UngriddedDimsSpec
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_MultiState
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_ExtensionAction
   use mapl3g_VerticalGeom
   use mapl3g_VerticalDimSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_NullAction
   use mapl3g_SequenceAction
   use mapl3g_CopyAction
   use mapl3g_RegridAction
   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use mapl3g_geom_mgr, only: MAPL_SameGeom
   use gftl2_StringVector
   use esmf
   use nuopc

   implicit none
   private

   public :: FieldSpec
   public :: new_FieldSpec_geom

   type, extends(AbstractStateItemSpec) :: FieldSpec
      private

      type(ESMF_Geom), allocatable :: geom
      type(VerticalGeom) :: vertical_geom
      type(VerticalDimSpec) :: vertical_dim
      type(ESMF_typekind_flag) :: typekind = ESMF_TYPEKIND_R4
      type(UngriddedDimsSpec) :: ungridded_dims
      type(StringVector) :: attributes

      ! Metadata
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name
      character(:), allocatable :: units
      ! TBD
!#      type(FrequencySpec) :: freq_spec
!#      class(AbstractFrequencySpec), allocatable :: freq_spec
!#      integer :: halo_width = 0

      type(ESMF_Field) :: payload
      real, allocatable :: default_value

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate
      procedure :: get_dependencies
      procedure :: get_payload

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: check_complete

      procedure :: extension_cost
      procedure :: make_extension
      procedure :: make_extension_safely
      procedure :: make_action
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_geom
!#      module procedure new_FieldSpec_defaults
   end interface FieldSpec

   interface match
      procedure :: match_geom
      procedure :: match_typekind
      procedure :: match_string
   end interface match

   interface get_cost
      procedure :: get_cost_geom
      procedure :: get_cost_typekind
      procedure :: get_cost_string
   end interface get_cost

   interface update_item
      procedure update_item_geom
      procedure update_item_typekind
      procedure update_item_string
   end interface update_item

contains


   function new_FieldSpec_geom(geom, vertical_geom, vertical_dim, typekind, ungridded_dims, &
        standard_name, long_name, units, &
        attributes, default_value) result(field_spec)
      type(FieldSpec) :: field_spec

      type(ESMF_Geom), intent(in) :: geom
      type(VerticalGeom), intent(in) :: vertical_geom
      type(VerticalDimSpec), intent(in) :: vertical_dim
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(UngriddedDimsSpec), intent(in) :: ungridded_dims

      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: long_name
      type(StringVector), optional, intent(in) :: attributes

      ! optional args last
      real, optional, intent(in) :: default_value

      field_spec%geom = geom
      field_spec%vertical_geom = vertical_geom
      field_spec%vertical_dim = vertical_dim
      field_spec%typekind = typekind
      field_spec%ungridded_dims = ungridded_dims

      if (present(standard_name)) field_spec%standard_name = standard_name
      if (present(long_name)) field_spec%long_name = long_name
      if (present(units)) field_spec%units = units

      if (present(attributes)) field_spec%attributes = attributes
      if (present(default_value)) field_spec%default_value = default_value

   end function new_FieldSpec_geom


!#   function new_FieldSpec_defaults(ungridded_dims, geom, units) result(field_spec)
!#      type(FieldSpec) :: field_spec
!#      type(ExtraDimsSpec), intent(in) :: ungridded_dims
!#      type(ESMF_Geom), intent(in) :: geom
!#      character(*), intent(in) :: units
!#      
!#      field_spec = FieldSpec(ungridded_dims, ESMF_TYPEKIND_R4, geom, units)
!#      
!#   end function new_FieldSpec_defaults
!#

   subroutine create(this, dependency_specs, rc)
      class(FieldSpec), intent(inout) :: this
      type(StateItemSpecPtr), intent(in) :: dependency_specs(:)
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldEmptyCreate(_RC)
      call MAPL_FieldEmptySet(this%payload, this%geom, _RC)

      call this%set_created()

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine MAPL_FieldEmptySet(field, geom, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Geom), intent(inout) :: geom
      integer, optional, intent(out) ::rc

      type(ESMF_GeomType_Flag) :: geom_type
      type(ESMF_Grid) :: grid
      type(ESMF_Mesh) :: mesh
      type(ESMF_XGrid) :: xgrid
      type(ESMF_LocStream) :: locstream
      integer :: status

      call ESMF_GeomGet(geom, geomtype=geom_type, _RC)

      if(geom_type == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         call ESMF_FieldEmptySet(field, grid, _RC)
      else if (geom_type == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomGet(geom, mesh=mesh, _RC)
         call ESMF_FieldEmptySet(field, mesh, _RC)
      else if (geom_type == ESMF_GEOMTYPE_XGRID) then
         call ESMF_GeomGet(geom, xgrid=xgrid, _RC)
         call ESMF_FieldEmptySet(field, xgrid, _RC)
      else if (geom_type == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom, locstream=locstream, _RC)
         call ESMF_FieldEmptySet(field, locstream, _RC)
      else
         _FAIL('Unsupported type of Geom')
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_FieldEmptySet

   subroutine destroy(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, nogarbage=.true., _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus
      integer, allocatable :: final_lbounds(:),final_ubounds(:)
      integer :: num_levels, total_ungridded_dims

      num_levels = this%vertical_geom%get_num_levels()
      if (this%vertical_dim == VERTICAL_DIM_NONE) then
         final_lbounds = this%ungridded_dims%get_lbounds()
         final_ubounds = this%ungridded_dims%get_ubounds()
      else
         total_ungridded_dims = size(this%ungridded_dims%get_lbounds())
         if (this%vertical_dim == VERTICAL_DIM_CENTER) then
            final_lbounds = [1, this%ungridded_dims%get_lbounds()]
            final_ubounds=[num_levels, this%ungridded_dims%get_ubounds()]
         else if (this%vertical_dim == VERTICAL_DIM_EDGE) then
            final_lbounds = [0, this%ungridded_dims%get_lbounds()]
            final_ubounds = [num_levels, this%ungridded_dims%get_ubounds()]
         end if
      end if
       
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      if (fstatus == ESMF_FIELDSTATUS_GRIDSET) then

         call ESMF_FieldEmptyComplete(this%payload, this%typekind, &
              ungriddedLBound= final_lbounds,  &
              ungriddedUBound= final_ubounds,  &
              _RC)
         call ESMF_FieldGet(this%payload, status=fstatus, _RC)
         _ASSERT(fstatus == ESMF_FIELDSTATUS_COMPLETE, 'ESMF field status problem.')

         if (allocated(this%default_value)) then
            call set_field_default(_RC)
         end if
          
          call this%set_allocated()
      end if

      _RETURN(ESMF_SUCCESS)

      contains
         subroutine set_field_default(rc)
            integer, intent(out), optional :: rc
            real(kind=ESMF_KIND_R4), pointer :: x_r4_1d(:),x_r4_2d(:,:),x_r4_3d(:,:,:),x_r4_4d(:,:,:,:)
            real(kind=ESMF_KIND_R8), pointer :: x_r8_1d(:),x_r8_2d(:,:),x_r8_3d(:,:,:),x_r8_4d(:,:,:,:)
            integer :: status, rank
                
            call ESMF_FieldGet(this%payload,rank=rank,_RC) 
            if (this%typekind == ESMF_TYPEKIND_R4) then
               if (rank == 1) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r4_1d,_RC)
                  x_r4_1d = this%default_value   
               else if (rank == 2) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r4_2d,_RC)
                  x_r4_2d = this%default_value   
               else if (rank == 3) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r4_3d,_RC)
                  x_r4_3d = this%default_value   
               else if (rank == 4) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r4_4d,_RC)
                  x_r4_4d = this%default_value   
               else
                  _FAIL('unsupported rank')
               end if
            else if (this%typekind == ESMF_TYPEKIND_R8) then
               if (rank == 1) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r8_1d,_RC)
                  x_r8_1d = this%default_value   
               else if (rank == 2) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r8_2d,_RC)
                  x_r8_2d = this%default_value   
               else if (rank == 3) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r8_3d,_RC)
                  x_r8_3d = this%default_value   
               else if (rank == 4) then
                  call ESMF_FieldGet(this%payload,farrayptr=x_r8_4d,_RC)
                  x_r8_4d = this%default_value   
               else
                  _FAIL('unsupported rank')
               end if
            else
               _FAIL('unsupported typekind')
            end if
            _RETURN(ESMF_SUCCESS)
         end subroutine set_field_default
            
   end subroutine allocate

   function get_dependencies(this, rc) result(dependencies)
      type(ActualPtVector) :: dependencies
      class(FieldSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      dependencies = ActualPtVector()

      _RETURN(_SUCCESS)
   end function get_dependencies

   subroutine connect_to(this, src_spec, actual_pt, rc)
      class(FieldSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (FieldSpec)
         ! ok
         call this%destroy(_RC)
         this%payload = src_spec%payload
         call mirror(dst=this%typekind, src=src_spec%typekind, _RC)

         call this%set_created()
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   contains
      
      subroutine mirror(dst, src, rc)
         type(ESMF_TypeKind_Flag), intent(inout) :: dst, src
         integer, optional, intent(out) :: rc
         if (dst /= src) then
            if (dst == MAPL_TYPEKIND_MIRROR) then
               dst = src
               _RETURN(_SUCCESS)
            end if
            if (src == MAPL_TYPEKIND_MIRROR) then
               src = dst
               _RETURN(_SUCCESS)
            end if
         end if

         _ASSERT(dst == src, 'unsupported typekind mismatch')
      end subroutine mirror

   end subroutine connect_to



   logical function can_connect_to(this, src_spec)
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (FieldSpec)
         can_connect_to = all ([ &
              this%ungridded_dims == src_spec%ungridded_dims, &
              this%vertical_dim == src_spec%vertical_dim, &
!#              can_convert_units(this, src_spec) &
              this%ungridded_dims == src_spec%ungridded_dims, & 
              includes(this%attributes, src_spec%attributes),  &
              match(this%units, src_spec%units) &
              ])
      class default
         can_connect_to = .false.
      end select
   contains

      logical function includes(mandatory, provided)
         type(StringVector), target, intent(in) :: mandatory
         type(StringVector), target, intent(in) :: provided

         integer :: i, j
         character(:), pointer :: attribute_name

         m: do i = 1, mandatory%size()
            attribute_name => mandatory%of(i)
            p: do j = 1, provided%size()
               if (attribute_name == provided%of(j)) cycle m
            end do p
            ! ith not found
            includes = .false.
            return
         end do m

         includes = .true.
      end function includes
   end function can_connect_to


   logical function same_typekind(a, b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b
      same_typekind = (a%typekind == b%typekind)
   end function same_typekind

   ! Eventually we will integrate UDunits, but for now
   ! we require units to exactly match when connecting
   ! fields.
   logical function can_convert_units(a,b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b

      can_convert_units = a%units == b%units

   end function can_convert_units

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(FieldSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: short_name

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)
      call get_substate(state, actual_pt%get_comp_name(), substate=substate, _RC)

      short_name = actual_pt%get_esmf_name()
      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(FieldSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldBundleAdd(bundle, [this%payload], multiflag=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

   logical function check_complete(this, rc)
      class(FieldSpec), intent(in) :: this
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus

      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      check_complete = (fstatus == ESMF_FIELDSTATUS_COMPLETE)

   end function check_complete

   integer function extension_cost(this, src_spec, rc) result(cost)
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      cost = 0
      select type (src_spec)
      type is (FieldSpec)
         cost = cost + get_cost(this%geom, src_spec%geom)
         cost = cost + get_cost(this%typekind, src_spec%typekind)
!#         cost = cost + get_cost(this%units, src_spec%units)
      class default
         _FAIL('Cannot extend to this StateItemSpec subclass.')
      end select

      _RETURN(_SUCCESS)
   end function extension_cost

   function make_extension(this, dst_spec, rc) result(extension)
      class(AbstractStateItemSpec), allocatable :: extension
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status

      find_mismatch: select type (dst_spec)
      type is (FieldSpec)
         allocate(extension, source=this%make_extension_safely(dst_spec))
         call extension%create([StateItemSpecPtr::], _RC)
      class default
         extension=this
         _FAIL('Unsupported subclass.')
      end select find_mismatch

      _RETURN(_SUCCESS)
   end function make_extension

   function make_extension_safely(this, src_spec) result(extension)
      type(FieldSpec) :: extension
      class(FieldSpec), intent(in) :: this
      type(FieldSpec), intent(in) :: src_spec

      logical :: found

      extension = this
      if (update_item(extension%geom, src_spec%geom)) return
      if (update_item(extension%typekind, src_spec%typekind)) then
         return
      end if
!#      if (update_item(extension%units, src_spec%units)) return

    end function make_extension_safely

   ! Return an atomic action that tranforms payload of "this"
   ! to payload of "goal".
   function make_action(this, dst_spec, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: dst_spec
      integer, optional, intent(out) :: rc

      integer :: status

      action = NullAction() ! default

      select type (dst_spec)
      type is (FieldSpec)

         if (.not. MAPL_SameGeom(this%geom, dst_spec%geom)) then
            deallocate(action)
            action = RegridAction(this%geom, this%payload, dst_spec%geom, dst_spec%payload)
            _RETURN(_SUCCESS)
         end if

         if (this%typekind /= dst_spec%typekind) then
            deallocate(action)
            action = CopyAction(this%payload, dst_spec%payload)
            _RETURN(_SUCCESS)
         end if
         
!#         if (this%units /= dst_spec%units) then
!#            action = ChangeUnitsAction(this%payload, dst_spec%payload)
!#            _RETURN(_SUCCESS)
!#         end if
         
      class default
         action = NullAction()
         _FAIL('Dst spec is incompatible with FieldSpec.')
      end select

      _RETURN(_SUCCESS)
   end function make_action

   logical function match_geom(a, b) result(match)
      type(ESMF_Geom), allocatable, intent(in) :: a, b

      integer :: status

      match = .false.

      if (allocated(a) .and. allocated(b)) then
         match = MAPL_SameGeom(a, b)
      end if


   end function match_geom

   logical function match_typekind(a, b) result(match)
      type(ESMF_TypeKind_Flag), intent(in) :: a, b

      ! If both typekinds are MIRROR then must fail (but not here)
      if (a /= b) then
         match = any([a%dkind,b%dkind] == MAPL_TYPEKIND_MIRROR%dkind)
      else
         match = (a == b)
      end if
   end function match_typekind

   logical function match_string(a, b) result(match)
      character(:), allocatable, intent(in) :: a, b
      match = .true.
      if (allocated(a) .and. allocated(b)) then
         match = (a == b)
      end if
   end function match_string

   integer function get_cost_geom(a, b) result(cost)
      type(ESMF_GEOM), allocatable, intent(in) :: a, b
      cost = 0
      if (.not. match(a, b)) cost = 1
   end function get_cost_geom

   integer function get_cost_typekind(a, b) result(cost)
      type(ESMF_TypeKind_Flag), intent(in) :: a, b
      cost = 0
      if (.not. match(a,b)) cost = 1
   end function get_cost_typekind

   integer function get_cost_string(a, b) result(cost)
      character(:), allocatable, intent(in) :: a, b
      cost = 0
      if (.not. match(a,b)) cost = 1
   end function get_cost_string

   logical function update_item_geom(a, b)
      type(ESMF_GEOM), allocatable, intent(inout) :: a
      type(ESMF_GEOM), allocatable, intent(in) :: b

      update_item_geom = .false.
      if (.not. match(a, b)) then
         a = b
         update_item_geom = .true.
      end if
   end function update_item_geom

   logical function update_item_typekind(a, b)
      type(ESMF_TypeKind_Flag), intent(inout) :: a
      type(ESMF_TypeKind_Flag), intent(in) :: b

      update_item_typekind = .false.
      if (.not. match(a, b)) then
         a = b
         update_item_typekind = .true.
      end if
   end function update_item_typekind

   logical function update_item_string(a, b)
      character(:), allocatable, intent(inout) :: a
      character(:), allocatable, intent(in) :: b
      
      update_item_string = .false.
      if (.not. match(a, b)) then
         a = b
         update_item_string = .true.
      end if
   end function update_item_string

   function get_payload(this) result(payload)
      type(ESMF_Field) :: payload
      class(FieldSpec), intent(in) :: this
      payload = this%payload
   end function get_payload

end module mapl3g_FieldSpec

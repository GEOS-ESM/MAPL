! This type should be private to generic layer.

#include "MAPL_ErrLog.h"

module MAPL_VarSpecTypeMod
   use ESMF, only: ESMF_MAXSTR, ESMF_SUCCESS
   use ESMF, only: ESMF_Grid
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_State
   use mapl_ErrorHandlingMod

   use oomph, only: HorizontalStaggerLoc
   use oomph, only: VerticalStaggerLoc
   use oomph, only: UngriddedDimSpec
   use oomph, only: DimsSpec
   use oomph, only: FieldSpec
   implicit none
   private

   public :: MAPL_VarSpecType
   public :: MAPL_VarSpecSet
   
   type :: MAPL_VarSpecType
      ! new
      type(FieldSpec) :: field_spec

      ! legacy
      character(len=ESMF_MAXSTR)               :: SHORT_NAME
      character(len=ESMF_MAXSTR)               :: LONG_NAME
      character(len=ESMF_MAXSTR)               :: UNITS
      character(len=ESMF_MAXSTR)               :: FRIENDLYTO
      character(len=ESMF_MAXSTR)               :: VECTOR_PAIR
      character(len=ESMF_MAXSTR), pointer      :: ATTR_INAMES(:) => null()
      character(len=ESMF_MAXSTR), pointer      :: ATTR_RNAMES(:) => null()
      integer,                    pointer      :: ATTR_IVALUES(:) => null()
      real,                       pointer      :: ATTR_RVALUES(:) => null()
      integer,                    pointer      :: UNGRIDDED_DIMS(:) => null()
      character(len=ESMF_MAXSTR)               :: UNGRIDDED_UNIT
      character(len=ESMF_MAXSTR)               :: UNGRIDDED_NAME
      real,                       pointer      :: UNGRIDDED_COORDS(:)
      integer                                  :: DIMS
      integer                                  :: LOCATION
      integer                                  :: NUM_SUBTILES
      integer                                  :: STAT
      integer                                  :: ACCMLT_INTERVAL
      integer                                  :: COUPLE_INTERVAL
      integer                                  :: OFFSET
      integer                                  :: LABEL
      integer                                  :: HALOWIDTH
      integer                                  :: PRECISION
      integer                                  :: FIELD_TYPE
      integer                                  :: VECTOR_ORDER
      integer                                  :: STAGGERING
      integer                                  :: ROTATION
      integer                                  :: RESTART
      logical                                  :: defaultProvided
      logical                                  :: doNotAllocate
      logical                                  :: alwaysAllocate ! meant for export specs
      real                                     :: DEFAULT
      type(ESMF_Field), pointer                :: FIELD => null()
      type(ESMF_FieldBundle), pointer          :: BUNDLE => null()
      type(ESMF_State), pointer                :: STATE => null()
      type(ESMF_Grid)                          :: GRID
   contains
      procedure :: MAPL_VarSpecSetNew
      generic :: MAPL_VarSpecSet => MAPL_VarSpecSetNew
   end type MAPL_VarSpecType

   interface MAPL_VarSpecSet
      module procedure MAPL_VarSpecSetNew
   end interface MAPL_VarSpecSet

contains
   
  subroutine MAPL_VarSpecSetNew(spec, short_name, long_name, units,       &
                             dims, vlocation, field, bundle, state,    &
                             stat, accmlt_interval, couple_interval,   &
                             offset, label,                            &
                             friendlyto,                               &
                             field_type,                               &
                             staggering,                               &
                             rotation,                                 &
                             grid,                                     &
                             donotallocate,                            &
                             alwaysallocate,                            &
                                                                    rc )

    class(mapl_varspectype),             intent(inout)   :: spec
    character(len=*)   , optional   , intent(in)      :: short_name
    character(len=*)   , optional   , intent(in)      :: long_name
    character(len=*)   , optional   , intent(in)      :: units
    integer            , optional   , intent(in)      :: dims
    integer            , optional   , intent(in)      :: vlocation
    integer            , optional   , intent(in)      :: accmlt_interval
    integer            , optional   , intent(in)      :: couple_interval
    integer            , optional   , intent(in)      :: offset
    integer            , optional   , intent(in)      :: stat
    integer            , optional   , intent(in)      :: label
    type(ESMF_field)   , optional   , intent(in)      :: field
    type(ESMF_fieldbundle)  , optional   , intent(in) :: bundle
    type(ESMF_state)   , optional   , intent(in)      :: state
    character(len=*)   , optional   , intent(in)      :: friendlyto
    integer            , optional   , intent(in)      :: field_type
    integer            , optional   , intent(in)      :: staggering
    integer            , optional   , intent(in)      :: rotation
    type(ESMF_grid)    , optional   , intent(in)      :: grid
    logical            , optional   , intent(in)      :: donotallocate
    logical            , optional   , intent(in)      :: alwaysallocate
    integer            , optional   , intent(out)     :: rc



      if(present(short_name)) then
        spec%short_name = short_name
      endif

      if(present(long_name)) then
        spec%long_name = long_name
      endif

      if(present(units)) then
        spec%units = units
      endif

      if(present(friendlyto)) then
        spec%friendlyto = friendlyto
      endif

      if(present(stat)) then
       spec%stat=stat
      endif

      if(present(dims)) then
       spec%dims=dims
      endif

      if(present(vlocation)) then
       spec%location=vlocation
      endif

      if(present(accmlt_interval)) then
       spec%accmlt_interval=accmlt_interval
      endif

      if(present(couple_interval)) then
       spec%couple_interval=couple_interval
      endif

      if(present(offset)) then
       spec%offset=offset
      endif

      if(present(label)) then
       spec%label=label
      endif

      if(present(field)) then
       spec%field = field
      endif

      if(present(bundle)) then
       spec%bundle = bundle
      endif

      if(present(state)) then
       spec%state = state
      endif

      if(present(grid)) then
         spec%grid = grid
      endif

      if(present(field_type)) then
         spec%field_type = field_type
      endif

      if(present(staggering)) then
         spec%staggering = staggering
      endif

      if(present(rotation)) then
         spec%rotation = rotation
      endif

      if(present(donotallocate)) then
         spec%donotallocate = donotallocate
      endif

      if(present(alwaysallocate)) then
         spec%alwaysallocate = alwaysallocate
      endif

      associate( &
           horz_spec => create_horz_stagger_spec(spec), &
           vert_spec => create_vert_stagger_spec(spec), &
           ungr_spec => create_ungridded_dim_specs(spec), &
           tk_spec => create_tk_spec(spec))
        associate( dims_spec => DimsSpec(horz_spec, vert_spec, ungr_spec, spec%halowidth))
          
          spec%field_spec = FieldSpec(spec%long_name, dims_spec, tk_spec)

        end associate
      end associate

      _RETURN(ESMF_SUCCESS)

   contains

      function create_horz_stagger_spec(this) result(horz_spec)
         type(HorizontalStaggerLoc) :: horz_spec
         type(MAPL_VarSpecType), intent(in) :: this
      end function create_horz_stagger_spec

      function create_vert_stagger_spec(this) result(vert_spec)
         type(VerticalStaggerLoc) :: vert_spec
         type(MAPL_VarSpecType), intent(in) :: this
      end function create_vert_stagger_spec

      function create_ungridded_dim_specs(this) result(ungr_spec)
         type(UngriddedDimSpec), allocatable :: ungr_spec(:)
         type(MAPL_VarSpecType), intent(in) :: this
      end function create_ungridded_dim_specs

      function create_tk_spec(this) result(tk_spec)
         use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8
         use esmf, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8
         use esmf, only: ESMF_TYPEKIND_FLAG
         type(ESMF_TypeKind_Flag) :: tk_spec
         type(MAPL_VarSpecType), intent(in) :: this

         select case (this%precision)
         case (ESMF_KIND_R4)
            tk_spec = ESMF_TYPEKIND_R4
         case (ESMF_KIND_R8)
            tk_spec = ESMF_TYPEKIND_R8
         case default
            error stop
         end select

      end function create_tk_spec

   end subroutine MAPL_VarSpecSetNew

end module MAPL_VarSpecTypeMod

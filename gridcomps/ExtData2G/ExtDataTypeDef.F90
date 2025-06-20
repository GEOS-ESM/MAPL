#include "MAPL_Exceptions.h"
module MAPL_ExtDataTypeDef
   use ESMF
   use MAPL_GriddedIOItemMod
   use MAPL_ExtDataBracket
   use MAPL_ExtDataPointerUpdate
   use MAPL_ExtDataAbstractFileHandler
   use MAPL_FileMetadataUtilsMod
   use MAPL_StateUtils
   use VerticalCoordinateMod
   implicit none

   public PrimaryExport
   public DerivedExport
   public BracketingFields
   public copy_primary

  integer, parameter         :: MAPL_ExtDataNullFrac      = -9999

  type BracketingFields
     ! fields to store endpoints for interpolation of a vector pair
     type(ExtDataBracket) :: comp1
     type(ExtDataBracket) :: comp2
     logical :: initialized = .false.
  end type BracketingFields

  type PrimaryExport
     character(len=ESMF_MAXSTR)   :: name
     character(len=:), allocatable :: units
     integer                      :: Trans
     character(len=ESMF_MAXSTR)   :: var
     character(len=ESMF_MAXPATHLEN)   :: file_template

     logical                      :: isConst
     real                         :: Const !remove
     integer                      :: vartype ! MAPL_FieldItem or MAPL_BundleItem

     class(ExtDataAbstractFileHandler), allocatable :: filestream

     ! if primary export represents a pair of vector fields
     logical                      :: isVector
     type(BracketingFields)       :: modelGridFields

     ! names of the two vector components in the gridded component where import is declared
     character(len=ESMF_MAXSTR)   :: vcomp1, vcomp2
     ! the corresponding names of the two vector components on file
     character(len=ESMF_MAXSTR)   :: fcomp1, fcomp2
     type(GriddedIOitem)          :: fileVars

     integer                      :: pfioCollection_id
     integer                      :: iclient_collection_id

     logical                      :: ExtDataAlloc
     integer                      :: FracVal = MAPL_ExtDataNullFrac
     ! do we have to do vertical interpolation
     logical                      :: do_VertInterp = .false.
     logical                      :: do_Fill = .false.
     type(FileMetadataUtils)      :: file_metadata
     type(ExtDataPointerUpdate)   :: update_freq
     type(VerticalCoordinate)     :: vcoord
     logical                      :: delivered_item = .true. 

     ! new stuff
     logical                      :: cycling
     logical                      :: persist_closest
     type(ESMF_Time), allocatable :: source_time(:)

     ! for multiple collections
     type(ESMF_Time), allocatable :: start_end_time(:)
     logical :: initialized = .false.
     logical :: fail_on_missing_file = .true.

     type(ESMF_FieldBundle) :: t_interp_bundle

     character(len=4) :: importVDir = "down"
     logical :: enable_vertical_regrid = .false.
     logical :: allow_vertical_regrid = .false.
     character(len=:), allocatable :: aux_ps, aux_q
     real, allocatable :: molecular_weight

  end type PrimaryExport
  
  type DerivedExport
     character(len=ESMF_MAXSTR)     :: name
     character(len=ESMF_MAXPATHLEN) :: expression
     logical                        :: masking
     type(StateMask), allocatable :: mask_definition 
     type(ExtDataPointerUpdate)     :: update_freq
     contains
        procedure :: evaluate_derived_field
  end type DerivedExport

  contains

      subroutine copy_primary(p1, p2, aux_type )
         type(PrimaryExport), intent(in) :: p1
         type(PrimaryExport), intent(out) :: p2
         character(len=*), intent(in) :: aux_type

         character(len=:), allocatable :: new_name, aux_name
         logical :: has_aux_item
         if (aux_type == 'Q') then
            has_aux_item = allocated(p1%aux_q)
            aux_name=p1%aux_q
         end if
         if (aux_type == 'PS') then
            has_aux_item = allocated(p1%aux_ps)
            aux_name=p1%aux_ps
         end if
         p2 = p1
         if (has_aux_item) then
            p2%var=aux_name
            new_name = aux_name//"_"//trim(p1%name)
            p2%name=new_name
            p2%fileVars%xname = aux_name
            p2%fcomp1 = aux_name
            p2%vcomp1=new_name
         else
            p2%file_template = "/dev/null"
            p2%isConst = .true.
         end if
         p2%delivered_item = .false.

      end subroutine
         
      subroutine evaluate_derived_field(this,state,rc)
         class(DerivedExport), intent(inout) :: this
         type(ESMF_State), intent(inout) :: state
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Field) :: field

         call ESMF_StateGet(state,trim(this%name),field,_RC)
         if (this%masking) then
            call this%mask_definition%evaluate_mask(state,field,_RC)
         else
            call MAPL_StateEval(state,trim(this%expression),field,_RC)
         end if
         _RETURN(_SUCCESS)
      end subroutine

end module  MAPL_ExtDataTypeDef

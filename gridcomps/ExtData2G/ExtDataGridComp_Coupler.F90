#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"

   MODULE MAPL_ExtDataGridComp_Coupler

   USE ESMF
   use MAPL_ExtDataGridComp_Base, only: extdata_base_setServices => SetServices
   use gFTL_StringVector
   use gFTL_IntegerVector
   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use ESMFL_Mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use MAPL_CFIOMod
   use MAPL_ConstantsMod, only: MAPL_PI,MAPL_PI_R8,MAPL_RADIANS_TO_DEGREES
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_GridManagerMod
   use pflogger, only: logging, Logger
   use MAPL_ExtDataLogger
   use MAPL_ExtDataConstants
   use MAPL_ExceptionHandling
   use MAPL_AbstractRegridderMod
   use MAPL_newRegridderManager
   use MAPL_RegridMethods
   use MAPL_ExtDataMask
   use MAPL_NewArthParserMod

   IMPLICIT NONE
   PRIVATE

   public :: SetServices
   integer, save :: extdata_base

CONTAINS



   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

    integer                             :: status

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, _RC)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_,        _RC)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize_,   _RC)
        
    extdata_base = MAPL_AddChild(GC, name = "EXTDATA_BASE", SS=extdata_base_setServices,_RC)  
    call MAPL_TimerAdd(gc,name="Initialize", rc=status)
    _VERIFY(STATUS)
   call MAPL_TimerAdd(gc,name="Run", rc=status)
    _VERIFY(STATUS)

    call MAPL_GenericSetServices ( GC, __RC__ )

!   All done
!   --------

    _RETURN(ESMF_SUCCESS)

  END SUBROUTINE SetServices

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout)   :: CLOCK 

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC 
   type(ESMF_State), intent(inout)    :: IMPORT
   type(ESMF_State), intent(inout)    :: EXPORT
   integer, intent(out)               :: rc

   integer                           :: Status
   type(MAPL_MetaComp),pointer :: MAPLSTATE
   integer                           :: item_count
   type (ESMF_StateItem_Flag), allocatable   :: item_types(:)
   character(len=ESMF_MAXSTR), allocatable   :: item_names(:)
   type(ESMF_State) :: base_extdata_state
   integer :: i
   type (ESMF_State),         pointer  :: GEX(:)
   type(ESMF_Field) :: empty_field

   call MAPL_GetLogger(gc, extdata_lgr, _RC)

!  Start Some Timers
!  -----------------
   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, _RC)
   call MAPL_Get(maplstate,gex=gex,_RC)
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Initialize")

   call ESMF_StateGet(export, itemCount=item_count, _RC)

   allocate(item_names(item_count), STAT=STATUS)
   _VERIFY(STATUS)
   allocate(item_types(item_count), STAT=STATUS)
   _VERIFY(STATUS)
   call ESMF_StateGet(export, ITEMNAMELIST=item_names, ITEMTYPELIST=item_types, _RC)
   call MAPL_ExportStateGet([gex(extdata_base)], "EXTDATA_BASE" ,base_extdata_state,_RC) 

   do i=1,item_count
      if (item_types(i) == ESMF_STATEITEM_FIELD) then
         empty_field=ESMF_FieldEmptyCreate(name=trim(item_names(i)),_RC)
         call MAPL_StateAdd(base_extdata_state,empty_field,_RC)
      end if
   enddo 

   deallocate(item_types)
   deallocate(item_names)

   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  _RC)
   call MAPL_TimerOff(MAPLSTATE,"Initialize")
   call MAPL_TimerOff(MAPLSTATE,"TOTAL")
!  All done
!  --------

   call extdata_lgr%debug('ExtData Initialize_(): End')

   _RETURN(ESMF_SUCCESS)

   END SUBROUTINE Initialize_

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 
   type(MAPL_MetaComp),pointer :: MAPLSTATE
   integer :: status
   type(ESMF_State) :: base_extdata_state
   type (ESMF_State),         pointer  :: GEX(:)

   _UNUSED_DUMMY(IMPORT)
   _UNUSED_DUMMY(EXPORT)

   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, _RC)
   call MAPL_TimerOn(MAPLSTATE,"Run")
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_GenericRunChildren(gc,import,export,clock,_RC)

   call MAPL_Get(maplstate,gex=gex,_RC)
   call MAPL_ExportStateGet([gex(extdata_base)], "EXTDATA_BASE" ,base_extdata_state,_RC) 

   call process_state(base_extdata_state,export,_RC)
   call evaluate_derived_fields(base_extdata_state,export,_RC)


   call MAPL_TimerOff(MAPLSTATE,"Run")
   call MAPL_TimerOff(MAPLSTATE,"TOTAL")

   _RETURN(ESMF_SUCCESS)

   END SUBROUTINE Run_

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

  implicit NONE

   type(ESMF_Clock),  intent(inout) :: CLOCK   
   type(ESMF_GridComp), intent(inout)  :: GC     
   type(ESMF_State), intent(inout) :: IMPORT     
   type(ESMF_State), intent(inout) :: EXPORT     
   integer, intent(out) ::  rc                   

   integer                           :: status

   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  __RC__ )

   _RETURN(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

 SUBROUTINE process_state ( input_state, output_state,  rc )
   type(ESMF_State), intent(inout) :: input_state
   type(ESMF_State), intent(inout) :: output_state
   integer, optional, intent(out) ::  rc         
                                                 
   integer :: status
   integer :: item_count,i
   type (ESMF_StateItem_Flag), allocatable   :: item_types(:)
   character(len=ESMF_MAXSTR), allocatable   :: item_names(:)

   call ESMF_StateGet(output_state, itemCount=item_count, _RC)
   allocate(item_names(item_count), STAT=STATUS)
   _VERIFY(STATUS)
   allocate(item_types(item_count), STAT=STATUS)
   _VERIFY(STATUS)
   call ESMF_StateGet(output_state, ITEMNAMELIST=item_names, ITEMTYPELIST=item_types, _RC)
   do i=1,item_count
      if (item_types(i) /= ESMF_StateItem_Field) cycle
      call process_field(item_names(i),input_state,output_state,_RC)
   enddo

   _RETURN(ESMF_SUCCESS)

   END SUBROUTINE process_state

   subroutine process_field(field_name,input_state,output_state,rc)
      character(len=*), intent(in) :: field_name
      type(ESMF_State), intent(inout) :: input_state
      type(ESMF_State), intent(inout) :: output_state
      integer, optional, intent(out) :: rc

      type(ESMF_Info) :: infoh
      integer :: status
      type(ESMF_Field) :: input_field, output_field
      logical :: is_derived,is_vector,do_update,is_constant
      integer :: vector_comp,regridding_method
      character(len=ESMF_MAXSTR) :: other_component_name
      type(ESMF_Field) :: input_auxiliary_field, output_auxiliary_field

      call ESMF_StateGet(input_state,field_name,input_field,_RC)
      call ESMF_StateGet(output_state,field_name,output_field,_RC)
      call ESMF_InfoGetFromHost(input_field,infoh,_RC)
      is_derived = ESMF_InfoIsPresent(infoh,extdata_expression)
      if (is_derived) then
         _RETURN(_SUCCESS)
      end if

      is_vector = ESMF_InfoIsPresent(infoh,extdata_vector_comp,_RC)
      call ESMF_InfoGet(infoh,extdata_regridding_method,regridding_method,_RC)
      is_constant = ESMF_InfoIsPresent(infoh,extdata_constant_value,_RC)

      if (is_vector) then
         call ESMF_InfoGet(infoh,extdata_vector_comp,value=vector_comp,_RC)
         if (vector_comp == 2) then
            _RETURN(_SUCCESS)
         else
            _ASSERT(vector_comp == 1, "I was mismarked as a vector")
         end if
         call ESMF_InfoGet(infoh,extdata_vector_partner,value=other_component_name,_RC)
         call ESMF_StateGet(input_state,other_component_name,input_auxiliary_field,_RC)
         call ESMF_StateGet(output_state,other_component_name,output_auxiliary_field,_RC)
         if (is_constant) then
            call fill_constant_field(input_field,output_field,_RC)
            call fill_constant_field(input_field,output_auxiliary_field,_RC)
         else
            call ESMF_InfoGet(infoh,extdata_update,do_update,_RC)
            if (do_update) then
               call regrid_vector(input_field,input_auxiliary_field,output_field,output_auxiliary_field,regridding_method,_RC)
            end if
         end if 

      else
         if (is_constant) then
            call fill_constant_field(input_field,output_field,_RC)
         else
            call ESMF_InfoGet(infoh,extdata_update,do_update,_RC)
            if (do_update) then
               call regrid_scalar(input_field,output_field,regridding_method,_RC)
            end if
         end if 
      end if
      _RETURN(_SUCCESS)
   end subroutine

   subroutine regrid_scalar(input_field,output_field,regrid_method,rc)
      type(ESMF_Field), intent(inout) :: input_field
      type(ESMF_Field), intent(inout) :: output_Field
      integer, intent(in) :: regrid_method
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: input_grid,output_grid
      integer :: input_rank,output_rank
      class(AbstractRegridder), pointer :: regrid_handle
      real(ESMF_KIND_R4), pointer :: input_ptr_2d(:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr_2d(:,:)
      real(ESMF_KIND_R4), pointer :: input_ptr_3d(:,:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr_3d(:,:,:)

      call ESMF_FieldGet(input_field,rank=input_rank,grid=input_grid,_RC)
      call ESMF_FieldGet(output_field,rank=output_rank,grid=output_grid,_RC)
      _ASSERT(input_rank == output_rank,"Something went wrong ranks dont match")
      regrid_handle => new_regridder_manager%make_regridder(input_grid,output_grid,regrid_method,_RC)
      if (input_rank==2) then
         call ESMF_FieldGet(input_field,0,farrayPtr=input_ptr_2d,_RC) 
         call ESMF_FieldGet(output_field,0,farrayPtr=output_ptr_2d,_RC)
         call regrid_handle%regrid(input_ptr_2d,output_ptr_2d,_RC)
      else if (input_rank==3) then
         call ESMF_FieldGet(input_field,0,farrayPtr=input_ptr_3d,_RC) 
         call ESMF_FieldGet(output_field,0,farrayPtr=output_ptr_3d,_RC) 
         call regrid_handle%regrid(input_ptr_3d,output_ptr_3d,_RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine

   subroutine regrid_vector(input_field_comp1,input_field_comp2,output_field_comp1,output_field_comp2,regrid_method,rc)
      type(ESMF_Field), intent(inout) :: input_field_comp1
      type(ESMF_Field), intent(inout) :: input_field_comp2
      type(ESMF_Field), intent(inout) :: output_field_comp1
      type(ESMF_Field), intent(inout) :: output_field_comp2
      integer, intent(in) :: regrid_method
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_Grid) :: input_grid,output_grid
      integer :: input_rank,output_rank
      class(AbstractRegridder), pointer :: regrid_handle

      real(ESMF_KIND_R4), pointer :: input_ptr1_2d(:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr1_2d(:,:)
      real(ESMF_KIND_R4), pointer :: input_ptr1_3d(:,:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr1_3d(:,:,:)

      real(ESMF_KIND_R4), pointer :: input_ptr2_2d(:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr2_2d(:,:)
      real(ESMF_KIND_R4), pointer :: input_ptr2_3d(:,:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr2_3d(:,:,:)

      call ESMF_FieldGet(input_field_comp1,rank=input_rank,grid=input_grid,_RC)
      call ESMF_FieldGet(output_field_comp2,rank=output_rank,grid=output_grid,_RC)
      _ASSERT(input_rank == output_rank,"Something went wrong ranks dont match")
      regrid_handle => new_regridder_manager%make_regridder(input_grid,output_grid,regrid_method,_RC)
      if (input_rank==2) then
         call ESMF_FieldGet(input_field_comp1,0,farrayPtr=input_ptr1_2d,_RC) 
         call ESMF_FieldGet(output_field_comp1,0,farrayPtr=output_ptr1_2d,_RC)
         call ESMF_FieldGet(input_field_comp2,0,farrayPtr=input_ptr2_2d,_RC) 
         call ESMF_FieldGet(output_field_comp2,0,farrayPtr=output_ptr2_2d,_RC)
         call regrid_handle%regrid(input_ptr1_2d,input_ptr2_2d,output_ptr1_2d,output_ptr2_2d,_RC)
      else if (input_rank==3) then
         call ESMF_FieldGet(input_field_comp1,0,farrayPtr=input_ptr1_3d,_RC) 
         call ESMF_FieldGet(output_field_comp1,0,farrayPtr=output_ptr1_3d,_RC)
         call ESMF_FieldGet(input_field_comp2,0,farrayPtr=input_ptr2_3d,_RC) 
         call ESMF_FieldGet(output_field_comp2,0,farrayPtr=output_ptr2_3d,_RC)
         call regrid_handle%regrid(input_ptr1_3d,input_ptr2_3d,output_ptr1_3d,output_ptr2_3d,_RC)
      end if


      _RETURN(_SUCCESS)
   end subroutine
   
   ! todo, we could evaluate the derived fields on the file grid
   subroutine evaluate_derived_fields ( input, output,  rc )
   type(ESMF_State), intent(inout) :: input
   type(ESMF_State), intent(inout) :: output
   integer, optional, intent(out) ::  rc         
                                                 
   integer :: status
   type(ESMF_Field) :: input_field,output_field
   character(len=ESMF_MAXSTR) :: expression
   logical :: is_derived,do_update
   integer :: item_count,i
   character(len=ESMF_MAXSTR), allocatable :: item_names(:)
   type(ESMF_StateItem_Flag), allocatable :: item_types(:) 
   type(ESMF_Info) :: info
   type(ExtDataMask) :: mask

   call ESMF_StateGet(output, itemCount=item_count, _RC)
   allocate(item_names(item_count), STAT=STATUS)
   _VERIFY(STATUS)
   allocate(item_types(item_count), STAT=STATUS)
   _VERIFY(STATUS)
   call ESMF_StateGet(output, ITEMNAMELIST=item_names, ITEMTYPELIST=item_types, _RC)
   do i=1,item_count
      if (item_types(i) /= ESMF_StateItem_Field) cycle
      call ESMF_StateGet(input,item_names(i),input_field,_RC)
      call ESMF_InfoGetFromHost(input_field,info,_RC)
      is_derived = ESMF_InfoIsPresent(info,extdata_expression,_RC)
      if (is_derived) then
         call ESMF_InfoGet(info,extdata_update,value=do_update,_RC)
         if (is_derived) then
            call ESMF_InfoGet(info,extdata_expression,expression,_RC)
            call ESMF_StateGet(output,item_names(i),output_field,_RC)
            if (index(expression,'mask')/=0) then
               mask = ExtDataMask(expression,_RC)
               call mask%evaluate_mask(output,item_names(i),_RC) 
            else
               call MAPL_StateEval(input,trim(expression),output_field,_RC)
            end if
         end if
      end if
   enddo

   _RETURN(_SUCCESS)
   end subroutine evaluate_derived_fields

   subroutine fill_constant_field(input_field,output_field,rc)
      type(ESMF_Field), intent(inout) :: input_field
      type(ESMF_Field), intent(inout) :: output_field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info
      logical :: is_set
      integer :: output_rank
      real(ESMF_KIND_R4), pointer :: output_ptr_2d(:,:)
      real(ESMF_KIND_R4), pointer :: output_ptr_3d(:,:,:)
      real(ESMF_KIND_R4) :: constant_value

      call ESMF_InfoGetFromHost(input_field,info,_RC)
      call ESMF_InfoGet(info,extdata_constant_value,constant_value,_RC)
      is_set = ESMF_InfoIsPresent(info,extdata_set_constant,_RC)
      if (.not.is_set) then
         call ESMF_FieldGet(output_field,rank=output_rank,_RC)
         if (output_rank==2) then
            call ESMF_FieldGet(output_field,0,farrayPtr=output_ptr_2d,_RC)
            output_ptr_2d=constant_value
         else if (output_rank==3) then
            call ESMF_FieldGet(output_field,0,farrayPtr=output_ptr_3d,_RC) 
            output_ptr_3d=constant_value
         end if
         call ESMF_InfoSet(info,extdata_set_constant,.true.,_RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine fill_constant_field

 END MODULE MAPL_ExtDataGridComp_Coupler

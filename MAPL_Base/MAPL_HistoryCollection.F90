#include "MAPL_Generic.h"

module MAPL_HistoryCollectionMod
  use ESMF
  use MAPL_CFIOMod
  implicit none
  
  private
  
  type, public :: HistoryCollection
     character(len=ESMF_MAXSTR)         :: collection
     character(len=ESMF_MAXSTR)         :: filename
     character(len=ESMF_MAXSTR)         :: template
     character(len=ESMF_MAXSTR)         :: format
     character(len=ESMF_MAXSTR)         :: mode
     character(len=ESMF_MAXSTR)         :: descr
     character(len=ESMF_MAXSTR),pointer :: fields (:,:)
     integer                            :: frequency
     integer                            :: acc_interval
     integer                            :: ref_date
     integer                            :: ref_time
     integer                            :: end_date
     integer                            :: end_time
     integer                            :: duration
     type(ESMF_Alarm)                   :: his_alarm ! when to write file
     type(ESMF_Alarm)                   :: seg_alarm ! segment alarm controls when to write to new file
     type(ESMF_Alarm)                   :: mon_alarm
     type(ESMF_Alarm)                   :: end_alarm
     integer,pointer                    :: expSTATE (:)
     integer                            :: unit
     integer                            :: nfield
     type(ESMF_FieldBundle)             :: bundle
     type(MAPL_CFIO)                    :: MCFIO
     real   , pointer                   :: levels(:)     => null()
     integer, pointer                   :: resolution(:) => null()
     real,    pointer                   :: subset(:) => null()
     integer,    pointer                :: chunksize(:) => null()
     integer, pointer                   :: peAve(:)
     integer                            :: verbose
     integer                            :: xyoffset
     logical                            :: disabled
     logical                            :: subVm
     logical                            :: backwards ! Adds support for clock running in reverse direction
     logical                            :: useNewFormat
     real                               :: vscale
     character(len=ESMF_MAXSTR)         :: vunit
     character(len=ESMF_MAXSTR)         :: vvars(2)
     integer                            :: conservative
     integer                            :: voting
     integer                            :: nbits
     integer                            :: deflate 
     integer                            :: slices
     integer                            :: Root
     integer                            :: Psize
     integer                            :: tm
     logical                            :: ForceOffsetZero
     ! Adding Arithemtic Field Rewrite
     character(len=ESMF_MAXSTR),pointer :: tmpfields(:) => null()
     logical, pointer                   :: ReWrite(:) => null()
     integer                            :: nPExtraFields
     character(len=ESMF_MAXSTR),pointer :: PExtraFields(:) => null()
     character(len=ESMF_MAXSTR),pointer :: PExtraGridComp(:) => null() 
     character(len=ESMF_MAXSTR),pointer :: vectorList(:,:) => null() 
     logical, pointer                   :: r8_to_r4(:) => null()
     type(ESMF_FIELD), pointer          :: r8(:) => null()
     type(ESMF_FIELD), pointer          :: r4(:) => null()
     character(len=ESMF_MAXSTR)         :: output_grid_label
     contains
        procedure :: AddGrid
  end type HistoryCollection

  contains

     subroutine AddGrid(this,output_grids,resolution,rc) 
        use MAPL_GridManagerMod
        use MAPL_AbstractGridFactoryMod
        use MAPL_ConfigMod
        use MAPL_GenericMod
        use MAPL_BaseMod
        use MAPL_StringGridMapMod
        class (HistoryCollection), intent(inout) :: this
        integer, intent(in) :: resolution(2)
        type (StringGridMap), intent(inout) :: output_grids
        integer, intent(inout), optional :: rc

        integer :: status
        character(len=ESMF_MAXSTR), parameter :: Iam = "AddGrid" 
        type(ESMF_Config) :: cfg
        integer :: nx,ny,im_world,jm_world
        character(len=ESMF_MAXSTR) :: tlabel
        type(ESMF_Grid) :: output_grid
        type(ESMF_Grid), pointer :: lgrid
        class(AbstractGridFactory), pointer :: factory

        tlabel="NewHistGrid"
        im_world=resolution(1)
        jm_world=resolution(2)

        cfg = MAPL_ConfigCreate(rc=status)
        VERIFY_(status)
        if (resolution(2)==resolution(1)*6) then
           call MAPL_MakeDecomposition(nx,ny,reduceFactor=6,rc=status)
           VERIFY_(status)
        else
           call MAPL_MakeDecomposition(nx,ny,rc=status)
           VERIFY_(status)
        end if
        call MAPL_ConfigSetAttribute(cfg,value=nx, label=trim(tlabel)//".NX:",rc=status)
        VERIFY_(status)
        call MAPL_ConfigSetAttribute(cfg,value=ny, label=trim(tlabel)//".NY:",rc=status)
        VERIFY_(status)
       
        if (resolution(2)==resolution(1)*6) then
          call MAPL_ConfigSetAttribute(cfg,value="Cubed-Sphere", label=trim(tlabel)//".GRID_TYPE:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cfg,value=6, label=trim(tlabel)//".NF:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cfg,value=im_world,label=trim(tlabel)//".IM_WORLD:",rc=status)
          VERIFY_(status)
        else
          call MAPL_ConfigSetAttribute(cfg,value="LatLon", label=trim(tlabel)//".GRID_TYPE:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cfg,value=im_world,label=trim(tlabel)//".IM_WORLD:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cfg,value=jm_world,label=trim(tlabel)//".JM_WORLD:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cfg,value='PC', label=trim(tlabel)//".POLE:",rc=status)
          VERIFY_(status)
          call MAPL_ConfigSetAttribute(cfg,value='DC', label=trim(tlabel)//".DATELINE:",rc=status)
          VERIFY_(status)
        end if
        output_grid = grid_manager%make_grid(cfg,prefix=trim(tlabel)//'.',rc=status)
        VERIFY_(status)
        
        factory => grid_manager%get_factory(output_grid,rc=status)
        VERIFY_(status)
        this%output_grid_label = factory%generate_grid_name()
        lgrid => output_grids%at(trim(this%output_grid_label))
        if (.not.associated(lgrid)) call output_grids%insert(this%output_grid_label,output_grid) 

     end subroutine AddGrid
  
end module MAPL_HistoryCollectionMod

module MAPL_HistoryCollectionVectorMod
  use MAPL_HistoryCollectionMod
  
#define _type type (HistoryCollection)
#define _vector HistoryCollectionVector
#define _iterator HistoryCollectionVectorIterator

#include "templates/vector.inc"
  
end module MAPL_HistoryCollectionVectorMod

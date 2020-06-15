#include "MAPL_Generic.h"

module MAPL_HistoryCollectionMod
  use ESMF
  use MAPL_CFIOMod
  use MAPL_newCFIOMod
  use MAPL_ExceptionHandling
  use MAPL_newCFIOitemVectorMod
  use MAPL_VerticalDataMod
  use MAPL_TimeDataMod
  use HistoryTrajectoryMod
  implicit none
  
  private

  type, public :: FieldSet
     character(len=ESMF_MAXSTR), pointer :: fields(:,:) => null()
     integer :: nfields = 0
  end type FieldSet

  type, public :: HistoryCollection
     character(len=ESMF_MAXSTR)         :: collection
     character(len=ESMF_MAXSTR)         :: filename
     character(len=ESMF_MAXSTR)         :: template
     character(len=ESMF_MAXSTR)         :: format
     character(len=ESMF_MAXSTR)         :: mode
     character(len=ESMF_MAXSTR)         :: descr
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
     type(ESMF_FieldBundle)             :: bundle
     type(MAPL_CFIO)                    :: MCFIO
     type(MAPL_newCFIO)                    :: mNewCFIO
     type(VerticalData) :: vdata
     type(TimeData) :: timeInfo
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
     logical                            :: monthly
     logical                            :: partial = .false.
     ! Adding Arithemtic Field Rewrite
     character(len=ESMF_MAXSTR),pointer :: tmpfields(:) => null()
     logical, pointer                   :: ReWrite(:) => null()
     integer                            :: nPExtraFields
     character(len=ESMF_MAXSTR),pointer :: PExtraFields(:) => null()
     character(len=ESMF_MAXSTR),pointer :: PExtraGridComp(:) => null()
     type (FieldSet), pointer :: field_set
     logical, pointer                   :: r8_to_r4(:) => null()
     type(ESMF_FIELD), pointer          :: r8(:) => null()
     type(ESMF_FIELD), pointer          :: r4(:) => null()
     character(len=ESMF_MAXSTR)         :: output_grid_label
     type(newCFIOItemVector)            :: items
     character(len=ESMF_MAXSTR)         :: currentFile
     character(len=ESMF_MAXPATHLEN)     :: trackFile
     logical                            :: splitField
     logical                            :: regex
     logical                            :: timeseries_output = .false.
     logical                            :: recycle_track = .false.
     type(HistoryTrajectory)            :: trajectory
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
        _VERIFY(status)
        if (resolution(2)==resolution(1)*6) then
           call MAPL_MakeDecomposition(nx,ny,reduceFactor=6,rc=status)
           _VERIFY(status)
        else
           call MAPL_MakeDecomposition(nx,ny,rc=status)
           _VERIFY(status)
        end if
        call MAPL_ConfigSetAttribute(cfg,value=nx, label=trim(tlabel)//".NX:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cfg,value=ny, label=trim(tlabel)//".NY:",rc=status)
        _VERIFY(status)
       
        if (resolution(2)==resolution(1)*6) then
          call MAPL_ConfigSetAttribute(cfg,value="Cubed-Sphere", label=trim(tlabel)//".GRID_TYPE:",rc=status)
          _VERIFY(status)
          call MAPL_ConfigSetAttribute(cfg,value=6, label=trim(tlabel)//".NF:",rc=status)
          _VERIFY(status)
          call MAPL_ConfigSetAttribute(cfg,value=im_world,label=trim(tlabel)//".IM_WORLD:",rc=status)
          _VERIFY(status)
        else
          call MAPL_ConfigSetAttribute(cfg,value="LatLon", label=trim(tlabel)//".GRID_TYPE:",rc=status)
          _VERIFY(status)
          call MAPL_ConfigSetAttribute(cfg,value=im_world,label=trim(tlabel)//".IM_WORLD:",rc=status)
          _VERIFY(status)
          call MAPL_ConfigSetAttribute(cfg,value=jm_world,label=trim(tlabel)//".JM_WORLD:",rc=status)
          _VERIFY(status)
          call MAPL_ConfigSetAttribute(cfg,value='PC', label=trim(tlabel)//".POLE:",rc=status)
          _VERIFY(status)
          call MAPL_ConfigSetAttribute(cfg,value='DC', label=trim(tlabel)//".DATELINE:",rc=status)
          _VERIFY(status)
        end if
        output_grid = grid_manager%make_grid(cfg,prefix=trim(tlabel)//'.',rc=status)
        _VERIFY(status)
        
        factory => grid_manager%get_factory(output_grid,rc=status)
        _VERIFY(status)
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

#undef _iterator
#undef _vector
#undef _type
  
end module MAPL_HistoryCollectionVectorMod

module MAPL_StringFieldSetMapMod
  use MAPL_HistoryCollectionMod

#include "types/key_deferredLengthString.inc"
#define _value type (FieldSet)
#define _map StringFieldSetMap
#define _iterator StringFieldSetMapIterator

#include "templates/map.inc"


#undef _iterator
#undef _map
#undef _value
#undef _key
  
end module MAPL_StringFieldSetMapMod

#include "MAPL_Generic.h"

module MAPL_HistoryCollectionMod
  use ESMF
  use MAPL_CFIOMod
  use MAPL_GriddedIOMod
  use MAPL_ExceptionHandling
  use MAPL_GriddedIOitemVectorMod
  use MAPL_VerticalDataMod
  use MAPL_TimeDataMod
  use HistoryTrajectoryMod
  use MaskSamplerMod
  use StationSamplerMod
  use gFTL2_StringStringMap
  use MAPL_EpochSwathMod
  implicit none

  private

  type, public :: FieldSet
     character(len=ESMF_MAXSTR), pointer :: fields(:,:) => null()
     integer :: nfields = 0
  end type FieldSet

  type, public :: HistoryCollectionGlobalAttributes
     character(len=ESMF_MAXSTR) :: filename
     character(len=ESMF_MAXSTR) :: descr
     character(len=ESMF_MAXSTR) :: comment
     character(len=ESMF_MAXSTR) :: contact
     character(len=ESMF_MAXSTR) :: conventions
     character(len=ESMF_MAXSTR) :: institution
     character(len=ESMF_MAXSTR) :: references
     character(len=ESMF_MAXSTR) :: source
     contains
        procedure :: define_collection_attributes
  end type HistoryCollectionGlobalAttributes

  type, public :: HistoryCollection
     character(len=ESMF_MAXSTR)         :: collection
     character(len=ESMF_MAXSTR)         :: filename
     character(len=ESMF_MAXSTR)         :: template
     character(len=ESMF_MAXSTR)         :: format
     character(len=ESMF_MAXSTR)         :: mode
     integer                            :: frequency
     integer                            :: acc_interval
     integer                            :: acc_ref_time
     integer                            :: acc_offset
     integer                            :: ref_date
     integer                            :: ref_time
     integer                            :: start_date
     integer                            :: start_time
     integer                            :: end_date
     integer                            :: end_time
     integer                            :: duration
     type(ESMF_Alarm)                   :: his_alarm ! when to write file
     type(ESMF_Alarm)                   :: seg_alarm ! segment alarm controls when to write to new file
     type(ESMF_Alarm)                   :: mon_alarm
     type(ESMF_Alarm)                   :: start_alarm
     type(ESMF_Alarm)                   :: end_alarm
     integer,pointer                    :: expSTATE (:)
     integer                            :: unit
     type(ESMF_FieldBundle)             :: bundle
     type(sampler)                      :: xsampler
     type(MAPL_CFIO)                    :: MCFIO
     type(MAPL_GriddedIO)               :: mGriddedIO
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
     logical                            :: skipWriting
     logical                            :: subVm
     logical                            :: backwards ! Adds support for clock running in reverse direction
     logical                            :: useNewFormat
     real                               :: vscale
     character(len=ESMF_MAXSTR)         :: vunit
     character(len=ESMF_MAXSTR)         :: vvars(2)
     integer                            :: regrid_method
     integer                            :: voting
     integer                            :: nbits_to_keep
     integer                            :: deflate
     character(len=ESMF_MAXSTR)         :: quantize_algorithm_string
     integer                            :: quantize_algorithm
     integer                            :: quantize_level
     integer                            :: zstandard_level
     integer                            :: slices
     integer                            :: Root
     integer                            :: Psize
     integer                            :: tm
     logical                            :: ForceOffsetZero
     logical                            :: timestampStart
     logical                            :: monthly
     logical                            :: partial = .false.
     ! Adding Arithemtic Field Rewrite
     character(len=ESMF_MAXSTR),pointer :: tmpfields(:) => null()
     logical, pointer                   :: ReWrite(:) => null()
     integer                            :: nPExtraFields
     character(len=ESMF_MAXSTR),pointer :: PExtraFields(:) => null()
     character(len=ESMF_MAXSTR),pointer :: PExtraGridComp(:) => null()
     type (FieldSet), pointer :: field_set
     logical, allocatable               :: r8_to_r4(:)
     type(ESMF_FIELD), allocatable      :: r8(:)
     type(ESMF_FIELD), allocatable      :: r4(:)
     character(len=ESMF_MAXSTR)         :: output_grid_label
     type(GriddedIOItemVector)          :: items
     character(len=ESMF_MAXSTR)         :: currentFile
     character(len=ESMF_MAXPATHLEN)     :: stationIdFile
     integer                            :: stationSkipLine
     logical                            :: splitField
     logical                            :: regex
     logical                            :: timeseries_output = .false.
     logical                            :: recycle_track = .false.
     type(HistoryTrajectory)            :: trajectory
     type(MaskSampler)                  :: mask_sampler
     type(StationSampler)               :: station_sampler
     character(len=ESMF_MAXSTR)         :: sampler_spec = ""
     character(len=ESMF_MAXSTR)         :: positive
     logical                            :: extrap_below_surf = .false.
     type(HistoryCollectionGlobalAttributes) :: global_atts
     contains
        procedure :: AddGrid
  end type HistoryCollection

  contains

     function define_collection_attributes(this,rc) result(global_attributes)
        class(HistoryCollectionGlobalAttributes), intent(inout) :: this
        integer, optional, intent(out) :: rc

        type(StringStringMap) :: global_attributes

        call global_attributes%insert("Title",trim(this%descr))
        call global_attributes%insert("History","File written by MAPL_PFIO")
        call global_attributes%insert("Source",trim(this%source))
        call global_attributes%insert("Contact",trim(this%contact))
        call global_attributes%insert("Conventions",trim(this%conventions))
        call global_attributes%insert("Institution",trim(this%institution))
        call global_attributes%insert("References",trim(this%references))
        call global_attributes%insert("Filename",trim(this%filename))
        call global_attributes%insert("Comment",trim(this%comment))

        _RETURN(_SUCCESS)
     end function define_collection_attributes

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
        character(len=*), parameter :: Iam = "AddGrid"
        type(ESMF_Config) :: cfg
        integer :: nx,ny,im_world,jm_world
        character(len=ESMF_MAXSTR) :: tlabel
        type(ESMF_Grid) :: output_grid
        type(ESMF_Grid), pointer :: lgrid
        class(AbstractGridFactory), pointer :: factory

        tlabel="NewHistGrid"
        im_world=resolution(1)
        jm_world=resolution(2)

        cfg = MAPL_ConfigCreate(_RC)
        if (resolution(2)==resolution(1)*6) then
           call MAPL_MakeDecomposition(nx,ny,reduceFactor=6,_RC)
        else
           call MAPL_MakeDecomposition(nx,ny,_RC)
        end if
        call MAPL_ConfigSetAttribute(cfg,value=nx, label=trim(tlabel)//".NX:",_RC)
        call MAPL_ConfigSetAttribute(cfg,value=ny, label=trim(tlabel)//".NY:",_RC)

        if (resolution(2)==resolution(1)*6) then
          call MAPL_ConfigSetAttribute(cfg,value="Cubed-Sphere", label=trim(tlabel)//".GRID_TYPE:",_RC)
          call MAPL_ConfigSetAttribute(cfg,value=6, label=trim(tlabel)//".NF:",_RC)
          call MAPL_ConfigSetAttribute(cfg,value=im_world,label=trim(tlabel)//".IM_WORLD:",_RC)
        else
          call MAPL_ConfigSetAttribute(cfg,value="LatLon", label=trim(tlabel)//".GRID_TYPE:",_RC)
          call MAPL_ConfigSetAttribute(cfg,value=im_world,label=trim(tlabel)//".IM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cfg,value=jm_world,label=trim(tlabel)//".JM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cfg,value='PC', label=trim(tlabel)//".POLE:",_RC)
          call MAPL_ConfigSetAttribute(cfg,value='DC', label=trim(tlabel)//".DATELINE:",_RC)
        end if
        output_grid = grid_manager%make_grid(cfg,prefix=trim(tlabel)//'.',_RC)

        factory => grid_manager%get_factory(output_grid,_RC)
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

#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

submodule (NonGridIOMod) NonGridIOMod_implement
  use ESMF
  use MAPL_ErrorHandlingMod
  use MAPL_GriddedIOItemMod
  use MAPL_GriddedIOItemVectorMod
  use MAPL_FileMetadataUtilsMod
  use pFIO_VariableMod   !! , only : variable, PFIO_REAL32
  use pFIO_ConstantsMod
  use MAPL_BaseMod, only: MAPL_UNDEF
  implicit none
contains

!  module procedure new_nongridio(bundle, rc) result(ngio)
!    type(nongridio) :: ngio
!    type(ESMF_FieldBundle), intent(in)   :: bundle
!    integer, optional, intent(out) :: rc

  module procedure new_nongridio
    ngio%count = 0
    ngio%x = 0.0
    ngio%pet_select = 0
    ngio%index_name_x = "lon,lat"

    ngio%input_bundle = bundle
    ngio%items = items
    allocate(ngio%metadata)

    _RETURN(_SUCCESS)
    
  end procedure new_nongridio

  module procedure restart
    if ( allocated (this%metadata) ) then
       deallocate ( this%metadata )
    end if
    allocate (this%metadata )
    this%count = 0
    this%x = 0.0
    this%pet_select = 0
    this%index_name_x = "lon,lat"

    _RETURN(_SUCCESS)
  end procedure restart

  
  
!  module procedure create_metadata(this,vname,rc)
!    class(nongridio), intent(inout) :: this
!    character(len=*), intent(in)            :: vname
!    integer, optional, intent(out)          :: rc

  module procedure add_metadata
    type(ESMF_VM) :: vm
    integer :: mypet, petcount, mpic
    integer :: status
    real :: x
    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    
    this%count = this%count + 1
    print*, 'this%count', this%count
    call ESMF_VMGetCurrent(vm,_RC)
    call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, _RC)
    print*, 'mpic, mypet=', mpic, mypet

    if ( mypet /= 0 )  then
       rc=0
       return
    end if


    !
    !-- only root proceeds to here
    !

    call random_number(x)
    this%pet_select = petCount * x
    print*, 'x, this%pet_select=', x, this%pet_select

    ! __ s1. add lon/lat/time
    
    ! push varible names down to each obs(k); see create_metadata_variable
    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       print*, 'list item%xname= ', trim(item%xname)

       if (item%itemType == ItemTypeScalar) then
          call this%create_metadata_variable(item%xname,_RC)
       else if (item%itemType == ItemTypeVector) then
          call this%create_metadata_variable(item%xname,_RC)
          call this%create_metadata_variable(item%yname,_RC)
       end if
       call iter%next()
    enddo
    
    _RETURN(_SUCCESS)
    
  end procedure add_metadata    


  module procedure append_file
    type(ESMF_VM) :: vm
    integer :: mypet, petcount, mpic
    integer :: status
    
    _RETURN(_SUCCESS)
    
  end procedure append_file
  


      module procedure create_metadata_variable
        type(ESMF_Field) :: field
        type(variable) :: v
        logical :: is_present
        integer :: field_rank, status
        character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
        integer :: k, ig

        call ESMF_FieldBundleGet(this%input_bundle,vname,field=field,_RC)
        call ESMF_FieldGet(field,name=var_name,rank=field_rank,_RC)
        call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=is_present,_RC)
        if ( is_present ) then
           call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=long_name, _RC)
        else
           long_name = var_name
        endif
        call ESMF_AttributeGet(field,name="UNITS",isPresent=is_present,_RC)
        if ( is_present ) then
           call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, _RC)
        else
           units = 'unknown'
        endif
        if (field_rank==2) then
           vdims = this%index_name_x
        else if (field_rank==3) then
           vdims = trim(this%index_name_x)//",lev"
        end if
        v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(long_name))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))

         _RETURN(_SUCCESS)
      end procedure create_metadata_variable
 
  
end submodule NonGridIOMod_implement

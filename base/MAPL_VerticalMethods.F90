#include "MAPL_Generic.h"

module MAPL_VerticalDataMod
  use ESMF
  use MAPL_BaseMod
  use MAPL_Profiler
  use pFIO
  use MAPL_AbstractRegridderMod
  use MAPL_ExceptionHandling
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none

  private

  public :: VERTICAL_METHOD_NONE
  public :: VERTICAL_METHOD_SELECT
  public :: VERTICAL_METHOD_ETA2LEV
  public :: VERTICAL_METHOD_FLIP
  enum, bind(c)
     enumerator :: VERTICAL_METHOD_NONE = -1
     enumerator :: VERTICAL_METHOD_SELECT
     enumerator :: VERTICAL_METHOD_ETA2LEV
     enumerator :: VERTICAL_METHOD_FLIP
  end enum

  type, public :: verticalData
     character(len=:), allocatable :: vunit
     character(len=:), allocatable :: func
     character(len=:), allocatable :: vvar
     character(len=:), allocatable :: positive
     character(len=:), allocatable :: long_name
     character(len=:), allocatable :: standard_name
     character(len=:), allocatable :: vcoord
     real :: vscale
     real :: pow=0.0
     real, allocatable :: levs(:)
     real, allocatable :: interp_levels(:)
     real, allocatable :: scaled_levels(:)
     real, allocatable :: surface_level(:,:)
     real, allocatable :: ple3d(:,:,:)
     real, allocatable :: pl3d(:,:,:)
     integer :: lm = 0
     integer :: regrid_type
     type(ESMF_Field) :: interp_var
     logical :: ascending
     integer              :: nedge       ! number of edge
     integer, allocatable :: ks(:,:,:)
     integer, allocatable :: ks_e(:,:,:) ! edge
     real,    allocatable :: weight(:,:,:)
     real,    allocatable :: weight_e(:,:,:) !edge
     contains
        procedure :: append_vertical_metadata
        procedure :: get_interpolating_variable
        procedure :: regrid_eta_to_pressure
        procedure :: regrid_select_level
        procedure :: skip_var
        procedure :: correct_topo
        procedure :: setup_eta_to_pressure
        procedure :: flip_levels
  end type verticalData

  interface verticalData
     module procedure newVerticalData
  end interface

  contains

     function newVerticalData(levels,vcoord,vscale,vunit,positive,long_name,standard_name,force_no_regrid,rc) result(vdata)
        type(VerticalData) :: vData
        real, pointer, intent(in), optional :: levels(:)
        real, intent(in), optional :: vscale
        character(len=*), optional, intent(in) :: vcoord
        character(len=*), optional, intent(in) :: vunit
        character(len=*), optional, intent(in) :: positive
        character(len=*), optional, intent(in) :: long_name
        character(len=*), optional, intent(in) :: standard_name
        logical, optional, intent(in) :: force_no_regrid
        integer, optional, intent(Out) :: rc

        logical :: local_force_no_regrid

        if (present(positive)) then
           _ASSERT(trim(positive)=='up'.or.trim(positive)=='down',trim(positive)//" not allowed for positive argument")
           vdata%positive=trim(positive)
        else
           vdata%positive='down'
        end if

        if (present(long_name)) then
           vdata%long_name = long_name
        else
           vdata%long_name = 'vertical level'
        end if
        if (present(standard_name)) then
           vdata%standard_name = standard_name
        else
           vdata%standard_name = 'model_layers'
        end if
        if (present(vcoord)) then
           vdata%vcoord = vcoord
        else
           vdata%vcoord = 'eta'
        endif

        if (present(vunit)) then
           vdata%vunit=vunit
         else
           vdata%vunit="layer"
        end if

        if (present(force_no_regrid)) then
           local_force_no_regrid = force_no_regrid
        else
           local_force_no_regrid = .false.
        end if

        if (.not.present(levels)) then
           if (trim(vdata%positive)=='down') then
              vdata%regrid_type = VERTICAL_METHOD_NONE
           else
              vdata%regrid_type = VERTICAL_METHOD_FLIP
           end if
        else
           allocate(vData%levs,source=levels)
           if (local_force_no_regrid) then
              if (trim(vdata%positive)=='down') then
                 vdata%regrid_type = VERTICAL_METHOD_NONE
              else
                 vdata%regrid_type = VERTICAL_METHOD_FLIP
              end if
              _RETURN(_SUCCESS)
           end if
           allocate(vData%scaled_levels,source=levels)
           if (present(vscale)) then
              vdata%vscale=vscale
              vData%scaled_levels = vData%scaled_levels*vdata%vscale
           end if
           if (present(vcoord)) then
              allocate(vData%interp_levels(size(vData%levs)))
              vdata%regrid_type = VERTICAL_METHOD_ETA2LEV
              vdata%VVAR = adjustl(vcoord)
              vdata%Func = vdata%Vvar(1:3)
              if    (vdata%Func=='log') then
                 vdata%Vvar = adjustl(vdata%Vvar(index(vdata%Vvar,'(')+1:index(vdata%Vvar,')')-1))
                 vdata%interp_levels = log(vdata%scaled_levels)
              elseif(vdata%Func=='pow') then
                 read( vdata%Vvar(index(vdata%Vvar,',')+1:index(vdata%Vvar,')')-1) , *) vdata%pow
                 vdata%Vvar = adjustl(vdata%Vvar(index(vdata%Vvar,'(')+1:index(vdata%Vvar,',')-1))
                 vdata%interp_levels =  (vdata%levs*vdata%scaled_levels)**vdata%pow
              else
                 vdata%interp_levels = vdata%scaled_levels
              endif
           else
              vdata%regrid_type = VERTICAL_METHOD_SELECT
           end if
        end if
     end function newVerticalData

     function skip_var(this,field,rc) result(skip)
        logical :: skip
        class(verticalData), intent(inout) :: this
        type(ESMF_Field), intent(inout) :: field
        integer, optional, intent(out) :: rc

        integer :: status
        character(len=ESMF_MAXSTR) :: name

        call ESMF_FieldGet(field,name=name,_RC)
        skip = trim(name)==trim(this%vvar)
     end function skip_var

     subroutine setup_eta_to_pressure(this,regrid_handle,output_grid,rc)
        class(verticaldata), intent(inout) :: this
        class(abstractRegridder), optional, intent(inout) :: regrid_handle
        type(ESMF_Grid), optional, intent(inout) :: output_grid
        integer, optional, intent(out) :: rc

        integer :: status
        real,         allocatable  :: ptrx(:,:,:)
        real, pointer              :: ptr3(:,:,:)
        real,         allocatable  :: orig_surface_level(:,:)
        integer ::  counts(3)

        if (allocated(this%ple3d)) deallocate(this%ple3d)
        if (allocated(this%pl3d)) deallocate(this%pl3d)
        call ESMF_FieldGet(this%interp_var,localde=0,farrayptr=ptr3,_RC)

        allocate(orig_surface_level(size(ptr3,1),size(ptr3,2)),stat=status)
        _VERIFY(status)
! the ptr3 interpolating variable is a zero-based (0-lm) edge variable
!---------------------------------------------------------------------
       if(lbound(ptr3,3)==0) then
          allocate( this%ple3d(size(ptr3,1),size(ptr3,2),size(ptr3,3)  ), stat=status)
          _VERIFY(status)
          allocate(  this%pl3d(size(ptr3,1),size(ptr3,2),size(ptr3,3)-1), stat=status)
          _VERIFY(status)

          if    (this%func=='log') then
             this%ple3d = log(ptr3)
             this%pl3d  = log( 0.5*(ptr3(:,:,1:)+ptr3(:,:,0:ubound(ptr3,3)-1)) )
          elseif(this%func=='pow') then
             this%ple3d = ptr3**this%pow
             this%pl3d  =    ( 0.5*(ptr3(:,:,1:)+ptr3(:,:,0:ubound(ptr3,3)-1)) )**this%pow
          else
             this%ple3d = ptr3
             this%pl3d  =    ( 0.5*(ptr3(:,:,1:)+ptr3(:,:,0:ubound(ptr3,3)-1)) )
          end if
          orig_surface_level = ptr3(:,:,ubound(ptr3,3))
          this%ascending = (ptr3(1,1,0)<ptr3(1,1,1))
       else

! the ptr3 interpolating variable is a (1-lm) mid-layer variable
!---------------------------------------------------------------
          allocate(  ptrx(size(ptr3,1),size(ptr3,2),0:size(ptr3,3)  ), stat=status)
          _VERIFY(status)
          allocate( this%ple3d(size(ptr3,1),size(ptr3,2),0:size(ptr3,3)  ), stat=status)
          _VERIFY(status)
          allocate(  this%pl3d(size(ptr3,1),size(ptr3,2),  size(ptr3,3)  ), stat=status)
          _VERIFY(status)

          ptrx(:,:,0               ) = 0.5*( 3* ptr3(:,:,1)             -ptr3(:,:,2)                )
          ptrx(:,:,1:size(ptr3,3)-1) = 0.5*(    ptr3(:,:,2:size(ptr3,3))+ptr3(:,:,1:size(ptr3,3)-1) )
          ptrx(:,:,  size(ptr3,3)  ) = 0.5*( 3* ptr3(:,:,  size(ptr3,3))-ptr3(:,:,  size(ptr3,3)-1) )

          if    (this%func=='log') then
             this%ple3d = log(ptrx)
             this%pl3d  = log( 0.5*(ptrx(:,:,1:)+ptrx(:,:,0:ubound(ptrx,3)-1)) )
          elseif(this%func=='pow') then
             this%ple3d = ptrx**this%pow
             this%pl3d  =    ( 0.5*(ptrx(:,:,1:)+ptrx(:,:,0:ubound(ptrx,3)-1)) )**this%pow
          else
             this%ple3d = ptrx
             this%pl3d  =    ( 0.5*(ptrx(:,:,1:)+ptrx(:,:,0:ubound(ptrx,3)-1)) )
          end if

          this%ascending = (ptrx(1,1,0)<ptrx(1,1,1))
          orig_surface_level = ptrx(:,:,ubound(ptrx,3))
          deallocate(ptrx)
       end if
       if (present(output_grid)) then
          _ASSERT(present(regrid_handle),"Must provide regridding handle")
          call MAPL_GridGet(output_grid,localCellCountPerDim=counts,_RC)
          if (.not.allocated(this%surface_level)) then
              allocate(this%surface_level(counts(1),counts(2)),stat=status)
             _VERIFY(status)
          end if
       end if
       if (present(regrid_handle)) then
          call regrid_handle%regrid(orig_surface_level,this%surface_level,_RC)
       end if
       deallocate(orig_surface_level)

       call init_indices(_RC)

       _RETURN(_SUCCESS)

       contains
          ! initialize for pl3d ( not ple3d)
          subroutine init_indices(rc)
            integer, optional, intent(inout) :: rc
            integer :: status
            integer :: k, lev, km, D1, D2, levo, km_e
            integer :: flip_sign, i,j
            real, allocatable :: pb(:,:), pt(:,:), ple3d(:,:,:)
            real :: pp

            D1   = size(this%pl3d,1)
            D2   = size(this%pl3d,2)
            km   = size(this%pl3d,3)
            levo = size(this%interp_levels)
            flip_sign = 1
            if( .not. this%ascending ) flip_sign = -1

            ! for cell values
            if(allocated(this%ks))  deallocate(this%ks)
            if(allocated(this%weight)) deallocate(this%weight)
            allocate(this%ks(D1,D2,levo),source  = -1)
            allocate(this%weight(D1,D2,levo),source = 0.0)

            do lev =1, levo
               pp = flip_sign*this%interp_levels(lev)
               pb = flip_sign*this%pl3d(:,:,km)
               do k = km-1, 1,-1 ! levels of input
                 if(all(pb<pp)) exit
                 pt = flip_sign*this%pl3d(:,:,k)
                 where (pp>pt .and. pp<=pb)
                    this%ks(:,:,lev) = k
                    this%weight(:,:,lev) = (pb-pp)/(pb-pt)
                 end where
                 pb = pt
               enddo
            enddo
            deallocate(this%pl3d) ! not needed any more. release the memory

            ! for edge values
            if(allocated(this%ks_e))  deallocate(this%ks_e)
            if(allocated(this%weight_e)) deallocate(this%weight_e)
            allocate(this%ks_e(D1,D2,levo),source  = -1)
            allocate(this%weight_e(D1,D2,levo),source = 0.0)
            km_e = size(this%ple3d,3)
            this%nedge = km_e
            allocate(ple3d(D1,D2,km_e))
            ple3d = this%ple3d 
            do lev =1, levo
               pp = flip_sign*this%interp_levels(lev)
               pb = flip_sign*ple3d(:,:,km_e)
               do k = km_e-1, 1,-1 ! levels of input
                 if(all(pb<pp)) exit
                 pt = flip_sign*ple3d(:,:,k)
                 where (pp>pt .and. pp<=pb)
                    this%ks_e(:,:,lev) = k
                    this%weight_e(:,:,lev) = (pb-pp)/(pb-pt)
                 end where
                 pb = pt
               enddo
            enddo
            deallocate(this%ple3d) ! not needed any more. release the memory

            _RETURN(_SUCCESS)
          end subroutine

     end subroutine setup_eta_to_pressure

     subroutine regrid_eta_to_pressure(this,ptrin,ptrout,rc)
        class(verticaldata), target, intent(inout) :: this
        real, intent(inout) :: ptrin(:,:,:)
        real, intent(inout) :: ptrout(:,:,:)
        integer, optional, intent(out) :: rc
        real :: weight

        integer :: status
        integer :: i,j,k,lev,levo, D1,D2, km
        integer, pointer :: ks_(:,:,:)
        real, pointer    :: weights_(:,:,:)

        km = size(ptrin,3)
        if (km == this%nedge -1) then
           ks_ => this%ks
           weights_ => this%weight
        else
           ks_ => this%ks_e
           weights_ => this%weight_e
        endif

        D1   = size(ks_,1)
        D2   = size(ks_,2)
        levo = size(ptrout,3)
        ptrout = MAPL_UNDEF
        do lev = 1, levo
          do j = 1, D2
            do i = 1, D1
               k = ks_(i,j,lev)
               if (k == -1) cycle
               !if (k == km) then
               !   ptrout(i,j,lev) = ptrin(i,j,k)
               !   cycle
               !endif
               weight = weights_(i,j,lev)
               if (ptrin(i,j,k)   == MAPL_UNDEF) then
                  ptrout(i,j,lev) = ptrin(i,j,k+1)
                  cycle
               endif
               if (ptrin(i,j,k+1) == MAPL_UNDEF) then
                  ptrout(i,j,lev) = ptrin(i,j,k)
                  cycle
               endif
               ptrout(i,j,lev) = ptrin(i,j,k)*weight + ptrin(i,j,k+1)*(1.0-weight)
            enddo
          enddo
        enddo
        _RETURN(_SUCCESS)

     end subroutine regrid_eta_to_pressure

     subroutine flip_levels(this,ptrin,ptrout,rc)
        class(verticaldata), intent(inout) :: this
        real, intent(inout) :: ptrin(:,:,:)
        real, intent(inout) :: ptrout(:,:,:)
        integer, optional, intent(out) :: rc

        integer :: km

        _ASSERT(all(shape(ptrin)==shape(ptrout)),"array must match shape to flip")

        km = size(ptrin,3)

        ptrout(:,:,1:km)=ptrin(:,:,km:1:-1)
        _RETURN(_SUCCESS)

        _UNUSED_DUMMY(this)

     end subroutine flip_levels

     subroutine correct_topo(this,field,rc)
        class(verticalData), intent(inout) :: this
        type(ESMF_Field), intent(inout) :: field
        integer, optional, intent(out) :: rc

        integer :: rank,k,status
        real, pointer :: ptr(:,:,:)
        type(ESMF_Grid) :: grid
        logical :: has_de

        _ASSERT(allocated(this%surface_level),"class not setup to do topography correction")
        if (this%regrid_type == VERTICAL_METHOD_ETA2LEV) then
           call ESMF_FieldGet(field,grid=grid,_RC)
           has_de = MAPL_GridHasDE(grid,_RC)
           if (has_de) then
              call ESMF_FieldGet(field,rank=rank,_RC)
              if (rank==3) then
                 call ESMF_FieldGet(field,0,farrayptr=ptr,_RC)
                 do k=1,size(ptr,3)
                    if (this%ascending) then
                       where(this%surface_level<this%scaled_levels(k)) ptr(:,:,k)=MAPL_UNDEF
                    else
                       where(this%surface_level>this%scaled_levels(k)) ptr(:,:,k)=MAPL_UNDEF
                    end if
                 end do
              end if
           end if
        end if
        _RETURN(_SUCCESS)

     end subroutine correct_topo

     subroutine regrid_select_level(this,ptrIn,PtrOut,rc)
        class(verticalData), intent(inout) :: this
        real, intent(inout) :: ptrIn(:,:,:)
        real, intent(inout) :: ptrOut(:,:,:)
        integer, optional, intent(out) :: rc

        integer :: i

        do i=1,size(this%levs)
           ptrOut(:,:,i)=ptrIn(:,:,nint(this%levs(i)))
        enddo
        _RETURN(ESMF_SUCCESS)

     end subroutine regrid_select_level

     subroutine get_interpolating_variable(this,bundle,rc)
        class(verticalData), intent(inout) :: this
        type(ESMF_FieldBundle), intent(inout) :: bundle
        integer, optional, intent(out) :: rc

        integer :: status

        call ESMF_FieldBundleGet(bundle,fieldName=trim(this%vvar),field=this%interp_var,_RC)

     end subroutine get_interpolating_variable


     subroutine append_vertical_metadata(this,metadata,bundle,rc)
        class (verticalData), intent(inout) :: this
        type(FileMetaData), intent(inout) :: metadata
        type(ESMF_FieldBundle), intent(inout) :: bundle
        integer, optional, intent(out) :: rc

        integer :: lm,i,NumVars,fieldRank,vlast,vloc,vlb
        integer, allocatable :: VarDims(:), location(:)
        type(ESMF_Field) :: field
        real, pointer :: ptr3d(:,:,:)
        logical :: haveVert

        logical, allocatable        :: HasUngrid(:)
        character(len=ESMF_MAXSTR), allocatable :: ungridded_units(:)
        character(len=ESMF_MAXSTR), allocatable :: ungridded_names(:)
        character(len=ESMF_MAXSTR)  :: ungridded_unit, ungridded_name
        integer                     :: ungrdsize
        real, allocatable           :: ungridded_coord(:)
        real, allocatable           :: ungridded_coords(:,:)
        logical                     :: unGrdNameCheck, unGrdUnitCheck, unGrdCoordCheck, have_ungrd, found_mixed_ce

        integer :: status
        type(Variable) :: v
        logical :: isPresent
        character(len=4) :: positive

        ! loop over variables in file
        call ESMF_FieldBundleGet(bundle,fieldCount=NumVars,_RC)
        allocate(VarDims(numVars),location(numVars))

        allocate(hasUngrid(NumVars))
        hasUngrid=.false.
        allocate(ungridded_names(NumVars), _STAT)
        ungridded_names=""
        allocate(ungridded_units(NumVars), _STAT)
        ungridded_units=""

        do i=1,numVars
           call ESMF_FieldBundleGet(bundle,i,field,_RC)
           positive = 'down'
           call ESMF_AttributeGet(field,NAME="POSITIVE",isPresent=isPresent,_RC)
           if (isPresent) then
              call ESMF_AttributeGet(field,name="POSITIVE", value=positive, _RC)
           end if
           if (i .eq. 1) this%positive=positive
           if (i .gt. 1) then
              _ASSERT(this%positive==positive,"Fields have mistmatched positive attributes")
              this%positive=positive
           end if
           call ESMF_FieldGet(field,dimCount=FieldRank,_RC)
           if (fieldRank==2) then
              varDims(i)=0
           else if (fieldRank==3) then
              call ESMF_AttributeGet(field,name="VLOCATION", value=location(i),_RC)
              call ESMF_FieldGet(field,farrayPtr=ptr3d,_RC)
              varDims(i)=size(ptr3d,3)
              if (location(i) == MAPL_VLocationNone) then
                 hasUngrid(I) = .true.
                 call ESMF_AttributeGet(field,NAME="UNGRIDDED_UNIT",value=ungridded_unit,_RC)
                 call ESMF_AttributeGet(field,NAME="UNGRIDDED_NAME",value=ungridded_name,_RC)
                 ungridded_names(i) = ungridded_name
                 ungridded_units(i) = ungridded_unit
                 call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",isPresent=isPresent,_RC)
                 if (isPresent) then
                    call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",itemcount=ungrdsize,_RC)
                    if (ungrdsize/=0) then
                       _ASSERT(varDims(i)==ungrdsize,"ungridded size does not match variable")
                       if (.not.allocated(ungridded_coord)) allocate(ungridded_coord(ungrdsize),stat=status)
                       if (.not.allocated(ungridded_coords)) allocate(ungridded_coords(NumVars,ungrdsize),stat=status)
                       _VERIFY(STATUS)
                       call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",valuelist=ungridded_coord,_RC)
                       ungridded_coords(i,:) = ungridded_coord
                    end if
                 end if
              end if
           end if
        end do

       have_ungrd = any(hasUngrid)
       if (any(hasUngrid)) then
          do i=1,NumVars
             if (hasUngrid(i)) then
                unGrdUnitCheck = ungridded_units(i) /= ungridded_unit
                unGrdNameCheck = ungridded_names(i) /= ungridded_name
                if ( allocated(ungridded_coords) .and. allocated(ungridded_coord) ) then
                   unGrdCoordCheck = any(ungridded_coords(i,:) /= ungridded_coord)
                else
                   unGrdCoordCheck = .false.
                end if
                if ( unGrdUnitCheck .or. unGrdNameCheck .or. unGrdCoordCheck) then
                   _FAIL('Ungridded attributes for variables in collection do not match')
                end if
             end if
          end do
       end if

        found_mixed_ce=.false.
        lm=1
        haveVert = any(varDims/=0)
        if (haveVert) then
           vlast=0
           do i=1,numVars
              if (varDims(i)/=0) then
                 if (vlast/=0) then
                    if (vlast /= varDims(i)) then
                       found_mixed_ce=.true.
                    end if
                 else
                    vlast=varDims(i)
                    vloc=location(i)
                 end if
              end if
           end do
           lm=vlast
           if (vloc == MAPL_VLocationCenter) then
              vlb = 1
           else if (vloc == MAPL_VlocationEdge) then
              vlb = 0
           else
              vlb = 1
           end if
        end if

        if (this%regrid_type == VERTICAL_METHOD_ETA2LEV) then
           lm = size(this%levs)
           vlb = 1
        end if
        if (this%regrid_type == VERTICAL_METHOD_SELECT) then
           if (lm == size(this%levs)) then
              this%regrid_type = VERTICAL_METHOD_NONE
           else
              lm = size(this%levs)
              vlb=1
           end if
        end if
        if (this%regrid_type == VERTICAL_METHOD_NONE) then
           _ASSERT(.not.(found_mixed_ce),'have mixed level/edge')
        end if


        if (haveVert) then
           this%lm=lm
           if (this%regrid_type == VERTICAL_METHOD_NONE .or. this%regrid_type == VERTICAL_METHOD_FLIP) then
              if (.not.allocated(this%levs)) then
                 allocate(this%levs(lm))
                 do i=1,lm
                    this%levs(i)=vlb+i-1
                 enddo
              end if
              if (have_ungrd) then
                 if (allocated(ungridded_coord)) then
                   this%levs=ungridded_coord
                 end if

                 call metadata%add_dimension('lev', lm)
                 v = Variable(type=PFIO_REAL64, dimensions='lev')
                 call v%add_attribute('units',ungridded_unit)
                 call v%add_attribute('standard_name',ungridded_name)
                 call v%add_attribute('coordinate','N/A')
                 call v%add_const_value(UnlimitedEntity(this%levs))
                 call metadata%add_variable('lev',v,_RC)
              else
                 call metadata%add_dimension('lev', lm)
                 v = Variable(type=PFIO_REAL64, dimensions='lev')
                 call v%add_attribute('long_name',this%long_name)
                 call v%add_attribute('units',this%vunit)
                 call v%add_attribute('positive',trim(this%positive))
                 call v%add_attribute('coordinate',this%vcoord)
                 call v%add_attribute('standard_name',this%standard_name)
                 call v%add_const_value(UnlimitedEntity(this%levs))
                 call metadata%add_variable('lev',v,_RC)
              end if

           else if (this%regrid_type == VERTICAL_METHOD_ETA2LEV) then
              call metadata%add_dimension('lev', size(this%levs))
              v = Variable(type=PFIO_REAL64, dimensions='lev')
              call v%add_attribute('long_name','vertical level')
              call v%add_attribute('units',trim(this%vunit))
              if (this%levs(1)>this%levs(size(this%levs))) then
                 call v%add_attribute('positive','down')
              else
                 call v%add_attribute('positive','up')
              end if
              call v%add_attribute('coordinate',trim(this%vvar))
              call v%add_attribute('standard_name',trim(this%vvar)//"_level")
              call v%add_const_value(UnlimitedEntity(this%levs))
              call metadata%add_variable('lev',v,_RC)

           else if (this%regrid_type == VERTICAL_METHOD_SELECT) then
              call metadata%add_dimension('lev', lm)
              v = Variable(type=PFIO_REAL64, dimensions='lev')
              call v%add_attribute('long_name','vertical level')
              call v%add_attribute('units','layer')
              call v%add_attribute('positive','down')
              call v%add_attribute('coordinate','eta')
              call v%add_attribute('standard_name','model_layers')
              call v%add_const_value(UnlimitedEntity(this%levs))
              call metadata%add_variable('lev',v,_RC)
           end if
        end if
        _RETURN(_SUCCESS)

     end subroutine append_vertical_metadata

end module MAPL_VerticalDataMod

#include "MAPL_Generic.h"

module MAPL_VerticalDataMod
  use ESMF
  use MAPL_GenericMod
  use MAPL_BaseMod
  use MAPL_IntegerVectorMod
  use pFIO
  use MAPL_ErrorHandlingMod
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  
  private

  public :: VERTICAL_METHOD_NONE
  public :: VERTICAL_METHOD_SELECT
  public :: VERTICAL_METHOD_ETA2LEV
  enum, bind(c)
     enumerator :: VERTICAL_METHOD_NONE = -1
     enumerator :: VERTICAL_METHOD_SELECT
     enumerator :: VERTICAL_METHOD_ETA2LEV
  end enum

  type, public :: verticalData
     character(len=:), allocatable :: vunit
     character(len=:), allocatable :: func
     character(len=:), allocatable :: vvar
     real :: vscale
     real :: pow=0.0
     real, allocatable :: levs(:)
     real, allocatable :: interp_levels(:)
     integer :: lm
     integer :: regrid_type
     type(ESMF_Field) :: interp_var
     contains
        procedure :: append_vertical_metadata
        procedure :: get_interpolating_variable
        procedure :: regrid_eta_to_pressure
        procedure :: regrid_select_level
        procedure :: skip_var
  end type verticalData

  interface verticalData
     module procedure newVerticalData
  end interface

  contains
  
     function newVerticalData(levels,vcoord,vscale,vunit,rc) result(vdata)
        type(VerticalData) :: vData
        real, pointer, intent(in), optional :: levels(:)
        real, intent(in), optional :: vscale
        character(len=*), optional, intent(in) :: vcoord
        character(len=*), optional, intent(in) :: vunit
        integer, optional, intent(Out) :: rc

        integer :: status

        if (.not.present(levels)) then
           vdata%regrid_type = VERTICAL_METHOD_NONE
           _RETURN(ESMF_SUCCESS)
        end if

        allocate(vData%levs,source=levels)
        if (present(vunit)) then
           vdata%vunit=vunit
         else
           vdata%vunit=""
        end if
        if (present(vscale)) then
           vdata%vscale=vscale
        else
           vdata%vscale=1.0
        end if
        if (present(vcoord)) then
           allocate(vData%interp_levels(size(vData%levs)))
           vdata%regrid_type = VERTICAL_METHOD_ETA2LEV
           vdata%VVAR = adjustl(vcoord)
           vdata%Func = vdata%Vvar(1:3)
           if    (vdata%Func=='log') then
              vdata%Vvar = adjustl(vdata%Vvar(index(vdata%Vvar,'(')+1:index(vdata%Vvar,')')-1))
              vdata%interp_levels = log(vdata%levs*vdata%vscale)
           elseif(vdata%Func=='pow') then
              read( vdata%Vvar(index(vdata%Vvar,',')+1:index(vdata%Vvar,')')-1) , *) vdata%pow
              vdata%Vvar = adjustl(vdata%Vvar(index(vdata%Vvar,'(')+1:index(vdata%Vvar,',')-1))
              vdata%interp_levels =  (vdata%levs*vdata%vscale)**vdata%pow
           else
              vdata%interp_levels = vdata%levs*vdata%vscale
           endif
        else
           vdata%regrid_type = VERTICAL_METHOD_SELECT
        end if
     end function newVerticalData

     function skip_var(this,field,rc) result(skip)
        logical :: skip
        class(verticalData), intent(inout) :: this
        type(ESMF_Field), intent(inout) :: field
        integer, optional, intent(out) :: rc

        integer :: status
        character(len=ESMF_MAXSTR) :: name

        call ESMF_FieldGet(field,name=name,rc=status)
        _VERIFY(status)
        skip = trim(name)==trim(this%vvar)
     end function skip_var

     subroutine regrid_eta_to_pressure(this,ptrIn,PtrOut,rc)
        class(verticalData), intent(inout) :: this
        real, intent(inout) :: ptrIn(:,:,:)
        real, intent(inout) :: ptrOut(:,:,:)
        integer, optional, intent(out) :: rc

        integer :: status
        real, target, allocatable  :: Ple3d(:,:,:)
        real,         allocatable  :: Pl3d(:,:,:)
        real,         allocatable  :: Ptrx(:,:,:)
        real, pointer              :: ptr3(:,:,:)
        integer :: k

        call ESMF_FieldGet(this%interp_var,localDE=0,farrayPtr=ptr3,rc=status)
        _VERIFY(status)
! The Ptr3 interpolating variable is a zero-based (0-LM) edge variable
!---------------------------------------------------------------------
       if(lbound(PTR3,3)==0) then
          allocate( ple3D(size(Ptr3,1),size(Ptr3,2),size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)
          allocate(  pl3D(size(Ptr3,1),size(Ptr3,2),size(Ptr3,3)-1), stat=status)
          _VERIFY(STATUS)

          if    (this%Func=='log') then
             ple3D = log(Ptr3)
             pl3D  = log( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )
          elseif(this%Func=='pow') then
             ple3D = Ptr3**this%pow
             pl3D  =    ( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )**this%pow
          else
             ple3D = Ptr3
             pl3D  =    ( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )
          end if

       else

! The Ptr3 interpolating variable is a (1-LM) mid-layer variable
!---------------------------------------------------------------
          allocate(  Ptrx(size(Ptr3,1),size(Ptr3,2),0:size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)
          allocate( ple3D(size(Ptr3,1),size(Ptr3,2),0:size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)
          allocate(  pl3D(size(Ptr3,1),size(Ptr3,2),  size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)

          Ptrx(:,:,0               ) = 0.5*( 3* Ptr3(:,:,1)             -Ptr3(:,:,2)                )
          Ptrx(:,:,1:size(Ptr3,3)-1) = 0.5*(    Ptr3(:,:,2:size(Ptr3,3))+Ptr3(:,:,1:size(Ptr3,3)-1) )
          Ptrx(:,:,  size(Ptr3,3)  ) = 0.5*( 3* Ptr3(:,:,  size(Ptr3,3))-Ptr3(:,:,  size(Ptr3,3)-1) )

          if    (this%Func=='log') then
             ple3D = log(Ptrx)
             pl3D  = log( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )
          elseif(this%Func=='pow') then
             ple3D = Ptrx**this%pow
             pl3D  =    ( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )**this%pow
          else
             ple3D = Ptrx
             pl3D  =    ( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )
          end if

          deallocate(Ptrx)
       end if
       do k=1,size(ptrOut,3)
          call VertInterp(ptrOut(:,:,k),ptrIn,this%interp_levels(k),ple3d,pl3d,rc=status)
          _VERIFY(status)
       end do
        

     end subroutine regrid_eta_to_pressure

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

        call ESMF_FieldBundleGet(bundle,fieldName=trim(this%vvar),field=this%interp_var,rc=status)
        _VERIFY(status)

     end subroutine get_interpolating_variable


     subroutine append_vertical_metadata(this,metadata,bundle,rc)
        class (verticalData), intent(inout) :: this
        type(FileMetaData), intent(inout) :: metadata
        type(ESMF_FieldBundle), intent(inout) :: bundle
        integer, optional, intent(out) :: rc

        integer :: lm,i,NumVars,fieldRank,vlast
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
        logical                     :: unGrdNameCheck, unGrdUnitCheck, unGrdCoordCheck, have_ungrd
      
        integer :: status
        type(Variable) :: v
        real(REAL64), allocatable :: levs(:)
        logical :: isPresent

        ! loop over variables in file
        call ESMF_FieldBundleGet(bundle,fieldCount=NumVars,rc=status)
        _VERIFY(status)
        allocate(VarDims(numVars),location(numVars))

        allocate(hasUngrid(NumVars))
        hasUngrid=.false.
        allocate(ungridded_names(NumVars), stat=STATUS)
        _VERIFY(STATUS)
        ungridded_names=""
        allocate(ungridded_units(NumVars), stat=STATUS)
        _VERIFY(STATUS)
        ungridded_units=""

        do i=1,numVars
           call ESMF_FieldBundleGet(bundle,i,field,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(field,dimCount=FieldRank,rc=status)
           _VERIFY(status)
           call ESMF_AttributeGet(field,name="VLOCATION", value=location(i),rc=status)
           if (fieldRank==2) then
              varDims(i)=1
           else if (fieldRank==3) then
              call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
              _VERIFY(status)
              varDims(i)=size(ptr3d,3)
              if (location(i) == MAPL_VLocationNone) then
                 hasUngrid(I) = .true.
                 call ESMF_AttributeGet(field,NAME="UNGRIDDED_UNIT",value=ungridded_unit,rc=status)
                 _VERIFY(STATUS)
                 call ESMF_AttributeGet(field,NAME="UNGRIDDED_NAME",value=ungridded_name,rc=status)
                 _VERIFY(STATUS)
                 ungridded_names(i) = ungridded_name
                 ungridded_units(i) = ungridded_unit
                 call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",isPresent=isPresent,rc=status)
                 _VERIFY(STATUS)
                 if (isPresent) then
                    call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",itemcount=ungrdsize,rc=status)
                    _VERIFY(STATUS)
                    if (ungrdsize/=0) then
                       _ASSERT(varDims(i)==ungrdsize,"ungridded size does not match variable")
                       if (.not.allocated(ungridded_coord)) allocate(ungridded_coord(ungrdsize),stat=status)
                       if (.not.allocated(ungridded_coords)) allocate(ungridded_coords(NumVars,ungrdsize),stat=status)
                       _VERIFY(STATUS)
                       call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",valuelist=ungridded_coord,rc=status)
                       _VERIFY(STATUS)
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
                   _ASSERT(.false.,'Ungridded attributes for variables in collection do not match')
                end if
             end if
          end do
       end if

        lm=1
        haveVert = any(varDims/=1)
        if (haveVert) then
           if (this%regrid_type == VERTICAL_METHOD_NONE ) then
              vlast=1
              do i=1,numVars
                 if (varDims(i)/=1) then
                    if (vlast/=1) then
                       if (vlast /= varDims(i)) then
                          _ASSERT(.false.,"have mixed level/edge")
                       end if
                    else
                       vlast=varDims(i)
                    end if
                 end if
              end do
              lm=vlast
           else if (this%regrid_type == VERTICAL_METHOD_ETA2LEV .or. & 
                    this%regrid_type == VERTICAL_METHOD_SELECT) then
              lm = size(this%levs) 
           end if
        end if

        if (haveVert) then
           this%lm=lm
           if (this%regrid_type == VERTICAL_METHOD_NONE) then
              allocate(this%levs(lm))
              do i=1,lm
                 this%levs(i)=i
              enddo
              if (have_ungrd) then
                 if (allocated(ungridded_coord)) then
                   this%levs=ungridded_coord
                 end if
             
                 call metadata%add_dimension('lev', lm, rc=status)
                 v = Variable(PFIO_REAL64, dimensions='lev')
                 call v%add_attribute('units',ungridded_unit)
                 call v%add_attribute('standard_name',ungridded_name)
                 call v%add_attribute('coordinate','N/A')
                 call v%add_const_value(UnlimitedEntity(this%levs))
                 call metadata%add_variable('lev',v,rc=status)
              else 
                 call metadata%add_dimension('lev', lm, rc=status)
                 v = Variable(PFIO_REAL64, dimensions='lev')
                 call v%add_attribute('long_name','vertical level')
                 call v%add_attribute('units','layer')
                 call v%add_attribute('positive','down')
                 call v%add_attribute('coordinate','eta')
                 call v%add_attribute('standard_name','model_layer')
                 call v%add_const_value(UnlimitedEntity(this%levs))
                 call metadata%add_variable('lev',v,rc=status)
              end if

           else if (this%regrid_type == VERTICAL_METHOD_ETA2LEV) then
              call metadata%add_dimension('lev', size(this%levs), rc=status)
              v = Variable(PFIO_REAL64, dimensions='lev')
              call v%add_attribute('long_name','vertical level')
              call v%add_attribute('units',trim(this%vunit))
              if (this%levs(1)>this%levs(size(this%levs))) then
                 call v%add_attribute('positive','down')
              else
                 call v%add_attribute('positive','up')
              end if
              call v%add_attribute('coordinate','eta')
              call v%add_attribute('standard_name','model_layer')
              call v%add_const_value(UnlimitedEntity(this%levs))
              call metadata%add_dimension('lev', lm, rc=status)
              v = Variable(PFIO_REAL64, dimensions='lev')
              call v%add_attribute('long_name','vertical level')
              call v%add_attribute('units','layer')
              call v%add_attribute('positive','down')
              call v%add_attribute('coordinate','eta')
              call v%add_attribute('standard_name','model_layer')
              call v%add_const_value(UnlimitedEntity(this%levs))
              call metadata%add_variable('lev',v,rc=status)
           end if
        end if

     end subroutine append_vertical_metadata

  subroutine VertInterp(v2,v3,pp,ple_,pl_,rc)

    real,              intent(OUT) :: v2(:,:)
    real,              intent(IN ) :: v3(:,:,:)
    real,              intent(IN ) :: pp
    real,     target,  intent(IN ) :: ple_(:,:,:)
    real,     target,  intent(IN ) :: pl_(:,:,:)
    integer, optional, intent(OUT) :: rc

    real, dimension(size(v2,1),size(v2,2)) :: al,PT,PB
    integer km, K, msn
    logical flip
    real    ppx
    real, pointer   :: plx(:,:,:),pl(:,:,:),ps(:,:)

    integer        :: status

    if(size(v3,3)==size(ple_,3)) then
       pl => ple_
       ps => ple_(:,:,ubound(ple_,3))
    else
       pl => pl_
       ps => null()
    endif

    km   = size(pl,3)

    flip = pl(1,1,2) < pl(1,1,1)

    if(flip) then
       allocate(plx(size(pl,1),size(pl,2),size(pl,3)),stat=status)
       _VERIFY(STATUS)
       plx = -pl
       ppx = -pp
       msn = -1
    else
       plx => pl
       ppx = pp
       msn = 1
    end if


    v2   = MAPL_UNDEF

       pb   = plx(:,:,km)
       do k=km-1,1,-1
          pt = plx(:,:,k)
          if(all(pb<ppx)) exit
          where(ppx>pt .and. ppx<=pb)
             al = (pb-ppx)/(pb-pt)
             where (v3(:,:,k)   .eq. MAPL_UNDEF ) v2 = v3(:,:,k+1)
             where (v3(:,:,k+1) .eq. MAPL_UNDEF ) v2 = v3(:,:,k)
             where (v3(:,:,k)   .ne. MAPL_UNDEF .and.  v3(:,:,k+1) .ne. MAPL_UNDEF  )
                    v2 = v3(:,:,k)*al + v3(:,:,k+1)*(1.0-al)
             end where
          end where
          pb = pt
       end do

! Extend Lowest Level Value to the Surface
! ----------------------------------------
    if( associated(ps) ) then
        where( (ppx>plx(:,:,km).and.ppx<=ps*msn) )
                v2 = v3(:,:,km)
        end where
    end if

    if(flip) then
       deallocate(plx,stat=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine VertInterp


end module MAPL_VerticalDataMod

#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

submodule (MaskSamplerGeosatMod)  MaskSamplerGeosat_implement
  implicit none
contains
  
  module procedure MaskSamplerGeosat_from_config
  use BinIOMod
  use pflogger, only         :  Logger, logging
  type(ESMF_Time)            :: currTime
  type(ESMF_TimeInterval)    :: epoch_frequency
  type(ESMF_TimeInterval)    :: obs_time_span
  integer                    :: time_integer, second
  integer                    :: status
  character(len=ESMF_MAXSTR) :: STR1, line
  character(len=ESMF_MAXSTR) :: symd, shms
  integer                    :: nline, col
  integer, allocatable       :: ncol(:)
  character(len=ESMF_MAXSTR), allocatable :: word(:)
  integer                    :: nobs, head, jvar
  logical                    :: tend
  integer                    :: i, j, k, M
  integer                    :: count
  integer                    :: unitr, unitw
  type(Logger), pointer :: lgr
  
  mask%clock=clock
  mask%grid_file_name=''
  call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
  if (mapl_am_I_root()) write(6,*) 'string', string  


  call ESMF_ConfigGetAttribute(config, value=mask%grid_file_name,label=trim(string)//'obs_files:',    default="",  _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%index_name_x,  label=trim(string)//'index_name_x:', default="x", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%index_name_y,  label=trim(string)//'index_name_y:', default="y", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%var_name_x,    label=trim(string)//'var_name_x:',   default="x", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%var_name_y,    label=trim(string)//'var_name_y:',   default="y", _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%var_name_proj, label=trim(string)//'var_name_proj:',default="",  _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%att_name_proj, label=trim(string)//'att_name_proj:',default="",  _RC)
  call ESMF_ConfigGetAttribute(config, value=mask%thin_factor,   label=trim(string)//'thin_factor:',  default=-1,  _RC)


  if (mapl_am_I_root()) write(6,*) 'thin_factor:', mask%thin_factor
  call ESMF_ConfigGetAttribute(config, value=STR1, label=trim(string)//'obs_file_begin:', default="", _RC)
  if (trim(STR1)=='') then
     mask%obsfile_start_time = currTime
     call ESMF_TimeGet(currTime, timestring=STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_begin missing, default = currTime :', trim(STR1)
     endif
  else
     call ESMF_TimeSet(mask%obsfile_start_time, STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_begin provided: ', trim(STR1)
     end if
  end if

  call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
       label=trim(string) // 'obs_file_end:', _RC)
  if (trim(STR1)=='') then
     call ESMF_TimeIntervalSet(obs_time_span, d=14, _RC)
     mask%obsfile_end_time = mask%obsfile_start_time + obs_time_span
     call ESMF_TimeGet(mask%obsfile_end_time, timestring=STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_end   missing, default = begin+14D:', trim(STR1)
     endif
  else
     call ESMF_TimeSet(mask%obsfile_end_time, STR1, _RC)
     if (mapl_am_I_root()) then
        write(6,105) 'obs_file_end provided:', trim(STR1)
     end if
  end if
         
  call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
       label=trim(string) // 'obs_file_interval:', _RC)
  _ASSERT(STR1/='', 'fatal error: obs_file_interval not provided in RC file')
  if (mapl_am_I_root()) write(6,105) 'obs_file_interval:', trim(STR1)
  
  
  i= index( trim(STR1), ' ' )
  if (i>0) then
     symd=STR1(1:i-1)
     shms=STR1(i+1:)
  else
     symd=''
     shms=trim(STR1)
  endif
  call convert_twostring_2_esmfinterval (symd, shms,  mask%obsfile_interval, _RC)
  
  mask%is_valid = .true.
  
         
  ! __  metadata and file_handle
  allocate (mask%metadata)
  if (mapl_am_i_root()) then
     allocate (mask%file_handle)
  end if
  
  _RETURN(_SUCCESS)
  
105 format (1x,a,2x,a)
106 format (1x,a,2x,i8)
   end procedure MaskSamplerGeosat_from_config


   !
   !-- integrate both initialize and reinitialize
   !
   module procedure initialize
   integer :: status
   type(ESMF_Grid) :: grid
   type(variable) :: v
   type(GriddedIOitemVectorIterator) :: iter
   type(GriddedIOitem), pointer :: item
   type(ESMF_Time)            :: currTime
   integer :: k

   if (.not. present(reinitialize)) then
      if(present(bundle))   this%bundle=bundle
      if(present(items))    this%items=items
      if(present(timeInfo)) this%time_info=timeInfo
      if (present(vdata)) then
         this%vdata=vdata
      else
         this%vdata=VerticalData(_RC)
      end if
   else
      if (reinitialize) then
         allocate (this%metadata)
         if (mapl_am_i_root()) then
            allocate (this%file_handle)
         end if
      end if
   end if

   !-- wrong, fix it later
   call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC)

   this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
   if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

   !call this%create_Geosat_grid_find_mask(_RC)
   call this%create_grid(_RC)   

   _RETURN(_SUCCESS)

   end procedure initialize

!!      module procedure create_new_bundle
!!        type(GriddedIOitemVectorIterator) :: iter
!!        type(GriddedIOitem), pointer :: item
!!        type(ESMF_Field) :: src_field,dst_field
!!        integer :: rank,lb(1),ub(1)
!!        integer :: status
!!
!!        new_bundle = ESMF_FieldBundleCreate(_RC)
!!        iter = this%items%begin()
!!        do while (iter /= this%items%end())
!!           item => iter%get()
!!           if (item%itemType == ItemTypeScalar) then
!!              call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
!!              call ESMF_FieldGet(src_field,rank=rank,_RC)
!!              if (rank==2) then
!!                 dst_field = ESMF_FieldCreate(this%LS_ds,name=trim(item%xname), &
!!                      typekind=ESMF_TYPEKIND_R4,_RC)
!!              else if (rank==3) then
!!                 call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
!!                 if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
!!                    lb(1)=1
!!                    ub(1)=this%vdata%lm
!!                 end if
!!                 dst_field = ESMF_FieldCreate(this%LS_ds,name=trim(item%xname), &
!!                      typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
!!              end if
!!              call MAPL_FieldBundleAdd(new_bundle,dst_field,_RC)
!!           else if (item%itemType == ItemTypeVector) then
!!              _FAIL("ItemTypeVector not yet supported")
!!           end if
!!           call iter%next()
!!        enddo
!!        _RETURN(_SUCCESS)
!!
!!      end procedure create_new_bundle
!!


      module procedure create_metadata_variable
        type(ESMF_Field) :: field
        type(variable) :: v
        logical :: is_present
        integer :: field_rank, status
        character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
        integer :: k, ig

        call ESMF_FieldBundleGet(this%bundle,vname,field=field,_RC)
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
           vdims = this%index_name_loc
        else if (field_rank==3) then
           vdims = trim(this%index_name_loc)//",lev"
        end if
        v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(long_name))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))

!!        do k = 1, this%nobs_type
!!           do ig = 1, this%obs(k)%ngeoval
!!              if (trim(var_name) == trim(this%obs(k)%geoval_name(ig))) then
!!                 call this%obs(k)%metadata%add_variable(trim(var_name),v,_RC)
!!              endif
!!           enddo
!!        enddo

         _RETURN(_SUCCESS)
      end procedure create_metadata_variable


      module procedure create_file_handle
         integer :: status
         integer :: k
         character(len=ESMF_MAXSTR) :: filename

         if (.NOT. this%is_valid) then
            _RETURN(ESMF_SUCCESS)
         endif
!!
!!         do k=1, this%nobs_type
!!            call this%obs(k)%metadata%modify_dimension(this%index_name_loc, this%obs(k)%nobs_epoch)
!!         enddo
!!         if (mapl_am_I_root()) then
!!            do k=1, this%nobs_type
!!               if (this%obs(k)%nobs_epoch > 0) then
!!                  filename=trim(this%obs(k)%name)//trim(filename_suffix)
!!                  call this%obs(k)%file_handle%create(trim(filename),_RC)
!!                  call this%obs(k)%file_handle%write(this%obs(k)%metadata,_RC)
!!                  write(6,*) "Sampling to new file : ",trim(filename)
!!               end if
!!            enddo
!!         end if

        _RETURN(_SUCCESS)
      end procedure create_file_handle


       module procedure close_file_handle
          integer :: status
          integer :: k

          if (.NOT. this%is_valid) then
             _RETURN(ESMF_SUCCESS)
          endif

!!         if (mapl_am_I_root()) then
!!            do k=1, this%nobs_type
!!               if (this%obs(k)%nobs_epoch > 0) then
!!                  call this%obs(k)%file_handle%close(_RC)
!!               end if
!!            end do
!!         end if

          _RETURN(_SUCCESS)
       end procedure close_file_handle


      module procedure append_file
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_RouteHandle) :: RH

         type(ESMF_Field) :: src_field, dst_field
         type(ESMF_Field) :: acc_field
         type(ESMF_Field) :: acc_field_2d_rt, acc_field_3d_rt
         real(kind=REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
         real(kind=REAL32), pointer :: p_acc_rt_3d(:,:),p_acc_rt_2d(:)
         real(kind=REAL32), pointer :: p_src(:,:),p_dst(:,:)

         integer :: is, ie, nx
         integer :: lm
         integer :: rank
         integer :: status
         integer :: j, k, ig
         integer, allocatable :: ix(:)

         if (.NOT. this%is_valid) then
            _RETURN(ESMF_SUCCESS)
         endif

         if (this%nobs_dur_sum==0) then
            rc=0
            return
         endif
!
!         is=1
!         do k = 1, this%nobs_type
!            !-- limit  nx < 2**32 (integer*4)
!            nx=this%obs(k)%nobs_epoch
!            if (nx >0) then
!               if (mapl_am_i_root()) then
!                  call this%obs(k)%file_handle%put_var(this%var_name_time, real(this%obs(k)%times_R8), &
!                       start=[is], count=[nx], _RC)
!                  call this%obs(k)%file_handle%put_var(this%var_name_lon, this%obs(k)%lons, &
!                       start=[is], count=[nx], _RC)
!                  call this%obs(k)%file_handle%put_var(this%var_name_lat, this%obs(k)%lats, &
!                       start=[is], count=[nx], _RC)
!               end if
!            end if
!         enddo
!
!         ! get RH from 2d field
!         src_field = ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
!         dst_field = ESMF_FieldCreate(this%LS_rt,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
!         call ESMF_FieldRedistStore(src_field,dst_field,RH,_RC)
!         call ESMF_FieldDestroy(src_field,noGarbage=.true.,_RC)
!         call ESMF_FieldDestroy(dst_field,noGarbage=.true.,_RC)
!
!         ! redist and put_var
!         lm = this%vdata%lm
!         acc_field_2d_rt = ESMF_FieldCreate (this%LS_rt, name='field_2d_rt', typekind=ESMF_TYPEKIND_R4, _RC)
!         acc_field_3d_rt = ESMF_FieldCreate (this%LS_rt, name='field_3d_rt', typekind=ESMF_TYPEKIND_R4, &
!              gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
!
!         iter = this%items%begin()
!         do while (iter /= this%items%end())
!            item => iter%get()
!            !!write(6, '(2x,a,2x,a)') 'item%xname', trim(item%xname)
!
!            if (item%itemType == ItemTypeScalar) then
!               call ESMF_FieldBundleGet(this%acc_bundle,trim(item%xname),field=acc_field,_RC)
!               call ESMF_FieldGet(acc_field,rank=rank,_RC)
!               if (rank==1) then
!                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_2d, _RC)
!                  call ESMF_FieldGet( acc_field_2d_rt, localDE=0, farrayPtr=p_acc_rt_2d, _RC)
!                  call ESMF_FieldRedist( acc_field,  acc_field_2d_rt, RH, _RC)
!                  if (mapl_am_i_root()) then
!                     !
!                     !-- pack fields to obs(k)%p2d and put_var
!                     !
!                     is=1
!                     ie=this%epoch_index(2)-this%epoch_index(1)+1
!                     do k=1, this%nobs_type
!                        nx = this%obs(k)%nobs_epoch
!                        allocate (this%obs(k)%p2d(nx))
!                     enddo
!
!                     allocate(ix(this%nobs_type))
!                     ix(:)=0
!                     do j=is, ie
!                        k = this%obstype_id(j)
!                        ix(k) = ix(k) + 1
!                        this%obs(k)%p2d(ix(k)) = p_acc_rt_2d(j)
!                     enddo
!
!                     do k=1, this%nobs_type
!                        if (ix(k) /= this%obs(k)%nobs_epoch) then
!                           print*, 'obs_', k, ' : ix(k) /= this%obs(k)%nobs_epoch'
!                           print*, 'obs_', k, ' : this%obs(k)%nobs_epoch, ix(k) =', this%obs(k)%nobs_epoch, ix(k)
!                           _FAIL('test ix(k) failed')
!                        endif
!                     enddo
!                     deallocate(ix)
!                     do k=1, this%nobs_type
!                        is = 1
!                        nx = this%obs(k)%nobs_epoch
!                        if (nx>0) then
!                           do ig = 1, this%obs(k)%ngeoval
!                              if (trim(item%xname) == trim(this%obs(k)%geoval_name(ig))) then
!                                 call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p2d(1:nx), &
!                                      start=[is],count=[nx])
!                              end if
!                           enddo
!                        endif
!                     enddo
!                  end if
!               else if (rank==2) then
!                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_3d, _RC)
!                  call ESMF_FieldGet( acc_field_3d_rt, localDE=0, farrayPtr=p_acc_rt_3d, _RC)
!
!                  dst_field=ESMF_FieldCreate(this%LS_rt,typekind=ESMF_TYPEKIND_R4, &
!                       gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
!                  src_field=ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4, &
!                       gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
!
!                  call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,_RC)
!                  call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,_RC)
!
!                  p_src= reshape(p_acc_3d,shape(p_src), order=[2,1])
!                  call ESMF_FieldRegrid(src_field,dst_field,RH,_RC)
!                  p_acc_rt_3d=reshape(p_dst, shape(p_acc_rt_3d), order=[2,1])
!
!                  call ESMF_FieldDestroy(dst_field,noGarbage=.true.,_RC)
!                  call ESMF_FieldDestroy(src_field,noGarbage=.true.,_RC)
!
!                  if (mapl_am_i_root()) then
!                     !
!                     !-- pack fields to obs(k)%p3d and put_var
!                     !
!                     is=1
!                     ie=this%epoch_index(2)-this%epoch_index(1)+1
!                     do k=1, this%nobs_type
!                        nx = this%obs(k)%nobs_epoch
!                        allocate (this%obs(k)%p3d(nx, size(p_acc_rt_3d,2)))
!                     enddo
!                     allocate(ix(this%nobs_type))
!                     ix(:)=0
!                     do j=is, ie
!                        k = this%obstype_id(j)
!                        ix(k) = ix(k) + 1
!                        this%obs(k)%p3d(ix(k),:) = p_acc_rt_3d(j,:)
!                     enddo
!                     deallocate(ix)
!                     do k=1, this%nobs_type
!                        is = 1
!                        nx = this%obs(k)%nobs_epoch
!                        if (nx>0) then
!                           do ig = 1, this%obs(k)%ngeoval
!                              if (trim(item%xname) == trim(this%obs(k)%geoval_name(ig))) then
!                                 call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p3d(:,:), &
!                                      start=[is,1],count=[nx,size(p_acc_rt_3d,2)])
!                              end if
!                           end do
!                        endif
!                     enddo
!                     !!write(6,'(10f8.2)') p_acc_rt_3d(:,:)
!                     !!write(6,*) 'here in append_file:  put_var 3d'
!                     !!call this%obs(k)%file_handle%put_var(trim(item%xname),p_acc_rt_3d(:,:),&
!                     !!     start=[is,1],count=[nx,size(p_acc_rt_3d,2)])
!                  end if
!               endif
!            else if (item%itemType == ItemTypeVector) then
!               _FAIL("ItemTypeVector not yet supported")
!            end if
!            call iter%next()
!         enddo
!         call ESMF_FieldDestroy(acc_field_2d_rt, noGarbage=.true., _RC)
!         call ESMF_FieldDestroy(acc_field_3d_rt, noGarbage=.true., _RC)
!         call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)
!
         _RETURN(_SUCCESS)
       end procedure append_file



       
!!       module procedure find_mask
!!       use pflogger, only: Logger, logging     
!!       use BinIOMod, only: GETFILE
!!       integer                 :: x_subset(2)
!!       type(ESMF_Time)         :: timeset(2)
!!       type(ESMF_Time)         :: current_time
!!       type(ESMF_TimeInterval) :: dur
!!       type(GriddedIOitemVectorIterator) :: iter
!!       type(GriddedIOitem), pointer :: item
!!
!!       real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
!!       real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
!!       real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
!!       real(kind=REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
!!       integer :: is, ie
!!       integer :: status
!!       
!!       lgr => logging%get_logger('HISTORY.sampler')
!!
!!       _RETURN(ESMF_SUCCESS)
!!
!!     end procedure find_mask
         


  ! __ this code does not handle zero observations
  !
       module procedure create_Geosat_grid_find_mask
       use pflogger, only: Logger, logging
       implicit none
       type(Logger), pointer :: lgr
       real(ESMF_KIND_R8), pointer :: ptAT(:)
       type(ESMF_routehandle) :: RH
       type(ESMF_Grid) :: grid
       integer :: mypet, petcount
       type (ESMF_LocStream) :: LS_rt
       type (ESMF_LocStream) :: LS_ds
       type (LocStreamFactory):: locstream_factory
       type (ESMF_Field) :: fieldA
       type (ESMF_Field) :: fieldB
       
       integer :: i, j, k, L
       integer :: n1, n2
       integer :: nx, ny, nx_sum
       integer :: nlon, nlat
       integer :: arr(1)
       integer :: len

       integer :: IM, JM, LM, COUNTS(3)
       type(ESMF_DistGrid) :: distGrid
       type(ESMF_DElayout) :: layout
       type(ESMF_VM) :: VM
       integer :: myid
       integer :: ndes
       integer :: dimCount
       integer, allocatable :: II(:)
       integer, allocatable :: JJ(:)
       real(REAL64), allocatable :: obs_lons(:)
       real(REAL64), allocatable :: obs_lats(:)
       integer :: mpic

       type (ESMF_Field) :: fieldI4
       type(ESMF_routehandle) :: RH_halo
       type(ESMF_Field) :: src_field,dst_field,acc_field
       integer :: useableHalo_width
       integer :: rank
       integer :: eLB(2), eUB(2)
       integer :: cLB(2), cUB(2)
       integer :: tLB(2), tUB(2)
       integer :: ecount(2)
       integer :: ccount(2)
       integer :: tcount(2)
       integer(ESMF_KIND_I4), pointer :: farrayPtr(:,:)
       real(ESMF_KIND_R8), pointer :: ptA(:)
       real(ESMF_KIND_R8), pointer :: ptB(:)       

       character(len=50) :: filename
       integer :: unit
       integer :: ix, jx
       integer :: i_1, i_n, j_1, j_n
       real(REAL64), pointer :: x(:)
       real(REAL64), pointer :: y(:)
       real(REAL64) :: lambda0_deg, lambda0
       real(REAL64) :: x0, y0
       real(REAL64) :: lon0, lat0      
       real(REAL64) :: lam_sat
       integer      :: mask0
       character(len=ESMF_MAXSTR) :: fn, key_x, key_y, key_p, key_p_att
       integer      :: Xdim_true, Ydim_true
       integer      :: Xdim_red, Ydim_red
       real(REAL64), allocatable :: lons(:), lats(:)
       real(REAL64), allocatable :: lons_ds(:), lats_ds(:)       
       integer,      allocatable :: mask(:,:)

       real(ESMF_kind_R8), pointer :: lons_ptr(:), lats_ptr(:)
       integer :: status

       lgr => logging%get_logger('HISTORY.sampler')

       ! Meta code:
       !   read ABI grid into LS_rt
       !   gen LS_ds with CS grid
       !   find mask on each PET
       !   determine CS_LS_ds


       if (mapl_am_i_root()) then
          ! __s1.  SAT file
          !
          fn    = this%grid_file_name
          key_x = this%var_name_x
          key_y = this%var_name_y
          key_p = this%var_name_proj
          key_p_att = this%att_name_proj
          call get_ncfile_dimension(fn,nlon=n1,nlat=n2,key_lon=key_x,key_lat=key_y,_RC)
          !
          ! use thin_factor to reduce regridding matrix size
          !
          xdim_true = n1
          ydim_true = n2
          xdim_red  = n1 / this%thin_factor
          ydim_red  = n2 / this%thin_factor
          allocate (x (xdim_true) )
          allocate (y (xdim_true) )         

          call get_v1d_netcdf_R8_complete (fn, key_x, x, _RC)
          call get_v1d_netcdf_R8_complete (fn, key_y, y, _RC)
          call get_att_real_netcdf (fn, key_p, key_p_att, lambda0_deg, _RC)
          lam_sat = lambda0_deg*MAPL_DEGREES_TO_RADIANS_R8

          nx=0
          do i=1, xdim_red
             do j=1, ydim_red
                x0 = x( i * this%thin_factor )
                y0 = y( j * this%thin_factor )
                call ABI_XY_2_lonlat (x0, y0, lam_sat, lon0, lat0, mask=mask0)
                if (mask0 > 0) then
                   nx=nx+1
                end if
             end do
          end do
          allocate (lons(nx), lats(nx))
          nx = 0
          do i=1, xdim_red
             do j=1, ydim_red
                x0 = x( i * this%thin_factor )
                y0 = y( j * this%thin_factor )
                call ABI_XY_2_lonlat (x0, y0, lam_sat, lon0, lat0, mask=mask0)
                if (mask0 > 0) then
                   nx=nx+1
                   lons(nx) = lon0
                   lats(nx) = lat0
                end if
             end do
          end do
          arr(1)=nx
       else
          allocate(lons(0),lats(0),_STAT)
          arr(1)=0
       endif

       call ESMF_VMGetCurrent(vm,_RC)
       call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx, &
            count=1, reduceflag=ESMF_REDUCE_SUM, _RC)
       this%nobs = nx
       if (mapl_am_I_root()) write(6,*) 'nobs tot :', nx

       if ( nx == 0 ) then
          this%is_valid = .false.
          _RETURN(ESMF_SUCCESS)
          !
          ! no valid obs points are found
          !
       end if


       ! __ s2. set distributed LS
       !

       locstream_factory = LocStreamFactory(lons,lats,_RC)
       LS_rt = locstream_factory%create_locstream(_RC)
       call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
       LS_ds = locstream_factory%create_locstream(grid=grid,_RC)

       fieldA = ESMF_FieldCreate (LS_rt, name='A', typekind=ESMF_TYPEKIND_R8, _RC)
       fieldB = ESMF_FieldCreate (LS_ds, name='B', typekind=ESMF_TYPEKIND_R8, _RC)

       call ESMF_FieldGet( fieldA, localDE=0, farrayPtr=ptA)
       call ESMF_FieldGet( fieldB, localDE=0, farrayPtr=ptB)
       if (mypet == 0) then
          ptA(:) = lons(:)
       end if
       call ESMF_FieldRedistStore (fieldA, fieldB, RH, _RC)
       call ESMF_FieldRedist      (fieldA, fieldB, RH, _RC)
       lons_ds = ptB

       if (mypet == 0) then
          ptA(:) = lats(:)
       end if
       call ESMF_FieldRedist      (fieldA, fieldB, RH, _RC)
       lats_ds = ptB

       call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)
       call ESMF_FieldDestroy(fieldA,nogarbage=.true.,_RC)
       call ESMF_FieldDestroy(fieldB,nogarbage=.true.,_RC)       

       !- debug
       !       write(6,'(2x,a,i5,100f10.1)') 'lons_ds pet=', mypet, lons_ds(::1000)
       write(6,'(2x,a,i5,100f10.1)') 'lats_ds pet=', mypet, lats_ds(::2000)

       !       write(6,'(2x,a,i5,100f10.1)') 'lons_rt pet=', mypet, lons(::20)
       !       write(6,'(2x,a,i5,100f10.1)') 'lats_rt pet=', mypet, lats(::5)
       call ESMF_VMBarrier (vm, _RC)


       ! __ s3. round-1: find n.n. CS pts for LS_ds
       !
       obs_lons = lons_ds
       obs_lats = lats_ds

       nx = size ( lons_ds )

       !!       ! independent test
       !!       nx = 2
       !!       obs_lons = [ 0.d0 , 10.d0 ]
       !!       obs_lats = [ 0.d0 , 10.d0 ]       

       allocate ( II(nx), JJ(nx) )
       call MPI_Barrier(mpic, status)
       !!       call MAPL_GetGlobalHorzIJIndex(nx,II,JJ,lon=this%lons_ds,lat=this%lats_ds,grid=Grid,_RC)
       !!call MAPL_GetGlobalHorzIJIndex(nx,II,JJ,lonR8=obs_lons,latR8=obs_lats,grid=grid,_RC)              
       call MAPL_GetHorzIJIndex(nx,II,JJ,lonR8=obs_lons,latR8=obs_lats,grid=grid,_RC)

       call ESMF_VMBarrier (vm, _RC)

       write(6,*) 'nx', nx
       !!       do i=1,nx,20
       !!          write(6,'(2x,a,i5,i10,2f12.2,10i5)') 'pet,i,lon,lat,II,JJ=', mypet,i,&
       !!               obs_lons(i),obs_lats(i),II(i),JJ(i)
       !!       end do


       !       call ESMF_GridGet(grid, DistGrid=distgrid, dimCount=dimCount, _RC)
       !       call ESMF_DistGridGet(distgrid, deLayout=LAYOUT, _RC)
       !       call ESMF_DELayoutGet(layout, VM=vm, _RC)
       !       call ESMF_VmGet(VM, localPet=myid, petCount=ndes, _RC)
       call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
       IM= COUNTS(1)
       JM= COUNTS(2)
       LM= COUNTS(3)
       if (mapl_am_i_root()) write(6,'(2x,a,2x,10i5)') 'grid counts(1:3)', counts(1:3)


       !
       ! __  halo for mask
       !
       useableHalo_width = 1
       fieldI4 = ESMF_FieldCreate (grid, ESMF_TYPEKIND_I4, &
            totalLwidth=[useableHalo_width,useableHalo_width],&
            totalUwidth=[useableHalo_width,useableHalo_width], _RC)
       call ESMF_FieldGetBounds (fieldI4, &
            exclusiveLBound=eLB, exclusiveUBound=eUB, exclusiveCount=ecount, &
            totalLBound=tLB, totalUBound=tUB, totalCount=tcount, &
            computationalLBound=cLB, computationalUBound=cUB, computationalCount=ccount, &
            _RC)
       call ESMF_FieldGet (fieldI4, farrayPtr=farrayPtr,  _RC)
       farrayPtr = 0
       do i=1, nx
          if ( II(i)>0 .AND. JJ(i)>0 ) then
             farrayPtr( II(i), JJ(i) ) = 1
          endif
       enddo

       write(6,'(2x,a,2x,i5)') 'pet=', mypet
       do j=tUB(2), tLB(2), -1
          write(6, '(2x,100i5)') farrayPtr(tLB(1):tUB(1), j)
       end do

       call ESMF_FieldHaloStore (fieldI4, routehandle=RH_halo, _RC)
       call ESMF_FieldHalo (fieldI4, routehandle=RH_halo, _RC)
       call ESMF_VMBarrier (vm, _RC)

       write(filename, '(i5)') mypet
       filename='t.'//trim(adjustl(filename))
       open(newunit=unit,  file=trim(filename), status='unknown', _IOSTAT)
       write(6,'(2x,a,2x,5i20)') 'pet,unit', mypet, unit

       write(unit,'(2x,a,2x,i5)') 'AF pet=', mypet
       do j=tUB(2), tLB(2), -1
          write(unit, '(2x,100i5)') farrayPtr(tLB(1):tUB(1), j)
       end do
       call MPI_Barrier(mpic, status)

       k=0
       do i=eLB(1), eUB(1)
          do j=eLB(2), eUB(2)          
             if ( farrayPtr(i,j)==0 .AND. ( &
                  farrayPtr(i-1,j)==1 .OR. &                  
                  farrayPtr(i+1,j)==1 .OR. &
                  farrayPtr(i,j-1)==1 .OR. &
                  farrayPtr(i,j+1)==1 )  ) then
                farrayPtr(i,j) = -1
             end if
             if (farrayPtr(i,j)/=0) k=k+1
          end do
       end do
       allocate( mask(IM, JM))
       mask(1:IM, 1:JM) = abs(farrayPtr(1:IM, 1:JM))
       
       this%npt_mask = k
       allocate( this%index_mask(2,k) )
       arr(1)=k
       call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=this%tot_npt_mask, &
            count=1, reduceflag=ESMF_REDUCE_SUM, _RC)
       
       k=0
       do i=1, IM
          do j=1, JM
             if ( mask(i,j)==1 ) then
                k=k+1
                this%index_mask(1,k) = i
                this%index_mask(2,k) = j
             end if
          end do
       end do

       
       !
       ! -- test and print mask locations
       !       
       write(unit,'(2x,a,2x,i5)') 'connect pet=', mypet
       do j=tUB(2), tLB(2), -1
          write(unit, '(2x,100i5)') farrayPtr(tLB(1):tUB(1), j)
       end do
       write(unit,'(2x,a,2x,i5)') 'mask pet=', mypet
       do j=eUB(2), eLB(2), -1
          write(unit, '(2x,100i5)') mask(eLB(1):eUB(1), j)
       end do

       write(6,'(2x,a,2x,7i10)')  'this%npt_mask, this%tot_npt_mask', this%npt_mask, this%tot_npt_mask
       write(6,'(2x,a,2x,7i10)')  'this%index_mask(1,1:N)', this%index_mask(1,::5)
       write(6,'(2x,a,2x,7i10)')  'this%index_mask(2,1:N)', this%index_mask(2,::5)       
       
       
       !       _FAIL('nail 2')       
       !       write(6,'(2x,a,i5,100i5)') 'lats_rt pet=', mypet, mask(::5,::5)       

       close(unit)



       ! __ s4. round-2: initialize mask%LS_ds and mask%LS_rt
       !
       call ESMF_GridGetCoord (grid, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lons_ptr, _RC)
       call ESMF_GridGetCoord (grid, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=lats_ptr, _RC)       
       deallocate (lons, lats)
       allocate (lons(this%npt_mask), lats(this%npt_mask))
       do i=1, this%npt_mask
          lons(i) = lons_ptr (this%index_mask(1,i))
          lats(i) = lats_ptr (this%index_mask(2,i))          
       end do
       locstream_factory = LocStreamFactory(lons,lats,_RC)
       this%LS_ds = locstream_factory%create_locstream(_RC)


       
       
       _FAIL('nail 1')

       _RETURN(_SUCCESS)
     end procedure create_Geosat_grid_find_mask

     
end submodule MaskSamplerGeosat_implement

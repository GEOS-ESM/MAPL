#include "MAPL_Generic.h"


module AEIO_Writer
   use MPI
   use ESMF
   use MAPL_BaseMod
   use MAPL_MemUtilsMod
   use CollectionMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   use AEIO_CollectionDescriptor
   use AEIO_CollectionDescriptorVector
   use AEIO_MpiConnection
   use MAPL_Profiler
   use AEIO_IOProfiler 
   implicit none
   private

   public :: Writer

   integer, parameter :: stag = 6782
   type Writer
      type(MpiConnection) :: front_back_connection
      integer, allocatable :: writer_ranks(:)
      integer, allocatable :: server_ranks(:)
      type(CollectionDescriptorVector)  :: collection_descriptors
   contains
      procedure :: start_writer
      procedure :: add_collection
      procedure :: write_collection
      procedure :: setup_transfer
      procedure :: run_writer_process
      procedure :: run_coordinator
   end type

   interface Writer
      module procedure new_Writer
   end interface Writer

contains

   function new_writer(front_back_connection,rc) result(c)
      type(MpiConnection), intent(in) :: front_back_connection
      integer, optional, intent(out) :: rc
      type(writer) :: c
      integer :: status,myPet
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm,_RC)
      call ESMF_VMGet(vm,localPet=myPet,_RC)
      c%front_back_connection=front_back_connection

   end function new_writer

   subroutine add_collection(this,coll_name,bundle,rh,rc)
      class(writer), intent(inout) :: this
      character(len=*), intent(in) :: coll_name
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(RHConnector), intent(in) :: rh
      integer, optional, intent(out) :: rc

      type(collectionDescriptor) :: collection_descriptor

      collection_descriptor = CollectionDescriptor(bundle,coll_name,rh)
      call this%collection_descriptors%push_back(collection_descriptor)
      _RETURN(_SUCCESS)
   end subroutine add_collection

   subroutine start_writer(this,rc)
      class(Writer), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status,back_local_rank
      integer :: back_comm

      back_comm = this%front_back_connection%get_back_comm()
      call io_prof%start('start_writer')
      call MPI_Barrier(back_comm,status)

      call MPI_COMM_RANK(back_comm,back_local_rank,status)
      _VERIFY(status)
      !write(*,*)"Starting writer on local rank: ",back_local_rank
      if (this%front_back_connection%am_i_back_root()) then
         call this%run_coordinator(_RC)
      else
         call this%run_writer_process(_RC)
      end if

      _RETURN(_SUCCESS)

   end subroutine start_writer

   subroutine run_coordinator(this,rc)
      class(Writer), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: collection_id
      integer, allocatable :: collection_ids(:),free_workers(:),free_workers_global(:)
      integer :: num_collections,current_free,collections_to_write
      logical, allocatable :: busy(:)
      integer :: nwriters,free_worker,free,no_job,i,j,status,back_local_rank
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      type(RHConnector) :: rh,rh_new
      type(CollectionDescriptor), pointer :: collection_descriptor
      integer :: back_comm, connector_comm
      integer, allocatable :: writer_ranks(:),server_ranks(:),back_pets(:)
      real :: mem_total,mem_commit,mem_percent

      num_collections = this%collection_descriptors%size()
      allocate(collection_ids(num_collections))
      allocate(free_workers(num_collections))
      allocate(free_workers_global(num_collections))
      free_workers_global = -1

      back_comm = this%front_back_connection%get_back_comm()
      connector_comm = this%front_back_connection%get_connection_comm()
      writer_ranks = this%front_back_connection%get_back_mpi_ranks()
      server_ranks = this%front_back_connection%get_front_mpi_ranks()
      back_pets = this%front_back_connection%get_back_pets()

      call MPI_COMM_RANK(back_comm,back_local_rank,status)
      _VERIFY(status)
      !write(*,*)"Starting coordinator on local rank: ",back_local_rank
      nwriters = size(writer_ranks)-1
      allocate(busy(nwriters))
      _ASSERT(num_collections <= nwriters,"need more writers")
      busy = .false.
      do while (.true.)
         call MPI_recv(collection_ids, num_collections, MPI_INTEGER, &
         server_ranks(1),writer_ranks(1),connector_comm, &
         MPI_STAT, status)
         _VERIFY(status)
         call MAPL_MemCommited ( mem_total, mem_commit, mem_percent)
         write(*,'(A,1x,f10.2,f10.2,f10.2)')"writer memory total, committed, percent: ",mem_total/1024.0, mem_commit/1024.0,mem_percent
         if (any(collection_ids > 0)) then

             current_free = nwriters-count(busy)
             collections_to_write = count(collection_ids > 0)

             if (collections_to_write <= current_free) then
                do j=1,num_collections
                   if (collection_ids(j) > 0) then
                      do i=1,nwriters
                         if (busy(i).eqv. .false.) then
                            busy(i) = .true.
                            free_workers(j) =  i
                            free_workers_global(j) = back_pets(1+i)
                            exit
                         endif   
                      enddo
                   end if
                enddo

             else

                do j=1,num_collections
                   if (collection_ids(j) > 0) then
                      if (all(busy)) then
                         call mpi_recv(free_worker,1, MPI_INTEGER, &
                            MPI_ANY_SOURCE,stag,back_comm, &
                            MPI_STAT, status)
                         _VERIFY(status)
                         busy(free_worker)=.true.
                         free_workers(j) = free_worker
                         free_workers_global(j) = back_pets(1+free_worker)
                      else
                         do i=1,nwriters
                            if (busy(i).eqv. .false.) then
                               busy(i) = .true.
                               free_workers(j) =  i
                               free_workers_global(j) = back_pets(1+i)
                               exit
                            endif   
                         enddo
                      end if
                   end if
                enddo
             end if 
             call MPI_send(free_workers_global,num_collections,MPI_INTEGER,  server_ranks(1), &
                  server_ranks(1),connector_comm,status)
             _VERIFY(status)
             do i=1,num_collections
                if (collection_ids(i) > 0) then
                  
                   call MPI_Send(collection_ids(i),1,MPI_INTEGER, free_workers(i), &
                        free_workers(i),back_comm,status)
                   _VERIFY(status)
                   collection_descriptor => this%collection_descriptors%at(collection_ids(i))
                   rh=collection_descriptor%get_rh()
                   rh_new = this%setup_transfer(rh,back_pets(1+free_workers(i)),_RC)

                end if
             enddo
         else
            no_job=-1
            do i=1,nwriters
               if (.not.busy(i)) then
                  call MPI_send(no_job,1,MPI_INTEGER,i,i,back_comm,status)
                  _VERIFY(status)
               else
                  call MPI_recv(free,1,MPI_INTEGER, &
                                i,stag,back_comm, MPI_STAT,status)
                  _VERIFY(status)
                  if (free /= i) stop 'free should be i'
                  call MPI_send(no_job,1,MPI_INTEGER,i,i,back_comm,status)
                  _VERIFY(status)
               end if
            end do
            !write(*,*)"Exiting writer on rank: ",back_local_rank
            call io_prof%stop('start_writer')
            exit
         end if
      enddo

      _RETURN(_SUCCESS)

   end subroutine run_coordinator

   subroutine run_writer_process(this,rc)
      class(Writer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: MPI_STAT(MPI_STATUS_SIZE)
      integer :: back_comm,back_local_rank,collection_id,status

      back_comm = this%front_back_connection%get_back_comm()
      call MPI_COMM_RANK(back_comm,back_local_rank,status)
      _VERIFY(status)
      do while (.true.)
         ! which collection am I working on
         call MPI_Recv(collection_id,1,MPI_INTEGER, &
                      0,back_local_rank,back_comm, &
                      MPI_STAT,status)
         _VERIFY(status)
         if (collection_id < 0) then
            !write(*,*)"Exiting writer on rank: ",back_local_rank
            call io_prof%stop('start_writer')
            exit
         end if
         ! do stuff
         call this%write_collection(collection_id,_RC)
         ! send back I am done
         call MPI_send(back_local_rank,1,MPI_INTEGER,0,stag,back_comm,status)
         _VERIFY(status)                      

      enddo
      _RETURN(_SUCCESS)
   end subroutine run_writer_process

   subroutine write_collection(this,collection_id,rc)
      class(Writer), intent(inout) :: this
      integer, intent(in) :: collection_id
      integer, optional, intent(out) :: rc

      type(RHConnector) :: rh,rh_new
      integer :: status,i,local_rank
      type(CollectionDescriptor), pointer :: collection_descriptor
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_ArrayBundle) :: output_bundle
      integer :: fieldCount
      type(ESMF_DELayout) :: de_layout
      type(ESMF_DistGrid) :: dist_grid
      type(ESMF_Grid) :: grid
      type(ESMF_Field) :: field
      type(ESMF_Array) :: array
      character(len=ESMF_MAXSTR), allocatable :: fieldNames(:)
      integer :: rank,lb(1),ub(1),gdims(3)
      character(len=:), allocatable :: coll_name
      integer, allocatable :: back_pets(:)
      integer :: writer_comm
      character(len=1) :: ic

      write(ic,"(I1)")collection_id
      call io_prof%start('write_collection_'//ic)
      back_pets = this%front_back_connection%get_back_pets()
      writer_comm = this%front_back_connection%get_back_comm()

      call MPI_COMM_RANK(writer_comm,local_rank,status)
      _VERIFY(status)
      collection_descriptor => this%collection_descriptors%at(collection_id)
      rh = collection_descriptor%get_rh()
      coll_name = collection_descriptor%get_coll_name()
      rh_new = this%setup_transfer(rh,back_pets(1+local_rank),_RC)

      bundle = collection_descriptor%get_bundle()
      ! transfer arrays
      call ESMF_FieldBundleGet(bundle,fieldCount=fieldCount,grid=grid,_RC)
      allocate(fieldNames(fieldCount))
      call ESMF_FieldBundleGet(bundle,fieldNameList=fieldNames,_RC)
      call MAPL_GridGet(grid,globalCellCountPerDim=gdims,_RC)
      de_layout = ESMF_DELayoutCreate(deCount=1,petList=[back_pets(1+local_rank)],_RC)
      dist_grid = ESMF_DistGridCreate([1,1],[gdims(1),gdims(2)],regDecomp=[1,1],delayout=de_layout,_RC)
      output_bundle = ESMF_ArrayBundleCreate(_RC)

      call io_prof%start('start_write_epoch_'//ic)
      call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER)!,throttle=1)

      do i=1,fieldCount
         call ESMF_FieldBundleGet(bundle,trim(fieldNames(i)),field=field,_RC)
         call ESMF_FieldGet(field,rank=rank,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
         if (rank==2) then
            array = ESMF_ArrayCreate(dist_grid,ESMF_TYPEKIND_R4,name=trim(fieldNames(i)),_RC)
         else if (rank==3) then
            array = ESMF_ArrayCreate(dist_grid,ESMF_TYPEKIND_R4,undistLBound=lb,undistUBound=ub,name=trim(fieldNames(i)),_RC)
         end if
         call ESMF_ArrayBundleAdd(output_bundle,[array],_RC)
         call rh_new%redist_arrays(dstArray=array,_RC)
         block
            real, pointer :: ptr2d(:,:),ptr3d(:,:,:)
            if (rank==2) then
               call ESMF_ArrayGet(array,farrayptr=ptr2d,_RC)
               !write(*,*)trim(coll_name)," ",trim(fieldNames(i)),size(ptr2d,1),size(ptr2d,2)
               !write(*,*)trim(coll_name)," ",trim(fieldNames(i)),local_rank,minval(ptr2d),maxval(ptr2d)
            else if (rank==3) then
               call ESMF_ArrayGet(array,farrayptr=ptr3d,_RC)
               !write(*,*)trim(coll_name)," ",trim(fieldNames(i)),local_rank,minval(ptr3d),maxval(ptr3d)
            end if
         end block
      enddo
      
      !call rh_new%redist_arraybundles(dstArrayBundle=output_bundle,_RC)
      call io_prof%stop('start_write_epoch_'//ic)
      call ESMF_VMEpochExit( keepAlloc=.false.)

      call rh_new%destroy(_RC)
      do i=1,fieldCount
         call ESMF_ArrayBundleGet(output_bundle,trim(fieldNames(i)),array=array,_RC)
         !block
            !integer :: rank
            !real, pointer :: ptr2d(:,:),ptr3d(:,:,:)
            !call ESMF_ArrayGet(array,rank=rank,_RC)
            !if (rank==2) then
               !call ESMF_ArrayGet(array,farrayptr=ptr2d,_RC)
               !!write(*,*)trim(coll_name)," ",trim(fieldNames(i)),size(ptr2d,1),size(ptr2d,2)
               !!write(*,*)trim(coll_name)," ",trim(fieldNames(i)),local_rank,minval(ptr2d),maxval(ptr2d)
            !else if (rank==3) then
               !call ESMF_ArrayGet(array,farrayptr=ptr3d,_RC)
               !!write(*,*)trim(coll_name)," ",trim(fieldNames(i)),local_rank,minval(ptr3d),maxval(ptr3d)
            !end if
         !end block
         call ESMF_ArrayDestroy(array,noGarbage=.true.,_RC)
      enddo
      call ESMF_ArrayBundleDestroy(output_bundle,noGarbage=.true.,_RC)
      call ESMF_DistGridDestroy(dist_grid,noGarbage=.true.,_RC)
      
      call io_prof%stop('write_collection_'//ic)
      _RETURN(_SUCCESS)
   end subroutine write_collection

   function setup_transfer(this,rh,transfer_rank,rc) result(new_rh)
      class(Writer), intent(inout) :: this
      type(RHConnector), intent(in) :: rh
      integer, intent(in) :: transfer_rank
      integer, optional, intent(out) :: rc

      type(RHConnector) :: new_rh
      integer, allocatable :: front_pets(:),back_pets(:)
      
      integer :: status,front_size
      integer, allocatable :: originPetList(:),targetPetList(:) 

      front_pets = this%front_back_connection%get_front_pets()
      back_pets = this%front_back_connection%get_back_pets()

      front_size=size(front_pets)
      allocate(originPetList(front_size+1),targetPetList(front_size+1))
      
      originPetList(1:front_size)=front_pets
      targetPetList(1:front_size)=front_pets
      originPetList(front_size+1)=back_pets(1)
      targetPetList(front_size+1)=transfer_rank
      new_rh=rh%transfer_rh(originPetList,targetPetList,_RC)
      _RETURN(_SUCCESS)
   end function setup_transfer


end module AEIO_Writer

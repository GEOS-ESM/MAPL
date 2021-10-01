#include "MAPL_Generic.h"


module AEIO_Writer
   use MPI
   use ESMF
   use MAPL_BaseMod
   use CollectionMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   use AEIO_CollectionDescriptor
   use AEIO_CollectionDescriptorVector
   use AEIO_MpiConnection
   
   implicit none
   private

   public :: Writer

   type Writer
      type(MpiConnection) :: front_back_connection
      integer, allocatable :: writer_ranks(:)
      integer, allocatable :: server_ranks(:)
      type(CollectionDescriptorVector) :: collection_descriptors
      integer :: connector_comm
      integer :: writer_comm
   contains
      procedure :: start_writer
      procedure :: add_collection
      procedure :: create_server_writer_rh
      procedure :: write_collection
      procedure :: setup_transfer
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

   subroutine create_server_writer_rh(this,rc)
      class(Writer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: originPetList(:), targetPetList(:)

      _RETURN(_SUCCESS) 

   end subroutine create_server_writer_rh

   subroutine start_writer(this,rc)
      class(Writer), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer, parameter :: stag = 6782
      integer :: collection_id
      logical, allocatable :: busy(:)
      integer :: nwriters,free_worker,free,no_job,i,status,back_local_rank
      integer :: MPI_STAT(MPI_STATUS_SIZE)
      type(RHConnector) :: rh,rh_new
      type(CollectionDescriptor) :: collection_descriptor
      integer :: back_comm, connector_comm
      integer, allocatable :: writer_ranks(:),server_ranks(:),back_pets(:)

      back_comm = this%front_back_connection%get_back_comm()
      connector_comm = this%front_back_connection%get_connection_comm()
      writer_ranks = this%front_back_connection%get_back_mpi_ranks()
      server_ranks = this%front_back_connection%get_front_mpi_ranks()
      back_pets = this%front_back_connection%get_back_pets()

      call MPI_COMM_RANK(back_comm,back_local_rank,status)
      _VERIFY(status)
      nwriters = size(writer_ranks)-1
      allocate(busy(nwriters))
      busy = .false.
      if (this%front_back_connection%am_i_back_root()) then
         do while (.true.)
            call MPI_recv(collection_id, 1, MPI_INTEGER, &
            server_ranks(1),writer_ranks(1),connector_comm, &
            MPI_STAT, status)
            _VERIFY(status)
            if (collection_id >= 1) then
                free_worker = 0
                do i=1,nwriters
                   if (busy(i) .eqv. .false.) then
                      free_worker = i
                      exit
                   end if
                enddo

                if (free_worker ==0) then
                    call mpi_recv(free_worker,1, MPI_INTEGER, &
                         MPI_ANY_SOURCE,stag,back_comm, &
                         MPI_STAT, status)
                    _VERIFY(status)
                end if

                busy(free_worker) = .true.

                call MPI_send(back_pets(1+free_worker),1,MPI_INTEGER,  server_ranks(1), &
                     server_ranks(1),connector_comm,status)
                _VERIFY(status)
                call MPI_Send(collection_id,1,MPI_INTEGER, free_worker, &
                     free_worker,back_comm,status)
                _VERIFY(status)
                collection_descriptor=this%collection_descriptors%at(collection_id)
                rh=collection_descriptor%get_rh()
                rh_new = this%setup_transfer(rh,back_pets(1+free_worker),_RC)
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
                     if (free /= i) stop("free should be i")
                     call MPI_send(no_job,1,MPI_INTEGER,i,i,back_comm,status)
                     _VERIFY(status)
                  end if
               end do
               exit
            end if
         enddo
      else
         do while (.true.)
            ! which collection am I working on
            call MPI_Recv(collection_id,1,MPI_INTEGER, &
                         0,back_local_rank,back_comm, &
                         MPI_STAT,status)
            _VERIFY(status)
            if (collection_id < 0) exit
            ! do stuff
            call this%write_collection(collection_id,_RC)
            ! send back I am done
            call MPI_send(back_local_rank,1,MPI_INTEGER,0,stag,back_comm,status)
            _VERIFY(status)                      

         enddo
      end if
      _RETURN(_SUCCESS)

   end subroutine start_writer

   subroutine write_collection(this,collection_id,rc)
      class(Writer), intent(inout) :: this
      integer, intent(in) :: collection_id
      integer, optional, intent(out) :: rc

      type(RHConnector) :: rh,rh_new
      integer :: status,i,local_rank
      type(CollectionDescriptor) :: collection_descriptor
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

      back_pets = this%front_back_connection%get_back_pets()
      writer_comm = this%front_back_connection%get_back_comm()

      call MPI_COMM_RANK(writer_comm,local_rank,status)
      _VERIFY(status)
      collection_descriptor = this%collection_descriptors%at(collection_id)
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

      call ESMF_VMEpochEnter(epoch=ESMF_VMEPOCH_BUFFER)

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
               write(*,*)trim(coll_name)," ",trim(fieldNames(i)),local_rank,minval(ptr2d),maxval(ptr2d)
            else if (rank==3) then
               call ESMF_ArrayGet(array,farrayptr=ptr3d,_RC)
               write(*,*)trim(coll_name)," ",trim(fieldNames(i)),local_rank,minval(ptr3d),maxval(ptr3d)
            end if
         end block
      enddo
      
      call ESMF_VMEpochExit()

      call rh_new%destroy(_RC)
      do i=1,fieldCount
         call ESMF_ArrayBundleGet(output_bundle,trim(fieldNames(i)),array=array,_RC)
         call ESMF_ArrayDestroy(array,noGarbage=.true.,_RC)
      enddo
      call ESMF_ArrayBundleDestroy(output_bundle,noGarbage=.true.,_RC)
      
      _RETURN(_SUCCESS)
   end subroutine write_collection

   function setup_transfer(this,rh,transfer_rank,rc) result(new_rh)
      class(Writer), intent(inout) :: this
      type(RHConnector), intent(in) :: rh
      integer, intent(in) :: transfer_rank
      integer, optional, intent(out) :: rc

      type(RHConnector) :: new_rh
      integer, allocatable :: front_pets(:),back_pets(:)
      
      integer :: status,front_size,i
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

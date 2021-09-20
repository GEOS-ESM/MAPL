#include "MAPL_Generic.h"


module AEIO_Writer
   use MPI
   use ESMF
   use CollectionMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use AEIO_RHConnector
   use AEIO_CollectionDescriptor
   use AEIO_CollectionDescriptorMap
   
   implicit none
   private

   public :: Writer

   type Writer
      integer, allocatable :: writer_ranks(:)
      integer, allocatable :: server_ranks(:)
      type(CollectionDescriptorMap) :: collection_descriptor_map
      integer :: connector_comm
      integer :: writer_comm
   contains
      procedure :: start_writer
      procedure :: i_am_back_root
      procedure :: add_collection
   end type

   interface Writer
      module procedure new_Writer
   end interface Writer

contains

   function i_am_back_root(this) result(I_am_root)
      class(writer), intent(inout) :: this
      logical :: i_am_root
      integer :: rank, status
      call MPI_COMM_RANK(this%writer_comm,rank,status)
      i_am_root=(rank==0)
   end function

   function new_writer(server_ranks,writer_ranks,connector_comm,writer_comm,rc) result(c)
      integer, intent(in)          :: server_ranks(:)
      integer, intent(in)          :: writer_ranks(:)
      integer, intent(in)          :: connector_comm
      integer, intent(in)          :: writer_comm
      integer, optional, intent(out) :: rc
      type(writer) :: c
      integer :: status,myPet
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm,_RC)
      call ESMF_VMGet(vm,localPet=myPet,_RC)
      allocate(c%server_ranks,source=server_ranks)
      allocate(c%writer_ranks,source=writer_ranks)
      c%connector_comm=connector_comm
      c%writer_comm=writer_comm

   end function new_writer

   subroutine add_collection(this,coll_name,bundle,rh,rc)
      class(writer), intent(inout) :: this
      character(len=*), intent(in) :: coll_name
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(RHConnector), intent(in) :: rh
      integer, optional, intent(out) :: rc

      type(collectionDescriptor) :: collection_descriptor

      collection_descriptor = CollectionDescriptor(bundle,rh)
      call this%collection_descriptor_map%insert(coll_name,collection_descriptor)
      _RETURN(_SUCCESS)
   end subroutine add_collection

   subroutine start_writer(this,rc)
      class(Writer), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer, parameter :: stag = 6782
      integer :: collection_id
      logical, allocatable :: busy(:)
      integer :: nwriters,free_worker,free,no_job,i,status,back_local_rank
      integer :: MPI_STAT(MPI_STATUS_SIZE)

      call MPI_COMM_RANK(this%writer_comm,back_local_rank,status)
      _VERIFY(status)
      nwriters = size(this%writer_ranks)-1
      allocate(busy(nwriters))
      busy = .false.
      write(*,*)"Starting writer ",nwriters
      if (this%i_am_back_root()) then
         do while (.true.)
            call MPI_recv(collection_id, 1, MPI_INTEGER, &
            this%server_ranks(1),this%writer_ranks(1),this%connector_comm, &
            MPI_STAT, status)
            _VERIFY(status)
            if (collection_id >= 1) then
                free_worker = 0
                do i=1,nwriters-1
                   if (busy(i) .eqv. .false.) then
                      free_worker = i
                      exit
                   end if
                enddo
 
                if (free_worker ==0) then
                    call mpi_recv(free_worker,1, MPI_INTEGER, &
                         MPI_ANY_SOURCE,stag,this%writer_comm, &
                         MPI_STAT, status)
                    _VERIFY(status)
                end if

                busy(free_worker) = .true.

                call MPI_send(free_worker,1,MPI_INTEGER,  this%server_ranks(1), &
                     this%server_ranks(1),this%connector_comm,status)
                _VERIFY(status)
                call MPI_send(collection_id,1,MPI_INTEGER,free_worker,free_worker,this%writer_comm,status)
                _VERIFY(status)
            else
               no_job=-1
               do i=1,nwriters
                  if (.not.busy(i)) then
                     call MPI_send(no_job,1,MPI_INTEGER,i,i,this%writer_comm,status)
                     _VERIFY(status)
                  else
                     call MPI_recv(free,1,MPI_INTEGER, &
                                   i,stag,this%writer_comm, MPI_STAT,status)
                     _VERIFY(status)
                     if (free /= i) stop("free should be i")
                     call MPI_send(no_job,1,MPI_INTEGER,i,i,this%writer_comm,status)
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
                         0,back_local_rank,this%writer_comm, &
                         MPI_STAT,status)
            _VERIFY(status)
            if (collection_id < 0) exit
            ! do stuff

            ! send back I am done
            call MPI_send(back_local_rank,1,MPI_INTEGER,0,stag,this%writer_comm,status)
            _VERIFY(status)                      

         enddo
      end if
      _RETURN(_SUCCESS)

   end subroutine start_writer

end module AEIO_Writer

#include "MAPL_Generic.h"
module MAPL_ESMFFieldBundleWrite
   use ESMF
   use pFIO
   use MAPL_newCFIOMod
   use MAPL_TimeDataMod
   use MAPL_newCFIOitemVectorMod
   use MAPL_newCFIOitemMod
   use MAPL_ioClientsMod, only: io_client, o_Clients
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: FieldBundleWriter
   type :: FieldBundleWriter
      private
      type(MAPL_newCFIO) :: cfio
      character(:), allocatable :: file_name
      contains
         procedure :: create_from_bundle
         procedure :: write_to_file
   end type
   
   contains

      subroutine create_from_bundle(this,bundle,clock,output_file,n_steps,rc)
         class(FieldBundleWRiter), intent(inout) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Clock), intent(inout) :: clock
         character(len=*), intent(in) :: output_file
         integer, optional, intent(in)  :: n_steps
         integer, optional, intent(out) :: rc

         type(TimeData) :: time_info
         integer :: num_fields,i,file_steps,clock_frequency,collection_id,status
         character(ESMF_MAXSTR), allocatable :: field_names(:)
         type(newCFIOItemVector) :: items
         type(newCFIOItem) :: item
         type(ESMF_TimeInterval) :: time_interval,clock_interval
       
         call ESMF_ClockGet(clock,timestep=clock_interval,rc=status)
         _VERIFY(status)
         call ESMF_TimeIntervalGet(clock_interval,s=clock_frequency,rc=status)
         _VERIFY(status)
         call ESMF_TimeIntervalSet(time_interval,s=0,rc=status)
         _VERIFY(status)
         if (present(n_steps)) then
            file_steps = n_steps
         else
            file_steps = 1
         end if

         time_info = TimeData(clock,file_steps,clock_frequency,time_interval)
         call ESMF_FieldBundleGet(bundle, fieldCount=num_fields,rc=status)
         _VERIFY(status)
         allocate(field_names(num_fields),stat=status)
         _VERIFY(status)
         call ESMF_FieldBundleGet(bundle, fieldNameList=field_names,rc=status)
         _VERIFY(status)
         do i=1,num_fields
            item%itemType=ItemTypeScalar
            item%xname = trim(field_names(i))
            call items%push_back(item)
         enddo
         call this%cfio%createFileMetadata(items,bundle,time_info,rc=status)
         _VERIFY(status) 
         this%file_name = output_file
         collection_id = o_clients%add_hist_collection(this%cfio%metadata)
         call this%cfio%set_param(write_collection_id=collection_id)
         _RETURN(_SUCCESS)

      end subroutine create_from_bundle

      subroutine write_to_file(this,rc)
         class(FieldBundleWriter), intent(inout) :: this
         integer, optional, intent(out) :: rc
         
         integer :: status

         call this%cfio%bundlepost(this%file_name,oClients=o_clients,rc=status)
         _VERIFY(status)
         call o_Clients%done_collective_stage()
         call o_Clients%wait()

      end subroutine write_to_file

end module MAPL_ESMFFieldBundleWrite

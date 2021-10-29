#include "MAPL_Generic.h"
module MAPL_ESMFFieldBundleWrite
   use ESMF
   use pFIO
   use MAPL_GriddedIOMod
   use MAPL_TimeDataMod
   use MAPL_GriddedIOitemVectorMod
   use MAPL_GriddedIOitemMod
   use pFIO_ClientManagerMod, only: o_Clients
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: FieldBundleWriter
   public :: MAPL_write_bundle
   type :: FieldBundleWriter
      private
      type(MAPL_GriddedIO) :: cfio
      character(:), allocatable :: file_name
      contains
         procedure :: create_from_bundle
         procedure :: write_to_file
         procedure :: start_new_file
   end type
   interface MAPL_Write_Bundle
      module procedure Write_bundle_single_time
   end interface
   
   contains
      
      subroutine write_bundle_single_time(bundle,clock,output_file,nbits,deflate,rc)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Clock), intent(inout) :: clock
         character(len=*), intent(in) :: output_file
         integer, optional, intent(in)  :: nbits
         integer, optional, intent(in)  :: deflate
         integer, optional, intent(out) :: rc

         integer :: status

         type(FieldBundleWriter) :: newWriter

         call newWriter%create_from_bundle(bundle,clock,output_file=output_File,n_steps=1,time_interval=0,nbits=nbits,deflate=deflate,rc=status)
         _VERIFY(status)
         call newWriter%write_to_file(rc=status)
         _VERIFY(status)
         _RETURN(_SUCCESS)
      end subroutine write_bundle_single_time

      subroutine create_from_bundle(this,bundle,clock,output_file,n_steps,time_interval,nbits,deflate,rc)
         class(FieldBundleWRiter), intent(inout) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Clock), intent(inout) :: clock
         character(len=*), optional, intent(in) :: output_file
         integer, optional, intent(in)  :: n_steps
         integer, optional, intent(in)  :: time_interval
         integer, optional, intent(in)  :: nbits
         integer, optional, intent(in)  :: deflate
         integer, optional, intent(out) :: rc

         type(TimeData) :: time_info
         integer :: num_fields,i,file_steps,collection_id,status
         character(ESMF_MAXSTR), allocatable :: field_names(:)
         type(GriddedIOItemVector) :: items
         type(GriddedIOItem) :: item
         type(ESMF_TimeInterval) :: offset
         integer :: time_interval_
       
         call ESMF_TimeIntervalSet(offset,s=0,rc=status)
         _VERIFY(status)
         if (present(n_steps)) then
            file_steps = n_steps
         else
            file_steps = 1
         end if
         if (present(time_interval)) then
            time_interval_=time_interval
         else
            time_interval_=0
         end if

         call this%cfio%set_param(nbits=nbits,deflation=deflate)
         time_info = TimeData(clock,file_steps,time_interval_,offset)
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
         if (present(output_file)) this%file_name = output_file
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

      subroutine start_new_file(this,filename,rc)
         class(fieldBundleWriter),intent(inout) :: this
         character(len=*), intent(in) :: filename
         integer, optional, intent(out) :: rc

         integer :: status

         this%file_name=filename
         call this%cfio%modifyTime(oClients=o_clients,_RC)
         _RETURN(_SUCCESS)
      end subroutine start_new_file

end module MAPL_ESMFFieldBundleWrite

#include "MAPL.h"
module mapl3g_fieldbundle_writer
   use ESMF
   use pFIO
   use MAPL_TimeDataMod
   use MAPL_VerticalDataMod
   use pFIO_ClientManagerMod, only: o_Clients
   use MAPL_ExceptionHandling
   use mapl3g_GeomPFIO
   use mapl3g_SharedIO
   use mapl3g_GeomCatagorizer
   implicit none
   private

   public :: FieldBundleWriter
   public :: MAPL_write_bundle
   type :: FieldBundleWriter
      private
      class(GeomPFIO), allocatable :: writer
      character(:), allocatable :: file_name
      type(ESMF_Time) :: initial_time
      real, allocatable :: file_times(:)
      contains
         procedure :: create_from_bundle
         procedure :: write_to_file
         procedure :: start_new_file
   end type
   interface MAPL_Write_Bundle
      module procedure Write_bundle_single_time
   end interface

   contains

      subroutine write_bundle_single_time(bundle,clock,output_file,nbits_to_keep,deflate,quantize_algorithm,quantize_level,zstandard_level,rc)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Clock), intent(inout) :: clock
         character(len=*), intent(in) :: output_file
         integer, optional, intent(in)  :: nbits_to_keep
         integer, optional, intent(in)  :: deflate
         integer, optional, intent(in)  :: quantize_algorithm
         integer, optional, intent(in)  :: quantize_level
         integer, optional, intent(in)  :: zstandard_level
         integer, optional, intent(out) :: rc

         integer :: status

         type(FieldBundleWriter) :: newWriter
         type(ESMF_Time) :: time

         call ESMF_ClockGet(clock, currTime=time, _RC)
         !call newWriter%create_from_bundle(bundle,clock,output_file=output_File,n_steps=1,time_interval=0,nbits_to_keep=nbits_to_keep,deflate=deflate,quantize_algorithm=quantize_algorithm,quantize_level=quantize_level,zstandard_level=zstandard_level,rc=status)
         call newWriter%create_from_bundle(bundle,clock,output_file=output_File,nbits_to_keep=nbits_to_keep,deflate=deflate,quantize_algorithm=quantize_algorithm,quantize_level=quantize_level,zstandard_level=zstandard_level,rc=status)
         _VERIFY(status)
         call newWriter%start_new_file(bundle, output_File, time, _RC)
         call newWriter%write_to_file(bundle, time, _RC)
         _RETURN(_SUCCESS)
      end subroutine write_bundle_single_time

      subroutine create_from_bundle(this,bundle,clock,output_file,nbits_to_keep,deflate,quantize_algorithm,quantize_level,zstandard_level,rc)
         class(FieldBundleWRiter), intent(inout) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Clock), intent(inout) :: clock
         character(len=*), optional, intent(in) :: output_file
         integer, optional, intent(in)  :: nbits_to_keep
         integer, optional, intent(in)  :: deflate
         integer, optional, intent(in)  :: quantize_algorithm
         integer, optional, intent(in)  :: quantize_level
         integer, optional, intent(in)  :: zstandard_level
         integer, optional, intent(out) :: rc

         type(TimeData) :: time_info
         integer :: num_fields,i,file_steps,collection_id,status
         type(ESMF_Geom) :: geom
         type(FileMetadata) :: metadata


         call ESMF_FieldBundleGet(bundle, geom=geom, _RC)
         metadata = bundle_to_metadata(bundle, geom, _RC)
         allocate(this%writer, source=make_geom_pfio(metadata,rc=status))
         _VERIFY(status)
         call this%writer%initialize(metadata, geom, _RC)

         !call this%cfio%set_param(nbits_to_keep=nbits_to_keep,deflation=deflate,quantize_algorithm=quantize_algorithm,quantize_level=quantize_level,zstandard_level=zstandard_level)
         !if (present(output_file)) this%file_name = output_file
         !collection_id = o_clients%add_data_collection(this%cfio%metadata)
         !call this%cfio%set_param(write_collection_id=collection_id)
         _RETURN(_SUCCESS)

      end subroutine create_from_bundle

      subroutine write_to_file(this,bundle, time, rc)
         class(FieldBundleWriter), intent(inout) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(ESMF_Time), intent(inout) :: time
         integer, optional, intent(out) :: rc

         integer :: status, time_index, old_size
         real, allocatable :: file_times(:)
         type(ESMF_TimeInterval) :: time_interval
         real(kind=ESMF_KIND_R8) :: real_time_interval

         old_size = size(this%file_times) 
         allocate(file_times, source=this%file_times, _STAT)
         deallocate(this%file_times)
         allocate(this%file_times(old_size+1), _STAT)
         time_index = size(this%file_times)
         this%file_times(1:old_size) = file_times
         time_interval = time-this%initial_time
         call ESMF_TimeIntervalGet(time_interval, m_r8=real_time_interval, _RC)
         this%file_times(time_index) = real_time_interval

         time_index = size(this%file_times)
         call this%writer%stage_time_to_file(this%file_name, this%file_times, _RC)
         call this%writer%stage_data_to_file(bundle, this%file_name, time_index, _RC)

         call o_Clients%done_collective_stage()
         call o_Clients%post_wait()
         _VERIFY(status)
         _RETURN(_SUCCESS)

      end subroutine write_to_file

      subroutine start_new_file(this, bundle, filename, time, rc)
         class(fieldBundleWriter),intent(inout) :: this
         type(ESMF_FieldBundle), intent(inout) :: bundle
         character(len=*), intent(in) :: filename
         type(ESMF_Time), intent(in) :: time
         integer, optional, intent(out) :: rc

         integer :: status

         allocate(this%file_times(0), _STAT)
         this%file_name=filename
         this%initial_time=time
         call this%writer%stage_coordinates_to_file(filename, _RC)
         call this%writer%update_time_on_server(time, _RC)
         _RETURN(_SUCCESS)
      end subroutine start_new_file

end module mapl3g_FieldBundle_Writer

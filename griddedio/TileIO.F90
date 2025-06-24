#include "MAPL_Generic.h"

module MAPL_TileIOMod
   use ESMF
   use pFIO
   use MAPL_BaseMod
   use MAPL_ExceptionHandling
   use MAPL_CommsMod
   use FIleIOSharedMod, only: MAPL_TileMaskGet

   implicit none

   private

   type tile_buffer
      real, allocatable :: ptr(:)
   end type

   type, public :: MAPL_TileIO
     private
      type(ESMF_FieldBundle) :: bundle
      integer :: read_collection_id
      type(tile_buffer), allocatable :: tile_buffer(:)
      contains
         procedure :: request_data_from_file
         procedure :: process_data_from_file
   end type MAPL_TileIO

   interface MAPL_TileIO
      module procedure new_MAPL_TileIO
   end interface MAPL_TileIO

   contains

      function new_MAPL_TileIO(bundle,read_collection_id) result(TileIO)
         type(MAPL_TileIO) :: TileIO
         type(ESMF_FieldBundle),intent(in) :: bundle
         integer, intent(in) :: read_collection_id

         TileIO%bundle = bundle
         TileIO%read_collection_id = read_collection_id
      end function

      subroutine request_data_from_file(this,filename,timeindex,rc)
         class(MAPL_TileIO), intent(inout) :: this
         character(len=*), intent(in) :: filename
         integer, intent(in) :: timeindex
         integer, intent(out), optional :: rc

         integer :: status
         integer :: num_vars,i,rank
         type(ArrayReference) :: ref
         character(len=ESMF_MAXSTR), allocatable :: names(:)
         type(ESMF_Field) :: field
         type(ESMF_Grid) :: grid
         integer :: counts(3)
         integer, allocatable :: local_start(:), global_start(:), global_count(:)


         call ESMF_FieldBundleGet(this%bundle, fieldCount=num_vars, _rc)
         allocate(this%tile_buffer(num_vars))
         allocate(names(num_vars))
         call ESMF_FieldBundleGet(this%bundle, fieldNameList=names, _rc)
         do i=1,num_vars
            call ESMF_FieldBundleGet(this%bundle,names(i),field=field,_rc)
            call ESMF_FieldGet(field,rank=rank,grid=grid,_rc)
            call MAPL_GridGet(grid,globalCellCountPerDim=counts,_rc)
            if (rank==1) then
               allocate(local_start(2),global_start(2),global_count(2))
               local_start = [1,timeindex]
               global_start = [1,timeindex]
               global_count = [counts(1),1]
               if (mapl_am_I_root()) then
                  allocate(this%tile_buffer(i)%ptr(counts(1)),_stat)
               else
                  allocate(this%tile_buffer(i)%ptr((0)),_stat)
               end if
               ref = ArrayReference(this%tile_buffer(i)%ptr)
               call i_clients%collective_prefetch_data(this%read_collection_id, filename, trim(names(i)), ref,  &
                  start=local_start, global_start=global_start, global_count = global_count, _rc)
               deallocate(local_start,global_start,global_count)
            else
               _fail("rank >1 tile fields not supported")
            end if
         end do

         _return(_success)
      end subroutine

      subroutine process_data_from_file(this,rc)
         class(MAPL_TileIO), intent(inout) :: this
         integer, intent(out), optional :: rc

         integer :: status
         integer :: i,num_vars,rank
         type(ESMF_Field) :: field
         character(len=ESMF_MAXSTR), allocatable :: names(:)
         type(ESMF_Grid) :: grid
         integer, pointer :: mask(:)
         real, pointer :: ptr1d(:)

         call ESMF_FieldBundleGet(this%bundle, fieldCount=num_vars, _rc)
         allocate(names(num_vars))
         call ESMF_FieldBundleGet(this%bundle, fieldNameList=names, _rc)
         do i=1,num_vars
            call ESMF_FieldBundleGet(this%bundle,names(i),field=field,_rc)
            call ESMF_FieldGet(field,rank=rank,grid=grid,_rc)
            call MAPL_TileMaskGet(grid,mask,_rc)
            if (rank==1) then
               call ESMF_FieldGet(field,localDE=0,farrayPtr=ptr1d,_rc)
               call ArrayScatter(ptr1d,this%tile_buffer(i)%ptr,grid,mask=mask,_rc)
               deallocate(this%tile_buffer(i)%ptr)
            else
               _fail("rank not supported for tile io")
            end if
         enddo
         deallocate(this%tile_buffer)
         _return(_success)
      end subroutine

end module

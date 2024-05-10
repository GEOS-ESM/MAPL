module NonGridIOMod
  use ESMF
  use MAPL_GriddedIOItemVectorMod
  use MAPL_FileMetadataUtilsMod
  implicit none
  private

  public :: nongridio
  type :: nongridio
     real :: x
     integer :: pet_select
     integer :: np
     integer :: count
     character(len=ESMF_MAXSTR) :: index_name_x
     type(ESMF_FieldBundle) :: input_bundle
     type(GriddedIOitemVector) :: items     
     type(FileMetaData), allocatable :: metadata
   contains
     procedure :: add_metadata
     procedure :: append_file
     procedure :: create_metadata_variable

  end type nongridio

  interface nongridio
     module procedure new_nongridio
  end interface nongridio


  interface
     module function new_nongridio(items, bundle, rc) result(ngio)
       type(nongridio) :: ngio
       type(ESMF_FieldBundle), intent(in)   :: bundle
       type(GriddedIOitemVector), intent(inout) :: items
       integer, optional, intent(out) :: rc
     end function new_nongridio
     
     module subroutine restart(this,rc)
       class(nongridio), intent(inout) :: this
       integer, optional, intent(out)  :: rc
     end subroutine restart

     module subroutine add_metadata(this,vname,rc)
       class(nongridio), intent(inout) :: this
       character(len=*), optional, intent(in)    :: vname
       integer, optional, intent(out)  :: rc
     end subroutine add_metadata

     module subroutine append_file(this,current_time,rc)
       class(nongridio), intent(inout) :: this
       type(ESMF_Time), intent(inout)  :: current_time
       integer, optional, intent(out)  :: rc
     end subroutine append_file

     module subroutine  create_metadata_variable(this,vname,rc)
       class(nongridio), intent(inout) :: this
       character(len=*), intent(in)    :: vname
       integer, optional, intent(out)  :: rc
     end subroutine create_metadata_variable
     
  end interface
end module NonGridIOMod

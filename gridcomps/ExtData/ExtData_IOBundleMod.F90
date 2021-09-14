!#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------

module MAPL_ExtData_IOBundleMod
  use ESMF
  use MAPL_BaseMod
  use MAPL_newCFIOMod
  use MAPL_ExceptionHandling
  use MAPL_newCFIOItemMod
  use MAPL_newCFIOItemVectorMod

  public :: ExtData_IoBundle

  type ExtData_IoBundle
     type (MAPL_newCFIO) :: cfio
     type (ESMF_FieldBundle) :: pbundle
     character(:), allocatable :: template
     integer :: regrid_method
     
     integer :: bracket_side
     integer :: entry_index
     character(:), allocatable :: file_name
     integer :: time_index
     integer :: fraction
     integer :: metadata_coll_id
     integer :: server_coll_id
     type(newCFIOItemVector) :: items
     
   contains
     
     procedure :: clean
     procedure :: make_cfio
     procedure :: assign
     generic :: assignment(=) => assign
  end type ExtData_IoBundle
  

  interface ExtData_IoBundle
     module procedure new_ExtData_IoBundle
  end interface ExtData_IoBundle

contains

  function new_ExtData_IoBundle(bracket_side, entry_index, file_name, time_index, regrid_method, fraction, template, metadata_coll_id,server_coll_id,items,rc) result(io_bundle)
    type (ExtData_IoBundle) :: io_bundle

    integer, intent(in) :: bracket_side
    integer, intent(in) :: entry_index
    character(len=*), intent(in) :: file_name
    integer, intent(in) :: time_index
    integer, intent(in) :: regrid_method
    integer, intent(in) :: fraction
    character(len=*), intent(in) :: template
    integer, intent(in) :: metadata_coll_id
    integer, intent(in) :: server_coll_id
    type(newCFIOItemVector) :: items
    integer, optional, intent(out) :: rc

    io_bundle%bracket_side = bracket_side
    io_bundle%entry_index = entry_index
    io_bundle%file_name = file_name
    io_bundle%time_index = time_index
    io_bundle%regrid_method = regrid_method
    io_bundle%fraction = fraction
    io_bundle%template = trim(template)

    io_bundle%metadata_coll_id=metadata_coll_id
    io_bundle%server_coll_id=server_coll_id
    io_bundle%items=items

    _RETURN(ESMF_SUCCESS)
  end function new_ExtData_IoBundle


  subroutine clean(this, rc)
    class (ExtData_IoBundle), intent(inout) :: this
    integer, optional, intent(out) :: rc

    integer :: status
    call ESMF_FieldBundleDestroy(this%pbundle, noGarbage=.true.,rc=status)
    _VERIFY(status)
    
     _RETURN(ESMF_SUCCESS)

  end subroutine clean


  subroutine make_cfio(this, rc)
    class (ExtData_IoBundle), intent(inout) :: this
    integer, optional, intent(out) :: rc

     this%cfio = MAPL_NewCFIO(output_bundle=this%pbundle,regrid_method=this%regrid_method, &
                        read_collection_id=this%server_coll_id, &
                        metadata_collection_id = this%metadata_coll_id, fraction = this%fraction, &
                        items=this%items)

     _RETURN(ESMF_SUCCESS)

   end subroutine make_cfio

   subroutine assign(to,from)
      class(ExtData_IOBundle), intent(out) :: to
      type(ExtData_IOBundle), intent(in) :: from
    
    to%bracket_side = from%bracket_side
    to%entry_index = from%entry_index
    to%file_name = from%file_name
    to%time_index = from%time_index
    to%regrid_method = from%regrid_method
    to%fraction = from%fraction
    to%template = from%template

    to%metadata_coll_id=from%metadata_coll_id
    to%server_coll_id=from%server_coll_id
    to%items=from%items 
    to%pbundle=from%pbundle 
    to%CFIO=from%CFIO 
 
   end subroutine assign

end module MAPL_ExtData_IOBundleMod


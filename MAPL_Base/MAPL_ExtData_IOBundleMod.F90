!#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------

module MAPL_ExtData_IOBundleMod
  use ESMF
  use MAPL_BaseMod
  use MAPL_CFIOMod
  use MAPL_ErrorHandlingMod

  public :: ExtData_IoBundle

  type ExtData_IoBundle
     type (MAPL_CFIO) :: cfio
     type (ESMF_FieldBundle) :: pbundle
     character(:), allocatable :: template
     integer :: regrid_method
     
     integer :: bracket_side
     integer :: entry_index
     character(:), allocatable :: file_name
     integer :: time_index
     integer :: fraction
     logical :: distributed_trans = .false.
     logical :: parallel_skip
     
   contains
     
     procedure :: clean
     procedure :: make_cfio
  end type ExtData_IoBundle
  

  interface ExtData_IoBundle
     module procedure new_ExtData_IoBundle
  end interface ExtData_IoBundle

contains

  function new_ExtData_IoBundle(bracket_side, entry_index, file_name, time_index, regrid_method, fraction, template, rc) result(io_bundle)
    type (ExtData_IoBundle) :: io_bundle

     __Iam__('new_ExtData_IoBundle')

    integer, intent(in) :: bracket_side
    integer, intent(in) :: entry_index
    character(len=*), intent(in) :: file_name
    integer, intent(in) :: time_index
    integer, intent(in) :: regrid_method
    integer, intent(in) :: fraction
    character(len=*), intent(in) :: template
    integer, optional, intent(out) :: rc

    io_bundle%bracket_side = bracket_side
    io_bundle%entry_index = entry_index
    io_bundle%file_name = file_name
    io_bundle%time_index = time_index
    io_bundle%regrid_method = regrid_method
    io_bundle%fraction = fraction
    io_bundle%template = trim(template)

    io_bundle%parallel_skip = .false.

    _RETURN(ESMF_SUCCESS)
  end function new_ExtData_IoBundle


  subroutine clean(this, rc)
    class (ExtData_IoBundle), intent(inout) :: this
    integer, optional, intent(out) :: rc

     __Iam__('clean')
    call MAPL_CFIOSet(this%cfio, root=-1, __RC__)
    call MAPL_CFIODestroy(this%cfio, __RC__)
    call ESMF_FieldBundleDestroy(this%pbundle, __RC__)
    
     _RETURN(ESMF_SUCCESS)

  end subroutine clean


  subroutine make_cfio(this, rc)
    use MAPL_RegridderSpecMod
    class (ExtData_IoBundle), intent(inout) :: this
    integer, optional, intent(out) :: rc

     __Iam__('make_cfio')

     call MAPL_CFIOCreateFromFile(this%cfio, this%pbundle, regridMethod=this%regrid_method, __RC__)
     if (this%regrid_method==REGRID_METHOD_FRACTION) call MAPL_CFIOSet(this%cfio,fraction=this%fraction,__RC__)

     _RETURN(ESMF_SUCCESS)

   end subroutine make_cfio

end module MAPL_ExtData_IOBundleMod


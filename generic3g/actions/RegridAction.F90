#include "MAPL_Generic.h"

module mapl3g_RegridAction

   use mapl3g_ExtensionAction
   use mapl3g_regridder_mgr
   use mapl_ErrorHandling
   use mapl3g_FieldDictionary
   use esmf
   use nuopc

   implicit none
   private

   public :: RegridAction

   type, extends(ExtensionAction) :: ScalarRegridAction
      class(Regridder), pointer :: regrdr
      type(ESMF_Field) :: f_src, f_dst
   contains
      procedure :: run => run_scalar
   end type ScalarRegridAction

   ! type, extends(AbstractAction) :: VectorRegridAction
   !    class(AbstractRegridder), pointer :: regridder
   !    type(ESMF_Field) :: uv_src(2), uv_dst(2)
   ! contains
   !    procedure :: run
   ! end type VectorRegridAction

   interface RegridAction
      module procedure :: new_ScalarRegridAction
      ! module procedure :: new_RegridAction_vector
      ! module procedure :: new_RegridAction_bundle
   end interface RegridAction

contains

   function new_ScalarRegridAction( &
        stdname_src, geom_src, f_src, param_src, &
        stdname_dst, geom_dst, f_dst, param_dst, rc) result (action)
      type(ScalarRegridAction) :: action
      character(:), allocatable, intent(in) :: stdname_src
      type(ESMF_Geom), intent(in) :: geom_src
      type(ESMF_Field), intent(in) :: f_src
      type(EsmfRegridderParam), allocatable, intent(in) :: param_src
      character(:), allocatable, intent(in) :: stdname_dst
      type(ESMF_Geom), intent(in) :: geom_dst
      type(ESMF_Field), intent(in) :: f_dst
      type(EsmfRegridderParam), allocatable, intent(in) :: param_dst
      integer, optional, intent(out) :: rc

      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      type(EsmfRegridderParam) :: regrid_param
      integer :: status

      regridder_manager => get_regridder_manager()
      regrid_param = choose_regrid_param_(stdname_src, param_src, stdname_dst, param_dst, _RC)
      spec = RegridderSpec(regrid_param, geom_src, geom_dst)
      action%regrdr => regridder_manager%get_regridder(spec, rc=status)

      action%f_src = f_src
      action%f_dst = f_dst

   end function new_ScalarRegridAction

   ! function new_RegridAction_vector(uv_src, uv_dst) then (action)
   !    use mapl_RegridderManager

   !    ptype(ESMF_Grid) :: grid_src, grid_dst

   !    action%uv_src = uv_src
   !    action%uv_dst = uv_dst

   !    get_grid(grid_src)
   !    get_grid(grid_dst)
   !    action%regridder => regridder_manager%get_regridder(grid_src, grid_dst)

   ! end function new_RegridAction_scalar


   subroutine run_scalar(this, rc)
      class(ScalarRegridAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: f_src, f_dst
      integer :: status

      call this%regrdr%regrid(this%f_src, this%f_dst, _RC)
      _RETURN(_SUCCESS)
   end subroutine run_scalar

   ! subroutine run_vector(this, importState, exporState)

   !    call get_pointer(importState, fname_src_u, f_src(1))
   !    call get_pointer(importState, fname_src_v, f_src(2)
   !    call get_pointer(exportState, fname_dst_u, f_dst(1))
   !    call get_pointer(exportState, fname_dst_v, f_dst(2))

   !    call regridder%regrid(f_src(:), f_dst(:), _RC)

   ! end subroutine run

   ! subroutine run_bundle(this)

   !    call this%regridder%regrid(this%b_src, this%b_dst, _RC)

   ! end subroutine run_bundle

   function choose_regrid_param_(stdname_src, param_src, stdname_dst, param_dst, rc) result(param)
      character(:), allocatable, intent(in) :: stdname_src
      type(EsmfRegridderParam), allocatable, intent(in) :: param_src
      character(:), allocatable, intent(in) :: stdname_dst
      type(EsmfRegridderParam), allocatable, intent(in) :: param_dst
      integer, optional, intent(out) :: rc
      type(EsmfRegridderParam) :: param ! result

      type(EsmfRegridderParam), allocatable :: tmp_param
      integer :: status

      tmp_param = choose_regrid_param_2_(param_src, param_dst, _RC)
      ! One or both of param_src/dst are specified
      if (allocated(tmp_param)) then
         param = tmp_param
         _RETURN(_SUCCESS)
      end if

      ! If none of param_src/dst are specified
      ! Step 1: Generate param from regridding method in field dictionary
      tmp_param = get_regrid_param_from_field_dictionary_(stdname_src, stdname_dst, _RC)
      if (allocated(tmp_param)) then
         param = tmp_param
         _RETURN(_SUCCESS)
      end if

      ! If none of param_src/dst are specified
      ! Step 2: Generate param from default regridding method
      param = EsmfRegridderParam()

      _RETURN(_SUCCESS)
   end function choose_regrid_param_

   function get_regrid_param_from_field_dictionary_(stdname_src, stdname_dst, rc) result(param)
      character(len=*), intent(in) :: stdname_src
      character(len=*), intent(in) :: stdname_dst
      integer, optional, intent(out) :: rc
      type(EsmfRegridderParam), allocatable :: param ! result
      
      character(len=*), parameter :: field_dictionary_yml = "field_dictionary.yml"
      type(FieldDictionary) :: field_dict
      type(ESMF_RegridMethod_Flag), allocatable :: regrid_method_src, regrid_method_dst
      logical :: file_exists
      integer :: status
      type(EsmfRegridderParam), allocatable :: tmp_param_src, tmp_param_dst

      inquire(file=trim(field_dictionary_yml), exist=file_exists)
      if (file_exists) then
         field_dict = FieldDictionary(filename=field_dictionary_yml, _RC)
         regrid_method_src = field_dict%get_regrid_method(stdname_src)
         regrid_method_dst = field_dict%get_regrid_method(stdname_dst)
      end if
      if (allocated(regrid_method_src)) then
         tmp_param_src = EsmfRegridderParam(regridmethod=regrid_method_src)
      end if
      if (allocated(regrid_method_dst)) then
         tmp_param_dst = EsmfRegridderParam(regridmethod=regrid_method_dst)
      end if
      param = choose_regrid_param_2_(tmp_param_src, tmp_param_dst, _RC)

      _HERE
      _RETURN(_SUCCESS) ! return unallocated param
   end function get_regrid_param_from_field_dictionary_

   function choose_regrid_param_2_(param_src, param_dst, rc) result(param)
      type(EsmfRegridderParam), allocatable, intent(in) :: param_src
      type(EsmfRegridderParam), allocatable, intent(in) :: param_dst
      integer, optional, intent(out) :: rc
      type(EsmfRegridderParam), allocatable :: param ! return value

      ! Exactly one of param_src/dst is specified
      if ((allocated(param_src)) .and. (.not. allocated(param_dst))) then
         allocate(param, source=param_src)
         _RETURN(_SUCCESS)
      end if
      if ((.not. allocated(param_src)) .and. (allocated(param_dst))) then
         allocate(param, source=param_dst)
         _RETURN(_SUCCESS)
      end if

      ! If both param_src/dst are specified, they need to be the same
      if ((allocated(param_src)) .and. (allocated(param_dst))) then
         _ASSERT(param_src == param_dst, "param_src /= param_dst")
         allocate(param, source=param_src)
         _RETURN(_SUCCESS)
      end if

      _HERE
      _RETURN(_SUCCESS) ! return unallocated param
   end function choose_regrid_param_2_

end module mapl3g_RegridAction

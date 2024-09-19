module oomph_FieldSpec
   use oomph_AbstractStateItemSpec
   use oomph_DimsSpec
   use oomph_CouplingSpec
   use ESMF, only: ESMF_TYPEKIND_FLAG
   implicit none
   private

   public :: FieldSpec

   type, extends(AbstractStateItemSpec) :: FieldSpec
      private
      character(:), allocatable :: standard_name
      type(DimsSpec) :: dims_spec
      type(ESMF_TYPEKIND_FLAG) :: typekind
      type(CouplingSpec) :: coupling_spec

!!$      ! Override default allocation behavior
!!$      logical :: do_not_allocate
!!$      logical :: always_allocate
!!$      
!!$      ! Uncategorized initialization aspects
!!$      integer :: restart
!!$      class(*), allocatable :: default_value
!!$
      
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec
   end interface FieldSpec

contains

   pure function new_FieldSpec(standard_name, dims_spec, typekind) result(field_spec)
      type(FieldSpec) :: field_spec
      character(*), intent(in) :: standard_name
      type(DimsSpec), intent(in) :: dims_spec
      type(ESMF_TYPEKIND_FLAG), intent(in) :: typekind

      field_spec%standard_name = standard_name
      field_spec%dims_spec = dims_spec
      field_spec%typekind = typekind

      field_spec%coupling_spec = CouplingSpec()
   end function new_FieldSpec

end module oomph_FieldSpec

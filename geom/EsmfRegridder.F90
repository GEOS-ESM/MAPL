module mapl3g_EsmfRegridder

   use mapl3g_FieldBLAS, only: FieldGEMV, FieldsAreConformable

   implicit none

   private

   public :: EsmfRegridder

   type :: EsmfRegridder
      type(ESMF_RouteHandle) :: routeHandle
   contains
      procedure(Regridder) :: regrid
   end type EsmfRegridder

contains
!wdb fixme This seems redundant. Why?  
   subroutine regrid_scalar1(this, x, y, rc)
      type(EsmfRegridder), intent(in) :: this
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc 

      conformable = FieldsAreConformable(x,y)

      call ESMF_FieldRegrid(src_field=x, dst_field=y, &
           routeHandle=this%routeHandle, &
           ... &
           )
      
   end subroutine regrid_scalar1
   
!wdb fixme This seems redundant. Why?  
   subroutine regrid_scalar2(this, x, y, )
      type(ESMF_Regrid), intent(inout) :: x(2), y(2)

      type(ESMF_Field) :: xyz(3)

      conformable = FieldsAreConformable(x,y)

      call GetBasis(x%grid)
      call FieldGEMV('N', this%a, 0.0, x, 1.0, y)

      do i = 1, 3
         call this%regrid(...)
      end do

      call FieldGEMV('T', ...)
      
   end subroutine regrid_scalar2
end module mapl3g_EsmfRegridder

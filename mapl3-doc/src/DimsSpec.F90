module mapl3g_DimsSpec
   use mapl3g_UngriddedDimSpec
   use mapl3g_VerticalStaggerLoc
   implicit none

   private

   public :: DimsSpec
   type :: DimsSpec
      type(VerticalStaggerLoc) :: vert_stagger_loc
      type(UngriddedDimSpec), allocatable :: ungridded_dim_specs(:)
      integer :: halo_width
   end type DimsSpec

   interface DimsSpec
      module procedure new_DimsSpec_vert
      module procedure new_DimsSpec_w_ungridded
      module procedure new_DimsSpec_w_halo
   end interface DimsSpec

contains


   pure function new_DimsSpec_vert(vert_stagger_loc) result(spec)
      type(DimsSpec) :: spec
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec) :: no_ungridded(0)
      spec = DimsSpec(vert_stagger_loc, ungridded_dim_specs=no_ungridded, halo_width=0)
   end function new_DimsSpec_vert


   pure function new_DimsSpec_simple(vert_stagger_loc) result(spec)
      type(DimsSpec) :: spec
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec) :: no_ungridded(0)
      spec = DimsSpec(vert_stagger_loc, ungridded_dim_specs=no_ungridded, halo_width=0)
   end function new_DimsSpec_simple


   pure function new_DimsSpec_w_ungridded(vert_stagger_loc, ungridded_dim_specs) result(spec)
      type(DimsSpec) :: spec
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec), intent(in) :: ungridded_dim_specs(:)
      spec = DimsSpec(vert_stagger_loc, ungridded_dim_specs, halo_width=0)
   end function new_DimsSpec_w_ungridded


   pure function new_DimsSpec_w_halo(vert_stagger_loc, ungridded_dim_specs, halo_width) result(spec)
      type(DimsSpec) :: spec
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec), intent(in) :: ungridded_dim_specs(:)
      integer, intent(in) :: halo_width

      spec%vert_stagger_loc = vert_stagger_loc
      spec%ungridded_dim_specs = ungridded_dim_specs
      spec%halo_width = halo_width

   end function new_DimsSpec_w_halo

end module mapl3g_DimsSpec


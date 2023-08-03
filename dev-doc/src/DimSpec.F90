module oomph_DimsSpec
   use oomph_UngriddedDimSpec
   use oomph_HorizontalStaggerLoc
   use oomph_VerticalStaggerLoc
   implicit none

   private

   public :: DimsSpec
   type :: DimsSpec
      type(HorizontalStaggerLoc) :: horz_stagger_loc  !  NONE, CENTER, TILE
      type(VerticalStaggerLoc) :: vert_stagger_loc
      type(UngriddedDimSpec), allocatable :: ungridded_dim_specs(:)
      integer :: halo_width
   end type DimsSpec

   interface DimsSpec
      module procedure new_DimsSpec_simple
      module procedure new_DimsSpec_w_ungridded
      module procedure new_DimsSpec_w_halo
   end interface DimsSpec

contains

   pure function new_DimsSpec_simple(horz_stagger_loc, vert_stagger_loc) result(spec)
      type(DimsSpec) :: spec
      type(HorizontalStaggerLoc), intent(in) :: horz_stagger_loc
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec) :: no_ungridded(0)
      spec = DimsSpec(horz_stagger_loc, vert_stagger_loc, ungridded_dim_specs=no_ungridded, halo_width=0)
   end function new_DimsSpec_simple


   pure function new_DimsSpec_w_ungridded(horz_stagger_loc, vert_stagger_loc, ungridded_dim_specs) result(spec)
      type(DimsSpec) :: spec
      type(HorizontalStaggerLoc), intent(in) :: horz_stagger_loc
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec), intent(in) :: ungridded_dim_specs(:)
      spec = DimsSpec(horz_stagger_loc, vert_stagger_loc, ungridded_dim_specs, halo_width=0)
   end function new_DimsSpec_w_ungridded


   pure function new_DimsSpec_w_halo(horz_stagger_loc, vert_stagger_loc, ungridded_dim_specs, halo_width) result(spec)
      type(DimsSpec) :: spec
      type(HorizontalStaggerLoc), intent(in) :: horz_stagger_loc
      type(VerticalStaggerLoc) , intent(in) :: vert_stagger_loc
      type(UngriddedDimSpec), intent(in) :: ungridded_dim_specs(:)
      integer, intent(in) :: halo_width
      spec%horz_stagger_loc = horz_stagger_loc
      spec%vert_stagger_loc = vert_stagger_loc
      spec%ungridded_dim_specs = ungridded_dim_specs
      spec%halo_width = halo_width
   end function new_DimsSpec_w_halo

end module oomph_DimsSpec


module mapl3g_VerticalDimSpec
   use mapl3g_UngriddedDimSpec
   implicit none
   private
  
   public :: VerticalDimSpec
  
   public :: V_STAGGER_LOC_NONE
   public :: V_STAGGER_LOC_EDGE
   public :: V_STAGGER_LOC_CENTER


   type, extends(UngriddedDimSpec) :: VerticalDimSpec
      private
      integer :: num_levels
      integer :: stagger
   contains
      procedure :: get_lbound
      procedure :: get_ubound
   end type VerticalDimSpec


   interface VerticalDimSpec
      module procedure new_VerticalDimSpec
   end interface VerticalDimSpec
  

   enum, bind(c)
      enumerator :: V_STAGGER_LOC_NONE = 1
      enumerator :: V_STAGGER_LOC_CENTER
      enumerator :: V_STAGGER_LOC_EDGE
   end enum

contains

         
   pure function new_VerticalDimSpec(num_levels, stagger) result(spec)
      type(VerticalDimSpec) :: spec
      integer, intent(in) :: num_levels
      integer, intent(in) :: stagger

      spec%num_levels = num_levels
      spec%stagger = stagger

      spec%UngriddedDimSpec = UngriddedDimSpec(name='levels', units='1', coordinates=spec%get_coordinates())
   end function New_VerticalDimSpec


   pure integer function get_lbound(this) result(lbound)
      class(VerticalDimSpec), intent(in) :: this

      select case (this%stagger)
      case (V_STAGGER_LOC_CENTER)
         lbound = 1
      case (V_STAGGER_LOC_EDGE)
         lbound = 0
      end select
         
   end function get_lbound


   pure integer function get_ubound(this) result(ubound)
      class(VerticalDimSpec), intent(in) :: this

      ubound = this%num_levels
         
   end function get_ubound

  
end module mapl3g_VerticalDimSpec

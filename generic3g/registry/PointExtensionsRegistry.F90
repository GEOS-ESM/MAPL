module mapl_PointExtensionsRegistry
   implicit none
   private
  
   public :: PointExtensionsRegistry
  
   type :: PointExtensionsRegistry
      private
      type(ConnPt_ConnPtVector_Map) :: map
   contains
      procedure :: add_point
      procedure :: add_extension
      ! helper
      procedure :: get_last_extension
      procedure :: get_vector
   end type PointExtensionsRegistry
  
contains

   function add_point(this, conn_pt) result(extension_pt)
      type(ConnectionPoint), pointer :: extension_pt
      class(PointExtensionsRegistry), target, intent(inout) :: this
      type(ConnectionPoint), intent(in) :: conn_pt

      type(ConnPtVector), pointer :: v


      _ASSERT(this%m%count(conn_pt) == 0, 'Simple connection points must precede extensions.')
      v => this%get_vector(conn_pt)
      call v%insert(conn_pt, ExtensionPoint(conn_pt))
      extension_pt => v%back()

   end function add_point

   function add_extension(this, conn_pt) result(extension_pt)
      type(ConnectionPoint), pointer :: extension_pt
      class(PointExtensionsRegistry), target, intent(inout) :: registry
      type(ConnectionPoint), target, intent(in) :: conn_pt

      type(ConnPtVector), pointer :: v
      
      v => this%get_vector(conn_pt)

      associate (base_pt => this%get_last_extension(conn_pt))
        call v%insert(base_pt)
      end associate

      extension_pt => v%back()


   end function add_extension

   function get_last_extension(this, conn_pt)
      type(ConnectionPoint), pointer :: extension_pt
      class(PointExtensionsRegistry), target, intent(inout) :: registry
      type(ConnectionPoint), target, intent(in) :: conn_pt

      type(ConnPtVector), pointer :: v
      
      v => this%get_vector(conn_pt)
      base_pt => v%back()
      if (v%size() == 0) base_pt => conn_pt

   end function get_last_extension

   ! Return vector associated with conn_pt in the map.  If it does not
   ! exist add an entry in the map.
   function get_vector(this, conn_pt) result(v)
      type(ConnPtVector), pointer :: v
      class(ConnectionPoint), target, intent(in) :: this
      type(ConnectionPoint), intent(in) :: conn_pt
      
      associate (m => this%map)
        call m%insert(conn_pt, ConnPtVector())
        v => m%of(conn_pt)
      end associate

   end function get_vector

end module mapl_PointExtensionsRegistry

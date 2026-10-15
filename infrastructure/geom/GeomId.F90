#include "MAPL.h"

module mapl_GeomId_mod

   implicit none
   private

   public :: GeomId
   public :: GeomIdManager
   public :: get_geom_id_manager
   public :: operator(==)
   public :: operator(/=)

   type :: GeomId
      private
      integer :: value = 0
   contains
      procedure :: is_assigned
      procedure :: get_value
   end type GeomId

   type :: GeomIdManager
      private
      integer :: counter = 0
   contains
      procedure :: get_next_geom_id
   end type GeomIdManager

   type(GeomIdManager), target :: geom_id_manager

   interface GeomId
      module procedure new_geom_id
   end interface GeomId

   interface operator(==)
      module procedure same_geom_id
   end interface operator(==)

   interface operator(/=)
      module procedure different_geom_id
   end interface operator(/=)

contains

   function new_geom_id(value) result(geom_id)
      type(GeomId) :: geom_id
      integer, optional, intent(in) :: value

      if (present(value)) geom_id%value = value
   end function new_geom_id

   logical function is_assigned(this)
      class(GeomId), intent(in) :: this

      is_assigned = this%value > 0
   end function is_assigned

   integer function get_value(this)
      class(GeomId), intent(in) :: this

      get_value = this%value
   end function get_value

   function get_geom_id_manager() result(manager)
      type(GeomIdManager), pointer :: manager

      manager => geom_id_manager
   end function get_geom_id_manager

   function get_next_geom_id(this) result(geom_id)
      type(GeomId) :: geom_id
      class(GeomIdManager), intent(inout) :: this

      this%counter = this%counter + 1
      geom_id = GeomId(this%counter)
   end function get_next_geom_id

   logical function same_geom_id(a, b)
      type(GeomId), intent(in) :: a
      type(GeomId), intent(in) :: b

      same_geom_id = a%value == b%value
   end function same_geom_id

   logical function different_geom_id(a, b)
      type(GeomId), intent(in) :: a
      type(GeomId), intent(in) :: b

      different_geom_id = a%value /= b%value
   end function different_geom_id

end module mapl_GeomId_mod

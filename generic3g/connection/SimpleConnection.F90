module mapl3g_SimpleConnection
   use mapl3g_ConnectionPt
   use mapl3g_Connection
   implicit none
   private

   public :: SimpleConnection
   public :: is_valid

!!$   public :: can_share_pointer

   type, extends(Connection) :: SimpleConnection
      private
      type(ConnectionPt) :: source
      type(ConnectionPt) :: destination
   contains
      procedure :: is_export_to_import
      procedure :: is_export_to_export
      procedure :: is_valid
      procedure :: is_sibling

      procedure :: get_source
      procedure :: get_destination
   end type SimpleConnection

   interface SimpleConnection
      module procedure :: new_SimpleConnection
   end interface SimpleConnection

contains

   function new_SimpleConnection(source, destination) result(this)
      type(SimpleConnection) :: this
      type(ConnectionPt), intent(in) :: source
      type(ConnectionPt), intent(in) :: destination

      this%source = source
      this%destination = destination

   end function new_SimpleConnection

   logical function is_export_to_import(this)
      class(SimpleConnection), intent(in) :: this

      is_export_to_import = ( &
           this%source%get_state_intent() == 'export' .and. &
           this%destination%get_state_intent() == 'import' )

   end function is_export_to_import

   ! NOTE: We include a src that is internal as also being an export
   ! in this case.
   logical function is_export_to_export(this)
      class(SimpleConnection), intent(in) :: this

      is_export_to_export = ( &
           any(this%source%get_state_intent() == ['export  ', 'internal']) .and. &
           this%destination%get_state_intent() == 'export' )

   end function is_export_to_export

   ! Only certain combinations of state intents are supported by MAPL.
   ! separate check must be performed elsewhere to ensure the
   ! connections are either sibling to sibling or parent to child, as
   ! component relationships are not available at this level.

   logical function is_valid(this)
      class(SimpleConnection), intent(in) :: this

      associate (intents => [character(len=len('internal')) :: this%source%get_state_intent(), this%destination%get_state_intent()])
        
        is_valid = any( [ &
             all( intents == ['export  ', 'import  '] ), &    ! E2I
             all( intents == ['export  ', 'export  '] ), &    ! E2E
             all( intents == ['internal', 'export  '] ), &    ! Z2E
             all( intents == ['import  ', 'import  '] )  &    ! I2I
             ])

      end associate
   end function is_valid

   ! Only sibling connections trigger allocation of exports.
   logical function is_sibling(this)
      class(SimpleConnection), intent(in) :: this

      character(:), allocatable :: src_intent, dst_intent

      src_intent = this%source%get_state_intent()
      dst_intent = this%destination%get_state_intent()
      is_sibling = (src_intent == 'export' .and. dst_intent == 'import')

   end function is_sibling

   function get_source(this) result(source)
      type(ConnectionPt) :: source
      class(SimpleConnection), intent(in) :: this
      source = this%source
   end function get_source

   function get_destination(this) result(destination)
      type(ConnectionPt) :: destination
      class(SimpleConnection), intent(in) :: this
      destination = this%destination
   end function get_destination

end module mapl3g_SimpleConnection

module mapl3g_ConnectionSpec
   use mapl3g_ConnectionPoint
   implicit none
   private

   public :: ConnectionSpec
   public :: is_valid
!!$   public :: can_share_pointer

   type :: ConnectionSpec
      type(ConnectionPoint) :: source
      type(ConnectionPoint) :: destination
   contains
      procedure :: is_export_to_import
      procedure :: is_valid
   end type ConnectionSpec


contains

   pure logical function is_export_to_import(this)
      class(ConnectionSpec), intent(in) :: this

      is_export_to_import = (this%source%state_intent == 'export' .and. this%destination%state_intent == 'import')

   end function is_export_to_import


   ! Only certain combinations of state intents are supported by MAPL.
   ! separate check must be performed elsewhere to ensure the
   ! connections are either sibling to sibling or parent to child, as
   ! component relationships are not available at this level.

   logical function is_valid(this)
      class(ConnectionSpec), intent(in) :: this

      associate (intents => [character(len=len('internal')) :: this%source%state_intent, this%destination%state_intent])
        
        is_valid = any( [ &
             all( intents == ['export  ', 'import  '] ), &    ! E2I
             all( intents == ['export  ', 'export  '] ), &    ! E2E
             all( intents == ['internal', 'export  '] ), &    ! Z2E
             all( intents == ['import  ', 'import  '] )  &    ! I2I
             ])

      end associate
   end function is_valid

end module mapl3g_ConnectionSpec

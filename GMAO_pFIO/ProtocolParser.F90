module pFIO_ProtocolParserMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_IntegerMessageMapMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_AddCollectionMessageMod
   use pFIO_CollectionIdMessageMod
   use pFIO_RequestIdMessageMod
   use pFIO_RequestDataMessageMod
   use pFIO_CollectiveRequestDataMessageMod
   use pFIO_WaitRequestDataMessageMod
   use pFIO_DummyMessageMod

   implicit none
   private

   public :: ProtocolParser

   type :: ProtocolParser
      private
      type (IntegerMessageMap) :: prototypes
      logical :: initialized = .false.
   contains
      procedure :: initialize
      procedure :: make_message
      procedure :: decode
      procedure :: encode
   end type ProtocolParser

   interface ProtocolParser
      module procedure new_ProtocolParser
   end interface ProtocolParser

contains

   function new_ProtocolParser() result(parser)
      type (ProtocolParser) :: parser

      call parser%initialize()

   end function new_ProtocolParser

   subroutine initialize(this)
      class (ProtocolParser), intent(inout) :: this

      type (TerminateMessage) :: terminate
      type (DoneMessage) :: done
      type (AddCollectionMessage) :: addCollection
      type (CollectionIdMessage):: collectionId
      type (RequestIdMessage) :: requestId
      type (RequestDataMessage) :: RequestData
      type (WaitRequestDataMessage) :: WaitRequestData
      type (CollectiveRequestDataMessage) :: CollectiveRequestData
      type (DummyMessage) :: dummy

      if (this%initialized) return

      call add_prototype(terminate)
      call add_prototype(done)
      call add_prototype(addCollection)
      call add_prototype(collectionId)
      call add_prototype(requestId)
      call add_prototype(RequestData)
      call add_prototype(WaitRequestData)
      call add_prototype(CollectiveRequestData)
      call add_prototype(dummy)

      this%initialized = .true.

   contains

      subroutine add_prototype(message)
         class (AbstractMessage), intent(in) :: message

         call this%prototypes%insert(message%get_type_id(), message)
         
      end subroutine add_prototype

   end subroutine initialize


   function make_message(this, type_id) result(message)
      class (AbstractMessage), allocatable :: message
      class (ProtocolParser), intent(in) :: this
      integer, intent(in) :: type_id

      class (AbstractMessage), pointer :: prototype
      
      prototype => this%prototypes%at(type_id)
      allocate(message, source=prototype)
      
   end function make_message


   function encode(this, message) result(buffer)
      integer, allocatable :: buffer(:)
      class (ProtocolParser), intent(in) :: this
      class (AbstractMessage), target, intent(in) :: message

      integer :: length

      length = message%get_length()
      allocate(buffer(1 + length))

      buffer(1) = message%get_type_id()
      call message%serialize(buffer(2:))
      
   end function encode

   
   function decode(this, buffer) result(message)
      class (ProtocolParser), intent(in) :: this
      class (AbstractMessage), allocatable :: message
      integer, intent(in) :: buffer(:)

      allocate(message, source=this%prototypes%at(buffer(1)))
      call message%deserialize(buffer(2:))
      
   end function decode


end module pFIO_ProtocolParserMod

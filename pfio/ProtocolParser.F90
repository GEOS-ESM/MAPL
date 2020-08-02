#include "unused_dummy.H"
module pFIO_ProtocolParserMod
   use pFIO_AbstractMessageMod
   use pFIO_AbstractMessageMod
   use pFIO_IntegerMessageMapMod
   use pFIO_FileMetadataMod

   use pFIO_TerminateMessageMod
   use pFIO_DoneMessageMod
   use pFIO_PrefetchDoneMessageMod
   use pFIO_CollectivePrefetchDoneMessageMod
   use pFIO_StageDoneMessageMod
   use pFIO_CollectiveStageDoneMessageMod
   use pFIO_AddExtCollectionMessageMod
   use pFIO_AddHistCollectionMessageMod
   use pFIO_IdMessageMod
   use pFIO_PrefetchDataMessageMod
   use pFIO_StageDataMessageMod
   use pFIO_CollectivePrefetchDataMessageMod
   use pFIO_CollectiveStageDataMessageMod
   use pFIO_ModifyMetadataMessageMod
   use pFIO_HandShakeMessageMod
   use pFIO_DummyMessageMod
   use pFIO_ForwardDataMessageMod

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
      type (DoneMessage)                   :: done
      type (PrefetchDoneMessage)           :: pdone
      type (CollectivePrefetchDoneMessage) :: cpdone
      type (StageDoneMessage)              :: sdone
      type (CollectiveStageDoneMessage)    :: csdone
      type (AddExtCollectionMessage)  :: addExtCollection
      type (AddHistCollectionMessage) :: addHistCollection
      type (IdMessage):: IDid
      type (PrefetchDataMessage)    :: PrefetchData
      type (StageDataMessage) :: StageData
      type (CollectivePrefetchDataMessage) :: CollectivePrefetchData
      type (CollectiveStageDataMessage)    :: CollectiveStageData
      type (ModifyMetadataMessage) :: ModifyMetadata
      type (HandShakeMessage) :: handshake
      type (DummyMessage) :: dummy
      type (ForwardDataMessage) :: ForwardData

      if (this%initialized) return

      call add_prototype(terminate)
      call add_prototype(done)
      call add_prototype(pdone)
      call add_prototype(cpdone)
      call add_prototype(sdone)
      call add_prototype(csdone)
      call add_prototype(addExtCollection)
      addHistCollection = AddHistCollectionMessage(FileMetadata())
      call add_prototype(addHistCollection)
      call add_prototype(IDId)
      call add_prototype(PrefetchData)
      call add_prototype(CollectivePrefetchData)
      call add_prototype(StageData)
      call add_prototype(CollectiveStageData)
      ModifyMetaData = ModifyMetadataMessage(collection_id=-1)
      call add_prototype(ModifyMetadata)
      call add_prototype(handshake)
      call add_prototype(dummy)
      call add_prototype(ForwardData)

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
      return 
      _UNUSED_DUMMY(this)
   end function encode

   
   function decode(this, buffer) result(message)
      class (ProtocolParser), intent(in) :: this
      class (AbstractMessage), allocatable :: message
      integer, intent(in) :: buffer(:)

      allocate(message, source=this%prototypes%at(buffer(1)))
      call message%deserialize(buffer(2:))
      
   end function decode


end module pFIO_ProtocolParserMod

#include "MAPL_Generic.h"

module AEIO_IOController
   use MAPL_ExceptionHandling
   use ESMF
   use AEIO_Client
   use AEIO_ClientMap
   use AEIO_Server
   use AEIO_ServerMap
   use yafYaml
   use HistoryConfigMod
   use CollectionMod
   use CollectionRegistryMod
   use gFTL_StringVector
   
   implicit none
   private

   type IOController
      private
      type(ClientMap) :: clients
      type(ServerMap) :: servers
      integer, allocatable :: pet_list(:,:)

   contains
      procedure :: initialize
      procedure :: transfer_grid
!     procedure :: create_client_server_rh
!     procedure :: create_server_writer_prototype
!     procedure :: run
   end type

contains

   ! state
   ! resource distribution
   ! configuration file or yaml object if already parsed
   subroutine initialize(this,state,configuration_file,clock,pet_list,rc)
      class(IOController), intent(inout) :: this
      type(ESMF_State), intent(in ) :: state
      character(len=*), intent(inout) :: configuration_file
      integer, intent(in) :: pet_list(:,:)
      type(ESMF_Clock), intent(in) :: clock
      integer, optional, intent(out) :: rc

      type(HistoryConfig) :: hist_config
      type(CollectionRegistry) :: coll_registry
      type(StringVector ):: enabled
      type(StringVectorIterator) :: enabled_iter
      type(Collection) :: hist_coll
      character(:), allocatable :: key
      type(Server) :: output_server
      type(Client) :: output_client



      integer :: status

      allocate(this%pet_list,source=pet_list)
      call hist_config%import_yaml_file(configuration_file,rc=status)
      _VERIFY(status)

      ! create client and server for each collection
      enabled = hist_config%get_enabled()
      coll_registry=hist_config%get_collections()
      enabled_iter = enabled%begin()
      do while(enabled_iter /= enabled%end())
         key=enabled_iter%get()
         hist_coll=coll_registry%at(key)
         output_server=Server(hist_coll,pet_list,rc=status)
         _VERIFY(status)
         call this%servers%insert(key,output_server)
         output_client=Client(hist_coll,pet_list,rc=status)
         _VERIFY(status)
         call this%clients%insert(key,output_client)
      enddo

      ! initialize client and server for each collection

      ! begin information exchange

      ! create route handles
      
      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine transfer_grid(this,coll_name,rc)
      class(IOController), intent(inout) :: this
      character(len=*), intent(in) :: coll_name
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: client_grid,front_server_grid
      integer :: status
      type(ESMF_GridComp) fake_gridcomp
      type(ESMF_VM) :: server_vm
      type(ESMF_DistGrid) :: client_distGrid,server_distGrid,balanced_distGrid
      type(Client), pointer :: collection_client
      character(len=ESMF_MAXSTR) :: grid_name

      fake_gridcomp = ESMF_GridCompCreate(petList=this%pet_list(2,:),rc=status)
      _VERIFY(status)
      call ESMF_GridCompSetServices(fake_gridcomp,fake_set_services,rc=status)
      _VERIFY(status)
      call ESMF_GridCompGet(fake_gridcomp,vm=server_vm,rc=status)
      _VERIFY(status)
      collection_client =>  this%clients%at(coll_name)
      client_grid = collection_client%get_grid(rc=status)
      _VERIFY(status)
      call ESMF_GridGet(client_grid,distGrid=client_distGrid,rc=status)
      _VERIFY(status)
      call ESMF_GridGet(client_grid,name=grid_name,rc=status)
      _VERIFY(status)
      server_distGrid=ESMF_DistGridCreate(client_distGrid,vm=server_vm,rc=status)
      _VERIFY(status)
      balanced_distGrid=ESMF_DistGridCreate(server_distGrid,balanceflag=.true.,rc=status)
      _VERIFY(status)
      front_server_grid = ESMF_GridCreate(client_Grid,balanced_distGrid,name=grid_name, &
      copyAttributes = .true.,rc=status)
      _VERIFY(status)
      
      

      _RETURN(_SUCCESS)
   end subroutine transfer_grid

   subroutine fake_set_services(gridcomp,rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc
   end subroutine

end module AEIO_IOController

#include "MAPL_ErrLog.h"
module mapl_ServiceServicesTypes
  use ESMF
  use MAPL_ExceptionHandling

  implicit none
  private


!  public ProvidedServiceType
  public ServiceConnectionGet

  type, public :: ProvidedServiceType
!!$      private
     character(len=:), allocatable :: SERVICE_NAME
     character(len=:), allocatable :: BUNDLE_NAME
     type(ESMF_FieldBundle)        :: BUNDLE
     !ALT currect assumption is the bundle for the provider will be in the import state
  end type ProvidedServiceType

  type, public :: RequestedServiceType
!!$      private
     character(len=:), allocatable :: SERVICE_NAME
     !ALT currect assumption is the bundle for the request will be in the export state
     character(len=:), allocatable :: VAR_LIST(:)
     type(ESMF_FieldBundle)        :: BUNDLE
  end type RequestedServiceType

  type, public :: ServiceConnectionType
!!$      private
     character(len=:), allocatable :: SERVICE_NAME
     character(len=:), allocatable :: PROVIDER_NAME
     character(len=:), allocatable :: REQUESTER_NAME
  end type ServiceConnectionType

  interface ProvidedServiceType
     module procedure newProvidedServiceType
  end interface ProvidedServiceType

contains

  function newProvidedServiceType(SERVICE, BUNDLE) result (item)
    character (len=*)             , intent(IN   ) :: SERVICE
    character (len=*)             , intent(IN   ) :: BUNDLE
    
    type (ProvidedServiceType)  :: item
            
    item%Service_name = SERVICE
    item%Bundle_name = BUNDLE
  end function newProvidedServiceType

  subroutine ServiceConnectionGet(item, &
            Service, Provider, Requester, RC)

    type (ServiceConnectionType), intent(IN)  :: ITEM
    character (len=*), optional, intent(  OUT) :: SERVICE
    character (len=*), optional, intent(  OUT) :: PROVIDER
    character (len=*), optional, intent(  OUT) :: REQUESTER
    integer,           optional, intent(  OUT) :: RC     ! Error code:
    
    if (present(Service)) then
       Service = item%Service_name
    end if

    if (present(Provider)) then
       Provider = item%Provider_name
    end if

    if (present(requester)) then
       requester = item%requester_name
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine ServiceConnectionGet

end module mapl_ServiceServicesTypes

#include "MAPL_ErrLog.h"
module mapl_ServiceServices
  use ESMF
  use MAPL_ExceptionHandling
  use mapl_ServiceConnectionItemVector
  use mapl_ProvidedServiceItemVector
  use mapl_RequestedServiceItemVector
  use mapl_VariableSpecification

  implicit none
  private


  public ProvidedServiceType
  public ServiceConnectionGet
  public ProvidedServiceGet
  public ProvidedServiceSet
  public RequestedServiceGet
  public FillRequestBundle

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

  subroutine ProvidedServiceGet(provider_list, advertised_service, bundle, rc)
    type(ProvidedServiceItemVector), intent(IN) :: provider_list
    character(len=*), intent(IN) :: advertised_service
    type(ESMF_FieldBundle), intent(OUT) :: bundle
    integer, optional, intent(out) :: rc
    
    logical :: found
    type (ProvidedServiceItemVectorIterator) :: iter
    type (ProvidedServiceType), pointer :: item
    
    _ASSERT(provider_list%size()>0,'provider_list should not be empty')
    
    found = .false.
    iter = provider_list%begin()
    ! loop over provided services
    do while (iter /= provider_list%end())
       item => iter%get()
       if(item%service_name == advertised_service) then
          found = .true.
          bundle = item%bundle
          exit
       end if
    end do
    _ASSERT(found, 'No match found for service')
    call iter%next()
    _RETURN(_SUCCESS)
  end subroutine ProvidedServiceGet
   
  subroutine ProvidedServiceSet(provider_list, state, rc)
    type(ProvidedServiceItemVector), intent(IN) :: provider_list
    type(ESMF_State), intent(IN) :: state
    integer, optional, intent(out) :: rc
    
    integer :: status
    type (ProvidedServiceItemVectorIterator) :: iter
    type (ProvidedServiceType), pointer :: item
    
    _ASSERT(provider_list%size()>0,'provider_list should not be empty')
    
    iter = provider_list%begin()
    ! loop over provided services
    do while (iter /= provider_list%end())
       item => iter%get()
       call ESMF_StateGet(state, item%bundle_name, &
            item%bundle, rc=status)
       _VERIFY(status)
       call iter%next()
    end do

    _RETURN(_SUCCESS)
  end subroutine ProvidedServiceSet
   
  subroutine RequestedServiceGet(request_list, service, bundle, rc)
    type(RequestedServiceItemVector), intent(IN) :: request_list
    character(len=*), intent(IN) :: service
    type(ESMF_FieldBundle), intent(OUT) :: bundle
    integer, optional, intent(out) :: rc
    
    logical :: found
    type (RequestedServiceItemVectorIterator) :: iter
    type (RequestedServiceType), pointer :: item
    
    _ASSERT(request_list%size()>0,'request_list should not be empty')
    
    found = .false.
    iter = request_list%begin()
    ! loop over requested services
    do while (iter /= request_list%end())
       item => iter%get()
       if(item%service_name == service) then
          found = .true.
          bundle = item%bundle
          exit
       end if
    END DO
    _ASSERT(found, 'No match found for service')
    _RETURN(_SUCCESS)
  end subroutine RequestedServiceGet
   
  subroutine FillRequestBundle(request_list, state, rc)
    type(RequestedServiceItemVector), intent(INOUT) :: request_list
    type(ESMF_State), intent(IN) :: state
    integer, optional, intent(out) :: rc
    
    integer :: status
    integer :: i, nl
    type(ESMF_Field), allocatable :: fields(:)
    type (RequestedServiceItemVectorIterator) :: iter
    type (RequestedServiceType), pointer :: item
    
    _ASSERT(request_list%size()>0,'request_list should not be empty')

    iter = request_list%begin()
    ! loop over requested services
    do while (iter /= request_list%end())
       item => iter%get()
       if (allocated(item%var_list)) then
          nl = size(item%var_list)
          allocate(fields(nl), stat=status)
          _VERIFY(status)
          do i=1, nl
             call ESMF_StateGet(state, item%var_list(i), &
                  fields(i), rc=status)
             _VERIFY(status)
          end do
          call ESMF_FieldBundleAdd(item%bundle, fields, rc=status)
          _VERIFY(status)
          deallocate(fields)
       end if
       call iter%next()
    end do
    
    _RETURN(_SUCCESS)
  end subroutine FillRequestBundle
   
end module mapl_ServiceServices

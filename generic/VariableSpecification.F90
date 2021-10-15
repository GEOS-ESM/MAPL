module mapl_VariableSpecification
   use ESMF
   use MAPL_VarSpecTypeMod
   use MAPL_VarSpecMod
   use MAPL_VarSpecPtrMod
   implicit none
   private


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

end module mapl_VariableSpecification

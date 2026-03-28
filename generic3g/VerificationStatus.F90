module mapl3g_VerificationStatus
   implicit none(type, external)
   private

   ! Type
   public :: VerificationStatus
   ! Operators
   public :: operator(==)
   public :: operator(/=)
   ! Parameters
   public :: VERIFICATION_STATUS_UNVERIFIED
   public :: VERIFICATION_STATUS_VERIFIED
   public :: VERIFICATION_STATUS_CF_COMPLIANT

   type :: VerificationStatus
      private
      integer :: id = -1
   contains
      procedure :: to_string
   end type VerificationStatus

   ! Define parameter instances
   type(VerificationStatus), parameter :: &
        VERIFICATION_STATUS_UNVERIFIED  = VerificationStatus(0), &
        VERIFICATION_STATUS_VERIFIED    = VerificationStatus(1), &
        VERIFICATION_STATUS_CF_COMPLIANT = VerificationStatus(2)

   interface VerificationStatus
      procedure new_from_string
   end interface VerificationStatus

   interface operator(==)
      procedure equal
   end interface operator(==)

   interface operator(/=)
      procedure not_equal
   end interface operator(/=)

contains

   function new_from_string(str) result(status)
      type(VerificationStatus) :: status
      character(*), intent(in) :: str

      select case (trim(str))
      case ('unverified', 'UNVERIFIED')
         status = VERIFICATION_STATUS_UNVERIFIED
      case ('verified', 'VERIFIED')
         status = VERIFICATION_STATUS_VERIFIED
      case ('cf_compliant', 'CF_COMPLIANT')
         status = VERIFICATION_STATUS_CF_COMPLIANT
      case default
         status = VERIFICATION_STATUS_UNVERIFIED
      end select
   end function new_from_string

   function to_string(this) result(s)
      character(:), allocatable :: s
      class(VerificationStatus), intent(in) :: this

      select case (this%id)
      case (VERIFICATION_STATUS_UNVERIFIED%id)
         s = 'unverified'
      case (VERIFICATION_STATUS_VERIFIED%id)
         s = 'verified'
      case (VERIFICATION_STATUS_CF_COMPLIANT%id)
         s = 'cf_compliant'
      case default
         s = 'unverified'
      end select
   end function to_string

   elemental logical function equal(a, b)
      class(VerificationStatus), intent(in) :: a, b
      equal = a%id == b%id
   end function equal

   elemental logical function not_equal(a, b)
      class(VerificationStatus), intent(in) :: a, b
      not_equal = .not. (a%id == b%id)
   end function not_equal

end module mapl3g_VerificationStatus

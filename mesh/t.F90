program main
   implicit none

   integer, allocatable :: factors(:)

   factors = get_factors(129600)

contains

   

   function get_factors(n) result(factors)
      integer, allocatable :: factors(:)
      integer, intent(in) :: n

      integer :: i, m, p

      integer, parameter :: PRIMES(*) = [2, 5, 7, 3]

      factors = [integer :: ] ! empty
      m = n

      do while (m /= 1)
         do i = 1, size(PRIMES)
            p = PRIMES(i)
            if (mod(m, p) == 0) then
               factors = [factors, p]
               m = m / p
               exit
            end if
         end do
         if (i > size(PRIMES)) then
            error stop "Exceeded permitted prime factors"
         end if
      end do

      print*, 'factors: ', factors
   end function get_factors
      
end program main

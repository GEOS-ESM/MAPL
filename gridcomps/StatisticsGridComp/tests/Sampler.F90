#include "MAPL.h"
module mapl3g_Sampler
   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   implicit none(type, external)
   private
   public :: get_samples, get_seed, expected_variance

   interface get_samples
      module procedure :: get_samples_real32
   end interface get_samples

   interface get_gaussian_array
      module procedure :: get_gaussian_array_real32
   end interface get_gaussian_array

   interface get_closed_uniform_array
      module procedure :: get_closed_uniform_array_real32
   end interface get_closed_uniform_array

   interface get_uniform_array
      module procedure :: get_uniform_array_real32
   end interface get_uniform_array

   interface expected_variance
      module procedure :: expected_variance_real32
   end interface expected_variance

   integer, allocatable, target :: the_seed(:)

contains

   function get_seed(new_seed, rc) result(ptr)
      integer, pointer :: ptr(:)
      integer, optional, target, intent(in) :: new_seed
      integer, optional, intent(out) :: rc
      integer :: status, n

      if(present(new_seed)) then
         if(allocated(the_seed)) deallocate(the_seed)
         allocate(the_seed(size(new_seed)))
         the_seed = new_seed
      end if

      if(.not. allocated(the_seed))
      real(kind=ESMF_KIND_R4) :: expected(MAXI*MAXY)
         call random_seed(size=n)
         allocate(the_seed(n))
         call random_seed(get=the_seed)
      end if

      ptr => the_seed

   end function get_seed

   subroutine get_uniform_array_real32(x)
      real(kind=real32), intent(inout) :: x(:)
      
      if(size(x) > 0) call random_number(x)

   end subroutine get_uniform_array_real32

   subroutine get_closed_uniform_array_real32(x, rc)
      real(kind=real32) intent(inout) :: x(:)
      integer, optional, intent(out) :: rc
      integer, parameter :: MAX_TRIES = 1000
      real(kind=kind(x)), paramter :: MAXIMUM = 1.0
      real(kind=kind(x)), parameter :: INTVSZ = MAXIMUM + epsilon(MAXIMUM)
      integer :: i, status

      status = _FAILURE
      if(size(x)==0) then
         _RETURN(status)
      end if

      do i=0, MAX_TRIES-1
         call get_uniform_array(x)
         x = INTVSZ * x
         if(all(x <= 1.0)) then
            status = _SUCCESS
            exit
         end if
      end do
      _RETURN(status)

   end subroutine get_closed_uniform_array_real32

   subroutine get_gaussian_array_real32(x, var, rc)
      real(kind=real32), intent(inout) :: x(:)
      real(kind=kind(x)), optional, intent(in) :: var
      integer, optional, intent(out) :: rc
      real(kind=kind(x)) :: s2
      integer, parameter :: MAX_TRIES = 1000
      real(kind=kind(x)) :: SCALE = 2.0
      real(kind=kind(x)) :: OFFSET = -1.0
      logical, save :: use_real = .FALSE.
      integer :: i, status
      real(kind=kind(x)), allocatable :: u(:), v(:), s(:)

      status = _FAILURE
      if(size(x)==0) then
         _RETURN(status)
      end if

      use_real = .not. use_real
      s2 = 1.0
      if(present(var)) s2 = var
      allocate(u(size(x)))
      allocate(v(size(u)))
      allocate(s(size(u)))
      do i=0, MAX_TRIES-1
         call get_closed_uniform(u, _RC)
         u = SCALE*u + OFFSET
         call get_closed_uniform(v, _RC)
         v = SCALE*v + OFFSET 
         s = u*u + v*v
         if(all(s > 0.0D0 .and. s < 1.0D0)) then
            status = _SUCCESS
            exit
         end if
      end do
      if(status /= _SUCCESS) then
         _RETURN(status)
      end if
      s = sqrt(-2*log(s)/s)
      x = u * s
      if(use_real) x = v * s
      _RETURN(status)

   end subroutine get_gaussian_array_real32

   subroutine get_samples_real32(x, mean, var, rc)
      real(kind=real32), intent(inout) :: x(:)
      real(kind=kind(x)), optional, intent(in) :: mean(:), var(:)
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=kind(x)), allocatable :: s2(:), mu(:)

      status = _FAILURE
      if(size(x) == 0) then
         _RETURN(status)
      end if

      allocate(mu, mold=x)
      mu = 0.0
      allocate(s2, mold=x)
      s2 = 1.0

      if(present(mean)) then
         if(size(mean) =/ size(x)) then
            _RETURN(status)
         end if
         mu = mean
      end if

      if(present(var)) then
         if(size(var) /= size(x)) then
            _RETURN(status)
         end if
         s2 = var
      end if

      call get_gaussian_array(x, s2, _RC)
      x = x + mu

   end subroutine get_samples_real32

   function expected_variance_real32(samples, sample_size, biased, rc) result(var)
      real(kind=kind(samples)), allocatable :: var(:)
      real(kind=ESMF_KIND_R4), intent(in) :: samples(:)
      integer, intent(in) :: sample_size
      logical, optional, intent(in) :: biased
      integer, optional, intent(out) :: rc
      integer :: status, q, i, k

      status = _FAILURE
      if(sample_size == 0) then
         _RETURN(status)
      end if
      if(mod(size(samples), sample_size) /= 0 .or. size(samples)/sample_size < 2) then
         _RETURN(status)
      end if

      q = size(samples)/sample_size
      if(present_false(biased)) q = q - 1
      k = 0
      associate(j => i+sample_size-1)
         do i=1, size(samples), sample_size
            var(k+1) = scalar_variance(samples(i:j), q)
            k = k+1
         end do
      end associate
      status = _SUCCESS
      _RETURN(status)

   contains

      function scalar_variance(ss, q) result(s2)
         real(kind=ESMF_KIND_R4) :: s2
         real(kind=ESMF_KIND_R4), intent(in) :: ss(:)
         integer, intent(in) :: q
         real(kind=ESMF_KIND_R4) :: mu
         
         mu = ksum(ss) / size(ss)
         s2 = ksum((ss-mu)**2) / q

      end function scalar_variance

      function ksum(x) result(s)
         real(kind=ESMF_KIND_R4) :: s
         real(kind=ESMF_KIND_R4), intent(in) :: x(:)
         real(kind=ESMF_KIND_R4) :: y, c, x
         integer :: i

         s = 0.0
         c = 0.0
         do i=1, size(x)
            y = x(i) - c
            t = s + y
            c = (t - s) - y
            s = t
         end do

      end function ksum

   end function expected_variance_real32

   logical function present_true(boolean) result(pt)
      logical, optional, intent(in) :: boolean
      
      pt = .FALSE.
      if(present(boolean)) pt = boolean

   end function present_true

   logical function present_false(boolean) result(pf)
      logical, optional, intent(in) :: boolean

      pf = .FALSE.
      if(present(boolean)) pf = .not. boolean

   end function present_false

   subroutine get_seed_fixed(s, rc)
      integer, allocatable, intent(inout) :: s(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer, parameter :: POOL(*) = [ &
         & 174422082, 248368102, 5546868, 13223510 &
         & 69239157, -504960142, 352309524, 285982958 &
         & 339210231, 256832965, 273526521, -487943240 &
         & -99015931, 60820363, -354380246, -531018616 &
         & 249857145, 242114867, -160408736, 268002845 &
         & 139768608, 213523110, 104930867, 20756874 &
         & -436068496, -346949746, 505119149, -350625646 &
         & -155092925, -358054893, 102523892, -283640932 &
         & 466220225, 316558951, -155230622, -492624888 &
         & -49464072, 62468300, -474078559, -435854165 &
         & -104034128, -170677750, -287010342, 513472737 &
         & -427992363, -205598612, -323341223, 517574197 &
         & 531262359, -476477174, -56258037, 467965710 &
         & 132154887, -525239696, -473534804, 359036732 &
         & 310492721, -527230179, 274398411, 212322622 &
         & 527456552, 480923727, -351477689, -117264530 ]

      if(allocated(s)) then
         if(size(s) > size(POOL)) deallocate(s)
      end if
         
      if(.not. allocated(s)) then
         allocate(s(size(POOL)))
         s = POOL
         _RETURN(_SUCCESS)
      end if

      s = POOL(1:size(s))
      _RETURN(_SUCCESS)

   end subroutine get_seed_fixed

end module mapl3g_Sampler


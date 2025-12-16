#include "MAPL.h"

! This module provides a wrapper for ESMF_DynamicMask
! to enable equality checking between instances.

module mapl3g_DynamicMask
   use esmf
   use mapl_ErrorHandlingMod
   implicit none
   private


   public :: DynamicMask

   public :: operator(==)
   public :: operator(/=)

   type :: DynamicMaskSpec
      character(:), allocatable :: mask_type
      logical :: handleAllElements = .false.
      real(kind=ESMF_KIND_R4), allocatable :: src_mask_value_r4
      real(kind=ESMF_KIND_R4), allocatable :: dst_mask_value_r4
      real(kind=ESMF_KIND_R8), allocatable :: src_mask_value_r8
      real(kind=ESMF_KIND_R8), allocatable :: dst_mask_value_r8
   end type DynamicMaskSpec


   type DynamicMask
      type(DynamicMaskSpec) :: spec
      type(ESMF_DynamicMask), allocatable :: esmf_mask_r4
      type(ESMF_DynamicMask), allocatable :: esmf_mask_r8
   end type DynamicMask

   interface operator(==)
      procedure :: equal_to
      procedure :: equal_to_spec
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
      procedure :: not_equal_to_spec
   end interface operator(/=)

   interface match
      procedure match_r4
      procedure match_r8
   end interface match

   interface DynamicMask
      procedure :: new_DynamicMask_r4
      procedure :: new_DynamicMask_r8
      procedure :: new_DynamicMask_r4r8
   end interface DynamicMask

   abstract interface

      subroutine I_r4r8r4(dynamicMaskList, dynamicSrcMaskValue, dynamicDstMaskValue, rc)
         use esmf
         type(ESMF_DynamicMaskElementR4R8R4V), pointer        :: dynamicMaskList(:)
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)  :: rc
      end subroutine I_r4r8r4

      subroutine I_r8r8r8(dynamicMaskList, dynamicSrcMaskValue, dynamicDstMaskValue, rc)
         use esmf
         type(ESMF_DynamicMaskElementR8R8R8V), pointer        :: dynamicMaskList(:)
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)  :: rc
      end subroutine I_r8r8r8
   end interface

contains

   function new_DynamicMask_r4(mask_type, src_mask_value, dst_mask_value, handleAllElements, rc) result(mask)
      type(DynamicMask) :: mask
      character(*), intent(in) :: mask_type
      real(kind=ESMF_KIND_R4) :: src_mask_value
      real(kind=ESMF_KIND_R4), optional, intent(in) :: dst_mask_value
      logical, optional :: handleAllElements
      integer, optional, intent(out) :: rc

      integer :: status
      type(DynamicMaskSpec) :: spec

      spec%mask_type = mask_type
      if (present(handleAllElements)) spec%handleAllElements = handleAllElements

      spec%src_mask_value_r4 = src_mask_value
      spec%src_mask_value_r8 = spec%src_mask_value_r4

      ! No default for dst_mask_value.  Usually left unallocated
      if (present(dst_mask_value)) then
         spec%dst_mask_value_r4 = dst_mask_value
         spec%dst_mask_value_r8 = dst_mask_value
      end if

      mask = DynamicMask(spec, _RC)

      _RETURN(_SUCCESS)
   end function new_DynamicMask_r4

   function new_DynamicMask_r8(mask_type, src_mask_value, dst_mask_value, handleAllElements, rc) result(mask)
      type(DynamicMask) :: mask
      character(*), intent(in) :: mask_type
      real(kind=ESMF_KIND_R8), optional, intent(in) :: src_mask_value
      real(kind=ESMF_KIND_R8), optional, intent(in) :: dst_mask_value
      logical, optional :: handleAllElements
      integer, optional, intent(out) :: rc

      integer :: status
      type(DynamicMaskSpec) :: spec

      spec%mask_type = mask_type
      if (present(handleAllElements)) spec%handleAllElements = handleAllElements

      spec%src_mask_value_r8 = src_mask_value
      spec%src_mask_value_r4 = spec%src_mask_value_r8

      ! No default for dst_mask_value.  Usually left unallocated
      if (present(dst_mask_value)) then
         spec%dst_mask_value_r8 = dst_mask_value
         spec%dst_mask_value_r4 = dst_mask_value
      end if

      mask = DynamicMask(spec, _RC)

      _RETURN(_SUCCESS)

   end function new_DynamicMask_r8

   function new_DynamicMask_r4r8(spec, rc) result(mask)
      type(DynamicMask) :: mask
      type(DynamicMaskSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status

      procedure(I_r4r8r4), pointer :: mask_routine_r4
      procedure(I_r8r8r8), pointer :: mask_routine_r8

      mask%spec = spec

      allocate(mask%esmf_mask_r4)
      mask_routine_r4 => get_mask_routine_r4(spec%mask_type, _RC)
      call ESMF_DynamicMaskSetR4R8R4V(mask%esmf_mask_r4, mask_routine_r4, &
           dynamicSrcMaskValue=spec%src_mask_value_r4, &
           dynamicDstMaskValue=spec%dst_mask_value_r4, &
           handleAllElements=spec%handleAllElements, &
           _RC)

      allocate(mask%esmf_mask_r8)
      mask_routine_r8 => get_mask_routine_r8(spec%mask_type, _RC)
      call ESMF_DynamicMaskSetR8R8R8V(mask%esmf_mask_r8, mask_routine_r8, &
           dynamicSrcMaskValue=spec%src_mask_value_r8, &
           dynamicDstMaskValue=spec%dst_mask_value_r8, &
           handleAllElements=spec%handleAllElements, &
           _RC)

      _RETURN(_SUCCESS)
   end function new_DynamicMask_r4r8

   function get_mask_routine_r4(mask_type, rc) result(mask_routine)
      procedure(I_r4r8r4), pointer :: mask_routine
      character(*), intent(in) :: mask_type
      integer, intent(out), optional :: rc

      integer :: status

      select case (mask_type)
      case ('missing_value')
         mask_routine => missing_r4r8r4v
      case ('monotonic')
         mask_routine => monotonic_r4r8r4v
      case ('vote')
         mask_routine => vote_r4r8r4v
      case ('fraction')
         mask_routine => fraction_r4r8r4v
      case default
         mask_routine => null()
         _FAIL("Unsupported mask type: "//mask_type)
      end select

      _RETURN(_SUCCESS)
   end function get_mask_routine_r4

   function get_mask_routine_r8(mask_type, rc) result(mask_routine)
      procedure(I_r8r8r8), pointer :: mask_routine
      character(*), intent(in) :: mask_type
      integer, intent(out), optional :: rc

      integer :: status

      select case (mask_type)
      case ('missing_value')
         mask_routine => missing_r8r8r8v
      case ('monotonic')
         mask_routine => monotonic_r8r8r8v
      case ('vote')
         mask_routine => vote_r8r8r8v
      case ('fraction')
         mask_routine => fraction_r8r8r8v
      case default
         mask_routine => null()
         _FAIL("Unsupported mask type: "//mask_type)
      end select

      _RETURN(_SUCCESS)
   end function get_mask_routine_r8


   subroutine missing_r8r8r8v(dynamicMaskList, dynamicSrcMaskValue, dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR8R8R8V), pointer        :: dynamicMaskList(:)
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc

      integer :: i, j, k, n
      real(ESMF_KIND_R8), allocatable  :: renorm(:)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) & 
                          + dynamicMaskList(i)%factor(j) &
                          * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                  endif
               end do
            end do
            where (renorm > 0.d0)
               dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
         enddo
      endif

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(dynamicDstMaskValue)
   end subroutine missing_r8r8r8v

   subroutine missing_r4r8r4v(dynamicMaskList, dynamicSrcMaskValue, dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer        :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc

      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) & 
                          + dynamicMaskList(i)%factor(j) &
                          * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                  endif
               end do
            end do
            where (renorm > 0.d0)
               dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
         enddo
      endif

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(dynamicDstMaskValue)
   end subroutine missing_r4r8r4v


   subroutine monotonic_r8r8r8V(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR8R8R8V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R8), allocatable  :: renorm(:),max_input(:),min_input(:)


      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n),max_input(n),min_input(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            max_input = -huge(0.0)
            min_input = huge(0.0)
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) &
                          + dynamicMaskList(i)%factor(j) &
                          * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                     if (dynamicMaskList(i)%srcElement(j)%ptr(k) > max_input(k)) max_input(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                     if (dynamicMaskList(i)%srcElement(j)%ptr(k) < min_input(k)) min_input(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                  endif
               end do
            end do
            where (renorm > 0.d0)
               dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
            where (renorm > 0.d0 .and. dynamicMaskList(i)%dstElement > max_input)
               dynamicMaskList(i)%dstElement = max_input
            end where
            where (renorm > 0.d0 .and. dynamicMaskList(i)%dstElement < min_input)
               dynamicMaskList(i)%dstElement = min_input
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
      _UNUSED_DUMMY(dynamicDstMaskValue)
   end subroutine monotonic_r8r8r8V

   subroutine monotonic_r4r8r4V(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:),max_input(:),min_input(:)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n),max_input(n),min_input(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            max_input = -huge(0.0)
            min_input = huge(0.0)
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) &
                          + dynamicMaskList(i)%factor(j) &
                          * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                     if (dynamicMaskList(i)%srcElement(j)%ptr(k) > max_input(k)) max_input(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                     if (dynamicMaskList(i)%srcElement(j)%ptr(k) < min_input(k)) min_input(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                  endif
               end do
            end do
            where (renorm > 0.d0)
               dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
            where (renorm > 0.d0 .and. dynamicMaskList(i)%dstElement > max_input)
               dynamicMaskList(i)%dstElement = max_input
            end where
            where (renorm > 0.d0 .and. dynamicMaskList(i)%dstElement < min_input)
               dynamicMaskList(i)%dstElement = min_input
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
      _UNUSED_DUMMY(dynamicDstMaskValue)

   end subroutine monotonic_r4r8r4V


   subroutine vote_r8r8r8v(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR8R8R8V), pointer       :: dynamicMaskList(:)
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R8), allocatable  :: renorm(:)


      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     if (dynamicMaskList(i)%factor(j) > renorm(k)) then
                        renorm(k) = dynamicMaskList(i)%factor(j)
                        dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                     end if
                  endif
               end do
            end do
            where (renorm > 0.d0)
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
      _UNUSED_DUMMY(dynamicDstMaskValue)
   end subroutine vote_r8r8r8v


   subroutine vote_r4r8r4v(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer       :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     if (dynamicMaskList(i)%factor(j) > renorm(k)) then
                        renorm(k) = dynamicMaskList(i)%factor(j)
                        dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                     end if
                  endif
               end do
            end do
            where (renorm > 0.d0)
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
      _UNUSED_DUMMY(dynamicDstMaskValue)

   end subroutine vote_r4r8r4v

   subroutine fraction_r8r8r8v(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR8R8R8V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     if (nint(dynamicMaskList(i)%srcElement(j)%ptr(k)) == 0) then
                        dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) + &
                             & dynamicMaskList(i)%factor(j)
                     end if
                  endif
               end do
            end do
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
      _UNUSED_DUMMY(dynamicDstMaskValue)

   end subroutine fraction_r8r8r8v

   subroutine fraction_r4r8r4v(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     if (nint(dynamicMaskList(i)%srcElement(j)%ptr(k)) == 0) then
                        dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) + &
                             & dynamicMaskList(i)%factor(j)
                     end if
                  endif
               end do
            end do
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
      _UNUSED_DUMMY(dynamicDstMaskValue)

   end subroutine fraction_r4r8r4v


   impure elemental logical function equal_to(a, b)
      type(DynamicMask), intent(in) :: a
      type(DynamicMask), intent(in) :: b

      equal_to = a%spec == b%spec
      if (.not. equal_to) return

   end function equal_to

   impure elemental logical function not_equal_to(a, b)
      type(DynamicMask), intent(in) :: a
      type(DynamicMask), intent(in) :: b

      not_equal_to = .not. (a == b)
   end function not_equal_to


   logical function equal_to_spec(a, b) result(equal_to)
      type(DynamicMaskSpec), intent(in) :: a
      type(DynamicMaskSpec), intent(in) :: b

      equal_to = allocated(a%mask_type) .eqv. allocated(b%mask_type)
      if (.not. equal_to) return

      if (.not. allocated(a%mask_type)) then
         equal_to = .true. ! uninit
         return
      end if

      equal_to = a%mask_type == b%mask_type
      if (.not. equal_to) return

      equal_to = a%src_mask_value_r4 == b%src_mask_value_r4
      if (.not. equal_to) return

      equal_to = allocated(a%dst_mask_value_r4) .eqv. allocated(b%dst_mask_value_r4)
      if (.not. equal_to) return

      if (allocated(a%dst_mask_value_r4)) then
         equal_to = a%dst_mask_value_r4 == b%dst_mask_value_r4
      end if
      if (.not. equal_to) return

      equal_to = a%src_mask_value_r8 == b%src_mask_value_r8
      if (.not. equal_to) return

      equal_to = allocated(a%dst_mask_value_r8) .eqv. allocated(b%dst_mask_value_r8)
      if (.not. equal_to) return

      if (allocated(a%dst_mask_value_r8)) then
         equal_to = a%dst_mask_value_r8 == b%dst_mask_value_r8
      end if

   end function equal_to_spec


   logical function not_equal_to_spec(a, b) result(not_equal_to)
      type(DynamicMaskSpec), intent(in) :: a
      type(DynamicMaskSpec), intent(in) :: b

      not_equal_to = .not. (a == b)
   end function not_equal_to_spec


   logical function match_r4(missing,b)
      real(kind=ESMF_KIND_R4), intent(in) :: missing, b
      match_r4 = (missing==b) 
   end function match_r4

   logical function match_r8(missing,b)
      real(kind=ESMF_KIND_R8), intent(in) :: missing, b
      match_r8 = (missing==b) 
   end function match_r8

end module mapl3g_DynamicMask

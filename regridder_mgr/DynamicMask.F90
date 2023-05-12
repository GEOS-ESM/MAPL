#include "MAPL_Generic.h"

module mapl_DynamicMask
   use esmf
   use mapl_ErrorHandlingMod
   use mapl_Base, only: MAPL_UNDEF
   implicit none
   private

   public :: DynamicMask
   public :: missing_value_dynamic_mask
   public :: monotonic_dynamic_mask
   public :: vote_dynamic_mask
   public :: fraction_dynamic_mask
   public :: operator(==)
   public :: operator(/=)

   type DynamicMask
      integer :: id = -1
      real(ESMF_KIND_R8), allocatable :: src_mask_value
      real(ESMF_KIND_R8), allocatable :: dst_mask_value
      type(ESMF_DynamicMask) :: esmf_mask
   end type DynamicMask

   interface operator(==)
      procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

   interface match
      procedure match_r4
      procedure match_r8
   end interface match

contains


   function missing_value_dynamic_mask(src_mask_value, dst_mask_value, rc) result(mask)
      type(DynamicMask) :: mask
      real(ESMF_KIND_R8), intent(in), optional :: src_mask_value
      real(ESMF_KIND_R8), intent(in), optional :: dst_mask_value
      integer, intent(out), optional :: rc

      integer :: status
      real(ESMF_KIND_R4), allocatable :: src_mask_value_r4
      real(ESMF_KIND_R4), allocatable :: dst_mask_value_r4

      mask%id = 1

      mask%src_mask_value = MAPL_UNDEF
      if (present(src_mask_value)) mask%src_mask_value = src_mask_value
      src_mask_value_r4 = mask%src_mask_value

      ! No default for dst_mask_value.  Usually left unallocated
      if (present(dst_mask_value)) then
         mask%dst_mask_value = dst_mask_value
         dst_mask_value_r4 = mask%dst_mask_value
      end if

      call ESMF_DynamicMaskSetR8R8R8V(mask%esmf_mask, missing_r8r8r8v, &
           dynamicSrcMaskValue= mask%src_mask_value, &
           dynamicDstMaskValue= mask%dst_mask_value, &
           _RC)

      
      call ESMF_DynamicMaskSetR4R8R4V(mask%esmf_mask, missing_r4r8r4v, &
           dynamicSrcMaskValue=src_mask_value_r4, &
           dynamicDstMaskValue=dst_mask_value_r4, &
           _RC)

      _RETURN(_SUCCESS)

   contains

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


   end function missing_value_dynamic_mask

   function monotonic_dynamic_mask(src_mask_value, dst_mask_value, rc) result(mask)
      type(DynamicMask) :: mask
      real(ESMF_KIND_R8), intent(in), optional :: src_mask_value
      real(ESMF_KIND_R8), intent(in), optional :: dst_mask_value
      integer, intent(out), optional :: rc

      integer :: status
      real(ESMF_KIND_R4), allocatable :: src_mask_value_r4
      real(ESMF_KIND_R4), allocatable :: dst_mask_value_r4

      mask%id = 2

      mask%src_mask_value = MAPL_UNDEF
      if (present(src_mask_value)) mask%src_mask_value = src_mask_value
      src_mask_value_r4 = mask%src_mask_value

      ! No default for dst_mask_value.  Usually left unallocated
      if (present(dst_mask_value)) then
         mask%dst_mask_value = dst_mask_value
         dst_mask_value_r4 = mask%dst_mask_value
      end if

      call ESMF_DynamicMaskSetR8R8R8V(mask%esmf_mask, monotonic_r8r8r8v, &
           dynamicSrcMaskValue=mask%src_mask_value, &
           dynamicDstMaskValue=mask%dst_mask_value, &
           _RC)

      call ESMF_DynamicMaskSetR4R8R4V(mask%esmf_mask, monotonic_r4r8r4v, &
           dynamicSrcMaskValue=src_mask_value_r4, &
           dynamicDstMaskValue=dst_mask_value_r4, &
           _RC)

      _RETURN(_SUCCESS)

   contains


      subroutine monotonic_r8r8r8V(dynamicMaskList, dynamicSrcMaskValue, &
           dynamicDstMaskValue, rc)
         type(ESMF_DynamicMaskElementR8R8R8V), pointer              :: dynamicMaskList(:)
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)          :: rc
         integer :: i, j, k, n
         real(ESMF_KIND_R8), allocatable  :: renorm(:),max_input(:),min_input(:)

         _UNUSED_DUMMY(dynamicDstMaskValue)

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
      end subroutine monotonic_r8r8r8V

      subroutine monotonic_r4r8r4V(dynamicMaskList, dynamicSrcMaskValue, &
           dynamicDstMaskValue, rc)
         type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)          :: rc
         integer :: i, j, k, n
         real(ESMF_KIND_R4), allocatable  :: renorm(:),max_input(:),min_input(:)

         _UNUSED_DUMMY(dynamicDstMaskValue)

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
      end subroutine monotonic_r4r8r4V

   end function monotonic_dynamic_mask


   function vote_dynamic_mask(src_mask_value, dst_mask_value, rc) result(mask)
      type(DynamicMask) :: mask
      real(ESMF_KIND_R8), intent(in), optional :: src_mask_value
      real(ESMF_KIND_R8), intent(in), optional :: dst_mask_value
      integer, intent(out), optional :: rc

      integer :: status
      real(ESMF_KIND_R4), allocatable :: src_mask_value_r4
      real(ESMF_KIND_R4), allocatable :: dst_mask_value_r4

      mask%id = 3

      mask%src_mask_value = MAPL_UNDEF
      if (present(src_mask_value)) mask%src_mask_value = src_mask_value
      src_mask_value_r4 = mask%src_mask_value

      ! No default for dst_mask_value.  Usually left unallocated
      if (present(dst_mask_value)) then
         mask%dst_mask_value = dst_mask_value
         dst_mask_value_r4 = mask%dst_mask_value
      end if

      call ESMF_DynamicMaskSetR8R8R8V(mask%esmf_mask, vote_r8r8r8v, &
           dynamicSrcMaskValue=mask%src_mask_value, &
           dynamicDstMaskValue=mask%dst_mask_value, &
           _RC)

      call ESMF_DynamicMaskSetR4R8R4V(mask%esmf_mask, vote_r4r8r4v, &
           dynamicSrcMaskValue=src_mask_value_r4, &
           dynamicDstMaskValue=dst_mask_value_r4, &
           _RC)

      _RETURN(_SUCCESS)

   contains


      subroutine vote_r8r8r8v(dynamicMaskList, dynamicSrcMaskValue, &
           dynamicDstMaskValue, rc)
         type(ESMF_DynamicMaskElementR8R8R8V), pointer       :: dynamicMaskList(:)
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)          :: rc
         integer :: i, j, k, n
         real(ESMF_KIND_R8), allocatable  :: renorm(:)

         _UNUSED_DUMMY(dynamicDstMaskValue)

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
      end subroutine vote_r8r8r8v


      subroutine vote_r4r8r4v(dynamicMaskList, dynamicSrcMaskValue, &
           dynamicDstMaskValue, rc)
         type(ESMF_DynamicMaskElementR4R8R4V), pointer       :: dynamicMaskList(:)
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)          :: rc
         integer :: i, j, k, n
         real(ESMF_KIND_R4), allocatable  :: renorm(:)

         _UNUSED_DUMMY(dynamicDstMaskValue)

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
      end subroutine vote_r4r8r4v

   end function vote_dynamic_mask

   function fraction_dynamic_mask(src_mask_value, dst_mask_value, rc) result(mask)
      type(DynamicMask) :: mask
      real(ESMF_KIND_R8), intent(in), optional :: src_mask_value
      real(ESMF_KIND_R8), intent(in), optional :: dst_mask_value
      integer, intent(out), optional :: rc

      integer :: status
      real(ESMF_KIND_R4), allocatable :: src_mask_value_r4
      real(ESMF_KIND_R4), allocatable :: dst_mask_value_r4

      mask%id = 4

      mask%src_mask_value = MAPL_UNDEF
      if (present(src_mask_value)) mask%src_mask_value = src_mask_value
      src_mask_value_r4 = mask%src_mask_value

      ! No default for dst_mask_value.  Usually left unallocated
      if (present(dst_mask_value)) then
         mask%dst_mask_value = dst_mask_value
         dst_mask_value_r4 = mask%dst_mask_value
      end if

      call ESMF_DynamicMaskSetR8R8R8V(mask%esmf_mask, fraction_r8r8r8v, &
           dynamicSrcMaskValue=mask%src_mask_value, &
           dynamicDstMaskValue=mask%dst_mask_value, &
           _RC)

      call ESMF_DynamicMaskSetR4R8R4V(mask%esmf_mask, fraction_r4r8r4v, &
           dynamicSrcMaskValue=src_mask_value_r4, &
           dynamicDstMaskValue=dst_mask_value_r4, &
           _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine fraction_r8r8r8v(dynamicMaskList, dynamicSrcMaskValue, &
           dynamicDstMaskValue, rc)
         type(ESMF_DynamicMaskElementR8R8R8V), pointer              :: dynamicMaskList(:)
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)          :: rc
         integer :: i, j, k, n
         real(ESMF_KIND_R8), allocatable  :: renorm(:)

         _UNUSED_DUMMY(dynamicDstMaskValue)

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
                        if (nint(dynamicMaskList(i)%srcElement(j)%ptr(k)) == 0) then
                           dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) + &
                                & dynamicMaskList(i)%factor(j)
                           renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                        end if
                     endif
                  end do
               end do
            enddo
         endif
         ! return successfully
         rc = ESMF_SUCCESS
      end subroutine fraction_r8r8r8v

      subroutine fraction_r4r8r4v(dynamicMaskList, dynamicSrcMaskValue, &
           dynamicDstMaskValue, rc)
         type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
         real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
         integer,                       intent(out)          :: rc
         integer :: i, j, k, n
         real(ESMF_KIND_R4), allocatable  :: renorm(:)

         _UNUSED_DUMMY(dynamicDstMaskValue)

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
                        if (nint(dynamicMaskList(i)%srcElement(j)%ptr(k)) == 0) then
                           dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) + &
                                & dynamicMaskList(i)%factor(j)
                           renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                        end if
                     endif
                  end do
               end do
            enddo
         endif
         ! return successfully
         rc = ESMF_SUCCESS
      end subroutine fraction_r4r8r4v
   end function fraction_dynamic_mask


   impure elemental logical function equal_to(a, b)
      type(DynamicMask), intent(in) :: a
      type(DynamicMask), intent(in) :: b

      equal_to = (a%id == b%id)
      if (.not. equal_to) return

      equal_to = same_value(a%src_mask_value, b%src_mask_value)
      if (.not. equal_to) return

      equal_to = same_value(a%dst_mask_value, b%dst_mask_value)
      if (.not. equal_to) return

   end function equal_to

   impure logical function same_value(a, b)
      real(ESMF_KIND_R8), allocatable, intent(in) :: a
      real(ESMF_KIND_R8), allocatable, intent(in) :: b

      same_value = (allocated(a) .eqv. allocated(b))
      if (.not. same_value) return

      if (allocated(a)) then
         same_value = (a == b)
      end if

   end function same_value

   impure elemental logical function not_equal_to(a, b)
      type(DynamicMask), intent(in) :: a
      type(DynamicMask), intent(in) :: b

      not_equal_to = .not. (a == b)
   end function not_equal_to


   logical function match_r4(missing,b)
      real(kind=ESMF_KIND_R4), intent(in) :: missing, b
      match_r4 = (missing==b) 
   end function match_r4

   logical function match_r8(missing,b)
      real(kind=ESMF_KIND_R8), intent(in) :: missing, b
      match_r8 = (missing==b) 
   end function match_r8

end module mapl_DynamicMask

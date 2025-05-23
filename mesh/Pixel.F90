module sf_Pixel
   use sf_Point
   use, intrinsic :: iso_fortran_env, only: REAL64, INT16, INT32

   private

   public :: Pixel

   ! CatchIndex:CatchIndex = "Ocean (0) Land (1-291284) Lakes (190000000) ice (200000000)" ;   

   type :: Pixel
      type(Point) :: center
      integer(kind=INT32) :: catch_index
!!$      integer(kind=INT16) :: catch_index
   end type Pixel

end module sf_Pixel

   

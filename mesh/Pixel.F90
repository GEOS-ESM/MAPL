module sf_Pixel
   use sf_Point
   use, intrinsic :: iso_fortran_env, only: REAL64

   private

   public :: Pixel

   ! CatchIndex:CatchIndex = "Ocean (0) Land (1-291284) Lakes (190000000) ice (200000000)" ;   

   type :: Pixel
      type(Point) :: center
      integer :: catch_index
   end type Pixel

end module sf_Pixel

   

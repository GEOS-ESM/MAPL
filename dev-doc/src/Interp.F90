module MAPL_InterpMod

  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64

  implicit none
  private

  public MAPL_Interp

  interface MAPL_Interp
     module procedure INTERP_LIN_0011_1
     module procedure INTERP_LIN_0011_2
     module procedure INTERP_LIN_1111_1
     module procedure INTERP_LIN_1111_2
     module procedure INTERP_LIN_2111_1
     module procedure INTERP_LIN_2111_2
     module procedure INTERP_LIN_2121_1
     module procedure INTERP_LIN_2121_2
     module procedure INTERP_LIN_3321_1
     module procedure INTERP_LIN_3321_2
  end interface MAPL_Interp

  interface 
     module subroutine INTERP_LIN_0011_1( OY, OX, IY, IX)

       ! !ARGUMENTS:

       real,     intent(OUT) :: OY
       real,     intent(IN ) :: OX
       real,     intent(IN ) :: IY(:)
       real,     intent(IN ) :: IX(:)
       !EOP
     end subroutine INTERP_LIN_0011_1

     module subroutine INTERP_LIN_0011_2( OY, OX, IY, IX)

       ! !ARGUMENTS:

       real(kind=REAL64),     intent(OUT) :: OY
       real(kind=REAL64),     intent(IN ) :: OX
       real(kind=REAL64),     intent(IN ) :: IY(:)
       real(kind=REAL64),     intent(IN ) :: IX(:)
       !EOP
     end subroutine INTERP_LIN_0011_2

     !=========================================================================

     module subroutine INTERP_LIN_1111_1( OY, OX, IY, IX)

       real,     intent(OUT) :: OY(:)
       real,     intent(IN ) :: OX(:)
       real,     intent(IN ) :: IY(:)
       real,     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_1111_1

     !=========================================================================

     module subroutine INTERP_LIN_1111_2( OY, OX, IY, IX)

       real(kind=REAL64),     intent(OUT) :: OY(:)
       real(kind=REAL64),     intent(IN ) :: OX(:)
       real(kind=REAL64),     intent(IN ) :: IY(:)
       real(kind=REAL64),     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_1111_2

     !=========================================================================

     module subroutine INTERP_LIN_2121_1( OY, OX, IY, IX)
       real,     intent(OUT) :: OY(:,:)
       real,     intent(IN ) :: OX(:)
       real,     intent(IN ) :: IY(:,:)
       real,     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_2121_1

     !=========================================================================

     module subroutine INTERP_LIN_2121_2( OY, OX, IY, IX)
       real(kind=REAL64),     intent(OUT) :: OY(:,:)
       real(kind=REAL64),     intent(IN ) :: OX(:)
       real(kind=REAL64),     intent(IN ) :: IY(:,:)
       real(kind=REAL64),     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_2121_2

     !=========================================================================

     module subroutine INTERP_LIN_2111_1( OY, OX, IY, IX)

       real,     intent(OUT) :: OY(:,:)
       real,     intent(IN ) :: OX(:)
       real,     intent(IN ) :: IY(:)
       real,     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_2111_1

     !=========================================================================

     module subroutine INTERP_LIN_2111_2( OY, OX, IY, IX)

       real(kind=REAL64),     intent(OUT) :: OY(:,:)
       real(kind=REAL64),     intent(IN ) :: OX(:)
       real(kind=REAL64),     intent(IN ) :: IY(:)
       real(kind=REAL64),     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_2111_2

     !=========================================================================

     module subroutine INTERP_LIN_3321_1( OY, OX, IY, IX)

       real,     intent(OUT) :: OY(:,:,:)
       real,     intent(IN ) :: OX(:,:,:)
       real,     intent(IN ) :: IY(:,:)
       real,     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_3321_1

     !=========================================================================

     module subroutine INTERP_LIN_3321_2( OY, OX, IY, IX)

       real(kind=REAL64),     intent(OUT) :: OY(:,:,:)
       real(kind=REAL64),     intent(IN ) :: OX(:,:,:)
       real(kind=REAL64),     intent(IN ) :: IY(:,:)
       real(kind=REAL64),     intent(IN ) :: IX(:)
     end subroutine INTERP_LIN_3321_2
  end interface
end module MAPL_InterpMod

submodule (MAPL_InterpMod) Interp_implementation

  implicit none

contains

  !BOP

  ! !IROUTINE: INTERP_LIN_0011

  ! !DESCRIPTION: Interpolates linearly in a 1-D table
  ! \newline
  !

  ! !INTERFACE:

  module procedure INTERP_LIN_0011_1
    integer    J

    J        = min(max(count(IX<=OX), 1), size(IX)-1)
    if   (IX(J+1)/=IX(J)) then
       OY = IY(J) + ((OX-IX(J)) / (IX(J+1)-IX(J)))*(IY(J+1) - IY(J))
    else
       OY = IY(J)
    end if

    return
  end procedure INTERP_LIN_0011_1

  module procedure INTERP_LIN_0011_2
    integer    J

    J        = min(max(count(IX<=OX), 1), size(IX)-1)
    if   (IX(J+1)/=IX(J)) then
       OY = IY(J) + ((OX-IX(J)) / (IX(J+1)-IX(J)))*(IY(J+1) - IY(J))
    else
       OY = IY(J)
    end if

    return
  end procedure INTERP_LIN_0011_2

  !=========================================================================

  module procedure INTERP_LIN_1111_1
    integer J

    do J=1,size(OY)
       call INTERP_LIN_0011_1(oy(j),ox(j),iy,ix)
    end do

    return
  end procedure INTERP_LIN_1111_1

  !=========================================================================

  module procedure INTERP_LIN_1111_2

    integer J

    do J=1,size(OY)
       call INTERP_LIN_0011_2(oy(j),ox(j),iy,ix)
    end do

    return
  end procedure INTERP_LIN_1111_2

  !=========================================================================

  module procedure INTERP_LIN_2121_1

    integer J

    do J=1,size(OY,2)
       call INTERP_LIN_1111_1(oy(:,j),ox,iy(:,j),ix)
    end do

    return
  end procedure INTERP_LIN_2121_1

  !=========================================================================

  module procedure INTERP_LIN_2121_2

    integer J

    do J=1,size(OY,2)
       call INTERP_LIN_1111_2(oy(:,j),ox,iy(:,j),ix)
    end do

    return
  end procedure INTERP_LIN_2121_2

  !=========================================================================

  module procedure INTERP_LIN_2111_1

    integer J

    do J=1,size(OY,2)
       call INTERP_LIN_1111_1( OY(:,J), OX, IY, IX)
    end do

    return
  end procedure INTERP_LIN_2111_1

  !=========================================================================

  module procedure INTERP_LIN_2111_2

    integer J

    do J=1,size(OY,2)
       call INTERP_LIN_1111_2( OY(:,J), OX, IY, IX)
    end do

    return
  end procedure INTERP_LIN_2111_2

  !=========================================================================

  module procedure INTERP_LIN_3321_1

    integer I,J

    do J=1,size(OY,2)
       do I=1,size(OY,1)
          call INTERP_LIN_1111_1( OY(I,J,:), OX(I,J,:), IY(J,:), IX(:))
       end do
    end do

    return
  end procedure INTERP_LIN_3321_1

  !=========================================================================

  module procedure INTERP_LIN_3321_2

    integer I,J

    do J=1,size(OY,2)
       do I=1,size(OY,1)
          call INTERP_LIN_1111_2( OY(I,J,:), OX(I,J,:), IY(J,:), IX(:))
       end do
    end do

    return
  end procedure INTERP_LIN_3321_2

end submodule Interp_implementation

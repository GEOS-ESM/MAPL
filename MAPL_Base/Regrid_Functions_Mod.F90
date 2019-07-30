!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: regrid_functions_mod
!
! !DESCRIPTION: Module regrid_functions_mod contains functions for regridding.
!\\
!\\
! !INTERFACE:
!
  MODULE Regrid_Functions_Mod
!
! !USES:
!
      Use netcdf
      use, intrinsic :: ISO_FORTRAN_ENV, only: INT64

      Implicit None
      Private
!
! !PUBLIC DATA MEMBERS:
!
!
! !PUBLIC MEMBER FUNCTIONS:
!
      Public :: Assert
      Public :: GetLUN
      Public :: Set_fID
      Public :: Cleanup
      Public :: readTileFile
      Public :: readTileFileNC
      Public :: readTileFileNC_file
      Public :: ReadInput
      Public :: genGridName
      Public :: parseGridName
      Public :: nXYToVec
      Public :: regridData
      Public :: II_In
      Public :: JJ_in
      Public :: II_Out
      public :: JJ_Out
      public :: W
!
! !PRIVATE MEMBER FUNCTIONS:
!
!
! !REVISION HISTORY:
!  09 Jan 2017 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !PRIVATE TYPES:
!
      ! Precision
      Integer, Parameter              :: sp = kind(1.0)
      Integer, Parameter              :: dp = selected_real_kind(2*precision(1.0_sp))

      ! File IDs
      Integer :: fIDInLocal  = -1 ! Shadow for the main variables
      Integer :: fIDOutLocal = -1 ! Shadow for the main variables

      Integer                    :: nLon, nLat, nCS

      ! Tile file variables
      Integer                      :: nWeight
      integer(kind=INT64), Allocatable :: II_In(:)
      integer(kind=INT64), Allocatable :: JJ_In(:)
      integer(kind=INT64), Allocatable :: II_Out(:)
      integer(kind=INT64), Allocatable :: JJ_Out(:)
      Real(Kind=sp), Allocatable   :: W(:)
      Real(Kind=sp), Allocatable   :: outSum(:,:)

      CONTAINS
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Assert
!
! !DESCRIPTION: Subroutine to check if the output value is valid
!\\
!\\
! !INTERFACE:
!
      Subroutine Assert(RC,CallRoutine,CallMsg,ncID)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Integer                              :: RC
      Character(Len=*),Intent(In)          :: CallRoutine
      Character(Len=*),Intent(In),Optional :: CallMsg
      Integer,Intent(In),Optional          :: ncID
!
! !OUTPUT PARAMETERS:
!
      !Real(kind=sp), Intent(Out) :: TestOut
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Character(Len=255) :: OutMsg
      Integer :: status

      !=================================================================
      ! Assert starts here!
      !=================================================================

      If (RC.ne.0) Then
         If (Present(ncID)) Then
            ! Close the open file
            RC = NF90_CLOSE(ncid=ncID)
         End If
         If (Present(CallMsg)) Then
            OutMsg = Trim(CallMsg)
         Else
            Write(OutMsg,'(a,I8)') 'Error ID: ',RC
         End If
         Write(6,'(a)')   '================ FAILURE ================'
         Write(6,'(a,a)') 'Regrid failed during: ', Trim(CallRoutine)
         Write(6,'(a,a)') 'Failure message     : ', Trim(OutMsg)
         Call Cleanup(RC=status)
         Write(6,'(a)')   '================ FAILURE ================'
         Status=RC
         stop 1
      End If

      End Subroutine Assert
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Set_fID
!
! !DESCRIPTION: Routine Set_fID sets the file ID shadow variables
!\\
!\\
! !INTERFACE:
!
      Subroutine Set_fID(fIDIn, fIDOut, RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Integer, Optional                    :: RC
      Integer, Intent(In), Optional        :: fIDIn
      Integer, Intent(In), Optional        :: fIDOut
!
! !OUTPUT PARAMETERS:
!
      !Real(kind=sp), Intent(Out) :: TestOut
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Character(Len=255) :: OutMsg

      !=================================================================
      ! Set_fID starts here!
      !=================================================================

      RC = 0
      If (Present(fIDIn)) Then
         If (fIDInLocal.lt.0) Then
            fIDInLocal = fIDIn
         Else
            Call Assert(-1,'Set_fID','fIDIn already set')
         End If
      End If
 
      If (Present(fIDOut)) Then
         If (fIDOutLocal.lt.0) Then
            fIDOutLocal = fIDOut
         Else
            Call Assert(-1,'Set_fID','fIDOut already set')
         End If
      End If

      End Subroutine Set_fID
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: Cleanup
!
! !DESCRIPTION: Routine Cleanup closes any remaining open files
!\\
!\\
! !INTERFACE:
!
      Subroutine Cleanup(RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Integer, Optional                    :: RC
!
! !OUTPUT PARAMETERS:
!
      !Real(kind=sp), Intent(Out) :: TestOut
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Character(Len=255) :: OutMsg

      !=================================================================
      ! Cleanup starts here!
      !=================================================================

      If (fIDInLocal.gt.-1) Then
         RC = NF90_CLOSE(ncid=fIDInLocal)
         Write(6,'(a,I0.3)') 'Closed input file.  Result: ', RC
      End If
 
      If (fIDOutLocal.gt.-1) Then
         RC = NF90_CLOSE(ncid=fIDOutLocal)
         Write(6,'(a,I0.3)') 'Closed output file. Result: ', RC
      End If

      ! Tile file variables
      If (Allocated(II_In))    Deallocate(II_In)
      If (Allocated(JJ_In))    Deallocate(JJ_In)
      If (Allocated(II_Out))   Deallocate(II_Out)
      If (Allocated(JJ_Out))   Deallocate(JJ_Out)
      If (Allocated(W))        Deallocate(W)
      If (Allocated(outSum))   Deallocate(outSum)
 
      End Subroutine Cleanup
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: readTileFileNC
!
! !DESCRIPTION: Routine readTileFileNC reads a Tempest NetCDF tile file
!  and stores the relevant data in module variables for later use.
!\\
!\\
! !INTERFACE:
!
      subroutine readTileFileNC(TFDir,gridIn,gridOut,RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Character(Len=*), Intent(In)         :: TFDir
      Character(Len=*), Intent(In)         :: gridIn
      Character(Len=*), Intent(In)         :: gridOut
      Integer, Optional                    :: RC
!
! !OUTPUT PARAMETERS:
!
      !Real(kind=sp), Intent(Out) :: TestOut
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Character(Len=255)        :: fName, errMsg
      Logical                   :: Found
      Integer                   :: status

      !=================================================================
      ! readTileFileNC starts here!
      !=================================================================

      ! Assemble the full tile file name
      Write(fName,'(a,a,a,a)') Trim(gridIn),'_',Trim(gridOut),'.nc'
      fName = Trim(TFDir) // '/' // Trim(fName)

      Inquire(File=fName,Exist=Found)
      If (.not.Found) Then
         ! Try the reverse name
         Write(fName,'(a,a,a,a)') Trim(gridOut),'_',Trim(gridIn),'.nc'
         fName = Trim(TFDir) // '/' // Trim(fName)

         Inquire(File=fName,Exist=Found)
         If (.not.Found) Then
            Write(6,'(a,a)') ' --- Could not find NetCDF tile file ',Trim(fName)
            RC = -1
            Return
         End If
      End If


      call readTileFileNC_file(fName, rc=status)
      if (present(rc)) rc = status
      
      end subroutine readTileFileNC

      subroutine  readTileFileNC_file(fName, RC)
        character(len=*), intent(in) :: fName
        integer, optional :: rc

        integer :: fID
      Real(Kind=sp),Allocatable :: RTemp(:)
      Integer,Allocatable       :: ITemp(:)
      Integer,Allocatable       :: IITemp(:), JJTemp(:)
      Integer                   :: I, nGrids, gridInIdx, iX, iY, iG
      Integer                   :: iGrid, iFace
      Integer                   :: nDimIn, nDimOut
      Integer                   :: nX(2), nY(2)


      ! Expected grid sizes
      Integer                   :: resIn(2), resOut(2)

      ! Grid sizes on file
      integer                   :: resInFile(2), resOutFile(2)

      ! Open NetCDF file
      RC = NF90_Open(path=fName,mode=NF90_NOWRITE,ncid=fID)
      Call Assert(RC,'readTileFileNC','Open tile file')

      ! How many weights are there?
      RC = NF90_INQ_DIMID(ncid=fID, name='n_s', dimid=I)
      If (RC.eq.0) Then
         RC = NF90_INQUIRE_DIMENSION(ncid=fID, dimid=I, len=nWeight)
      End If
      Call Assert(RC,'readTileFileNC','Get weight count',fID)

      ! Allocate the arrays
      Allocate(II_In(nWeight))
      Allocate(JJ_In(nWeight))
      Allocate(II_Out(nWeight))
      Allocate(JJ_Out(nWeight))
      Allocate(W(nWeight))

      ! Each Tempest tile file connects two grids
      nGrids = 2

      ! Determine the input and output dimensions
      RC = NF90_INQ_DIMID(ncid=fID, name='src_grid_rank', dimid=I)
      RC = NF90_INQUIRE_DIMENSION(ncid=fID, dimid=I, len=nDimIn)
      Allocate(ITemp(nDimIn))
      RC = NF90_INQ_VARID(ncid=fID, name='src_grid_dims', varid=I)
      RC = NF90_GET_VAR(ncid=fID, varid=I, values=ITemp)
      resInFile(1:nDimIn) = ITemp(1:nDimIn)
      Deallocate(ITemp)
      If (nDimIn == 1) Then
         ! Cubed-sphere grid
         I = resInFile(1)
         resInFile(1) = Int(sqrt(float(I/6)))
         resInFile(2) = resInFile(1) * 6
      End If

      RC = NF90_INQ_DIMID(ncid=fID, name='dst_grid_rank', dimid=I)
      RC = NF90_INQUIRE_DIMENSION(ncid=fID, dimid=I, len=nDimOut)
      Allocate(ITemp(nDimOut))
      RC = NF90_INQ_VARID(ncid=fID, name='dst_grid_dims', varid=I)
      RC = NF90_GET_VAR(ncid=fID, varid=I, values=ITemp)
      resOutFile(1:nDimOut) = ITemp(1:nDimOut)
      Deallocate(ITemp)
      If (nDimOut == 1) Then
         ! Cubed-sphere grid
         I = resOutFile(1)
         resOutFile(1) = Int(sqrt(float(I/6)))
         resOutFile(2) = resOutFile(1) * 6
      End If

!$$      ! Get the expected grid sizes
!$$      Call parseGridName(gridIn,  resIn(1),  resIn(2))
!$$      Call parseGridName(gridOut, resOut(1), resOut(2))
!$$
      ! Assign nX and nY to match the format used for binary tile file
      nX(1) = resInFile(1)
      nY(1) = resInFile(2)
      nX(2) = resOutFile(1)
      nY(2) = resOutFile(2)

!$$      If (all(resIn.eq.resInFile).and.all(resOut.eq.resOutFile)) Then
!$$         ! Matched, and the direction matches
         gridInIdx = 1
!$$      Else If (all(resIn.eq.resOutFile).and.all(resOut.eq.resInFile)) Then
!$$         ! Matched, but for reverse transform
!$$         gridInIdx = 2
!$$         I = nX(2)
!$$         nX(2) = nX(1)
!$$         nX(1) = I
!$$
!$$         I = nY(2)
!$$         nY(2) = nY(1)
!$$         nY(1) = I
!$$      Else
!$$         RC = NF90_CLOSE(ncid=fID)
!$$         RC = -2
!$$         Return
!$$      End If

      ! Allocate temporary arrays
      Allocate(ITemp(nWeight))
      Allocate(IITemp(nWeight))
      Allocate(JJTemp(nWeight))
      Allocate(RTemp(nWeight))

      ! Read data for grid 1
      ! X-dim and Y-dim indices
      RC = NF90_INQ_VARID(ncid=fID, name='col', varid=I)
      RC = NF90_GET_VAR(ncid=fID, varid=I, values=ITemp)
      iG = gridInIdx
      Do I=1,nWeight
         ! Exploit integer division
         iY = 1 + ((ITemp(I)-1)/nX(iG))
         iX = ITemp(I) - ((iY-1)*nX(iG))
         IITemp(I) = iX
         JJTemp(I) = iY
      End Do
      If (gridInIdx == 1) Then
         II_In = IITemp
         JJ_In = JJTemp
      Else
         II_Out = IITemp
         JJ_Out = JJTemp
      End If

      ! Read data for grid 2
      ! X-dim and Y-dim indices
      RC = NF90_INQ_VARID(ncid=fID, name='row', varid=I)
      RC = NF90_GET_VAR(ncid=fID, varid=I, values=ITemp)
      iG = 2 - (gridInIdx - 1)
      Do I=1,nWeight
         ! Exploit integer division
         iY = 1 + ((ITemp(I)-1)/nX(iG))
         iX = ITemp(I) - ((iY-1)*nX(iG))
         IITemp(I) = iX
         JJTemp(I) = iY
      End Do
      If (gridInIdx == 1) Then
         II_Out = IITemp
         JJ_Out = JJTemp
      Else
         II_In = IITemp
         JJ_In = JJTemp
      End If

      ! Weights
      RC = NF90_INQ_VARID(ncid=fID, name='S', varid=I)
      RC = NF90_GET_VAR(ncid=fID, varid=I, values=RTemp)
      W = RTemp

      ! Close the tile file 
      RC = NF90_CLOSE(ncid=fID)

      ! Remap the cube faces
      Do iGrid = 1, nGrids
         If (nY(iGrid)==(6*nX(iGrid))) Then
            ! Assume this is a CS resolution
            If (iGrid==1) Then
               IITemp = II_In
               JJTemp = JJ_In
            Else
               IITemp = II_Out
               JJTemp = JJ_Out
            End If
            ! Re-order faces to match GMAO conventions
            Call swapCS(IITemp,JJTemp,nX(iGrid),nY(iGrid),nWeight)
            ! Flip face 6 in both II and JJ
            Call flipCS(IITemp,JJTemp,nX(iGrid),nY(iGrid),nWeight,6,0)
            ! Transpose faces 3-5 and flip their II indices
            Do iFace=3,5
               Call transposeCS(IITemp,JJTemp,nX(iGrid),nY(iGrid),nWeight,iFace)
               Call flipCS(IITemp,JJTemp,nX(iGrid),nY(iGrid),nWeight,iFace,1)
            End Do
            ! Reassign
            If (iGrid==1) Then
               II_In = IITemp
               JJ_In = JJTemp
            Else
               II_Out = IITemp
               JJ_Out = JJTemp
            End If
         EndIf
      EndDo

#undef CSREGRID_DEBUG
#if defined ( CSREGRID_DEBUG )
      Write(6,'(a,I12)') 'Writing out data. Count: ',nWeight
      Do I=1,nWeight
         Write(6,'(I12,4(x,I12),x,E16.5E4)') I,II_In(I),JJ_In(I),&
           II_Out(I), JJ_Out(I), W(I)
      End Do
#endif

      ! Allocate the counting variable
      ! nX and nY already swapped
      Allocate(outSum(nX(2),nY(2)))

      ! Deallocate temporary arrays
      Deallocate(ITemp)
      Deallocate(IITemp)
      Deallocate(JJTemp)
      Deallocate(RTemp)

      RC = 0

      ! Error check
      If ((MinVal(II_In)<1).or.(MinVal(II_Out)<1).or.&
          (MinVal(JJ_In)<1).or.(MinVal(JJ_Out)<1)) RC = -5
      If ((MaxVal(II_In) >nX(1)).or.&
          (MaxVal(II_Out)>nX(2)).or.&
          (MaxVal(JJ_In) >nY(1)).or.&
          (MaxVal(JJ_Out)>nY(2))) RC = -15


      end subroutine readTileFileNC_file
!EOC
      Subroutine transposeCS(II,JJ,nX,nY,nVal,iFace)
         Integer, Intent(In)     :: nVal
         Integer, Intent(InOut)  :: II(nVal)
         Integer, Intent(InOut)  :: JJ(nVal)
         Integer, Intent(In)     :: nX
         Integer, Intent(In)     :: nY
         Integer, Intent(In)     :: iFace
         Integer                 :: minJJ, maxJJ
         Integer                 :: II0(nVal)
         Integer                 :: JJ0(nVal)
         Integer                 :: I

         ! Copy input
         II0 = II
         JJ0 = JJ

         minJJ = nX*(iFace-1) + 1
         maxJJ = nX*iFace

         Do I=1,nVal
            If ((JJ0(I).ge.minJJ).and.(JJ0(I).le.maxJJ)) Then
               II(I) = JJ0(I) - minJJ + 1
               JJ(I) = II0(I) + minJJ - 1
            End If
         End Do

      End Subroutine transposeCS
      Subroutine flipCS(II,JJ,nX,nY,nVal,iFace,iDir)
         Integer, Intent(In)     :: nVal
         Integer, Intent(InOut)  :: II(nVal)
         Integer, Intent(InOut)  :: JJ(nVal)
         Integer, Intent(In)     :: nX
         Integer, Intent(In)     :: nY
         Integer, Intent(In)     :: iFace
         Integer, Intent(In)     :: iDir
         Integer                 :: minII, maxII
         Integer                 :: minJJ, maxJJ
         Integer                 :: II0(nVal)
         Integer                 :: JJ0(nVal)
         Integer                 :: I
         Logical                 :: flipII, flipJJ

         ! Copy input
         II0 = II
         JJ0 = JJ

         minJJ = nX*(iFace-1) + 1
         maxJJ = nX*iFace
         minII = 1
         maxII = nX
         flipII = ((iDir.eq.0).or.(iDir.eq.1))
         flipJJ = ((iDir.eq.0).or.(iDir.eq.2))

         Do I=1,nVal
            If ((JJ0(I).ge.minJJ).and.(JJ0(I).le.maxJJ)) Then
               if (flipII) II(I) = minII + (maxII - II0(I))
               if (flipJJ) JJ(I) = minJJ + (maxJJ - JJ0(I))
            End If
         End Do

      End Subroutine flipCS
      Subroutine swapCS(II,JJ,nX,nY,nVal)
         Integer, Intent(In)     :: nVal
         Integer, Intent(InOut)  :: II(nVal)
         Integer, Intent(InOut)  :: JJ(nVal)
         Integer, Intent(In)     :: nX
         Integer, Intent(In)     :: nY
         Integer                 :: iFace
         Integer                 :: xFace
         Integer                 :: minJJ, maxJJ, minJJOut
         Integer                 :: II0(nVal)
         Integer                 :: JJ0(nVal)
         Integer                 :: I
         Logical                 :: flipII, flipJJ
         Integer, Parameter      :: faceMap(6) = (/4,5,1,2,6,3/)

         ! Copy input
         II0 = II
         JJ0 = JJ

         Do iFace = 1,6
            ! Re-order faces from Tempest to GMAO convention
            xFace = faceMap(iFace)
            minJJ    = nX*(iFace-1) + 1
            minJJOut = nX*(xFace-1) + 1
            maxJJ    = nX*iFace
            Do I=1,nVal
               If ((JJ0(I).ge.minJJ).and.(JJ0(I).le.maxJJ)) Then
                  JJ(I) = minJJOut + JJ0(I) - minJJ
               End If
            End Do
         End Do

      End Subroutine swapCS
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: readTileFile
!
! !DESCRIPTION: Routine readTileFile reads a tile file and stores the
!  relevant data in module variables for later use.
!\\
!\\
! !INTERFACE:
!
      Subroutine readTileFile(TFDir,gridIn,gridOut,RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Character(Len=*), Intent(In)         :: TFDir
      Character(Len=*), Intent(In)         :: gridIn
      Character(Len=*), Intent(In)         :: gridOut
      Integer, Optional                    :: RC
!
! !OUTPUT PARAMETERS:
!
      !Real(kind=sp), Intent(Out) :: TestOut
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Character(Len=255)        :: fName, errMsg
      Logical                   :: Found
      Integer                   :: fID, status
      Real(Kind=sp),Allocatable :: RTemp(:)
      Integer,Allocatable       :: ITemp(:)
      Integer                   :: I, nGrids, gridInIdx
      Integer                   :: nX(2), nY(2)
      Character(Len=128)        :: STemp
      Character(Len=128)        :: gridNameTF(2)

      !=================================================================
      ! readTileFile starts here!
      !=================================================================

      ! Assemble the full tile file name
      Write(fName,'(a,a,a,a)') Trim(gridIn),'_',Trim(gridOut),'.bin'
      fName = Trim(TFDir) // '/' // Trim(fName)

      Inquire(File=fName,Exist=Found)
      If (.not.Found) Then
         ! Try the reverse name
         Write(fName,'(a,a,a,a)') Trim(gridOut),'_',Trim(gridIn),'.bin'
         fName = Trim(TFDir) // '/' // Trim(fName)

         Inquire(File=fName,Exist=Found)
         If (.not.Found) Then
            Write(6,'(a,a)') ' --- Could not find binary tile file ',Trim(fName)
            RC = -1
            Return
         End If
      End If

      ! Tile file variables
      Call GetLUN(fID,RC=RC)
      Call Assert(RC,'readTileFile','GetLUN failed')

      ! NOTE: Tile files are little-endian
      Open(File=Trim(fName),Unit=fID,IOStat=status,&
               FORM='UNFORMATTED',STATUS='OLD',CONVERT='little_endian')
      If (status/=0) Then
         Write(errMsg,'(a,a,a,I8)') 'Failed to open ',Trim(fName),&
            '. ID: ', status
         Call Assert(status,'readTileFile',errMsg)
      End If

      ! Read in the number of weights
      Read(fID) nWeight

      ! Allocate the arrays
      Allocate(II_In(nWeight))
      Allocate(JJ_In(nWeight))
      Allocate(II_Out(nWeight))
      Allocate(JJ_Out(nWeight))
      Allocate(W(nWeight))

      ! Also allocate temporary arrays
      Allocate(ITemp(nWeight))
      Allocate(RTemp(nWeight))

      Read(fID) nGrids
      If (nGrids.ne.2) Then
         Close(Unit=fID)
         Call Assert(-1,'readTileFile','Bad grid count')
      End If
      
      Do I=1,nGrids
         Read(fID) STemp
         Read(fID) nX(I)
         Read(fID) nY(I)
         gridNameTF(I) = Trim(STemp)
      End Do 

      Found = .False.
      Do I=1,nGrids
         If (Trim(gridNameTF(I)) == Trim(gridIn)) Then
            Found = .True.
            Exit
         End If
      End Do

      If (.not.Found) Then
         Close(Unit=fID)
         Call Assert(-1,'readTileFile','Input grid name mismatch')
      End If

      gridInIdx = I
      If (.not.Found) Then
         Close(Unit=fID)
         Call Assert(-1,'readTileFile','Input grid name mismatch')
      End If

      ! Must arrange so that grid 1 is the input grid
      If (gridInIdx .ne. 1) Then
         STemp = gridNameTF(2)
         gridNameTF(2) = gridNameTF(1)
         gridNameTF(1) = STemp

         I = nX(2)
         nX(2) = nX(1)
         nX(1) = I

         I = nY(2)
         nY(2) = nY(1)
         nY(1) = I
      End If

      Found = (gridNameTF(1) == gridIn)
      If (.not.Found) Then
         Close(Unit=fID)
         Call Assert(-1,'readTileFile','Rearrangement failed')
      End If

      Found = (gridNameTF(2) == gridOut)
      If (.not.Found) Then
         Close(Unit=fID)
         Call Assert(-1,'readTileFile','Output grid name mismatch')
      End If

      ! Skip 3 fields
      Read(fID)
      Read(fID)
      Read(fID)

      ! Read data for grid 1
      ! X-dim indices
      Read(fID) RTemp
      ITemp = NINT(RTemp)
      If (gridInIdx == 1) Then
         II_In = ITemp
      Else
         II_Out = ITemp
      End If

      ! Y-dim indices
      Read(fID) RTemp
      ITemp = NINT(RTemp)
      If (gridInIdx == 1) Then
         JJ_In = ITemp
      Else
         JJ_Out = ITemp
      End If

      Read(fID) RTemp
      ! Doesn't actually matter whether we use W_In or W_Out
      W = RTemp

      ! Now repeat for grid 2
      ! X-dim indices
      Read(fID) RTemp
      ITemp = NINT(RTemp)
      If (gridInIdx == 2) Then
         II_In = ITemp
      Else
         II_Out = ITemp
      End If

      ! Y-dim indices (in)
      Read(fID) RTemp
      ITemp = NINT(RTemp)
      If (gridInIdx == 2) Then
         JJ_In = ITemp
      Else
         JJ_Out = ITemp
      End If

      !Read(fID) RTemp
      !W = RTemp

      ! Close the tile file 
      Close(Unit=fID)

      ! Allocate the counting variable
      ! nX and nY already swapped
      Allocate(outSum(nX(2),nY(2)))

      ! Deallocate temporary arrays
      Deallocate(ITemp)
      Deallocate(RTemp)

      End Subroutine readTileFile
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: genGridName
!
! !DESCRIPTION: Routine genGridName returns the name of a grid based on
!  certain parameters. This mimics the MAPL behavior.
!\\
!\\
! !INTERFACE:
!
      Subroutine genGridName(nX, nY, gridName, xVec, yVec, &
                                   isCS, isPC, isDE, rc)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Integer, Intent(In)                  :: nX, nY
      Real(Kind=sp), Optional, Intent(In)  :: xVec(:)
      Real(Kind=sp), Optional, Intent(In)  :: yVec(:)
      Integer, Optional                    :: RC
!
! !OUTPUT PARAMETERS:
!
      Character(Len=255), Intent(Out)      :: gridName
      Logical, Optional, Intent(Out)       :: isCS
      Logical, Optional, Intent(Out)       :: isDE
      Logical, Optional, Intent(Out)       :: isPC
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Character(Len=2)         :: dateLine, pole
      Integer                  :: I
      Real(Kind=sp)            :: dY
      Real(Kind=sp), Parameter :: eps=1.0e-4
      Logical                  :: isCS_
      Logical                  :: isDE_
      Logical                  :: isPC_

      !=================================================================
      ! genGridName starts here!
      !=================================================================

      ! Defaults
      isCS_ = .False.
      isDE_ = .False.
      isPC_ = .False.
      If (nY .ne. (6*nX)) Then
         isCS_ = .False.
         ! Rectangular (lat-lon) grid
         dateLine = 'UU' ! Undefined
         pole = 'UU'     ! Undefined
         If (present(xVec) .and. present(yVec)) then
            !==============================================================
            ! There are two ways that a half-polar grid could be specified
            ! Either the grid center is specified as being at the pole, or
            ! it is specified accurately.
            !==============================================================
            dY = yVec(4) - yVec(3)
            if ((abs(yVec(1) + 90.0) < eps).or.&
                  (abs(yVec(1) + 90.0 - 0.25*dY) < eps)) then
               isPC_ = .True.
               pole='PC'
            else if (abs(yVec(1) + 90.0 - 0.5*dY) < eps) then
               pole='PE'
            end if
            !==============================================================
            do I=0,1
               if(abs(xVec(1) + 180.0*I) < eps) then
                  dateline='DC'
                  exit
               else if (abs(xVec(1) + 180.0*I - &
                          0.5*(xVec(2)-xVec(1))) < eps) then
                  isDE_ = .True.
                  dateline='DE'
                  exit
               end if
            end do
         end if
         write(gridname,'(a,i4.4,a,a,i4.4)') dateline,nX,'x',pole,nY
      else
         ! cubed-sphere
         isCS_ = .True.
         dateline='CF'
         pole='6C'
         write(gridname,'(a,i4.4,a,a)') dateline,nX,'x',pole
      end if
 
      ! Assign outputs 
      If (present(isCS)) isCS = isCS_
      If (present(isDE)) isDE = isDE_
      If (present(isPC)) isPC = isPC_

      End Subroutine genGridName
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: parseGridName
!
! !DESCRIPTION: Routine parseGridName determines a grid specification
! based on a string. The string can either be a standardized MAPL grid
! name (eg DE0180xPC0091, CF0024x6C) or a small number of shorthand
! options:
!       4x5          GMAO 4x5 grid
!       2x2.5        GMAO 2x2.5 grid
!       1x1.25       GMAO 1x1.25 grid
!       1x1          GMAO 1x1 grid
!       0.5x0.625    GMAO 0.5x0.625 grid
!       0.25x0.3125  GMAO 0.25x0.3125 grid
!\\
!\\
! !INTERFACE:
!
      Subroutine parseGridName( gridName, nX, nY, isCS, isDE, isPC )
!
! !USES:
!

!
! !INPUT PARAMETERS:
!
      Character(Len=255), Intent(In)       :: gridName
!
! !OUTPUT PARAMETERS:
!
      Integer, Intent(Out)                 :: nX, nY
      Logical, Optional, Intent(Out)       :: isCS
      Logical, Optional, Intent(Out)       :: isDE
      Logical, Optional, Intent(Out)       :: isPC
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Integer                  :: nChar, xIdx
      Character(Len=255)       :: trimName
      Logical                  :: isCS_, isDE_, isPC_
      Real(sp)                 :: nFloat

      !=================================================================
      ! parseGridName starts here!
      !=================================================================

      ! Set defaults
      nX = 0
      nY = 0
      isCS_ = .True.
      isDE_ = .True.
      isPC_ = .True.

      ! Branching options
      ! First: check that we have enough characters to do anything useful
      trimName = AdjustL(gridName)
      nChar = len(Trim(trimName))

      ! Basic input check
      If (nChar > 1) Then
          If (trimName(1:1) == "C") Then
             ! Two possibilities
             ! C[N]         Shorthand
             ! CFNNNNx6C    MAPL notation
             ! Assume MAPL if last character is also C
             isCS_ = .True.
             isDE_ = .False.
             isPC_ = .False.
             If (trimName(nChar:nChar) == "C") Then
                Read(trimName(3:6),*) nFloat
             Else
                Read(trimName(2:),*) nFloat
             End If
             ! Check that the number was parseable
             If (nFloat == nFloat) Then
                nX = Int(nFloat)
                nY = nX * 6
             End If
          ElseIf (trimName(1:1) == "D") Then
             ! Fixed length MAPL lat-lon identifier
             isCS_ = .False.
             If (nChar.eq.13) Then
                ! Use as temporary error indicators
                nX = 0
                nY = 0
                Select Case (trimName(1:2))
                   Case ('DC')
                      isDE_ = .False.
                   Case ('DE')
                      isDE_ = .True.
                   Case Default
                      nX = -1
                End Select
                Select Case (trimName(8:9))
                   Case ('PC')
                      isPC_ = .True.
                   Case ('PE')
                      isPC_ = .False.
                   Case Default
                      nY = -1
                End Select
                If ((nX + nY) == 0) Then
                   ! Doing OK
                   Read(trimName(3:6),*) nFloat
                   If (nFloat == nFloat) nX = Int(nFloat)
                   Read(trimName(10:13),*) nFloat
                   If (nFloat == nFloat) nY = Int(nFloat)
                   If ((nX*nY) == 0) Then
                      nX = 0
                      nY = 0
                   End If
                End If
             End If
          Else
             ! Assume a GMAO shorthand string (lat-lon)
             isCS_ = .False.
             xIdx = Index(trimName,'x')
             ! Did we find an x?
             If (xIdx > 0) Then
                Read(trimName(1:(xIdx-1)),*) nFloat
                If (nFloat == nFloat) nY = Int(180.0/nFloat) + 1
                Read(trimName((xIdx+1):nChar),*) nFloat
                If (nFloat == nFloat) nX = Int(360.0/nFloat)
                If ((nX*nY == 0)) Then
                   nX = 0
                   nY = 0
                Else
                   ! Successful parse
                   isDE_ = .False.
                   isPC_ = .True.
                End If
             End If
          End If
      End If
     
      ! Assign outputs 
      If (present(isCS)) isCS = isCS_
      If (present(isDE)) isDE = isDE_
      If (present(isPC)) isPC = isPC_

      End Subroutine parseGridName
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: nXYtoVec
!
! !DESCRIPTION: Routine nXYtoVec estimate the X/Y mid-point vectors
! based on as little data as possible.
!\\
!\\
! !INTERFACE:
!
      Subroutine nXYtoVec(xVec,yVec,isCS,isPC,isDE,RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
!
! !OUTPUT PARAMETERS:
!
      Real(sp),Intent(InOut)               :: xVec(:), yVec(:)
      Logical,Intent(In)                   :: isCS, isPC, isDE
      Integer, Optional                    :: RC
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Integer     :: nX, nY
      Integer     :: I, RC_
      Real(sp)    :: fTemp, fMin, fMax, fStride

      !=================================================================
      ! nXYtoVec starts here!
      !=================================================================

      ! Get sizes
      nX = Size(xVec)
      nY = Size(yVec)

      ! Assume success
      RC_ = 0

      ! Cubed sphere?
      If (isCS) Then
         If ((nX*6).ne.nY) Then
            RC_ = -1
         Else
            ! Simple system
            Do I = 1, nX
               xVec(I) = Float(I)
            End Do
            Do I = 1, nY
               yVec(I) = Float(I)
            End Do
         End If
      Else
         ! Longitude first 
         fStride = 360.0/Float(nX)
         If (isDE) Then
            fMin = (-180.0) - (fStride/2.0)
         Else
            fMin = (-180.0) - fStride
         End If
         Do I = 1, nX
            xVec(I) = fMin + (fStride * Float(I))
         End Do
         ! Now latitude
         If (isPC) Then
            fStride = (180.0 / Float(nY - 1))
            fMin = (-90.0) - fStride
         Else
            fStride = (180.0 / Float(nY))
            fMin = (-90.0) - (fStride/2.0)
         End If
         Do I = 1, nY
            yVec(I) = fMin + (fStride * Float(I))
         End Do
         If (isPC) Then
            yVec(1)  = (-90.0) + (fStride/4.0)
            yVec(nY) = ( 90.0) - (fStride/4.0)
         End If 
      End If
      If (Present(RC)) RC = RC_
 
      End Subroutine nXYToVec
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: GetLUN
!
! !DESCRIPTION: Routine GetLUN finds a valid logical unit number in the
! range 1-100. IMPORTANT: This is the minimal-effort version of this
! routine, and extremely slow!
!\\
!\\
! !INTERFACE:
!
      Subroutine GetLUN(LUN,RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
!
! !OUTPUT PARAMETERS:
!
      Integer, Intent(Out)                 :: LUN
      Integer, Optional                    :: RC
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Integer :: I, RC_
      Logical :: isOpen

      !=================================================================
      ! GetLUN starts here!
      !=================================================================

      isOpen = .True.
      LUN = -1
      RC_ = 0
      Do I = 8,100
         Inquire(Unit=I,Opened=isOpen)
         If (.not.isOpen) Then
            LUN = I
            Exit
         End If
      End Do
      If (isOpen) RC_ = -1
      If (Present(RC)) RC = RC_
 
      End Subroutine GetLUN
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: regridData
!
! !DESCRIPTION: Routine regridData regrids from lat-lon to cubed-sphere,
!  using tile file data already read in.
!\\
!\\
! !INTERFACE:
!
      Subroutine regridData(in2D,out2D,RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
      Real(Kind=sp),Intent(In)             :: in2D(:,:)
!
! !INPUT/OUTPUT PARAMETERS:
!
      Real(Kind=sp),Intent(InOut)          :: out2D(:,:)
!
! !OUTPUT PARAMETERS:
!
      Integer, Optional                    :: RC
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Integer                  :: I, iX, iY
      Real(kind=sp)            :: wVal, inVal, outVal, rCount
      Real(kind=sp), Parameter :: missingVal=0.0

      !=================================================================
      ! regridData starts here!
      !=================================================================

      ! Zero the output arrays
      outSum(:,:) = 0.0e0
      out2D(:,:) = 0.0e0
      Do I=1,nWeight
         iX = II_In(I)
         iY = JJ_In(I)
         wVal = W(I)
         inVal = in2D(iX,iY)
         iX = II_Out(I)
         iY = JJ_Out(I)
         outVal = wVal*inVal
         out2D(iX,iY) = out2D(iX,iY) + outVal
         outSum(iX,iY) = outSum(iX,iY) + wVal
      End Do
      Do iX = 1,Size(out2D,1)
      Do iY = 1,Size(out2D,2)
         If (outSum(iX,iY) .le. Tiny(1.0e+0_sp)) Then
            out2D(iX,iY) = missingVal
         Else
            wVal = outSum(iX,iY)
            out2D(iX,iY) = out2D(iX,iY)/wVal
         End If
      End Do 
      End Do
      If (Present(RC)) RC = 0
 
      End Subroutine regridData 
!EOC
!-----------------------------------------------------------------------
!                 GEOS-Chem Global Chemical Transport Model            !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: ReadInput
!
! !DESCRIPTION: Routine ReadInput reads the input options file.
!\\
!\\
! !INTERFACE:
!
      Subroutine ReadInput(resOut,fNameIn,fNameOut,reverseLev,&
                              isCSOut,isPCOut,isDEOut,RC)
!
! !USES:
!

!      Use Precision_Mod, Only: f4

!
! !INPUT PARAMETERS:
!
!
! !OUTPUT PARAMETERS:
!
      Integer, Intent(Out)                 :: resOut(2)
      Logical, Intent(Out)                 :: isCSOut
      Logical, Intent(Out)                 :: isPCOut
      Logical, Intent(Out)                 :: isDEOut
      Character(Len=255), Intent(Out)      :: fNameIn
      Character(Len=255), Intent(Out)      :: fNameOut
      Logical, Intent(Out)                 :: reverseLev
      Integer, Optional                    :: RC
!
! !REVISION HISTORY:
!  09 Jan 2016 - S. D. Eastham - Initial version
!EOP
!-----------------------------------------------------------------------
!BOC
!
! !LOCAL VARIABLES:
!
      Integer            :: fIDGCHP, RC_, IOS, nRead, I
      Integer            :: resTemp(2)
      Character(Len=255) :: currLine, strRead, leftStr, rightStr
      Logical            :: Found, logRead

      !=================================================================
      ! ReadInput starts here!
      !=================================================================

      ! Default outputs
      fNameIn=''
      fNameOut=''
      resOut(:) = 0
      isCSOut = .True.
      isPCOut = .True.
      isDEOut = .True.
      reverseLev=.False.

      ! First, get LUN
      Call GetLUN(fIDGCHP,RC=RC_)
      If (Present(RC)) RC = RC_
      If (RC_.ne.0) Return

      Open(File='input.regrid',Unit=fIDGCHP,IOStat=RC_,&
              STATUS='OLD',FORM='FORMATTED')
      If (RC_.ne.0) Return

      ! Read in the file; skip lines until we hit one that doesn't start
      ! with the # symbol
      RC_ = 0
      Found = .False.
      Do While (.not.Found)
         Read(fIDGCHP,'(a)',IOStat=RC_) currLine
         If (RC_.ne.0) Exit
         currLine = Trim(AdjustL(currLine))
         I = Scan(currLine,'#')
         Found = (I.ne.1)
      End Do

      ! Start processing
      If (Found) Then
         ! Output resolution
         I = SCAN(currLine,':')
         Read(currLine((I+1):),*,IOStat=RC_) strRead

         ! Parse the string
         Call parseGridName(strRead, resTemp(1), resTemp(2),&
                isCS=isCSOut, isDE=isDEOut, isPC=isPCOut)
         ! Invalid resolution string?
         If ((resTemp(1)*resTemp(2)) == 0) Then
            resTemp(:) = 0
            RC_ = -10
         End If
         resOut = resTemp
 
         ! Input file name
         Read(fIDGCHP,'(a)',IOStat=RC_) currLine
         I = SCAN(currLine,':')
         Read(currLine((I+1):),*,IOStat=RC_) strRead
         fNameIn = Trim(AdjustL(strRead))
 
         ! Output file name
         Read(fIDGCHP,'(a)',IOStat=RC_) currLine
         I = SCAN(currLine,':')
         Read(currLine((I+1):),*,IOStat=RC_) strRead
 
         fNameOut = Trim(AdjustL(strRead))

         ! Reverse vertical grid?
         Read(fIDGCHP,'(a)',IOStat=RC_) currLine
         I = SCAN(currLine,':')
         Read(currLine((I+1):),*,IOStat=RC_) logRead
         reverseLev = logRead
      Else 
         ! Report failure
         RC_ = -1
      End If
      Close(Unit=fIDGCHP)

      If (Present(RC)) RC = RC_
 
      End Subroutine ReadInput
!EOC
      End Module Regrid_Functions_Mod

!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#ifndef __PROTEX__
#  include "MAPL_Generic.h"
#endif
!
!>
!### MODULE: `MAPL_SimpleBundleMod`
!
! Author: GMAO SI-Team
!
! The module `MAPL_SimpleBundleMod` implements a wrapper class around the
! ESMF_FieldBundle. By construction, this is NOT an opaque object.
!
!#### History
!- April2010: Arlindo da Silva <arlindo.dasilva@nasa.gov>
!- 11Feb2011: Todling - remove ESMFL_BundleAddState since in MAPL_CFIO
!
   module MAPL_SimpleBundleMod

   use ESMF
   use ESMFL_Mod
   use MAPL_BaseMod
   use MAPL_CFIOMod
   use MAPL_MaxMinMod
   use MAPL_CommsMod, only: MAPL_AM_I_ROOT
   use MAPL_Constants, only: MAPL_PI
   use MAPL_ExceptionHandling

   implicit NONE
   private

   public MAPL_SimpleBundleCreate
   public MAPL_SimpleBundlePrint
   public MAPL_SimpleBundleGetIndex
   public MAPL_SimpleBundleDestroy

   public MAPL_SimpleBundleRead
   public MAPL_SimpleBundleWrite

   public MAPL_SimpleBundle

   type SimpleArray_1D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer   :: q(:) => null()      !! alias for qr4
      real(kind=ESMF_KIND_R4), pointer   :: qr4(:) => null()
      real(kind=ESMF_KIND_R8), pointer   :: qr8(:) => null()
   end type SimpleArray_1D

   type SimpleArray_2D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer   :: q(:,:) => null()    !! alias for qr4
      real(kind=ESMF_KIND_R4), pointer   :: qr4(:,:) => null()
      real(kind=ESMF_KIND_R8), pointer   :: qr8(:,:) => null()
   end type SimpleArray_2D

   type SimpleArray_3D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer   :: q(:,:,:) => null()   !! alias for qr4
      real(kind=ESMF_KIND_R4), pointer   :: qr4(:,:,:) => null()
      real(kind=ESMF_KIND_R8), pointer   :: qr8(:,:,:) => null()
   end type SimpleArray_3D

   type LcvGrid
      real(kind=ESMF_KIND_R4)          :: ptop = 0.01
      real(kind=ESMF_KIND_R4), pointer :: delp(:,:,:) => null()
   end type LcvGrid

   type SimpleGrid
      real(kind=ESMF_KIND_R4), pointer   :: Lons(:,:) => null() !! Longitudes in degrees
      real(kind=ESMF_KIND_R4), pointer   :: Lats(:,:) => null() !! Latitudes in degrees
      real(kind=ESMF_KIND_R4), pointer   :: Levs(:)   => null() !! Vertical Levels
      character(len=ESMF_MAXSTR)         :: LevUnits = '1'      !! Vertical Level units
      type(LcvGrid)                      :: lcv                 !! Lagrangian Control Volume
   end type SimpleGrid

   type MAPL_SimpleBundle
      character(len=ESMF_MAXSTR) :: name
      type(ESMF_FieldBundle), pointer    :: Bundle  !! Associated ESMF bundle
      type(ESMF_Grid)                    :: grid    !! Associated ESMF grid
      type(SimpleGrid)                   :: coords  !! Coordinate variables
      integer :: n1d=-1
      integer :: n2d=-1
      integer :: n3d=-1
      logical :: bundleAlloc = .false.
      type(SimpleArray_1D), pointer :: r1(:) => null()
      type(SimpleArray_2D), pointer :: r2(:) => null()
      type(SimpleArray_3D), pointer :: r3(:) => null()
   end type MAPL_SimpleBundle

!----------------------------------------------------------------------------

   interface MAPL_SimpleBundleWrite
      module procedure MAPL_SimpleBundleWrite1
      module procedure MAPL_SimpleBundleWrite2
   end interface MAPL_SimpleBundleWrite

   interface MAPL_SimpleBundleCreate
      module procedure MAPL_SimpleBundleCreateEmpty
      module procedure MAPL_SimpleBundleCreateFromBundle
      module procedure MAPL_SimpleBundleCreateFromState
   end interface MAPL_SimpleBundleCreate

CONTAINS

!............................................................................................


!-----------------------------------------------------------------------------
!>
! Given inputs, create a SimpleBundle.

  Function MAPL_SimpleBundleCreateEmpty ( grid, rc, &
                                          Levs, LevUnits, &
                                          ptop, delp,     &
                                          name) result (self)

    type(MAPL_SimpleBundle)                        :: self          !! Simple Bundle !rename to simpleBundle
    type(ESMF_Grid),                intent(in)     :: grid
    integer, OPTIONAL,              intent(out)    :: rc
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: Levs(:)       !! Vertical coordinates
                                                                    !! Constant levels
    character(len=*), OPTIONAL,     intent(in)     :: LevUnits      !! Level units
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: ptop          !! top pressure (Pa)
                                                                    !! Lagrangian Control Volume Info
    real(ESMF_KIND_R4), OPTIONAL, pointer, &
                                    intent(in)     :: delp(:,:,:)   !! layer thickness (Pa)
    character(len=*), OPTIONAL,     intent(in)     :: name          !! name
!
    character(len=ESMF_MAXSTR) :: bundleName
    integer :: im, jm, km, dims(3), i
    character(len=ESMF_MAXSTR) :: message
    real(ESMF_KIND_R8), pointer :: LonsRad(:,:), LatsRad(:,:)

    __Iam__('MAPL_SimpleBundleCreateEmpty')
!                           ------

    _UNUSED_DUMMY(Iam)
    self%Bundle => null()
!   Name the SimpleBundle
    if (present(name)) then
       if (len_trim(name) > ESMF_MAXSTR) then
           message = 'string "'//trim(name)//'" is too long to be used '// &
                     'as a Simple Bundle name'
           __raise__(MAPL_RC_ERROR, message)
       end if
       self%name = trim(name)
    else
       self%name = bundleName
    end if

!                             --------------------
!                             Coordinate variables
!                             --------------------
    self%grid = grid
    call MAPL_GridGet(self%Grid, localCellCountPerDim = dims, _RC)
    im = dims(1);  jm = dims(2);  km = dims(3)
    allocate(self%coords%Lons(im,jm), self%coords%Lats(im,jm), self%coords%Levs(km), __STAT__)

!   Retrieve the lat/lon from Grid and convert to degrees
!   -----------------------------------------------------
   call ESMF_GridGetCoord (self%Grid, coordDim=1, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=LonsRad, _RC)
   call ESMF_GridGetCoord (self%Grid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=LatsRad, _RC)
   self%coords%Lons(:,:) = ( 180. / MAPL_PI) * LonsRad(:,:)
   self%coords%Lats(:,:) = ( 180. / MAPL_PI) * LatsRad(:,:)

!  1-D Levels
!  ----------
   if ( present(Levs) ) then
      if ( size(Levs) == km ) then
         self%coords%Levs(:) = Levs(:)
      else
         _FAIL('Levs different than Levs from ESMF_Grid')
      end if
   else
      self%coords%Levs(:) = [(i, i = 1, km)]
   end if
   if ( present(LevUnits) ) then
      self%coords%LevUnits = LevUnits
   else
      self%coords%LevUnits = '1'
   end if

!  Optional Lagrangian Control Volume info
!  ---------------------------------------
   if ( present(ptop) ) self%coords%lcv%ptop = ptop
   if ( present(delp) ) then ! User specied
!ALT      self%coords%lcv%delp = delp
      self%coords%lcv%delp => delp
   else ! Look inside bundle for delp or DELP
      self%coords%lcv%delp => NULL()
   end if

   _RETURN(_SUCCESS)

  end Function MAPL_SimpleBundleCreateEmpty

!-----------------------------------------------------------------------------
!>
! Given an ESMF Bundle, creates a corresponding Simple Bundle. The
! specification of a vertical grid is optional but useful in many
! cases. The 1-D `Levs` will default to the layer number, and units of "1".
! Input parameters `(ptop,delp)` can be used to record the corresponding
! Lagrangian Control Volume Grid. When `delp` is not specified, variables
! `DELP` or `delp` are used if present inside the bundle.
!
  Function MAPL_SimpleBundleCreateFromBundle ( Bundle, rc,     &
                                               Levs, LevUnits, &
                                               ptop, delp,     &
                                               only_vars,      &
                                               strict,         &
                                               name) result (self)

    type(MAPL_SimpleBundle)                        :: self          !! Simple Bundle

    type(ESMF_FieldBundle), target, intent(inout)  :: Bundle        !! ESMF Bundle
    integer, OPTIONAL,              intent(out)    :: rc
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: Levs(:)       !! Vertical coordinates
                                                                    !! Constant levels
    character(len=*), OPTIONAL,     intent(in)     :: LevUnits      !! Level units
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: ptop          !! top pressure (Pa)
                                                                    !! Lagrangian Control Volume Info
    real(ESMF_KIND_R4), OPTIONAL, pointer, &
                                    intent(in)     :: delp(:,:,:)   !! layer thickness (Pa)
    character(len=*), OPTIONAL,     intent(in)     :: only_vars     !! comma separated field names
    logical, OPTIONAL,              intent(in)     :: strict        !! force name matching, ignored if only_vars is not present
    character(len=*), OPTIONAL,     intent(in)     :: name          !! name

!                           ------

    type(ESMF_Field) :: Field
    type(ESMF_Array) :: Array
    type(ESMF_TypeKind_Flag) :: typeKind
    real(ESMF_KIND_R8), pointer :: LonsRad(:,:), LatsRad(:,:)

    integer :: arrayRank, I, n, n1d, n2d, n3d, NumVars
    integer :: im, jm, km, dims(3)
    type(ESMF_FieldStatus_Flag) :: fieldStatus


    logical :: strict_match
    logical :: isPresent
    logical :: isPresentBundle
    logical :: haveDelp
    integer :: n_vars
    character(len=ESMF_MAXSTR) :: message
    logical, allocatable       :: isRequested(:)
    character(len=ESMF_MAXSTR), allocatable :: var_list(:)

    character(len=ESMF_MAXSTR) :: bundleName
    character(len=ESMF_MAXSTR) :: fieldName

    integer :: status

    self%Bundle => Bundle ! remember where it came from

    self%bundleAlloc = .false. ! this is the default. We can overwrite it later

    call ESMF_FieldBundleGet (BUNDLE, name=bundleName, &
                                      grid=self%grid, &
                                      FieldCount=NumVars, _RC )

    if (present(name)) then
       if (len_trim(name) > ESMF_MAXSTR) then
           message = 'string "'//trim(name)//'" is too long to be used '// &
                     'as a Simple Bundle name'
           __raise__(MAPL_RC_ERROR, message)
       end if

       self%name = trim(name)
    else
       self%name = bundleName
    end if

!                             -------------------
!                             Requested variables
!                             -------------------

    if (present(strict)) then
       strict_match = strict
    else
       strict_match = .true.  ! by default do strict name matching
    end if

    allocate(isRequested(NumVars), __STAT__)
    isRequested = .false.

    if (present(only_vars)) then
       n_vars = csv_tokens_count_(only_vars)
       _ASSERT(n_vars <= NumVars,'needs informative message')

       allocate(var_list(n_vars), __STAT__)

       var_list = '__NONE__'
       call csv_tokens_get_(only_vars, var_list, _RC)

       do i = 1, size(var_list)
          isPresent = .false.

          do n = 1, NumVars
             call MAPL_FieldBundleGet(BUNDLE, n, FIELD, _RC)
             call ESMF_FieldGet (FIELD, name=fieldName, _RC)

             if (fieldName == var_list(i)) then
                isPresent = .true.
                exit
             end if
          end do

          if (isPresent) then
             isRequested(n) = .true.
          else
             if (strict_match) then
                message = 'could not find field '//trim(var_list(i))// &
                          ' in Simple Bundle <'//trim(self%name)//'>'
                __raise__(MAPL_RC_ERROR, message)
             end if
          end if
       end do

       deallocate(var_list)
    else
       isRequested = .true.
    end if

!                             --------------------
!                             Coordinate variables
!                             --------------------

    call MAPL_GridGet(self%Grid, localCellCountPerDim = dims, _RC)
    im = dims(1);  jm = dims(2);  km = dims(3)
    allocate(self%coords%Lons(im,jm), self%coords%Lats(im,jm), self%coords%Levs(km), __STAT__)

!   Retrieve the lat/lon from Grid and convert to degrees
!   -----------------------------------------------------
   call ESMF_GridGetCoord (self%Grid, coordDim=1, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=LonsRad, _RC)
   call ESMF_GridGetCoord (self%Grid, coordDim=2, localDE=0, &
                           staggerloc=ESMF_STAGGERLOC_CENTER, &
                           farrayPtr=LatsRad, _RC)
   self%coords%Lons(:,:) = ( 180. / MAPL_PI) * LonsRad(:,:)
   self%coords%Lats(:,:) = ( 180. / MAPL_PI) * LatsRad(:,:)

!  1-D Levels
!  ----------
   if ( present(Levs) ) then
      if ( size(Levs) == km ) then
         self%coords%Levs(:) = Levs(:)
      else
         STATUS = 77
         _VERIFY(STATUS)
      end if
   else
      self%coords%Levs(:) = (/ (i, i = 1, km) /)
   end if
   if ( present(LevUnits) ) then
      self%coords%LevUnits = LevUnits
   else
      self%coords%LevUnits = '1'
   end if

!  Optional Lagrangian Control Volume info
!  ---------------------------------------
   if ( present(ptop) ) self%coords%lcv%ptop = ptop
   if ( present(delp) ) then ! User specied
!ALT      self%coords%lcv%delp = delp
      self%coords%lcv%delp => delp
   else ! Look inside bundle for delp or DELP
      self%coords%lcv%delp => NULL()

      haveDelp = .FALSE.
      call ESMF_FieldBundleGet (Bundle, fieldName='DELP', isPresent=isPresentBundle, RC=STATUS)
      _VERIFY(STATUS)
      if (isPresentBundle) then
         call ESMF_FieldBundleGet (Bundle, fieldName='DELP', field=Field, RC=STATUS)
         _VERIFY(STATUS)
         haveDelp = .TRUE.
      else
         call ESMF_FieldBundleGet (Bundle, fieldName='delp', isPresent=isPresentBundle, RC=STATUS)
         _VERIFY(STATUS)
         if (isPresentBundle) then
            call ESMF_FieldBundleGet (Bundle, fieldName='delp', field=Field, RC=STATUS)
            _VERIFY(STATUS)
            haveDelp = .TRUE.
         end if
      end if

      if (haveDelp) then
         call ESMF_FieldGet(Field, status=fieldStatus, _RC)
         if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
            call ESMF_FieldGet(Field, 0, self%coords%lcv%delp, _RC)
         end if
      end if
   end if

!                             --------------------
!                                 Data Arrays
!                             --------------------

!   A little overestimate for the sizes, but why bother
!   ---------------------------------------------------
    allocate(self%r1(NumVars), &
             self%r2(NumVars), &
             self%r3(NumVars), &
             __STAT__)

    n1d = 0
    n2d = 0
    n3d = 0
    DO I = 1, NumVars

       call MAPL_FieldBundleGet(BUNDLE, I, FIELD, _RC)
       call ESMF_FieldGet (FIELD, name=fieldName, status=fieldStatus, _RC)

       if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE .and. isRequested(I)) then
          call ESMF_FieldGet (FIELD, ARRAY=array, _RC )
          call ESMF_ArrayGet (array, rank=arrayRank, typeKind = typeKind, _RC )
       else
          cycle
       end if

!      Real*4
!      ------
       if ( typeKind == ESMF_TYPEKIND_R4 ) then
          if ( arrayRank == 1 ) then
             n1d = n1d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r1(n1d)%qr4, _RC )
             self%r1(n1d)%name = trim(fieldName)
             self%r1(n1d)%myKind = ESMF_KIND_R4
             self%r1(n1d)%q => self%r1(n1d)%qr4 ! convenience alias
          else if ( arrayRank == 2 ) then
             n2d = n2d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r2(n2d)%qr4, _RC )
             self%r2(n2d)%name = trim(fieldName)
             self%r2(n2d)%myKind = ESMF_KIND_R4
             self%r2(n2d)%q => self%r2(n2d)%qr4 ! convenience alias
         else if ( arrayRank == 3 ) then
             n3d = n3d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r3(n3d)%qr4, _RC )
             self%r3(n3d)%name = trim(fieldName)
             self%r3(n3d)%myKind = ESMF_KIND_R4
             self%r3(n3d)%q => self%r3(n3d)%qr4 ! convenience alias
          else
             STATUS = 77
             _VERIFY(STATUS)
          end if

!      Real*8
!      ------
       else if ( typeKind == ESMF_TYPEKIND_R8 ) then
          if ( arrayRank == 1 ) then
             n1d = n1d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r1(n1d)%qr8, _RC )
             self%r1(n1d)%name = trim(fieldName)
             self%r1(n1d)%myKind = ESMF_KIND_R8
          else if ( arrayRank == 2 ) then
             n2d = n2d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r2(n2d)%qr8, _RC )
             self%r2(n2d)%name = trim(fieldName)
             self%r2(n2d)%myKind = ESMF_KIND_R8
          else if ( arrayRank == 3 ) then
             n3d = n3d + 1
             call ESMF_FieldGet(Field, localDE=0, farrayPtr=self%r3(n3d)%qr8, _RC )
             self%r3(n3d)%name = trim(fieldName)
             self%r3(n3d)%myKind = ESMF_KIND_R8
          else
             STATUS = 77
             _VERIFY(STATUS)
          end if

!      Unknown kind
!      ------------
       else
          STATUS = 88
          _VERIFY(STATUS)
       end if

    end do

    self%n1d = n1d
    self%n2d = n2d
    self%n3d = n3d

    deallocate(isRequested, __STAT__)

    _RETURN(_SUCCESS)

  contains

    function csv_tokens_count_(str, delimiter) result(n)
      implicit none

      integer                         :: n
      character(len=*), intent(in)    :: str
      character, optional, intent(in) :: delimiter

      ! local
      character, parameter :: char_comma = ','
      character            :: c
      integer              :: i

      if (present(delimiter)) then
         c = delimiter
      else
         c = char_comma
      end if

      n = 1
      do i = 1, len_trim(str)
         if (str(i:i) == c) then
            n = n + 1
         end if
      end do
    end function csv_tokens_count_

    subroutine csv_tokens_get_(str, list, delimiter, ignore, rc)
      implicit none

      character(len=*), intent(in)     :: str
      character(len=*), intent(inout)  :: list(:)
      character, optional, intent(in)  :: delimiter
      character, optional, intent(in)  :: ignore
      integer, optional, intent(inout) :: rc

      ! local
      character, parameter :: char_empty = ''
      character, parameter :: char_space = ' '
      character, parameter :: char_comma = ','

      character :: c_dlm, c_ign
      integer   :: i, j, n, err

      if (present(delimiter)) then
         c_dlm = delimiter
      else
         c_dlm = char_comma
      end if

      if (present(ignore)) then
         c_ign = ignore
      else
         c_ign = char_space
      end if


      list = char_empty
      j = 1
      n = 1

      err = 0

      do i = 1, len_trim(str)

         if (str(i:i) /= c_dlm) then
            if (n > size(list)) then
               err = 99
               exit
            end if

            if (str(i:i) /= c_ign) then
               list(n)(j:j) = str(i:i)
               j = j + 1
            end if
         else
            j = 1
            n = n + 1
         end if

      end do

      if (present(rc)) then
         rc = err
      end if
    end subroutine csv_tokens_get_

  end Function MAPL_SimpleBundleCreateFromBundle

!-----------------------------------------------------------------------------
!>
! Given an ESMF Staete, creates a corresponding Simple Bundle.
! The specificatiopn of a vertical grid is optional but useful in many
! cases. The 1-D `Levs` will default to the layer number, and units of "1".
! Input parameters `(ptop,delp)` can be used to record the corresponding
! Lagrangian Control Volume Grid. When `delp` is not specified, variables
! `DELP` or `delp` are used if present inside the bundle.
!
! **IMPORTANT:** It is assumed that the ESMF State has a single grid.
!
  Function MAPL_SimpleBundleCreateFromState ( State, rc,      &
                                              Levs, LevUnits, &
                                              ptop, delp,     &
                                              only_vars,      &
                                              strict,         &
                                              name) result (self)

    type(MAPL_SimpleBundle)                        :: self          !! Simple Bundle

    type(ESMF_State), target, intent(inout)        :: State         !! ESMF State
    integer, OPTIONAL,              intent(out)    :: rc
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: Levs(:)       !! Vertical coordinates
                                                                    !! Constant levels
    character(len=*), OPTIONAL,     intent(in)     :: LevUnits      !! Level units
    real(ESMF_KIND_R4), OPTIONAL,   intent(in)     :: ptop          !! top pressure (Pa)
                                                                    !! Lagrangian Control Volume Info
    real(ESMF_KIND_R4), OPTIONAL, pointer, &
                                    intent(in)     :: delp(:,:,:)   !! layer thickness (Pa)
    character(len=*), OPTIONAL,     intent(in)     :: only_vars     !! comma separated field names
    logical, OPTIONAL,              intent(in)     :: strict        !! force name maching, ignored if only_vars is not present
    character(len=*), OPTIONAL,     intent(in)     :: name          !! name

    character(len=ESMF_MAXSTR) :: stateName
    character(len=ESMF_MAXSTR) :: bundleName
    character(len=ESMF_MAXSTR) :: message
    type (ESMF_FieldBundle)    :: Bundle

    integer :: status

    call ESMF_StateGet(State, name=stateName, _RC)

    if (present(name)) then
       if (len_trim(name) > ESMF_MAXSTR) then
          message = 'string "'//trim(name)//'" is too long to be used '// &
                    'as a Simple Bundle name'
          __raise__(MAPL_RC_ERROR, message)
       end if

       bundleName = trim(name)
    else
       bundleName = stateName
    end if

    Bundle = ESMF_FieldBundleCreate(name=bundleName, _RC)
    call ESMFL_BundleAddState ( Bundle, State, _RC)
    self = MAPL_SimpleBundleCreateFromBundle ( Bundle, Levs=Levs, LevUnits=LevUnits, &
                                               ptop=ptop, delp=delp, only_vars=only_vars, &
                                               strict=strict, name=name, _RC )

    _RETURN(_SUCCESS)

  end Function MAPL_SimpleBundleCreateFromState

!-----------------------------------------------------------------------------
!>
! Destructor for the MAPL Simple Bundle.
! It is assumed that the bundle has been created from an ESMF Field Bundle.
!
  subroutine MAPL_SimpleBundleDestroy (self, rc )

    type(MAPL_SimpleBundle)                    :: self ! Simple Bundle
    integer, OPTIONAL,           intent(out)   :: rc

!-----------------------------------------------------------------------------

    integer :: status

    deallocate(self%coords%Lons, self%coords%Lats, self%coords%Levs, __STAT__)
!    deallocate(self%r1, self%r2, self%r3, __STAT__)
    if(associated(self%r1)) deallocate(self%r1)
    if(associated(self%r2)) deallocate(self%r2)
    if(associated(self%r3)) deallocate(self%r3)

    if (associated(self%bundle)) then
       call MAPL_FieldBundleDestroy(self%bundle, _RC)
    end if

    if (self%bundleAlloc) then
       deallocate(self%bundle, __STAT__)
    end if

    _RETURN(_SUCCESS)

  end subroutine MAPL_SimpleBundleDestroy

!-----------------------------------------------------------------------------
!>
! Given an ESMF Config object and a filename, reads the corresponding file into
! a MAPL SimpleBundle.
!
  Function MAPL_SimpleBundleRead (filename, bundle_name, grid, time, verbose, &
                                  only_vars, expid, voting, unusable, rc ) result (self)
         use mapl_KeywordEnforcerMod

    type(MAPL_SimpleBundle)                    :: self !! Simple Bundle

    character(len=*),            intent(in)    :: filename
    character(len=*),            intent(in)    :: bundle_name
    type(ESMF_Time),             intent(inout) :: Time
    type(ESMF_Grid),             intent(in)    :: Grid
    logical, OPTIONAL,           intent(in)    :: verbose
    character(len=*), optional,  intent(IN)    :: only_vars
    character(len=*), optional,  intent(IN)    :: expid
    class(KeywordEnforcer), optional, intent(in) :: unusable
    logical,          optional,  intent(in)    :: voting
    integer, OPTIONAL,           intent(out)   :: rc

!-----------------------------------------------------------------------------

    integer :: status
    type(ESMF_FieldBundle),  pointer :: Bundle

    allocate(Bundle, stat=STATUS)
    _VERIFY(STATUS)

    Bundle = ESMF_FieldBundleCreate ( name=bundle_name, _RC )
    call ESMF_FieldBundleSet ( bundle, grid=Grid, _RC )
    call MAPL_CFIORead  ( filename, Time, Bundle, verbose=verbose, &
                          ONLY_VARS=only_vars, expid=expid, voting=voting, _RC )
    self = MAPL_SimpleBundleCreate ( Bundle, _RC )
    self%bundleAlloc = .true.

    _RETURN(_SUCCESS)

    _UNUSED_DUMMY(unusable)

  end function MAPL_SimpleBundleRead

!-----------------------------------------------------------------------------
!>
! Writes a MAPL SimpleBundle to file fiven an ESMF Clock object.
! The file opened, written to, and closed.

  subroutine MAPL_SimpleBundleWrite1 ( self, filename, clock, verbose, rc )

    type(MAPL_SimpleBundle)                 :: self
    character(len=*),           intent(in) :: filename
    type(ESMF_Clock),           intent(inout) :: Clock
    logical, OPTIONAL,          intent(in)  :: verbose
    integer, OPTIONAL,          intent(out) :: rc
!                                ---
    type(MAPL_CFIO)            :: cfio
    integer                    :: status

    call MAPL_CFIOCreate ( cfio, filename, clock, self%Bundle, _RC)
    call MAPL_CFIOWrite  ( cfio, Clock, self%Bundle, verbose=verbose, _RC)
    call MAPL_CFIODestroy ( cfio, _RC )
    _RETURN(_SUCCESS)

  end subroutine MAPL_SimpleBundleWrite1

!............................................................................................
!>
! Writes a MAPL SimpleBundle to file fiven an ESMF Time object.
! The file opened, written to, and closed.
! A fake timestep of 30 minutes is assumed.
!
  subroutine MAPL_SimpleBundleWrite2 ( self, filename, time, verbose, rc )
!
    type(MAPL_SimpleBundle)                 :: self
    character(len=*),           intent(in) :: filename
    type(ESMF_Time),            intent(in)  :: time
    logical, OPTIONAL,          intent(in)  :: verbose
    integer, OPTIONAL,          intent(out) :: rc
!                                ---
    type(ESMF_TimeInterval)    :: TimeStep
    type(ESMF_Clock)           :: Clock
    type(MAPL_CFIO)            :: cfio
    integer                    :: status

    call ESMF_TimeIntervalSet( TimeStep, h=0, m=30, s=0, _RC )
    CLOCK = ESMF_ClockCreate ( name="Clock", timeStep=TimeStep, startTime=Time, _RC )

    call MAPL_CFIOCreate ( cfio, filename, clock, self%Bundle, _RC)
    call MAPL_CFIOWrite  ( cfio, Clock, self%Bundle, verbose=verbose, _RC)
    call MAPL_CFIODestroy ( cfio, _RC )
    _RETURN(_SUCCESS)

  end subroutine MAPL_SimpleBundleWrite2

!............................................................................................
!>
! Prints the global max/min for each variable in the Simple Bundle.
!
  subroutine MAPL_SimpleBundlePrint ( self )

    type(MAPL_SimpleBundle) :: self
!
!-----------------------------------------------------------------------------

    integer :: i

    if ( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'Simple Bundle: ', trim(self%name)
    end if

!   1-D
!   ---
    do i = 1, self%n1d
       if ( self%r1(i)%myKind == ESMF_KIND_R4 ) then
          if ( associated(self%r1(i)%qr4) ) then
             call MAPL_MaxMin(trim(self%r1(i)%name), self%r1(i)%qr4)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r1(i)%name)//' ------    ------'
          endif
       else if ( self%r1(i)%myKind == ESMF_KIND_R8 ) then
          if ( associated(self%r1(i)%qr8) ) then
             call MAPL_MaxMin(trim(self%r1(i)%name), self%r1(i)%qr8)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r1(i)%name)//' ------    ------'
          endif
       end if
    end do

!   2-D
!   ---
    do i = 1, self%n2d
       if ( self%r2(i)%myKind == ESMF_KIND_R4 ) then
          if ( associated(self%r2(i)%qr4) ) then
             call MAPL_MaxMin(trim(self%r2(i)%name), self%r2(i)%qr4)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r2(i)%name)//' ------    ------'
          endif
       else if ( self%r2(i)%myKind == ESMF_KIND_R8 ) then
          if ( associated(self%r2(i)%qr8) ) then
             call MAPL_MaxMin(trim(self%r2(i)%name), self%r2(i)%qr8)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r2(i)%name)//' ------    ------'
          endif
       end if
    end do

!   3-D
!   ---
    do i = 1, self%n3d
       if ( self%r3(i)%myKind == ESMF_KIND_R4 ) then
          if ( associated(self%r3(i)%qr4) ) then
             call MAPL_MaxMin(trim(self%r3(i)%name), self%r3(i)%qr4)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r3(i)%name)//' ------    ------'
          endif
       else if ( self%r3(i)%myKind == ESMF_KIND_R8 ) then
          if ( associated(self%r3(i)%qr8) ) then
             call MAPL_MaxMin(trim(self%r3(i)%name), self%r3(i)%qr8)
          else
             if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r3(i)%name)//' ------    ------'
          endif
       end if
    end do
    if ( MAPL_AM_I_ROOT() ) then
       print *
    end if
end subroutine MAPL_SimpleBundlePrint

!-----------------------------------------------------------------------------
!>
! Finds the index of the first variable with name `vname`.
! This routine is case insensitive.
!
  function MAPL_SimpleBundleGetIndex ( self, name, rank, rc, quiet ) result(iq)

    integer                                  :: iq    !! index of variable
    type(MAPL_SimpleBundle)                  :: self
    character(len=*),            intent(in)  :: name  !! variable name
    integer,                     intent(in)  :: rank
    integer, OPTIONAL,           intent(out) :: rc
    logical, OPTIONAL,           intent(in)  :: quiet
!
!-----------------------------------------------------------------------------

    character(len=ESMF_MAXSTR) :: message
    logical :: quiet_
    integer :: i

    if ( present(quiet) ) then
       quiet_ = quiet
    else
       quiet_ = .FALSE.
    end if

    iq = -1
    if ( rank == 1 ) then
       do i = 1, self%n1d
          if (trim(self%r1(i)%name) == trim(name) ) then
              iq = i
              exit
           endif
        end do
    else if ( rank == 2 ) then
       do i = 1, self%n2d
          if (trim(self%r2(i)%name) == trim(name) ) then
              iq = i
              exit
           endif
        end do
    else if ( rank == 3 ) then
       do i = 1, self%n3d
          if (trim(self%r3(i)%name) == trim(name) ) then
              iq = i
              exit
           endif
        end do
    else
       if ( present(rc) ) then
          __raise__(MAPL_RC_ERROR,"invalid rank")
       end if
    end if

    if ( present(rc) ) then
       if ( iq <= 0 ) then
          if ( quiet_ ) then
             rc = MAPL_RC_ERROR
             return
          else
             message = "could not find index for "//trim(name)// &
                  ' in Simple Bundle <'//trim(self%name)//'>'
             __raise__(MAPL_RC_ERROR,message)
          end if
       else
          _RETURN(ESMF_SUCCESS)
       end if
    end if
    _RETURN(_SUCCESS)

  end function MAPL_SimpleBundleGetIndex

end module MAPL_SimpleBundleMod

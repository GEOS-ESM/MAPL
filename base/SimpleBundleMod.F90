!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!>
!### MODULE: `MAPL_SimpleBundleMod`
!
! Author: GMAO SI-Team
!
! The module `MAPL_SimpleBundleMod` implements a wrapper class around the
! ESMF_FieldBundle. By construction, this is NOT an opaque object.
!
!#### History
!- April 2010: Arlindo da Silva <arlindo.dasilva@nasa.gov>
!- 2026: Modernized and moved to base3g/ as part of MAPL3 migration
!
#include "MAPL.h"

module mapl_SimpleBundleMod_impl_mod

   use ESMF
   use mapl_geom_api, only: MAPL_GridGet
   use mapl_field_bundle_api, only: MAPL_FieldBundleGetByIndex, MAPL_FieldBundleDestroy
   use mapl_ArrayReductions_mod, only: MaxMin => MAPL_MaxMin
   use mapl_Comms_mod, only: MAPL_AM_I_ROOT => am_i_root
   use MAPL_Constants, only: MAPL_PI
   use mapl_ErrorHandling_mod

   implicit none
   private

   public :: MAPL_SimpleBundleCreate
   public :: MAPL_SimpleBundlePrint
   public :: MAPL_SimpleBundleGetIndex
   public :: MAPL_SimpleBundleDestroy
   public :: MAPL_SimpleBundle

   type SimpleArray_1D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer :: q(:)   => null() !! alias for qr4
      real(kind=ESMF_KIND_R4), pointer :: qr4(:) => null()
      real(kind=ESMF_KIND_R8), pointer :: qr8(:) => null()
   end type SimpleArray_1D

   type SimpleArray_2D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer :: q(:,:)   => null() !! alias for qr4
      real(kind=ESMF_KIND_R4), pointer :: qr4(:,:) => null()
      real(kind=ESMF_KIND_R8), pointer :: qr8(:,:) => null()
   end type SimpleArray_2D

   type SimpleArray_3D
      character(len=ESMF_MAXSTR) :: name
      integer :: myKind = ESMF_KIND_R4
      real(kind=ESMF_KIND_R4), pointer :: q(:,:,:)   => null() !! alias for qr4
      real(kind=ESMF_KIND_R4), pointer :: qr4(:,:,:) => null()
      real(kind=ESMF_KIND_R8), pointer :: qr8(:,:,:) => null()
   end type SimpleArray_3D

   type LcvGrid
      real(kind=ESMF_KIND_R4)          :: ptop = 0.01
      real(kind=ESMF_KIND_R4), pointer :: delp(:,:,:) => null()
   end type LcvGrid

   type SimpleGrid
      real(kind=ESMF_KIND_R4), pointer :: Lons(:,:) => null() !! Longitudes in degrees
      real(kind=ESMF_KIND_R4), pointer :: Lats(:,:) => null() !! Latitudes in degrees
      real(kind=ESMF_KIND_R4), pointer :: Levs(:)   => null() !! Vertical Levels
      character(len=ESMF_MAXSTR)       :: LevUnits = '1'      !! Vertical Level units
      type(LcvGrid)                    :: lcv                  !! Lagrangian Control Volume
   end type SimpleGrid

   type MAPL_SimpleBundle
      character(len=ESMF_MAXSTR)      :: name
      type(ESMF_FieldBundle), pointer :: Bundle  => null() !! Associated ESMF bundle
      type(ESMF_Grid)                 :: grid              !! Associated ESMF grid
      type(SimpleGrid)                :: coords            !! Coordinate variables
      integer :: n1d = -1
      integer :: n2d = -1
      integer :: n3d = -1
      logical :: bundleAlloc = .false.
      type(SimpleArray_1D), pointer :: r1(:) => null()
      type(SimpleArray_2D), pointer :: r2(:) => null()
      type(SimpleArray_3D), pointer :: r3(:) => null()
   end type MAPL_SimpleBundle

   interface MAPL_SimpleBundleCreate
      module procedure MAPL_SimpleBundleCreateEmpty
      module procedure MAPL_SimpleBundleCreateFromBundle
      module procedure MAPL_SimpleBundleCreateFromState
   end interface MAPL_SimpleBundleCreate

contains

!-----------------------------------------------------------------------------
!>
! Given inputs, create an empty SimpleBundle with coordinate arrays but no
! data fields.
!
   function MAPL_SimpleBundleCreateEmpty(grid, rc, Levs, LevUnits, ptop, delp, name) result(self)

      type(MAPL_SimpleBundle)                                :: self
      type(ESMF_Grid),                        intent(in)    :: grid
      integer,                      optional, intent(out)   :: rc
      real(ESMF_KIND_R4),           optional, intent(in)    :: Levs(:)     !! Vertical coordinates
      character(len=*),             optional, intent(in)    :: LevUnits    !! Level units
      real(ESMF_KIND_R4),           optional, intent(in)    :: ptop        !! top pressure (Pa)
      real(ESMF_KIND_R4), optional, pointer,  intent(in)    :: delp(:,:,:) !! layer thickness (Pa)
      character(len=*),             optional, intent(in)    :: name

      integer :: im, jm, km, i, status
      real(ESMF_KIND_R8), pointer :: LonsRad(:,:), LatsRad(:,:)

      self%Bundle => null()

      if (present(name)) then
         self%name = trim(name)
      else
         self%name = ''
      end if

      self%grid = grid
      call MAPL_GridGet(self%grid, im=im, jm=jm, _RC)
      if (present(Levs)) then
         km = size(Levs)
      else
         km = 0
      end if
      allocate(self%coords%Lons(im,jm), self%coords%Lats(im,jm), self%coords%Levs(km), __STAT__)

      call ESMF_GridGetCoord(self%grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=LonsRad, _RC)
      call ESMF_GridGetCoord(self%grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=LatsRad, _RC)
      self%coords%Lons(:,:) = (180. / MAPL_PI) * LonsRad(:,:)
      self%coords%Lats(:,:) = (180. / MAPL_PI) * LatsRad(:,:)

      if (present(Levs)) then
         self%coords%Levs(:) = Levs(:)
      else
         self%coords%Levs(:) = [(i, i=1, km)]
      end if

      self%coords%LevUnits = '1'
      if (present(LevUnits)) self%coords%LevUnits = LevUnits

      if (present(ptop)) self%coords%lcv%ptop = ptop
      if (present(delp)) then
         self%coords%lcv%delp => delp
      else
         self%coords%lcv%delp => null()
      end if

      _RETURN(_SUCCESS)

   end function MAPL_SimpleBundleCreateEmpty

!-----------------------------------------------------------------------------
!>
! Given an ESMF Bundle, creates a corresponding Simple Bundle. The
! specification of a vertical grid is optional but useful in many
! cases. The 1-D `Levs` will default to the layer number, and units of "1".
! Input parameters `(ptop,delp)` can be used to record the corresponding
! Lagrangian Control Volume Grid. When `delp` is not specified, variables
! `DELP` or `delp` are used if present inside the bundle.
!
   function MAPL_SimpleBundleCreateFromBundle(Bundle, rc, Levs, LevUnits, ptop, delp, &
        only_vars, strict, name) result(self)

      type(MAPL_SimpleBundle)                                :: self

      type(ESMF_FieldBundle), target,         intent(inout) :: Bundle
      integer,                      optional, intent(out)   :: rc
      real(ESMF_KIND_R4),           optional, intent(in)    :: Levs(:)
      character(len=*),             optional, intent(in)    :: LevUnits
      real(ESMF_KIND_R4),           optional, intent(in)    :: ptop
      real(ESMF_KIND_R4), optional, pointer,  intent(in)    :: delp(:,:,:)
      character(len=*),             optional, intent(in)    :: only_vars  !! comma-separated field names
      logical,                      optional, intent(in)    :: strict     !! error if only_vars name not found
      character(len=*),             optional, intent(in)    :: name

      type(ESMF_Field)            :: field
      type(ESMF_Array)            :: array
      type(ESMF_TypeKind_Flag)    :: typeKind
      type(ESMF_FieldStatus_Flag) :: fieldStatus
      real(ESMF_KIND_R8), pointer :: LonsRad(:,:), LatsRad(:,:)

      integer :: arrayRank, i, n, n1d, n2d, n3d, NumVars
      integer :: im, jm, km, status
      real(ESMF_KIND_R4), pointer :: tmp3d(:,:,:)
      logical :: strict_match, isPresent, isPresentBundle, haveDelp
      integer :: n_vars
      logical, allocatable :: isRequested(:)
      character(len=ESMF_MAXSTR), allocatable :: var_list(:)
      character(len=ESMF_MAXSTR) :: bundleName, fieldName

      self%Bundle => Bundle
      self%bundleAlloc = .false.

      call ESMF_FieldBundleGet(Bundle, name=bundleName, grid=self%grid, fieldCount=NumVars, _RC)

      if (present(name)) then
         self%name = trim(name)
      else
         self%name = bundleName
      end if

      strict_match = .true.
      if (present(strict)) strict_match = strict

      allocate(isRequested(NumVars), __STAT__)
      isRequested = .false.

      if (present(only_vars)) then
         n_vars = csv_tokens_count_(only_vars)
         _ASSERT(n_vars <= NumVars, 'more vars requested than in bundle')
         allocate(var_list(n_vars), __STAT__)
         var_list = '__NONE__'
         call csv_tokens_get_(only_vars, var_list, _RC)

         do i = 1, size(var_list)
            isPresent = .false.
             do n = 1, NumVars
               call MAPL_FieldBundleGetByIndex(Bundle, n, field, _RC)
               call ESMF_FieldGet(field, name=fieldName, _RC)
               if (fieldName == var_list(i)) then
                  isPresent = .true.
                  exit
               end if
            end do
            if (isPresent) then
               isRequested(n) = .true.
            else
               if (strict_match) then
                  _FAIL('could not find field '//trim(var_list(i))//' in bundle '//trim(self%name))
               end if
            end if
         end do
         deallocate(var_list)
      else
         isRequested = .true.
      end if

      call MAPL_GridGet(self%grid, im=im, jm=jm, _RC)
      if (present(Levs)) then
         km = size(Levs)
      else
         km = 0
         do n = 1, NumVars
            call MAPL_FieldBundleGetByIndex(Bundle, n, field, _RC)
            call ESMF_FieldGet(field, status=fieldStatus, _RC)
            if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) cycle
            call ESMF_FieldGet(field, array=array, _RC)
            call ESMF_ArrayGet(array, rank=arrayRank, typeKind=typeKind, _RC)
            if (arrayRank == 3 .and. typeKind == ESMF_TYPEKIND_R4) then
               call ESMF_FieldGet(field, localDE=0, farrayPtr=tmp3d, _RC)
               km = size(tmp3d, 3)
               exit
            end if
         end do
      end if
      allocate(self%coords%Lons(im,jm), self%coords%Lats(im,jm), self%coords%Levs(km), __STAT__)

      call ESMF_GridGetCoord(self%grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=LonsRad, _RC)
      call ESMF_GridGetCoord(self%grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=LatsRad, _RC)
      self%coords%Lons(:,:) = (180. / MAPL_PI) * LonsRad(:,:)
      self%coords%Lats(:,:) = (180. / MAPL_PI) * LatsRad(:,:)

      if (present(Levs)) then
         self%coords%Levs(:) = Levs(:)
      else
         self%coords%Levs(:) = [(i, i=1, km)]
      end if

      self%coords%LevUnits = '1'
      if (present(LevUnits)) self%coords%LevUnits = LevUnits

      if (present(ptop)) self%coords%lcv%ptop = ptop
      if (present(delp)) then
         self%coords%lcv%delp => delp
      else
         self%coords%lcv%delp => null()
         haveDelp = .false.

         call ESMF_FieldBundleGet(Bundle, fieldName='DELP', isPresent=isPresentBundle, rc=status)
         _VERIFY(status)
         if (isPresentBundle) then
            call ESMF_FieldBundleGet(Bundle, fieldName='DELP', field=field, rc=status)
            _VERIFY(status)
            haveDelp = .true.
         else
            call ESMF_FieldBundleGet(Bundle, fieldName='delp', isPresent=isPresentBundle, rc=status)
            _VERIFY(status)
            if (isPresentBundle) then
               call ESMF_FieldBundleGet(Bundle, fieldName='delp', field=field, rc=status)
               _VERIFY(status)
               haveDelp = .true.
            end if
         end if

         if (haveDelp) then
            call ESMF_FieldGet(field, status=fieldStatus, _RC)
            if (fieldStatus == ESMF_FIELDSTATUS_COMPLETE) then
               call ESMF_FieldGet(field, 0, self%coords%lcv%delp, _RC)
            end if
         end if
      end if

      allocate(self%r1(NumVars), self%r2(NumVars), self%r3(NumVars), __STAT__)

      n1d = 0; n2d = 0; n3d = 0
      do i = 1, NumVars

         call MAPL_FieldBundleGetByIndex(Bundle, i, field, _RC)
         call ESMF_FieldGet(field, name=fieldName, status=fieldStatus, _RC)

         if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE .or. .not. isRequested(i)) cycle

         call ESMF_FieldGet(field, array=array, _RC)
         call ESMF_ArrayGet(array, rank=arrayRank, typeKind=typeKind, _RC)

         if (typeKind == ESMF_TYPEKIND_R4) then
            select case (arrayRank)
            case (1)
               n1d = n1d + 1
               call ESMF_FieldGet(field, localDE=0, farrayPtr=self%r1(n1d)%qr4, _RC)
               self%r1(n1d)%name   = trim(fieldName)
               self%r1(n1d)%myKind = ESMF_KIND_R4
               self%r1(n1d)%q      => self%r1(n1d)%qr4
            case (2)
               n2d = n2d + 1
               call ESMF_FieldGet(field, localDE=0, farrayPtr=self%r2(n2d)%qr4, _RC)
               self%r2(n2d)%name   = trim(fieldName)
               self%r2(n2d)%myKind = ESMF_KIND_R4
               self%r2(n2d)%q      => self%r2(n2d)%qr4
            case (3)
               n3d = n3d + 1
               call ESMF_FieldGet(field, localDE=0, farrayPtr=self%r3(n3d)%qr4, _RC)
               self%r3(n3d)%name   = trim(fieldName)
               self%r3(n3d)%myKind = ESMF_KIND_R4
               self%r3(n3d)%q      => self%r3(n3d)%qr4
            case default
               _FAIL('unsupported array rank for R4 field '//trim(fieldName))
            end select

         else if (typeKind == ESMF_TYPEKIND_R8) then
            select case (arrayRank)
            case (1)
               n1d = n1d + 1
               call ESMF_FieldGet(field, localDE=0, farrayPtr=self%r1(n1d)%qr8, _RC)
               self%r1(n1d)%name   = trim(fieldName)
               self%r1(n1d)%myKind = ESMF_KIND_R8
            case (2)
               n2d = n2d + 1
               call ESMF_FieldGet(field, localDE=0, farrayPtr=self%r2(n2d)%qr8, _RC)
               self%r2(n2d)%name   = trim(fieldName)
               self%r2(n2d)%myKind = ESMF_KIND_R8
            case (3)
               n3d = n3d + 1
               call ESMF_FieldGet(field, localDE=0, farrayPtr=self%r3(n3d)%qr8, _RC)
               self%r3(n3d)%name   = trim(fieldName)
               self%r3(n3d)%myKind = ESMF_KIND_R8
            case default
               _FAIL('unsupported array rank for R8 field '//trim(fieldName))
            end select

         else
            _FAIL('unsupported typeKind for field '//trim(fieldName))
         end if

      end do

      self%n1d = n1d
      self%n2d = n2d
      self%n3d = n3d

      deallocate(isRequested, __STAT__)
      _RETURN(_SUCCESS)

   contains

      function csv_tokens_count_(str, delimiter) result(n)
         integer                                :: n
         character(len=*),           intent(in) :: str
         character,        optional, intent(in) :: delimiter
         character, parameter :: char_comma = ','
         character            :: c
         integer              :: i
         c = char_comma
         if (present(delimiter)) c = delimiter
         n = 1
         do i = 1, len_trim(str)
            if (str(i:i) == c) n = n + 1
         end do
      end function csv_tokens_count_

      subroutine csv_tokens_get_(str, list, delimiter, ignore, rc)
         character(len=*),           intent(in)    :: str
         character(len=*),           intent(inout) :: list(:)
         character,        optional, intent(in)    :: delimiter
         character,        optional, intent(in)    :: ignore
         integer,          optional, intent(inout) :: rc
         character, parameter :: char_empty = '', char_space = ' ', char_comma = ','
         character :: c_dlm, c_ign
         integer   :: i, j, n, err
         c_dlm = char_comma; if (present(delimiter)) c_dlm = delimiter
         c_ign = char_space;  if (present(ignore))    c_ign = ignore
         list = char_empty
         j = 1; n = 1; err = 0
         do i = 1, len_trim(str)
            if (str(i:i) /= c_dlm) then
               if (n > size(list)) then; err = 99; exit; end if
               if (str(i:i) /= c_ign) then
                  list(n)(j:j) = str(i:i)
                  j = j + 1
               end if
            else
               j = 1; n = n + 1
            end if
         end do
         if (present(rc)) rc = err
      end subroutine csv_tokens_get_

   end function MAPL_SimpleBundleCreateFromBundle

!-----------------------------------------------------------------------------
!>
! Given an ESMF State, creates a corresponding Simple Bundle.
! It is assumed that the ESMF State has a single grid.
!
   function MAPL_SimpleBundleCreateFromState(State, rc, Levs, LevUnits, ptop, delp, &
        only_vars, strict, name) result(self)

      type(MAPL_SimpleBundle)                                :: self

      type(ESMF_State), target,               intent(inout) :: State
      integer,                      optional, intent(out)   :: rc
      real(ESMF_KIND_R4),           optional, intent(in)    :: Levs(:)
      character(len=*),             optional, intent(in)    :: LevUnits
      real(ESMF_KIND_R4),           optional, intent(in)    :: ptop
      real(ESMF_KIND_R4), optional, pointer,  intent(in)    :: delp(:,:,:)
      character(len=*),             optional, intent(in)    :: only_vars
      logical,                      optional, intent(in)    :: strict
      character(len=*),             optional, intent(in)    :: name

      character(len=ESMF_MAXSTR) :: stateName, bundleName
      type(ESMF_FieldBundle)     :: Bundle
      integer :: status

      call ESMF_StateGet(State, name=stateName, _RC)
      bundleName = stateName
      if (present(name)) bundleName = trim(name)

      Bundle = ESMF_FieldBundleCreate(name=bundleName, _RC)
      call BundleAddState_(Bundle, State, _RC)
      self = MAPL_SimpleBundleCreateFromBundle(Bundle, Levs=Levs, LevUnits=LevUnits, &
           ptop=ptop, delp=delp, only_vars=only_vars, strict=strict, name=name, _RC)

      _RETURN(_SUCCESS)

   end function MAPL_SimpleBundleCreateFromState

!-----------------------------------------------------------------------------
!>
! Destructor for the MAPL Simple Bundle.
!
   subroutine MAPL_SimpleBundleDestroy(self, rc)

      type(MAPL_SimpleBundle),           intent(inout) :: self
      integer,               optional,   intent(out)   :: rc

      integer :: status

      deallocate(self%coords%Lons, self%coords%Lats, self%coords%Levs, __STAT__)
      if (associated(self%r1)) deallocate(self%r1)
      if (associated(self%r2)) deallocate(self%r2)
      if (associated(self%r3)) deallocate(self%r3)

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
! Prints the global max/min for each variable in the Simple Bundle.
!
   subroutine MAPL_SimpleBundlePrint(self, rc)

      type(MAPL_SimpleBundle), intent(in)    :: self
      integer,       optional, intent(out)   :: rc

      integer :: i, comm, status
      real    :: mm(2)
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)

      if (MAPL_AM_I_ROOT()) then
         print *
         print *, 'Simple Bundle: ', trim(self%name)
      end if

      do i = 1, self%n1d
         if (self%r1(i)%myKind == ESMF_KIND_R4) then
            if (associated(self%r1(i)%qr4)) then
               mm = MaxMin(self%r1(i)%qr4, comm, _RC)
               if (MAPL_AM_I_ROOT()) write(*,'(a16,2(1x,a,1x,g12.4))') &
                    trim(self%r1(i)%name), 'max=', mm(1), 'min=', mm(2)
            else
               if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r1(i)%name)//' ------    ------'
            end if
         else if (self%r1(i)%myKind == ESMF_KIND_R8) then
            if (associated(self%r1(i)%qr8)) then
               mm = MaxMin(self%r1(i)%qr8, comm, _RC)
               if (MAPL_AM_I_ROOT()) write(*,'(a16,2(1x,a,1x,g12.4))') &
                    trim(self%r1(i)%name), 'max=', mm(1), 'min=', mm(2)
            else
               if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r1(i)%name)//' ------    ------'
            end if
         end if
      end do

      do i = 1, self%n2d
         if (self%r2(i)%myKind == ESMF_KIND_R4) then
            if (associated(self%r2(i)%qr4)) then
               mm = MaxMin(self%r2(i)%qr4, comm, _RC)
               if (MAPL_AM_I_ROOT()) write(*,'(a16,2(1x,a,1x,g12.4))') &
                    trim(self%r2(i)%name), 'max=', mm(1), 'min=', mm(2)
            else
               if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r2(i)%name)//' ------    ------'
            end if
         else if (self%r2(i)%myKind == ESMF_KIND_R8) then
            if (associated(self%r2(i)%qr8)) then
               mm = MaxMin(self%r2(i)%qr8, comm, _RC)
               if (MAPL_AM_I_ROOT()) write(*,'(a16,2(1x,a,1x,g12.4))') &
                    trim(self%r2(i)%name), 'max=', mm(1), 'min=', mm(2)
            else
               if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r2(i)%name)//' ------    ------'
            end if
         end if
      end do

      do i = 1, self%n3d
         if (self%r3(i)%myKind == ESMF_KIND_R4) then
            if (associated(self%r3(i)%qr4)) then
               mm = MaxMin(self%r3(i)%qr4, comm, _RC)
               if (MAPL_AM_I_ROOT()) write(*,'(a16,2(1x,a,1x,g12.4))') &
                    trim(self%r3(i)%name), 'max=', mm(1), 'min=', mm(2)
            else
               if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r3(i)%name)//' ------    ------'
            end if
         else if (self%r3(i)%myKind == ESMF_KIND_R8) then
            if (associated(self%r3(i)%qr8)) then
               mm = MaxMin(self%r3(i)%qr8, comm, _RC)
               if (MAPL_AM_I_ROOT()) write(*,'(a16,2(1x,a,1x,g12.4))') &
                    trim(self%r3(i)%name), 'max=', mm(1), 'min=', mm(2)
            else
               if (MAPL_AM_I_ROOT()) write(*,*) trim(self%r3(i)%name)//' ------    ------'
            end if
         end if
      end do

      if (MAPL_AM_I_ROOT()) print *
      _RETURN(_SUCCESS)

   end subroutine MAPL_SimpleBundlePrint

!-----------------------------------------------------------------------------
!>
! Finds the index of the first variable with name `vname`.
! Note: comparison is case-sensitive (trim only).
!
   function MAPL_SimpleBundleGetIndex(self, name, rank, rc, quiet) result(iq)

      integer                                  :: iq
      type(MAPL_SimpleBundle),   intent(in)    :: self
      character(len=*),          intent(in)    :: name
      integer,                   intent(in)    :: rank
      integer,         optional, intent(out)   :: rc
      logical,         optional, intent(in)    :: quiet

      logical :: quiet_
      integer :: i

      quiet_ = .false.
      if (present(quiet)) quiet_ = quiet

      iq = -1
      select case (rank)
      case (1)
         do i = 1, self%n1d
            if (trim(self%r1(i)%name) == trim(name)) then; iq = i; exit; end if
         end do
      case (2)
         do i = 1, self%n2d
            if (trim(self%r2(i)%name) == trim(name)) then; iq = i; exit; end if
         end do
      case (3)
         do i = 1, self%n3d
            if (trim(self%r3(i)%name) == trim(name)) then; iq = i; exit; end if
         end do
      case default
         if (present(rc)) then
            _FAIL('invalid rank; must be 1, 2, or 3')
         end if
      end select

      if (present(rc)) then
         if (iq <= 0) then
            if (quiet_) then
               rc = MAPL_RC_ERROR
               return
            else
               _FAIL('could not find index for '//trim(name)//' in Simple Bundle <'//trim(self%name)//'>')
            end if
         else
            _RETURN(ESMF_SUCCESS)
         end if
      end if
      _RETURN(_SUCCESS)

   end function MAPL_SimpleBundleGetIndex

! Moved here from ESMFL_Mod as part of MAPL3 cleanup (MAPL#4862).
! Adds contents of an ESMF_State (fields, bundles, nested states) into
! a pre-created ESMF_FieldBundle, preserving MAPL ordering metadata.
    RECURSIVE subroutine BundleAddState_ ( BUNDLE, STATE, rc, &
                                           GRID, VALIDATE )
    implicit NONE
    type(ESMF_FieldBundle), intent(inout)         :: BUNDLE
    type(ESMF_State),  intent(INout)            :: STATE
    integer, optional                        :: rc
    type(ESMF_Grid),  optional, intent(in)   :: GRID
    logical, optional, intent(in)            :: VALIDATE

    character(len=*), parameter          :: Iam="ESMFL_StateSerialize"
    integer                              :: STATUS

    type(ESMF_State)                     :: tSTATE
    type(ESMF_FieldBundle)                    :: tBUNDLE
    type(ESMF_Field)                     :: tFIELD
    type(ESMF_Grid)                      :: tGRID

    integer                              :: I, J
    integer                              :: ItemCount, FieldCount
    type (ESMF_StateItem_Flag), pointer  :: ItemTypes(:)
    character(len=ESMF_MAXSTR ), pointer :: ItemNames(:), FieldNames(:)
    logical                              :: needGrid = .true.
    logical                              :: validate_ = .false.
    integer, allocatable :: orderlist(:)
    integer :: jj
    character(len=ESMF_MAXSTR)           :: attrName
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    integer                                 :: natt
    type(ESMF_Info)                      :: infoh

    if ( present(validate) ) validate_ = validate

    call ESMF_StateGet(STATE,ItemCount=ItemCount,RC=STATUS)
    _VERIFY(STATUS)
    _ASSERT(ItemCount>0, 'itemCount should be > 0')
    allocate ( ItemNames(ItemCount), stat=STATUS)
    _VERIFY(STATUS)
    allocate ( ItemTypes(ItemCount), stat=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet ( STATE,      ItemNameList = ItemNames, &
                                ItemtypeList = ItemTypes, &
                                rc=STATUS)
    _VERIFY(STATUS)

    attrName = 'MAPL_StateItemOrderList'
    call ESMF_InfoGetFromHost(state,infoh,RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_InfoGet(infoh,key=attrName,size=natt,RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(natt > 0, 'natt should be > 0')
    allocate(orderlist(natt), stat=status)
    _VERIFY(STATUS)
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    call ESMF_InfoGet(infoh,key=attrName,values=currList,rc=status)
    _VERIFY(STATUS)

    orderList = -1
    do i = 1, natt
       do jj = 1, ITEMCOUNT
          if (itemNames(jj) == currList(i)) then
             orderList(i) = jj
             exit
          end if
       end do
    end do

    deallocate(currList)

    do JJ = 1, natt

       I = ORDERLIST(JJ)

          if (ItemTypes(I) == ESMF_StateItem_Field) THEN

             call ESMF_StateGet ( STATE, ItemNames(i), tFIELD, rc=status)
             _VERIFY(STATUS)
             call AddThisField_()

          else if (ItemTypes(I) == ESMF_StateItem_FieldBundle) then

             call ESMF_StateGet(STATE, ItemNames(i), tBUNDLE, rc=STATUS)
             _VERIFY(STATUS)
             call ESMF_FieldBundleGet ( tBUNDLE, FieldCount = FieldCount, rc=STATUS)
             _VERIFY(STATUS)
             _ASSERT(FieldCount>0, 'FieldCount should be > 0')
             do j = 1, FieldCount
                call ESMF_FieldBundleGet ( tBUNDLE, j, tFIELD, rc=STATUS)
                _VERIFY(STATUS)
                call AddThisField_()
             end do

          else if (ItemTypes(I) == ESMF_StateItem_State) then

             call ESMF_StateGet(STATE, ItemNames(i), tSTATE, rc=STATUS)
             _VERIFY(STATUS)
             call BundleAddState_ ( BUNDLE, tSTATE, rc=STATUS )
             _VERIFY(STATUS)
             if (needGrid) then
                call ESMF_FieldBundleGet ( BUNDLE, GRID=tGRID, rc=STATUS )
                _VERIFY(STATUS)
                needGrid = .false.
             end if

          else
             cycle
          end IF

    end do

    deallocate(orderlist)

    call ESMF_FieldBundleGet ( BUNDLE, FieldCount = FieldCount, rc=STATUS)
    _VERIFY(STATUS)
    _ASSERT(FieldCount>0, 'FieldCount should be > 0')

    if ( present(GRID) ) then
       call ESMF_FieldBundleSet ( BUNDLE, grid=GRID, rc=STATUS )
       _VERIFY(STATUS)
       needGrid = .false.
    else
       _ASSERT(.not. needGrid, 'could not find a grid')
    end if

    allocate ( FieldNames(FieldCount), stat=STATUS )
    _VERIFY(STATUS)
    call ESMF_FieldBundleGet ( BUNDLE, FieldNameList=FieldNames, rc=STATUS )
    _VERIFY(STATUS)

    if ( validate_ ) then
       do j = 1, FieldCount
          do i = j+1, FieldCount
             if ( trim(FieldNames(i)) == trim(FieldNames(j)) ) then
                STATUS = -1
                _VERIFY(STATUS)
             end if
          end do
       end do
    end if

    deallocate(ItemNames)
    deallocate(ItemTypes)
    deallocate(FieldNames)

    _RETURN(ESMF_SUCCESS)

CONTAINS

    subroutine AddThisField_()
      call ESMF_FieldBundleAdd ( BUNDLE, [tField], rc=STATUS )
      _VERIFY(STATUS)
      if ( needGrid ) then
         call ESMF_FieldGet ( tFIELD, grid=tGRID, rc=STATUS )
         _VERIFY(STATUS)
         needGrid = .false.
      end if
    end subroutine AddThisField_

  end subroutine BundleAddState_

end module mapl_SimpleBundleMod_impl_mod

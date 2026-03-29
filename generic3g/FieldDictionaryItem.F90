module mapl3g_FieldDictionaryItem

   use gftl2_StringVector
   use esmf
   use mapl3g_VerificationStatus

   implicit none(type,external)
   private

   public :: FieldDictionaryItem
   public :: CF_Provenance

   type :: CF_Provenance
      character(:), allocatable :: verified_by
      ! Extensible: future fields may include verification_date, notes, etc.
   end type CF_Provenance

   type :: FieldDictionaryItem
      private
      character(:), allocatable :: long_name
      character(:), allocatable :: canonical_units
      character(:), allocatable :: physical_dimension
      logical :: conserved = .false.
      type(VerificationStatus) :: verification_status_
      type(CF_Provenance) :: provenance_
      type(ESMF_RegridMethod_Flag) :: regrid_method
      type(StringVector) :: aliases
   contains
      procedure :: get_long_name
      procedure :: get_units
      procedure :: get_aliases
      procedure :: get_regrid_method
      procedure :: get_physical_dimension
      procedure :: get_verification_status
      procedure :: get_provenance
      procedure :: is_conserved
   end type FieldDictionaryItem

   !************************
   ! Caution:  Multiple constructor arguments are strings, and
   ! as such incorrect order is a potential source of error
   ! in client code.
   !************************

   interface FieldDictionaryItem
      module procedure new_FieldDictionaryItem_
      module procedure new_FieldDictionaryItem_one_alias
      module procedure new_FieldDictionaryItem_multi_aliases
      module procedure new_FieldDictionaryItem_full
   end interface

contains

   function new_FieldDictionaryItem_(long_name, canonical_units) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units

      item = FieldDictionaryItem(long_name, canonical_units, [character(1) ::])

   end function new_FieldDictionaryItem_

   function new_FieldDictionaryItem_one_alias(long_name, canonical_units, alias) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units
      character(*), intent(in) :: alias

      item = FieldDictionaryItem(long_name, canonical_units, [alias])

   end function new_FieldDictionaryItem_one_alias

   function new_FieldDictionaryItem_multi_aliases(long_name, canonical_units, aliases) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units
      character(*), intent(in) :: aliases(:)

      integer :: i
      type(StringVector) :: aliases_vector

      do i = 1, size(aliases)
         call aliases_vector%push_back(trim(aliases(i)))
      end do

      item = FieldDictionaryItem(long_name, canonical_units, aliases_vector)

   end function new_FieldDictionaryItem_multi_aliases

   function new_FieldDictionaryItem_full( &
         long_name, canonical_units, aliases, &
         physical_dimension, conserved, verification_status, provenance) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units
      type(StringVector), intent(in) :: aliases
      character(*), optional, intent(in) :: physical_dimension
      logical, optional, intent(in) :: conserved
      type(VerificationStatus), optional, intent(in) :: verification_status
      type(CF_Provenance), optional, intent(in) :: provenance
      item%long_name = long_name
      item%canonical_units = canonical_units
      item%aliases = aliases

      if (present(physical_dimension)) then
         item%physical_dimension = physical_dimension
      else
         item%physical_dimension = ''
      end if

      if (present(conserved)) then
         item%conserved = conserved
      else
         item%conserved = .false.
      end if

      if (present(verification_status)) then
         item%verification_status_ = verification_status
      else
         item%verification_status_ = VERIFICATION_STATUS_UNVERIFIED
      end if

      if (present(provenance)) then
         item%provenance_ = provenance
      end if

      ! Conserved quantities use conservative regridding by default
      if (item%conserved) then
         item%regrid_method = ESMF_REGRIDMETHOD_CONSERVE
      else
         item%regrid_method = ESMF_REGRIDMETHOD_BILINEAR
      end if

   end function new_FieldDictionaryItem_full

   ! accessors

   pure function get_long_name(this) result(long_name)
      character(len=:), allocatable :: long_name
      class(FieldDictionaryItem), intent(in) :: this
      long_name = this%long_name
   end function get_long_name

   pure function get_units(this) result(units)
      character(len=:), allocatable :: units
      class(FieldDictionaryItem), intent(in) :: this
      units = this%canonical_units
   end function get_units

   pure function get_aliases(this) result(aliases)
      type(StringVector) :: aliases
      class(FieldDictionaryItem), intent(in) :: this
      aliases = this%aliases
   end function get_aliases

   pure function get_regrid_method(this) result(regrid_method)
      type(ESMF_RegridMethod_Flag) :: regrid_method
      class(FieldDictionaryItem), intent(in) :: this
      regrid_method = this%regrid_method
   end function get_regrid_method

   pure function get_physical_dimension(this) result(physical_dimension)
      character(len=:), allocatable :: physical_dimension
      class(FieldDictionaryItem), intent(in) :: this
      physical_dimension = this%physical_dimension
   end function get_physical_dimension

   pure function get_verification_status(this) result(verification_status)
      type(VerificationStatus) :: verification_status
      class(FieldDictionaryItem), intent(in) :: this
      verification_status = this%verification_status_
   end function get_verification_status

   pure function get_provenance(this) result(prov)
      type(CF_Provenance) :: prov
      class(FieldDictionaryItem), intent(in) :: this
      prov = this%provenance_
   end function get_provenance

   pure logical function is_conserved(this)
      class(FieldDictionaryItem), intent(in) :: this
      is_conserved = this%conserved
   end function is_conserved

end module mapl3g_FieldDictionaryItem

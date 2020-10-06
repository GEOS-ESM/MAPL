#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_MAPLconfig
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   use ESMF
   use MAPL_Mod
   use yaFyaml
   use gFTL_StringVector

   implicit none
   private

   character(*), parameter :: rc_label = 'NUOPC_config'

   character(*), parameter :: NUOPC_imports = 'NUOPC_imports'
   character(*), parameter :: NUOPC_exports = 'NUOPC_exports'

   character(*), parameter :: NUOPCname               = 'NUOPCname'
   character(*), parameter :: TransferOfferGeomObject = 'TransferOfferGeomObject'
   character(*), parameter :: SharePolicyField        = 'SharePolicyField'
   character(*), parameter :: SharePolicyGeomObject   = 'SharePolicyGeomObject'

   character(*), parameter :: default_TransferOfferGeomObject = 'will provide'
   character(*), parameter :: default_SharePolicyField        = 'not share'

   type :: Field_Config
      character(:), allocatable :: name
      character(:), allocatable :: NUOPCname
      character(:), allocatable :: TransferOfferGeomObject
      character(:), allocatable :: SharePolicyField
      character(:), allocatable :: SharePolicyGeomObject
   contains
      procedure :: fill_defaults
      procedure :: read_field_config
   end type Field_Config

contains
   subroutine fill_defaults(this)
      class(Field_Config), intent(inout) :: this

      if (.not. allocated(this%NUOPCname))               this%NUOPCname               = this%name
      if (.not. allocated(this%TransferOfferGeomObject)) this%TransferOfferGeomObject = default_TransferOfferGeomObject
      if (.not. allocated(this%SharePolicyField))        this%SharePolicyField        = default_SharePolicyField
      if (.not. allocated(this%SharePolicyGeomObject))   this%SharePolicyGeomObject   = this%SharePolicyField
   end subroutine fill_defaults

   subroutine read_field_config(this, name, config)
      class(Field_Config), intent(inout) :: this
      character(*),        intent(in   ) :: name
      type(Configuration), intent(inout) :: config

      type(ConfigurationIterator) :: iter
      character(:), pointer       :: key

      this%name = name

      iter = config%begin()
      do while (iter /= config%end())
         key => iter%key()

         select case(key)
         case(NUOPCname)
            this%NUOPCname = iter%get()
         case(TransferOfferGeomObject)
            this%TransferOfferGeomObject = iter%get()
         case(SharePolicyField)
            this%SharePolicyField = iter%get()
         case(SharePolicyGeomObject)
            this%SharePolicyGeomObject = iter%get()
         end select
      end do

      call this%fill_defaults()
   end subroutine read_field_config

   subroutine read_file_name_from_config(config, filename, rc)
      type(ESMF_Config),         intent(inout) :: config
      character(:), allocatable, intent(  out) :: filename
      integer, optional,         intent(  out) :: rc

      character(len=ESMF_MaxStr) :: value
      integer                    :: status

      call ESMF_ConfigGetAttribute(config, value=value, label=rc_label, __RC__)
      filename = trim(value)

      _RETURN(_SUCCESS)
   end subroutine read_file_name_from_config
end module NUOPC_MAPLconfig

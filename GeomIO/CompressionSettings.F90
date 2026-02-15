#include "MAPL_Exceptions.h"
module mapl3g_CompressionSettings
    use ESMF
    use MAPL_ErrorHandling
    use MAPL_Constants
    use mapl3g_esmf_info_keys
    implicit none
    private

    ! Compression info keys
    character(len=*), parameter :: KEY_COMPRESSION = '/compression'
    character(len=*), parameter :: KEY_DEFLATE = '/deflate'
    character(len=*), parameter :: KEY_ZSTANDARD = '/zstandard'
    character(len=*), parameter :: KEY_QUANTIZE_LEV = '/quantize_level'
    character(len=*), parameter :: KEY_QUANTIZE_ALGO = '/quantize_algo'
    character(len=*), parameter :: KEY_NBITS = '/nbits'    
    ! Define the derived type (class)
    type, public :: CompressionSettings
        private
        integer :: deflate_level = 0
        integer :: zstandard_level = 0
        integer :: quantize_level = 0
        integer :: quantize_algorithm = MAPL_NOQUANTIZE
        integer :: nbits = 0
    contains
        ! Original setter methods
        procedure :: set_deflate_level
        procedure :: set_zstandard_level
        procedure :: set_quantize_level
        procedure :: set_quantize_algorithm
        procedure :: set_nbits
        
        ! Original getter methods
        procedure :: get_deflate_level
        procedure :: get_zstandard_level
        procedure :: get_quantize_level
        procedure :: get_quantize_algorithm
        procedure :: get_nbits
        
        ! ESMF_Info methods
        procedure :: set_info_attributes
        procedure :: get_info_attributes
        procedure :: update_from_info
        procedure :: sync_to_info
        
        ! Utility methods
        procedure :: set_all_levels
    end type CompressionSettings

    interface CompressionSettings
       procedure new_CompressionSettings
    end interface CompressionSettings

contains

    function new_CompressionSettings(hconfig, rc) result(compression_settings)
       type(CompressionSettings) :: compression_settings
       type(ESMF_HConfig), intent(in) :: hconfig
       integer, optional, intent(out) :: rc

       integer :: status
       logical :: has_key
       character(len=:), allocatable :: temp_string

       has_key = ESMF_HConfigIsDefined(hconfig, keyString='deflate', _RC)
       if (has_key) then
          compression_settings%deflate_level = ESMF_HConfigAsI4(hconfig, keyString='deflate', _RC)
          _ASSERT( compression_settings%deflate_level >= 0 .and. compression_settings%deflate_level <= 9, 'deflate level must be between 0 and 9')
       end if

       has_key = ESMF_HConfigIsDefined(hconfig, keyString='zstandard', _RC)
       if (has_key) then
          compression_settings%zstandard_level = ESMF_HConfigAsI4(hconfig, keyString='zstandard', _RC)
          _ASSERT( compression_settings%zstandard_level >= 0 .and. compression_settings%zstandard_level <= 22, 'zstandard level must be between 0 and 22')
       end if

       has_key = ESMF_HConfigIsDefined(hconfig, keyString='nbits', _RC)
       if (has_key) then
          compression_settings%nbits= ESMF_HConfigAsI4(hconfig, keyString='nbits', _RC)
          _ASSERT( compression_settings%nbits >= 0 .and. compression_settings%nbits <= 23, 'zstandard level must be between 0 and 23')
       end if

       has_key = ESMF_HConfigIsDefined(hconfig, keyString='quantize_level', _RC)
       if (has_key) then
          compression_settings%quantize_level= ESMF_HConfigAsI4(hconfig, keyString='quantize_level', _RC)
       end if

       has_key = ESMF_HConfigIsDefined(hconfig, keyString='quantize_algorithm', _RC)
       if (has_key) then
          temp_string = ESMF_HConfigAsString(hconfig, keyString='quantize_algorithm', _RC)
          temp_string = ESMF_UtilStringUpperCase(temp_string)
          select case (temp_string)
          case ('NONE')
             compression_settings%quantize_algorithm = MAPL_NOQUANTIZE
             _ASSERT( compression_settings%quantize_level == 0 , 'quantize_algorithm is none, so quantize_level must be "none"')
          case ('BITGROOM')
             compression_settings%quantize_algorithm = MAPL_QUANTIZE_BITGROOM
          case ('GRANULARBR', 'GRANULAR_BITROUND')
             compression_settings%quantize_algorithm = MAPL_QUANTIZE_GRANULAR_BITROUND
          case ('BITROUND')
             compression_settings%quantize_algorithm = MAPL_QUANTIZE_BITROUND
          case default
             _FAIL('Invalid quantize_algorithm. Allowed values are none, bitgroom, granular_bitround, granularbr (deprecated), and bitround')
          end select
       end if
       _RETURN(_SUCCESS)
    end function new_CompressionSettings

    ! Original setter methods
    subroutine set_deflate_level(this, level)
        class(CompressionSettings), intent(inout) :: this
        integer, intent(in) :: level
        this%deflate_level = level
    end subroutine set_deflate_level
    
    subroutine set_zstandard_level(this, level)
        class(CompressionSettings), intent(inout) :: this
        integer, intent(in) :: level
        this%zstandard_level = level
    end subroutine set_zstandard_level
    
    subroutine set_quantize_level(this, level)
        class(CompressionSettings), intent(inout) :: this
        integer, intent(in) :: level
        this%quantize_level = level
    end subroutine set_quantize_level
    
    subroutine set_quantize_algorithm(this, algorithm)
        class(CompressionSettings), intent(inout) :: this
        integer, intent(in) :: algorithm
        this%quantize_algorithm = algorithm
    end subroutine set_quantize_algorithm

    ! New setter method for nbits
    subroutine set_nbits(this, nbits_value)
        class(CompressionSettings), intent(inout) :: this
        integer, intent(in) :: nbits_value
        this%nbits = nbits_value
    end subroutine set_nbits

    ! Original getter methods
    function get_deflate_level(this) result(level)
        class(CompressionSettings), intent(in) :: this
        integer :: level
        level = this%deflate_level
    end function get_deflate_level
    
    function get_zstandard_level(this) result(level)
        class(CompressionSettings), intent(in) :: this
        integer :: level
        level = this%zstandard_level
    end function get_zstandard_level
    
    function get_quantize_level(this) result(level)
        class(CompressionSettings), intent(in) :: this
        integer :: level
        level = this%quantize_level
    end function get_quantize_level
    
    function get_quantize_algorithm(this) result(algorithm)
        class(CompressionSettings), intent(in) :: this
        integer :: algorithm
        algorithm = this%quantize_algorithm
    end function get_quantize_algorithm

    ! New getter method for nbits
    function get_nbits(this) result(nbits_value)
        class(CompressionSettings), intent(in) :: this
        integer :: nbits_value
        nbits_value = this%nbits
    end function get_nbits

    ! Set compression settings in ESMF_Info object
    subroutine set_info_attributes(this, info, rc)
        class(CompressionSettings), intent(in) :: this
        type(ESMF_Info), intent(inout) :: info
        integer, intent(out), optional :: rc
        
        integer :: status
        
        if (present(rc)) rc = ESMF_SUCCESS
        
        ! Set deflate level in Info object
        call ESMF_InfoSet(info, key='MAPL'//KEY_COMPRESSION//KEY_DEFLATE, value=this%deflate_level, _RC)
        
        ! Set zstandard level in Info object
        call ESMF_InfoSet(info, key='MAPL'//KEY_COMPRESSION//KEY_ZSTANDARD, value=this%zstandard_level, _RC)
        
        ! Set quantize level in Info object
        call ESMF_InfoSet(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_LEV, value=this%quantize_level, _RC)
        
        ! Set quantize algorithm in Info object
        call ESMF_InfoSet(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_ALGO, value=this%quantize_algorithm, _RC)
        
        ! Set nbits in Info object
        call ESMF_InfoSet(info, key='MAPL'//KEY_COMPRESSION//KEY_NBITS, value=this%nbits, _RC)
        
        _RETURN(_SUCCESS)
        
    end subroutine set_info_attributes

    ! Get compression settings from ESMF_Info object
    subroutine get_info_attributes(this, info, rc)
        class(CompressionSettings), intent(inout) :: this
        type(ESMF_Info), intent(in) :: info
        integer, intent(out), optional :: rc
        
        integer :: status
        logical :: isPresent
        
        if (present(rc)) rc = ESMF_SUCCESS
        
        ! Get deflate level from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_DEFLATE, _RC)
        if (isPresent) then 
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_DEFLATE, value=this%deflate_level, _RC)
        end if
        
        ! Get zstandard level from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_ZSTANDARD, _RC)
        if (isPresent) then
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_ZSTANDARD, value=this%zstandard_level, _RC)
        end if
        
        ! Get quantize level from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_LEV, _RC)
        if (isPresent) then 
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_LEV, value=this%quantize_level, _RC)
        end if
        
        ! Get quantize algorithm from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_ALGO, _RC)
        if (isPresent) then
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_ALGO, value=this%quantize_algorithm, _RC)
        end if
        
        ! Get nbits from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_NBITS, _RC)
        if (isPresent) then
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_NBITS, value=this%nbits, _RC)
        end if

        _RETURN(_SUCCESS)
        
    end subroutine get_info_attributes

    ! Update internal settings from ESMF_Info (alias for get_info_attributes)
    subroutine update_from_info(this, info, rc)
        class(CompressionSettings), intent(inout) :: this
        type(ESMF_Info), intent(in) :: info
        integer, intent(out), optional :: rc
       
        integer :: status 
        call this%get_info_attributes(info, _RC)
        _RETURN(_SUCCESS)
    end subroutine update_from_info

    ! Synchronize internal settings to ESMF_Info (alias for set_info_attributes)
    subroutine sync_to_info(this, info, rc)
        class(CompressionSettings), intent(in) :: this
        type(ESMF_Info), intent(inout) :: info
        integer, intent(out), optional :: rc
        
        integer :: status 
        call this%set_info_attributes(info, _RC)
        _RETURN(_SUCCESS)
    end subroutine sync_to_info

    ! Original utility methods - updated to include nbits
    subroutine set_all_levels(this, deflate, zstandard, quantize, algorithm, nbits)
        class(CompressionSettings), intent(inout) :: this
        integer, intent(in) :: deflate, zstandard, quantize, algorithm, nbits
        this%deflate_level = deflate
        this%zstandard_level = zstandard
        this%quantize_level = quantize
        this%quantize_algorithm = algorithm
        this%nbits = nbits
    end subroutine set_all_levels
    
end module mapl3g_CompressionSettings

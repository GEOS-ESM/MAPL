#include "MAPL_Exceptions.h"
module mapl3g_compression_settings_mod
    use ESMF
    use MAPL_ErrorHandling
    use mapl3g_esmf_info_keys
    implicit none
    private
    
    ! Define the derived type (class)
    type, public :: compression_settings
        private
        integer :: deflate_level = 0
        integer :: zstandard_level = 0
        integer :: quantize_level = 0
        integer :: quantize_algorithm = 0
    contains
        ! Original setter methods
        procedure :: set_deflate_level
        procedure :: set_zstandard_level
        procedure :: set_quantize_level
        procedure :: set_quantize_algorithm
        
        ! Original getter methods
        procedure :: get_deflate_level
        procedure :: get_zstandard_level
        procedure :: get_quantize_level
        procedure :: get_quantize_algorithm
        
        ! ESMF_Info methods
        procedure :: set_info_attributes
        procedure :: get_info_attributes
        procedure :: update_from_info
        procedure :: sync_to_info
        
        ! Utility methods
        procedure :: set_all_levels
    end type compression_settings

contains

    ! Original setter methods
    subroutine set_deflate_level(this, level)
        class(compression_settings), intent(inout) :: this
        integer, intent(in) :: level
        this%deflate_level = level
    end subroutine set_deflate_level
    
    subroutine set_zstandard_level(this, level)
        class(compression_settings), intent(inout) :: this
        integer, intent(in) :: level
        this%zstandard_level = level
    end subroutine set_zstandard_level
    
    subroutine set_quantize_level(this, level)
        class(compression_settings), intent(inout) :: this
        integer, intent(in) :: level
        this%quantize_level = level
    end subroutine set_quantize_level
    
    subroutine set_quantize_algorithm(this, algorithm)
        class(compression_settings), intent(inout) :: this
        integer, intent(in) :: algorithm
        this%quantize_algorithm = algorithm
    end subroutine set_quantize_algorithm

    ! Original getter methods
    function get_deflate_level(this) result(level)
        class(compression_settings), intent(in) :: this
        integer :: level
        level = this%deflate_level
    end function get_deflate_level
    
    function get_zstandard_level(this) result(level)
        class(compression_settings), intent(in) :: this
        integer :: level
        level = this%zstandard_level
    end function get_zstandard_level
    
    function get_quantize_level(this) result(level)
        class(compression_settings), intent(in) :: this
        integer :: level
        level = this%quantize_level
    end function get_quantize_level
    
    function get_quantize_algorithm(this) result(algorithm)
        class(compression_settings), intent(in) :: this
        integer :: algorithm
        algorithm = this%quantize_algorithm
    end function get_quantize_algorithm

    ! Set all compression settings in an ESMF_Info object
    subroutine set_info_attributes(this, info, rc)
        class(compression_settings), intent(in) :: this
        type(ESMF_Info), intent(inout) :: info
        integer, intent(out), optional :: rc
        
        integer :: status
        
        if (present(rc)) rc = ESMF_SUCCESS
        
        ! Set deflate level in Info object
        call ESMF_InfoSet(info, key='MAPL/compression/deflate_level', &
                         value=this%deflate_level, _RC)
        
        ! Set zstandard level in Info object
        call ESMF_InfoSet(info, key='MAPL/compression/zstandard_level', &
                         value=this%zstandard_level, _RC)
        
        ! Set quantize level in Info object
        call ESMF_InfoSet(info, key='MAPL/compression/quantize_level', &
                         value=this%quantize_level, _RC)
        
        ! Set quantize algorithm in Info object
        call ESMF_InfoSet(info, key='MAPL/compression/quantize_algorithm', &
                         value=this%quantize_algorithm, _RC)

        _RETURN(_SUCCESS)
        
    end subroutine set_info_attributes

    ! Get compression settings from ESMF_Info object
    subroutine get_info_attributes(this, info, rc)
        class(compression_settings), intent(inout) :: this
        type(ESMF_Info), intent(in) :: info
        integer, intent(out), optional :: rc
        
        integer :: status, temp_value
        logical :: isPresent
        
        if (present(rc)) rc = ESMF_SUCCESS
        
        ! Get deflate level from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_DEFLATE, _RC)
        if (isPresent) then 
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_DEFLATE, value=temp_value, _RC)
           this%deflate_level = temp_value
        end if
        
        ! Get zstandard level from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_ZSTANDARD, _RC)
        if (isPresent) then
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_ZSTANDARD, value=temp_value, _RC)
           this%zstandard_level = temp_value
        end if
        
        ! Get quantize level from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_LEV, _RC)
        if (isPresent) then 
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_LEV, value=temp_value, _RC)
           this%quantize_level = temp_value
        end if
        
        ! Get quantize algorithm from Info object
        isPresent = ESMF_InfoIsPresent(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_ALGO, _RC)
        if (isPresent) then
           call ESMF_InfoGet(info, key='MAPL'//KEY_COMPRESSION//KEY_QUANTIZE_ALGO, value=temp_value, _RC)
           this%quantize_algorithm = temp_value
        end if

        _RETURN(_SUCCESS)
        
    end subroutine get_info_attributes

    ! Update internal settings from ESMF_Info (alias for get_info_attributes)
    subroutine update_from_info(this, info, rc)
        class(compression_settings), intent(inout) :: this
        type(ESMF_Info), intent(in) :: info
        integer, intent(out), optional :: rc
       
        integer :: status 
        call this%get_info_attributes(info, _RC)
        _RETURN(_SUCCESS)
    end subroutine update_from_info

    ! Synchronize internal settings to ESMF_Info (alias for set_info_attributes)
    subroutine sync_to_info(this, info, rc)
        class(compression_settings), intent(in) :: this
        type(ESMF_Info), intent(inout) :: info
        integer, intent(out), optional :: rc
        
        integer :: status 
        call this%set_info_attributes(info, _RC)
        _RETURN(_SUCCESS)
    end subroutine sync_to_info

    ! Original utility methods
    subroutine set_all_levels(this, deflate, zstandard, quantize, algorithm)
        class(compression_settings), intent(inout) :: this
        integer, intent(in) :: deflate, zstandard, quantize, algorithm
        this%deflate_level = deflate
        this%zstandard_level = zstandard
        this%quantize_level = quantize
        this%quantize_algorithm = algorithm
    end subroutine set_all_levels
    
end module mapl3g_compression_settings_mod

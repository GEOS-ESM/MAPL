module mapl3g_StringDictionary
    use gftl2_StringVector
    implicit none
    private
    
    public :: StringDictionary
    public :: dict_put, dict_get, dict_has_key, dict_size, dict_clear
    public :: dict_keys, dict_values
    
    type :: StringDictionary
        private
        type(StringVector) :: keys
        type(StringVector) :: values
    contains
        procedure :: put => dict_put
        procedure :: get => dict_get
        procedure :: has_key => dict_has_key
        procedure :: size => dict_size
        procedure :: clear => dict_clear
        procedure :: get_keys => dict_keys
        procedure :: get_values => dict_values
        procedure :: print => dict_print
    end type StringDictionary
    
    interface StringDictionary
        module procedure :: new_string_dictionary
    end interface
    
contains

    function new_string_dictionary() result(dict)
        type(StringDictionary) :: dict
        ! Vectors are automatically initialized in gFTL2
        ! No explicit initialization needed
    end function new_string_dictionary
    
    subroutine dict_put(this, key, value)
        class(StringDictionary), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        integer :: index
        
        ! Check if key already exists
        index = find_key_index(this, key)
        
        if (index > 0) then
            ! Key exists, update value
            call this%values%set(index, value)
        else
            ! Key doesn't exist, add new key-value pair
            call this%keys%push_back(key)
            call this%values%push_back(value)
        end if
    end subroutine dict_put
    
    function dict_get(this, key, found) result(value)
        class(StringDictionary), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        character(len=:), allocatable :: value
        integer :: index
        
        index = find_key_index(this, key)
        
        if (index > 0) then
            value = this%values%of(index)
            if (present(found)) found = .true.
        else
            value = ""
            if (present(found)) found = .false.
        end if
    end function dict_get
    
    function dict_has_key(this, key) result(exists)
        class(StringDictionary), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: exists
        
        exists = find_key_index(this, key) > 0
    end function dict_has_key
    
    function dict_size(this) result(n)
        class(StringDictionary), intent(in) :: this
        integer :: n
        
        n = this%keys%size()
    end function dict_size
    
    subroutine dict_clear(this)
        class(StringDictionary), intent(inout) :: this
        
        call this%keys%clear()
        call this%values%clear()
    end subroutine dict_clear
    
    function dict_keys(this) result(keys_copy)
        class(StringDictionary), intent(in) :: this
        type(StringVector) :: keys_copy
        
        keys_copy = this%keys
    end function dict_keys
    
    function dict_values(this) result(values_copy)
        class(StringDictionary), intent(in) :: this
        type(StringVector) :: values_copy
        
        values_copy = this%values
    end function dict_values
    
    subroutine dict_print(this)
        class(StringDictionary), intent(in) :: this
        integer :: i
        
        write(*,*) 'Dictionary contents:'
        do i = 1, this%keys%size()
            write(*,'(A,A,A,A,A)') '"', this%keys%of(i), '" => "', this%values%of(i), '"'
        end do
    end subroutine dict_print
    
    ! Private helper function
    function find_key_index(this, key) result(index)
        class(StringDictionary), intent(in) :: this
        character(len=*), intent(in) :: key
        integer :: index
        integer :: i
        
        index = 0
        do i = 1, this%keys%size()
            if (this%keys%of(i) == key) then
                index = i
                return
            end if
        end do
    end function find_key_index

end module mapl3g_StringDictionary

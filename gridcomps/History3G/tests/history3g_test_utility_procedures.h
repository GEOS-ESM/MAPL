
   subroutine make_esmf_info(info, prefix, num_levels, vloc, num_ungridded, names, units_array, rc)
      type(ESMF_Info), intent(inout) :: info
      character(len=*), intent(in) :: prefix
      integer, intent(in) :: num_levels
      character(len=*), intent(in) :: vloc
      integer, intent(in) :: num_ungridded
      character(len=*), optional, intent(in) :: names(:)
      character(len=*), optional, intent(in) :: units_array(:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: NUMLEV_LABEL = 'num_levels'
      character(len=*), parameter :: VLOC_LABEL = 'vloc'
      character, parameter :: NUM_UNGRID_LABEL = 'num_ungridded'
      integer :: status

      call ESMF_InfoSet(info, prefix // NUMLEV_LABEL, num_levels, _RC)
      call ESMF_InfoSet(info, prefix // VLOC_LABEL, vloc, _RC)
      call make_esmf_ungridded_info(info, prefix, num_ungridded, names, units_array, _RC)

      SET_RC

   end subroutine make_esmf_info

   subroutine make_esmf_ungridded_info(info, prefix, num_ungridded, names, units_array, rc)
      type(ESMF_Info), intent(inout) :: info
      character(len=*), intent(in) :: prefix
      integer, intent(in) :: num_ungridded
      character(len=*), optional, intent(in) :: names(:)
      character(len=*), optional, intent(in) :: units_array(:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: NAME_LABEL = 'name'
      character(len=*), parameter :: UNITS_LABEL = 'units'
      character(len=*), parameter :: COORDINATES_LABEL = 'coordinates'
      real, parameter :: COORDINATES(3) = [2.0, 2.4, 2.5]
      type(ESMF_Info) :: comp_info
      character(len=:), allocatable :: name_, units_
      integer :: status, i

      status = -1

      SET_RC

      if(present(names)) then
         if(size(names) /= num_ungridded) return
      end if

      if(present(units_array)) then
         if(size(units_array) /= num_ungridded) return
      end if

      do i=1, num_ungridded
         name_ = NAME
         if(present(names)) name_ = names(i)
         units_ = UNITS
         if(present(units_array)) units_ = units_array(i)
         comp_info = ESMF_InfoCreate(_RC)
         call ESMF_InfoSet(comp_info, prefix // NAME_LABEL, name_, _RC)
         call ESMF_InfoSet(comp_info, prefix // UNITS_LABEL, units_, _RC)
         call ESMF_InfoSet(comp_info, prefix // COORDINATES_LABEL, COORDINATES, _RC)
         call ESMF_InfoSet(info, prefix // make_component_label(i), comp_info, _RC)
         call ESMF_InfoDestroy(comp_info)
      end do

      SET_RC

   end subroutine make_esmf_ungridded_info
   
   function make_component_label(n, rc) result(name)
      character(len=:), allocatable :: name
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: COMP_PREFIX = 'dim_'
      character(len=32) :: strn
      integer :: status

      write(strn, fmt='(I0)', iostat=status) n
      if(status == 0) name = COMP_PREFIX // trim(adjustl(strn))

      SET_RC

   end function make_component_label


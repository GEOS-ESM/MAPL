#define SET_RC if(present(rc)) rc = status

   subroutine make_esmf_info(info, num_levels, vloc, num_ungridded, names, units_array, rc)
      type(ESMF_Info), intent(inout) :: info
      integer, optional, intent(in) :: num_levels
      character(len=*), optional, intent(in) :: vloc
      integer, optional, intent(in) :: num_ungridded
      character(len=*), optional, intent(in) :: names(:)
      character(len=*), optional, intent(in) :: units_array(:)
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: NUMLEV_LABEL = 'num_levels'
      character(len=*), parameter :: VLOC_LABEL = 'vloc'
      character(len=*), parameter :: NUM_UNGRID_LABEL = 'num_ungridded'
      type(ESMF_Info) :: inner_info
      integer :: num_levels_
      character(len=:), allocatable :: vloc_

      num_levels_ = NUM_LEVELS_DEFAULT
      if(present(num_levels)) num_levels_ = num_levels
      vloc_ = VLOC_DEFAULT
      if(present(vloc)) vloc_ = vloc
      num_ungridded_ = NUM_UNGRIDDED_DEFAULT
      if(present(num_ungridded)) num_ungridded_ = num_ungridded

      inner_info = ESMF_InfoCreate(_RC)
      call make_vertical_dim(inner_info, VLOC_LABEL, vloc_, _RC)
      call ESMF_InfoSet(info, PREFIX // 'vertical_dim', value=inner_info, _RC)
      call ESMF_InfoDestroy(inner_info, _RC)
      
      inner_info = ESMF_InfoCreate(_RC)
      call make_vertical_geom(inner_info, NUMLEV_LABEL, num_levels_, _RC)
      call ESMF_InfoSet(info, PREFIX // 'vertical_geom', value=inner_info, _RC)
      call ESMF_InfoDestroy(inner_info, _RC)

      inner_info = ESMF_InfoCreate(_RC)
      call make_ungridded_dims_info(inner_info, num_ungridded_, names, units_array, _RC)
      call ESMF_InfoSet(info, PREFIX // 'ungridded_dims', value=inner_info, _RC)
      call ESMF_InfoDestroy(inner_info, _RC)

      SET_RC

   end subroutine make_esmf_info

   subroutine make_vertical_dim(info, label, value, rc)
      type(ESMF_Info), intent(inout) :: info
      character(len=*), intent(in) :: label
      character(len=*), intent(in) :: value
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_InfoSet(info, label, value, _RC)

   end subroutine make_vertical_dim

   subroutine make_vertical_geom(info, label, value, rc)
      type(ESMF_Info), intent(inout) :: info
      character(len=*), intent(in) :: label
      integer, intent(in) :: value
      integer, optional, intent(out) :: rc
      integer :: status

      call ESMF_InfoSet(info, label, value, _RC)

   end subroutine make_vertical_geom

   subroutine make_ungridded_dims_info(info, num_ungridded, names, units_array, rc)
      type(ESMF_Info), intent(inout) :: info
      integer, intent(in) :: num_ungridded
      character(len=*), optional, intent(in) :: names(:)
      character(len=*), optional, intent(in) :: units_array(:)
      integer, optional, intent(out) :: rc
      integer :: status, i
      character(len=*), parameter :: NAME_LABEL = 'name'
      character(len=*), parameter :: UNITS_LABEL = 'units'
      character(len=*), parameter :: COORDINATES_LABEL = 'coordinates'
      real, parameter :: COORDINATES(3) = [2.0, 2.4, 2.5]
      type(ESMF_Info) :: comp_info
      character(len=:), allocatable :: name_, units_

      status = -1

      SET_RC

      if(present(names)) then
         if(size(names) /= num_ungridded) return
      end if

      if(present(units_array)) then
         if(size(units_array) /= num_ungridded) return
      end if

      do i=1, num_ungridded
         name_ = NAME_DEFAULT
         if(present(names)) name_ = names(i)
         units_ = UNITS_DEFAULT
         if(present(units_array)) units_ = units_array(i)
         comp_info = ESMF_InfoCreate(_RC)
         call ESMF_InfoSet(comp_info, NAME_LABEL, name_, _RC)
         call ESMF_InfoSet(comp_info, UNITS_LABEL, units_, _RC)
         call ESMF_InfoSet(comp_info, COORDINATES_LABEL, COORDINATES, _RC)
         call ESMF_InfoSet(info, make_component_label(i), comp_info, _RC)
         call ESMF_InfoDestroy(comp_info)
      end do

      SET_RC

   end subroutine make_ungridded_dims_info
   
   function make_component_label(n, rc) result(name)
      character(len=:), allocatable :: name
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: COMP_PREFIX = 'dim_'
      character(len=32) :: strn

      write(strn, fmt='(I0)', iostat=status) n
      if(status == 0) name = COMP_PREFIX // trim(adjustl(strn))

      SET_RC

   end function make_component_label

! vim:ft=fortran

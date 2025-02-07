#include "MAPL_Exceptions.h"
module VerticalCoordinateMod
   use PFIO
   use MAPL_ExceptionHandling
   use MAPL_FileMetadataUtilsMod
   use MAPL_CommsMod
   use gFTL_StringVector
   use udunits2f, UDUNITS_are_convertible => are_convertible, &
      initialize_udunits => initialize, finalize_udunits => finalize
   use GEOS_GmapMod
   use MAPL_ConstantsMod, only: MAPL_GRAV

   implicit none
   public VerticalCoordinate
   public model_pressure
   public simple_coord
   public no_coord
   public check_conservation
   public remap_column
   public mass_mixing
   public volume_mixing
   public emission

   enum, bind(C)
      enumerator :: no_coord
      enumerator :: simple_coord
      enumerator :: fixed_pressure
      enumerator :: fixed_height
      enumerator :: model_pressure
   end enum

   enum, bind(c)
      enumerator :: mass_mixing 
      enumerator :: volume_mixing
      enumerator :: emission
      
   end enum

   enum, bind(C)
      enumerator :: vertical_stagger_center
      enumerator :: vertical_stagger_edge
   end enum

   type VerticalCoordinate
      integer :: num_levels
      real, allocatable :: bk(:)
      real, allocatable :: ak(:)
      real, allocatable :: levels(:)
      character(len=:), allocatable :: surf_name
      character(len=:), allocatable :: surf_units
      character(len=:), allocatable :: positive
      character(len=:), allocatable :: level_units
      integer :: stagger
      integer :: vertical_type
      contains
         procedure :: compute_ple
   end type

   interface VerticalCoordinate
      procedure :: new_VerticalCoordinate
   end interface verticalCoordinate

contains

   function new_verticalCoordinate(metadata, var_name, rc) result(vertical_coord)
      type(VerticalCoordinate) :: vertical_coord
      type(FileMetaDataUtils), intent(in) :: metadata
      character(len=*), intent(in) :: var_name
      integer, optional, intent(out) :: rc

      integer :: status, i
      type(StringVector), pointer :: dimensions
      type(StringVectorIterator) :: iter
      type(Variable), pointer :: var, dim_var, ps_var
      class(CoordinateVariable), pointer :: coord_var
      character(len=:), pointer :: dim_name
      logical :: is_vertical_coord_var, has_pressure_units, has_height_units
      character(len=:), allocatable :: lev_name, temp_units, formula_terms, standard_name, bounds_var, ak_name, bk_name, ps_name, source_file
      type(NETCDF4_FileFormatter) :: file_formatter
      real, allocatable :: temp_ak(:,:), temp_bk(:,:)
   
      var => metadata%get_variable(var_name, _RC)
      dimensions => var%get_dimensions()
      lev_name = ''
      iter = dimensions%begin()
      dimensions => var%get_dimensions()
      do while(iter /= dimensions%end())
         dim_name => iter%get()
         if (metadata%has_variable(dim_name)) then
            dim_var => metadata%get_variable(dim_name)
            is_vertical_coord_var = detect_cf_vertical_coord_var(dim_var, _RC)
            if (is_vertical_coord_var) then
               lev_name = dim_name
               exit 
            end if
         end if
         call iter%next()
      end do
      ! if not blank, we found something that "looks" like a vertical coordinate according to cf, now lets fill it out
      if (lev_name /= '') then
         coord_var => metadata%get_coordinate_variable(lev_name, _RC)
         vertical_coord%levels = get_coords(coord_var,_RC) 
         vertical_coord%num_levels = size(vertical_coord%levels)

         if (coord_var%is_attribute_present("positive")) vertical_coord%positive = coord_var%get_attribute_string("positive")
         if (coord_var%is_attribute_present("units"))  temp_units = coord_var%get_attribute_string("units")

         ! now test if this is a "fixed" pressure level, if has units of pressure, then CF says is pressure dimensional coordinate
         has_pressure_units = safe_are_convertible(temp_units, 'hPa', _RC)
         if (has_pressure_units) then
            vertical_coord%level_units = temp_units
            vertical_coord%vertical_type = fixed_pressure
            if (vertical_coord%levels(1) > vertical_coord%levels(2)) vertical_coord%positive = "up" !bmaa
            _RETURN(_SUCCESS)
         end if
         ! now test if this is a "fixed" height level, if has height units, then dimensioanl coordinate, but must have positive 
         has_height_units = safe_are_convertible(temp_units, 'm', _RC)
         if (has_height_units) then
            _ASSERT(allocated(vertical_coord%positive),"non pressure veritcal dimensional coordinates must have positive attribute")
            vertical_coord%level_units = temp_units
            vertical_coord%vertical_type = fixed_height
            _RETURN(_SUCCESS)
         end if
         ! now test if this is a model pressure, the positive says is vertical and formula_terms says, this is a parametric quantity
         if (coord_var%is_attribute_present("positive") .and. coord_var%is_attribute_present("formula_terms")) then
            standard_name = coord_var%get_attribute_string("standard_name") 
            formula_terms = coord_var%get_attribute_string("formula_terms")
            if (standard_name == "atmosphere_hybrid_sigma_pressure_coordinate") then
               ! do we have bounds, if so this is centers
               source_file = metadata%get_source_file()
               if (coord_var%is_attribute_present('bounds')) then
                  bounds_var = coord_var%get_attribute_string("bounds") 
                  var => metadata%get_variable(bounds_var, _RC)
                  formula_terms = var%get_attribute_string("formula_terms")
                  call parse_formula_terms(formula_terms, ps_name, ak_name, bk_name, _RC)
                  vertical_coord%surf_name = ps_name
                  ps_var => metadata%get_variable(ps_name)
                  vertical_coord%surf_units = ps_var%get_attribute_string("units")
                  vertical_coord%stagger = vertical_stagger_center
                  allocate(temp_ak(2,vertical_coord%num_levels))
                  allocate(temp_bk(2,vertical_coord%num_levels))
                  allocate(vertical_coord%ak(vertical_coord%num_levels+1))
                  allocate(vertical_coord%bk(vertical_coord%num_levels+1))
                  call file_formatter%open(source_file, PFIO_READ, _RC)
                  call file_formatter%get_var(ak_name, temp_ak, _RC)
                  call file_formatter%get_var(bk_name, temp_bk, _RC)
                  do i=2,vertical_coord%num_levels+1
                     vertical_coord%ak(i-1) = temp_ak(1,i-1) 
                     vertical_coord%ak(i) = temp_ak(2,i-1) 
                     vertical_coord%bk(i-1) = temp_bk(1,i-1) 
                     vertical_coord%bk(i) = temp_bk(2,i-1)
                  enddo   
               else
               ! do we not have bounds, if so this is edge
                  vertical_coord%num_levels = vertical_coord%num_levels - 1
                  call parse_formula_terms(formula_terms, ps_name, ak_name, bk_name, _RC)
                  vertical_coord%surf_name = ps_name
                  ps_var => metadata%get_variable(ps_name)
                  vertical_coord%surf_units = ps_var%get_attribute_string("units")
                  vertical_coord%stagger = vertical_stagger_edge
                  allocate(vertical_coord%ak(vertical_coord%num_levels+1))
                  allocate(vertical_coord%bk(vertical_coord%num_levels+1))
                  call file_formatter%open(source_file, PFIO_READ, _RC)
                  call file_formatter%get_var(ak_name, vertical_coord%ak, _RC)
                  call file_formatter%get_var(bk_name, vertical_coord%bk, _RC)
               end if
            else
               _FAIL("unsupported hybrid vertical coordinate")
            end if
            vertical_coord%vertical_type = model_pressure
            _RETURN(_SUCCESS)
         end if
         ! if this is none of those, then a simple coordinate
         vertical_coord%vertical_type = simple_coord
      else
         vertical_coord%vertical_type = no_coord
      end if
      _RETURN(_SUCCESS)
      
    end function new_VerticalCoordinate

    ! this is what CF says makes a vertical coordinate
    ! see 2nd paragraph of section 4.3
    ! but, either the coordinate variable is dimensional  has units of pressure, in this case positive is optional
    ! or it still dimensional and has some other units of height and positive is not optional
    ! or it dimensionless in which case must have positive
    function detect_cf_vertical_coord_var(var, rc) result(is_vertical_coord_var)
       logical :: is_vertical_coord_var
       type(Variable), intent(in) :: var
       integer, optional, intent(out) :: rc

       integer :: status

       logical :: has_positive, has_pressure_units, has_units
       character(len=:), allocatable :: units
       character(len=3) :: pressure_hpa

       is_vertical_coord_var = .false.
       pressure_hpa = "Pa"
       has_positive = var%is_attribute_present("positive", _RC)
       has_units = var%is_attribute_present("units", _RC)
       has_pressure_units = .false.
       if (has_units) then
          units = var%get_attribute_string("units", _RC) 
          has_pressure_units = safe_are_convertible(units, pressure_hpa, _RC)
       end if
       is_vertical_coord_var = has_pressure_units .or. has_positive
       _RETURN(_SUCCESS)
    end function detect_cf_vertical_coord_var

    function get_coords(coord_var, rc) result(coords)
       real, allocatable :: coords(:)
       class(CoordinateVariable), intent(in) :: coord_var
       integer, intent(out), optional :: rc

       class(*), pointer :: ptr(:)

       ptr => coord_var%get_coordinate_data()
       _ASSERT(associated(ptr),"coord variable coordinate data not found")
       select type (ptr)
       type is (real(kind=REAL64))
          coords=ptr
       type is (real(kind=REAL32))
          coords=ptr
       type is (integer(kind=INT64))
          coords=ptr
       type is (integer(kind=INT32))
          coords=ptr
       class default
          _FAIL("unsupported coordinate variable type in ")
       end select
       _RETURN(_SUCCESS)
    end function get_coords

    subroutine parse_formula_terms(formula_terms, ps, ak, bk, rc)
       character(len=*), intent(in) :: formula_terms
       character(len=:), allocatable, intent(out) :: ps
       character(len=:), allocatable, intent(out) :: ak
       character(len=:), allocatable, intent(out) :: bk
       integer, intent(out), optional :: rc

       ps = find_term(formula_terms, "ps:")
       ak = find_term(formula_terms, "ap:")
       bk = find_term(formula_terms, "b:")

       _RETURN(_SUCCESS)
    end subroutine parse_formula_terms

    function find_term(string, key) result(string_value)
       character(len=:), allocatable :: string_value
       character(len=*), intent(in) :: string
       character(len=*), intent(in) :: key

       integer :: key_pos, key_len, space_pos
       character(len=:), allocatable :: temp_string
       key_pos = index(string, key)
       key_len = len_trim(key)
       temp_string = string(key_pos+key_len:) 
       temp_string = adjustl(trim(temp_string))
       space_pos = index(temp_string," ")
       if (space_pos > 0) then
          string_value = temp_string(1:space_pos-1) 
       else
          string_value = temp_string 
       end if

    end function find_term

    function safe_are_convertible(from, to, rc) result(convertible)
       logical :: convertible
       character(*), intent(in) :: from, to
       integer, optional, intent(out) :: rc

       integer :: status
       type(UDUnit) :: unit1, unit2
       logical :: from_invalid, to_invalid

       unit1 = UDUnit(from)
       unit2 = UDUnit(to)
    
       from_invalid = unit1%is_free()
       to_invalid = unit2%is_free()

       if (from_invalid .or. to_invalid) then
          convertible = .false.
          _RETURN(_SUCCESS)
       end if
       convertible = UDUNITS_are_convertible(unit1, unit2, _RC)

       _RETURN(_SUCCESS)
    end function safe_are_convertible

    function compute_ple(this, ps, rc) result(ple)
       real, allocatable :: ple(:,:,:)
       class(VerticalCoordinate), intent(in) :: this
       real, intent(in) :: ps(:,:)
       integer, optional, intent(out) :: rc
       integer :: status, im, jm, i
       im=size(ps,1)
       jm=size(ps,2)
       allocate(ple(im,jm,this%num_levels+1))
       do i=1,this%num_levels+1
          ple(:,:,i)=this%ak(i)+(ps*this%bk(i))
       enddo
       _RETURN(_SUCCESS)
    end function

    subroutine remap_column(src_pressure, src_values, dst_pressure, dst_values)
       real, intent(in) :: src_pressure(:)
       real, intent(in) :: src_values(:)
       real, intent(in) :: dst_pressure(:)
       real, intent(inout) :: dst_values(:)
       real, allocatable :: temp_pressures_src(:,:,:), temp_values_src(:,:,:)
       real, allocatable :: temp_pressures_dst(:,:,:), temp_values_dst(:,:,:)
       real :: bottom_lev,delp1,delp2
       integer :: lb_src, lb_dst, lm_src, lm_dst, im ,jm, ub_src, ub_dst
       lm_src = size(src_values)
       lm_dst = size(dst_values)
       lb_src = lbound(src_pressure,1)
       lb_dst = lbound(dst_pressure,1)
       ub_src = ubound(src_pressure,1)
       ub_dst = ubound(dst_pressure,1)
       im = 1
       jm = 1
       if (src_pressure(ub_src) < dst_pressure(ub_dst)) then

          allocate(temp_pressures_dst(1,1,lm_dst+1))
          allocate(temp_values_dst(1,1,lm_dst))
          temp_pressures_dst(1,1,1:lm_dst+1)=dst_pressure(lb_dst:ub_dst)

          allocate(temp_values_src(1,1,lm_src+1),source=0.0)
          allocate(temp_pressures_src(1,1,lm_src+2),source=0.0)
          temp_values_src(1,1,1:lm_src) = src_values(:)
          temp_pressures_src(1,1,1:lm_src+1) = src_pressure(lb_src:ub_src)
          temp_pressures_src(1,1,lm_src+2) = temp_pressures_src(1,1,lm_src+1)+10.0
          call gmap(im, jm, lm_src+1, temp_pressures_src, temp_values_src, lm_dst, temp_pressures_dst, temp_values_dst)
          dst_values(:)=temp_values_dst(1,1,:)

       else if (src_pressure(ub_src) > dst_pressure(ub_dst)) then

          allocate(temp_pressures_src(1,1,lm_src+1))
          allocate(temp_values_src(1,1,lm_src))
          temp_pressures_src(1,1,1:lm_src+1)=src_pressure(lb_src:ub_src)
          temp_values_src(1,1,:) = src_values(:)

          allocate(temp_values_dst(1,1,lm_dst+1),source=0.0)
          allocate(temp_pressures_dst(1,1,lm_dst+2),source=0.0)

          temp_values_dst(1,1,1:lm_dst)=dst_values(:)
          temp_pressures_dst(1,1,1:lm_dst+1) = dst_pressure(lb_dst:ub_dst)

          temp_pressures_dst(1,1,lm_dst+2) = temp_pressures_src(1,1,lm_src+1)

          call gmap(im, jm, lm_src, temp_pressures_src, temp_values_src, lm_dst+1, temp_pressures_dst, temp_values_dst)
          delp1 = temp_pressures_dst(1,1,lm_dst+2) - temp_pressures_dst(1,1,lm_dst+1)
          delp2 = temp_pressures_dst(1,1,lm_dst+1) - temp_pressures_dst(1,1,lm_dst)
          bottom_lev = temp_values_dst(1,1,lm_dst+1)*(delp1/MAPL_GRAV) + temp_values_dst(1,1,lm_dst)*(delp2/MAPL_GRAV)
          temp_values_dst(1,1,lm_dst) = bottom_lev*MAPL_GRAV/delp2
          dst_values(1:lm_dst)=temp_values_dst(1,1,1:lm_dst)

       else if (src_pressure(ub_src) == dst_pressure(ub_dst)) then

       end if
       call check_conservation(src_pressure, src_values, dst_pressure, dst_values)

    end subroutine remap_column

    subroutine check_conservation(src_pressure, src_values, dst_pressure, dst_values)
       real, intent(in) :: src_pressure(:)
       real, intent(in) :: src_values(:)
       real, intent(in) :: dst_pressure(:)
       real, intent(inout) :: dst_values(:)
       real :: src_mass, dst_mass, delp
       integer :: lb_src, lb_dst, lm_src, lm_dst, i
       lm_src = size(src_values)
       lm_dst = size(dst_values)
       lb_src = lbound(src_pressure,1)
       lb_dst = lbound(dst_pressure,1)
       src_mass=0.0
       dst_mass=0.0
       do i=1,lm_src
          delp = src_pressure(lb_src+i)-src_pressure(lb_src+i-1)
          src_mass = src_mass + src_values(i)*delp/MAPL_GRAV
       enddo
       do i=1,lm_dst
          delp = dst_pressure(lb_dst+i)-dst_pressure(lb_dst+i-1)
          dst_mass = dst_mass + dst_values(i)*delp/MAPL_GRAV
       enddo
       if (src_mass .ne. 0.0) then
       _HERE,(dst_mass-src_mass)/src_mass
       end if
    end subroutine check_conservation


end module VerticalCoordinateMod   

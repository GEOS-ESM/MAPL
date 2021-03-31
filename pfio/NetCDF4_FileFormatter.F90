#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module pFIO_NetCDF4_FileFormatterMod
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use MAPL_ExceptionHandling
   use pFIO_ConstantsMod
   use pFIO_UnlimitedEntityMod
   use pFIO_AttributeMod
   use pFIO_VariableMod
   use pFIO_CoordinateVariableMod
   use pFIO_FileMetadataMod
   use pFIO_KeywordEnforcerMod
   use gFTL_StringVector
   use gFTL_StringIntegerMap
   use pFIO_StringVariableMapMod
   use pFIO_StringAttributeMapMod
   use netcdf
   implicit none
   private

   public :: NetCDF4_FileFormatter

   include 'mpif.h'
   type :: NetCDF4_FileFormatter
!$$      private
      integer :: ncid = -1
      logical :: parallel = .false.
      integer :: comm = -1
      integer :: info = -1
   contains
      procedure :: create
      procedure :: create_par
      procedure :: open
      procedure :: close
      procedure :: read
      procedure :: write

#include "new_overload.macro"
      procedure :: ___SUB(get_var,int32,0)
      procedure :: ___SUB(get_var,int32,1)
      procedure :: ___SUB(get_var,int32,2)
      procedure :: ___SUB(get_var,int32,3)
      procedure :: ___SUB(get_var,int64,0)
      procedure :: ___SUB(get_var,int64,1)
      procedure :: ___SUB(get_var,int64,2)
      procedure :: ___SUB(get_var,int64,3)
      procedure :: ___SUB(get_var,real32,0)
      procedure :: ___SUB(get_var,real32,1)
      procedure :: ___SUB(get_var,real32,2)
      procedure :: ___SUB(get_var,real32,3)
      procedure :: ___SUB(get_var,real32,4)
      procedure :: ___SUB(get_var,real32,5)
      procedure :: ___SUB(get_var,real64,0)
      procedure :: ___SUB(get_var,real64,1)
      procedure :: ___SUB(get_var,real64,2)
      procedure :: ___SUB(get_var,real64,3)
      procedure :: ___SUB(get_var,real64,4)

      procedure :: ___SUB(put_var,int32,0)
      procedure :: ___SUB(put_var,int32,1)
      procedure :: ___SUB(put_var,int32,2)
      procedure :: ___SUB(put_var,int32,3)
      procedure :: ___SUB(put_var,int64,0)
      procedure :: ___SUB(put_var,int64,1)
      procedure :: ___SUB(put_var,int64,2)
      procedure :: ___SUB(put_var,int64,3)
      procedure :: ___SUB(put_var,real32,0)
      procedure :: ___SUB(put_var,real32,1)
      procedure :: ___SUB(put_var,real32,2)
      procedure :: ___SUB(put_var,real32,3)
      procedure :: ___SUB(put_var,real32,4)
      procedure :: ___SUB(put_var,real32,5)
      procedure :: ___SUB(put_var,real64,0)
      procedure :: ___SUB(put_var,real64,1)
      procedure :: ___SUB(put_var,real64,2)
      procedure :: ___SUB(put_var,real64,3)
      procedure :: ___SUB(put_var,real64,4)
      

      generic :: get_var => ___SUB(get_var,int32,0)
      generic :: get_var => ___SUB(get_var,int32,1)
      generic :: get_var => ___SUB(get_var,int32,2)
      generic :: get_var => ___SUB(get_var,int32,3)
      generic :: get_var => ___SUB(get_var,int64,0)
      generic :: get_var => ___SUB(get_var,int64,1)
      generic :: get_var => ___SUB(get_var,int64,2)
      generic :: get_var => ___SUB(get_var,int64,3)
      generic :: get_var => ___SUB(get_var,real32,0)
      generic :: get_var => ___SUB(get_var,real32,1)
      generic :: get_var => ___SUB(get_var,real32,2)
      generic :: get_var => ___SUB(get_var,real32,3)
      generic :: get_var => ___SUB(get_var,real32,4)
      generic :: get_var => ___SUB(get_var,real32,5)
      generic :: get_var => ___SUB(get_var,real64,0)
      generic :: get_var => ___SUB(get_var,real64,1)
      generic :: get_var => ___SUB(get_var,real64,2)
      generic :: get_var => ___SUB(get_var,real64,3)
      generic :: get_var => ___SUB(get_var,real64,4)

      generic :: put_var => ___SUB(put_var,int32,0)
      generic :: put_var => ___SUB(put_var,int32,1)
      generic :: put_var => ___SUB(put_var,int32,2)
      generic :: put_var => ___SUB(put_var,int32,3)
      generic :: put_var => ___SUB(put_var,int64,0)
      generic :: put_var => ___SUB(put_var,int64,1)
      generic :: put_var => ___SUB(put_var,int64,2)
      generic :: put_var => ___SUB(put_var,int64,3)
      generic :: put_var => ___SUB(put_var,real32,0)
      generic :: put_var => ___SUB(put_var,real32,1)
      generic :: put_var => ___SUB(put_var,real32,2)
      generic :: put_var => ___SUB(put_var,real32,3)
      generic :: put_var => ___SUB(put_var,real32,4)
      generic :: put_var => ___SUB(put_var,real32,5)
      generic :: put_var => ___SUB(put_var,real64,0)
      generic :: put_var => ___SUB(put_var,real64,1)
      generic :: put_var => ___SUB(put_var,real64,2)
      generic :: put_var => ___SUB(put_var,real64,3)
      generic :: put_var => ___SUB(put_var,real64,4)

#include "undo_overload.macro"
      
      procedure, private :: def_dimensions
      procedure, private :: put_attributes
      procedure, private :: put_var_attributes
      procedure, private :: def_variables

      procedure, private :: inq_dimensions
      procedure, private :: inq_variables
      procedure, private :: inq_attributes
      procedure, private :: inq_var_attributes
      procedure, private :: write_const_variables
      procedure, private :: write_coordinate_variables

      procedure :: inq_dim
      procedure :: is_coordinate_dimension
   end type NetCDF4_FileFormatter

contains

   subroutine create(this, file, unusable, mode, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      character(len=*), intent(in) :: file
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mode
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: mode_

      if (present(mode)) then
         mode_=mode
      else
         mode_=NF90_CLOBBER
      end if
      !$omp critical
      status = nf90_create(file, IOR(mode_, NF90_NETCDF4), this%ncid)
      !$omp end critical
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine create


   subroutine create_par(this, file, unusable, mode, comm, info, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      character(len=*), intent(in) :: file
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: mode
      integer, optional, intent(in) :: comm
      integer, optional, intent(in) :: info
      integer, optional, intent(out) :: rc

      integer :: comm_
      integer :: info_
      integer :: status
      integer :: mode_

      if (present(mode)) then
         mode_=mode
      else
         mode_=NF90_CLOBBER
      end if

      if (present(comm)) then
         comm_ = comm
      else
         comm_ = MPI_COMM_WORLD
      end if

      if (present(info)) then
         info_ = info
      else
         info_ = MPI_INFO_NULL
      end if

      this%parallel = .true.
      this%comm = comm_
      this%info = info_

      mode_ = IOR(mode_, NF90_NETCDF4)
      mode_ = IOR(mode_, NF90_SHARE)
      mode_ = IOR(mode_, NF90_MPIIO)

      !$omp critical
      status = nf90_create(file, mode_, comm=comm_, info=info_, ncid=this%ncid)
      !$omp end critical
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine create_par


   subroutine open(this, file, mode, unusable, comm, info, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      character(len=*), intent(in) :: file
      integer, intent(in) :: mode
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      integer, optional, intent(in) :: info
      integer, optional, intent(out) :: rc

      integer :: omode
      integer :: status

      select case (mode)
      case (pFIO_READ)
         omode = NF90_NOWRITE
      case (pFIO_WRITE)
         omode = NF90_WRITE
      case default
         _ASSERT(.false.,"read or write mode")
      end select

      if (present(comm)) then
         this%comm = comm
         this%parallel=.true.
      end if

      if (present(info)) then
         this%info = info
      else
         this%info = MPI_INFO_NULL
      end if

      if (this%parallel) then
         !$omp critical
         status = nf90_open(file, IOR(omode, NF90_MPIIO), comm=this%comm, info=this%info, ncid=this%ncid)
         !$omp end critical
         _VERIFY(status)
      else
         !$omp critical
         status = nf90_open(file, IOR(omode, NF90_SHARE), this%ncid)
         !$omp end critical
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine open

   subroutine close(this, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      !$omp critical
      status = nf90_close(this%ncid)
      !$omp end critical
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine close


   subroutine write(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), intent(in) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call this%def_dimensions(cf, rc=status)
      _VERIFY(status)

      call this%def_variables(cf, rc=status)
      _VERIFY(status)
      
      call this%put_attributes(cf, NF90_GLOBAL, rc=status)
      _VERIFY(status)
 
      !$omp critical
      status= nf90_enddef(this%ncid)
      !$omp end critical
      _VERIFY(status)

      call this%write_coordinate_variables(cf, rc=status)
      _VERIFY(status)
      call this%write_const_variables(cf, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine write

   subroutine def_dimensions(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), intent(in) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dimid
      type (StringIntegerMap), pointer :: dims
      type (StringIntegerMapIterator) :: iter
      character(len=:), pointer :: dim_name
      integer, pointer :: dim_len

      integer :: nf90_len
      
      dims => cf%get_dimensions()
      iter = dims%begin()
      do while (iter /= dims%end())
         dim_name => iter%key()
         dim_len => iter%value()
         select case (dim_len)
         case (pFIO_UNLIMITED)
            nf90_len = NF90_UNLIMITED
         case default
            nf90_len = dim_len
         end select
         !$omp critical
         status = nf90_def_dim(this%ncid, dim_name, nf90_len, dimid)
         !$omp end critical
         _VERIFY(status)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine def_dimensions


   subroutine put_attributes(this, cf, varid, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), intent(in) :: cf
      integer, intent(in) :: varid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type (StringAttributeMap), pointer :: attributes
      type (StringAttributeMapIterator) :: iter
      character(len=:), pointer :: attr_name
      integer, allocatable :: shp(:)
      type (Attribute), pointer :: p_attribute
      class (*), pointer :: attr_values(:)
      class (*), pointer :: attr_value


      attributes => cf%get_attributes()
      iter = attributes%begin()
      do while (iter /= attributes%end())
         attr_name => iter%key()
         p_attribute => iter%value()
         shp = p_attribute%get_shape()

         if (size(shp) > 0) then 
           attr_values => p_attribute%get_values()
           _ASSERT(associated(attr_values), "should have values")

           select type (q => attr_values)
           type is (integer(INT32))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
             !$omp end critical
           type is (integer(INT64))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           type is (real(REAL32))
              !$omp critical
             status = nf90_put_att(this%ncid, varid, attr_name, q)
             !$omp end critical
           type is (real(REAL64))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           class default
             status = _FAILURE
           end select

           _VERIFY(status)
           call iter%next()

         else

           attr_value => p_attribute%get_value()
           _ASSERT(associated(attr_value), "should have value")
           select type (q => attr_value)
           type is (integer(INT32))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           type is (integer(INT64))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           type is (real(REAL32))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           type is (real(REAL64))
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           type is (character(len=*)) ! treat as scalar
              !$omp critical
              status = nf90_put_att(this%ncid, varid, attr_name, q)
              !$omp end critical
           class default
              status = _FAILURE
           end select

           _VERIFY(status)
           call iter%next()
         end if
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine put_attributes

   subroutine write_const_variables(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), intent(in) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type (StringVariableMap), pointer :: vars
      type (StringVariableMapIterator) :: var_iter
      type (UnlimitedEntity), pointer :: const_value_ptr
      character(len=:), pointer :: var_name
      type (Variable), pointer :: var
      integer, allocatable :: shp(:)
      class (*), pointer :: var_values(:)


      vars => cf%get_variables()

      var_iter = vars%begin()
      do while (var_iter /= vars%end())
         var_name => var_iter%key()
         var => var_iter%value()
         const_value_ptr => var%get_const_value()
         if ( .not. const_value_ptr%is_empty()) then
            shp = const_value_ptr%get_shape()
            var_values => const_value_ptr%get_values()
            select type(q => var_values)
            type is (integer(INT32))
               call this%put_var(trim(var_name), q, count=shp, rc=status)
               _VERIFY(status)
            type is (integer(INT64))
               call this%put_var(trim(var_name), q, count=shp, rc=status)
               _VERIFY(status)
            type is (real(REAL32))
               call this%put_var(trim(var_name), q, count=shp, rc=status)
               _VERIFY(status)
            type is (real(REAL64))
               call this%put_var(trim(var_name), q, count=shp, rc=status)
               _VERIFY(status)
            class default
               status = _FAILURE
            end select
         end if
         call var_iter%next() 
      enddo

      _UNUSED_DUMMY(unusable)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine write_const_variables

   subroutine write_coordinate_variables(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), intent(in) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type (StringVariableMap), pointer :: vars
      type (StringVariableMapIterator) :: var_iter

      character(len=:), pointer :: var_name
      type (CoordinateVariable), pointer :: var
      class (*), pointer :: dim_var_values(:)


      vars => cf%get_variables()

      var_iter = vars%begin()
      do while (var_iter /= vars%end())
         var_name => var_iter%key()
         var => cf%get_coordinate_variable(trim(var_name),rc=status)
         _VERIFY(status)
         if (associated(var))  then ! is a coordinate variable
            dim_var_values => var%get_coordinate_data()
            select type(q => dim_var_values)
            type is (integer(INT32))
               call this%put_var(trim(var_name),q,rc=status)
               _VERIFY(status)
            type is (integer(INT64))
               call this%put_var(trim(var_name),q,rc=status)
               _VERIFY(status)
            type is (real(REAL32))
               call this%put_var(trim(var_name),q,rc=status)
               _VERIFY(status)
            type is (real(REAL64))
               call this%put_var(trim(var_name),q,rc=status)
               _VERIFY(status)
            class default
               status = _FAILURE
            end select
         end if
         call var_iter%next() 

      enddo

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine write_coordinate_variables

   subroutine put_var_attributes(this, var, varid, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      class (Variable), target, intent(in) :: var
      integer, intent(in) :: varid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type (StringAttributeMap), pointer :: attributes
      type (StringAttributeMapIterator) :: iter
      character(len=:), pointer :: attr_name
      integer, allocatable :: shp(:)
      type (Attribute), pointer :: p_attribute
      class (*), pointer :: attr_value
      class (*), pointer :: attr_values(:)


      attributes => var%get_attributes()
      iter = attributes%begin()
      do while (iter /= attributes%end())
         attr_name => iter%key()

         p_attribute => iter%value()
         shp = p_attribute%get_shape()
         if (size(shp) == 0) then ! scalar
            attr_value => p_attribute%get_value()
            select type (q => attr_value)
            type is (integer(INT32))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (integer(INT64))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (real(REAL32))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (real(REAL64))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (character(len=*))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (stringWrap) 
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q%value)
               !$omp end critical
            class default
               status = _FAILURE
            end select
         else
            attr_values => p_attribute%get_values()
            select type (q => attr_values)
            type is (integer(INT32))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (integer(INT64))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (real(REAL32))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            type is (real(REAL64))
               !$omp critical
               status = nf90_put_att(this%ncid, varid, attr_name, q)
               !$omp end critical
            class default
               status = _FAILURE
            end select
         end if
         _VERIFY(status)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine put_var_attributes


   subroutine def_variables(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), target, intent(in) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      !integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type (StringVariableMap), pointer :: vars
      type (StringVector) :: order
      type (StringVectorIterator) :: var_iter
      type (StringVectorIterator) :: dim_iter
      integer :: xtype

      type (StringVector), pointer :: var_dims
      integer :: idim
      integer, allocatable :: dimids(:)
      integer, pointer :: chunksizes(:)
      integer :: deflation
      character(len=:), pointer :: var_name
      character(len=:), pointer :: dim_name
      class (Variable), pointer :: var
      integer :: varid

      type (StringIntegerMap), pointer :: all_dims


      vars => cf%get_variables()
      all_dims => cf%get_dimensions()

      order = cf%get_order()
      var_iter = order%begin()
      do while (var_iter /= order%end())
         var_name => var_iter%get()
         var => vars%at(var_name)
         xtype = get_xtype(var%get_type(),rc=status)
         _VERIFY(status)
         var_dims => var%get_dimensions()
         allocate(dimids(var_dims%size()))

         dim_iter = var_dims%begin()
         idim = 1
         do while (dim_iter /= var_dims%end())
            dim_name => dim_iter%get()
            !$omp critical
            status = nf90_inq_dimid(this%ncid, dim_name, dimids(idim))
            !$omp end critical
            call dim_iter%next()
            idim = idim + 1
         end do
         _VERIFY(status)

         !$omp critical
         status = nf90_def_var(this%ncid, var_name, xtype, dimids, varid)
         !$omp end critical
         _VERIFY(status)
         !$omp critical
         status = nf90_def_var_fill(this%ncid, varid, NF90_NOFILL, 0)
         !$omp end critical
         _VERIFY(status)
         chunksizes => var%get_chunksizes()
         if (size(chunksizes) > 0) then
            !$omp critical
           status = nf90_def_var_chunking(this%ncid, varid, NF90_CHUNKED, chunksizes=chunksizes)
           !$omp end critical
           _VERIFY(status)
         end if

         deflation = var%get_deflation()
         if (deflation > 0) then 
            !$omp critical
           status = nf90_def_var_deflate(this%ncid, varid, 1, 1, deflation)
           !$omp end critical
           _VERIFY(status)
         end if

         call this%put_var_attributes(var, varid, rc=status)
         _VERIFY(status)
         
         deallocate(dimids)

         call var_iter%next()
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine def_variables


   integer function get_xtype(fio_type, rc) result(xtype)
      integer, intent(in) :: fio_type
      integer, intent(out) :: rc

      rc = _SUCCESS

      select case (fio_type)
      case (pFIO_INT32)
         xtype = NF90_INT
      case (pFIO_INT64)
         xtype = NF90_INT64
      case (pFIO_REAL32)
         xtype = NF90_FLOAT
      case (pFIO_REAL64)
         xtype = NF90_DOUBLE
      case (pFIO_STRING)
         xtype = NF90_CHAR
      case default
         rc = _FAILURE
      end select

      return
   end function get_xtype


   integer function get_fio_type(xtype, rc) result(fio_type)
      integer, intent(in) :: xtype
      integer, intent(out) :: rc

      rc = _SUCCESS

      select case (xtype)
      case (NF90_INT)
         fio_type = pFIO_INT32
      case (NF90_INT64)
         fio_type = pFIO_INT64
      case (NF90_FLOAT)
         fio_type = pFIO_REAL32
      case (NF90_DOUBLE)
         fio_type = pFIO_REAL64
      case (NF90_CHAR)
         fio_type = pFIO_STRING
      case default
         rc = _FAILURE
      end select

      return
   end function get_fio_type
   
   function read(this, unusable, rc) result(cf)
      type (FileMetadata), target :: cf
      class (NetCDF4_FileFormatter), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call this%inq_dimensions(cf, rc=status)
      _VERIFY(status)

      call this%inq_variables(cf, rc=status)
      _VERIFY(status)

      call this%inq_attributes(cf, NF90_GLOBAL, rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function read


   subroutine inq_dimensions(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), target, intent(inout) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dim_len
      integer :: dimid
      integer :: nDimensions
      type (StringIntegerMap), pointer :: cf_dims
      character(len=NF90_MAX_NAME) :: dim_name

      !$omp critical
      status = nf90_inquire(this%ncid, nDimensions=nDimensions)
      !$omp end critical
      _VERIFY(status)

      cf_dims => cf%get_dimensions()

      do dimid = 1, nDimensions
         !$omp critical
         status = nf90_inquire_dimension(this%ncid, dimid, dim_name, dim_len)
         !$omp end critical
         _VERIFY(status)
         call cf_dims%insert(trim(dim_name), dim_len)
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine inq_dimensions

   subroutine inq_attributes(this, cf, varid, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), target, intent(inout) :: cf
      integer, intent(in) :: varid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      
      integer :: attnum, nAttributes
      integer :: xtype
      integer :: len

      integer(kind=INT32), allocatable :: i32(:)
      integer(kind=INT64), allocatable :: i64(:)
      real(kind=REAL32), allocatable :: r32(:)
      real(kind=REAL64), allocatable :: r64(:)
      character(len=:), allocatable :: str
      character(len=NF90_MAX_NAME) :: attr_name


      !$omp critical
      status = nf90_inquire(this%ncid, nAttributes=nAttributes)
      !$omp end critical
      _VERIFY(status)

      do attnum = 1, nAttributes
         !$omp critical
         status = nf90_inq_attname(this%ncid, varid, attnum, attr_name)
         !$omp end critical
         _VERIFY(status)
         !$omp critical
         status = nf90_inquire_attribute(this%ncid, varid, trim(attr_name), xtype, len)
         !$omp end critical
         _VERIFY(status)
         select case (xtype)
         case (NF90_INT)
            allocate(i32(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), i32)
            !$omp end critical
            _VERIFY(status)
            call cf%add_attribute(trim(attr_name), i32)
            deallocate(i32)
         case (NF90_INT64)
            allocate(i64(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), i64)
            !$omp end critical
            _VERIFY(status)
            call cf%add_attribute(trim(attr_name), i64)
            deallocate(i64)
         case (NF90_FLOAT)
            allocate(r32(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), r32)
            !$omp end critical
            _VERIFY(status)
            call cf%add_attribute(trim(attr_name), r32)
            deallocate(r32)
         case (NF90_DOUBLE)
            allocate(r64(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), r64)
            !$omp end critical
            _VERIFY(status)
            call cf%add_attribute(trim(attr_name), r64)
            deallocate(r64)
         case (NF90_CHAR)
            allocate(character(len=len) :: str)
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), str)
            !$omp end critical
            _VERIFY(status)
            call cf%add_attribute(trim(attr_name), str)
            deallocate(str)
         case default
            _RETURN(_FAILURE)
         end select
            
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine inq_attributes


   subroutine inq_var_attributes(this, var, varid, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      class (Variable), target, intent(inout) :: var
      integer, intent(in) :: varid
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      
      integer :: attnum, nAttributes
      integer :: xtype
      integer :: len

      integer(kind=INT32), allocatable :: i32(:)
      integer(kind=INT64), allocatable :: i64(:)
      real(kind=REAL32), allocatable :: r32(:)
      real(kind=REAL64), allocatable :: r64(:)
      character(len=:), allocatable :: str
      character(len=NF90_MAX_NAME) :: attr_name


      !$omp critical
      status = nf90_inquire_variable(this%ncid, varid, nAtts=nAttributes)
      !$omp end critical
      _VERIFY(status)


      do attnum = 1, nAttributes
         !$omp critical
         status = nf90_inq_attname(this%ncid, varid, attnum, attr_name)
         !$omp end critical
         _VERIFY(status)
         !$omp critical
         status = nf90_inquire_attribute(this%ncid, varid, attr_name, xtype, len)
         !$omp end critical
         _VERIFY(status)

         select case (xtype)
         case (NF90_INT)
            allocate(i32(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, attr_name, i32)
            !$omp end critical
            _VERIFY(status)
            call var%add_attribute(attr_name, i32)
            deallocate(i32)
         case (NF90_INT64)
            allocate(i64(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), i64)
            !$omp end critical
            _VERIFY(status)
            call var%add_attribute(trim(attr_name), i64)
            deallocate(i64)
         case (NF90_FLOAT)
            allocate(r32(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), r32)
            !$omp end critical
            _VERIFY(status)
            call var%add_attribute(trim(attr_name), r32)
            deallocate(r32)
         case (NF90_DOUBLE)
            allocate(r64(len))
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), r64)
            !$omp end critical
            _VERIFY(status)
            call var%add_attribute(trim(attr_name), r64)
            deallocate(r64)
         case (NF90_CHAR)
            allocate(character(len=len) :: str)
            !$omp critical
            status = nf90_get_att(this%ncid, varid, trim(attr_name), str)
            !$omp end critical
            _VERIFY(status)
            call var%add_attribute(trim(attr_name), str)
            deallocate(str)
         case default
            _RETURN(_FAILURE)
         end select

      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine inq_var_attributes

   
   subroutine inq_variables(this, cf, unusable, rc)
      class (NetCDF4_FileFormatter), intent(inout) :: this
      type (FileMetadata), target, intent(inout) :: cf
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: nVariables
      integer :: varid
      integer :: xtype

      character(len=:), allocatable :: dim_string
      integer :: idim, ndims
      integer, allocatable :: dimids(:)
      character(len=NF90_MAX_NAME) :: var_name
      character(len=NF90_MAX_NAME) :: dim_name
      class (Variable), allocatable :: var
      class (*), allocatable :: coordinate_data(:)
      integer(kind=INT32), allocatable :: data_int32(:)
      integer(kind=INT64), allocatable :: data_int64(:)
      real(kind=REAL32), allocatable :: data_real32(:)
      real(kind=REAL64), allocatable :: data_real64(:)

      integer :: len
      integer :: dimid


      !$omp critical
      status = nf90_inquire(this%ncid, nVariables=nVariables)
      !$omp end critical
      _VERIFY(status)

      do varid = 1, nVariables
         !$omp critical
         status = nf90_inquire_variable(this%ncid, varid, name=var_name, xtype=xtype, ndims=ndims)
         !$omp end critical
         _VERIFY(status)
         
         allocate(dimids(ndims))
         !$omp critical
         status = nf90_inquire_variable(this%ncid, varid, dimids=dimids)
         !$omp end critical
         _VERIFY(status)

         dim_string = ''
         do idim = 1, ndims
            !$omp critical
            status = nf90_inquire_dimension(this%ncid, dimids(idim), name=dim_name)
            !$omp end critical
            _VERIFY(status)
            dim_string = dim_string // trim(dim_name)
            if(idim < ndims) dim_string = dim_string // pFIO_DIMENSION_SEPARATOR
         end do
         deallocate(dimids)

         if ( this%is_coordinate_dimension(trim(var_name))) then
            !$omp critical
            status = nf90_inq_dimid(this%ncid, dim_string, dimid=dimid)
            !$omp end critical
            _VERIFY(status)
            !$omp critical
            status = nf90_inquire_dimension(this%ncid, dimid, len=len)
            !$omp end critical
            _VERIFY(status)

            select case (xtype)
            case (NF90_INT)
               allocate(data_int32(len))
               call this%get_var(trim(var_name), data_int32, count=[len], rc=status)
               _VERIFY(status)
               allocate(coordinate_data, source=data_int32)
               deallocate(data_int32)
            case (NF90_INT64)
               allocate(data_int64(len))
               call this%get_var(trim(var_name), data_int64, count=[len], rc=status)
               _VERIFY(status)
               allocate(coordinate_data, source=data_int64)
               deallocate(data_int64)
            case (NF90_FLOAT)
               allocate(data_real32(len))
               call this%get_var(trim(var_name), data_real32, count=[len],rc=status)
               _VERIFY(status)
               allocate(coordinate_data, source=data_real32)
               deallocate(data_real32)
            case (NF90_DOUBLE)
               allocate(data_real64(len))
               call this%get_var(trim(var_name), data_real64, count=[len], rc=status)
               _VERIFY(status)
               allocate(coordinate_data, source=data_real64)
               deallocate(data_real64)
            case default
               ! Cannot be string
               _RETURN(_FAILURE)
            end select

            allocate(var, source= &
                 & CoordinateVariable(Variable(type = get_fio_type(xtype,rc=status), dimensions=dim_string), &
                 & coordinate_data))
            _VERIFY(status)
            deallocate(coordinate_data)
         else
            allocate(var, source=Variable(type= get_fio_type(xtype,rc=status), dimensions=dim_string))
            _VERIFY(status)
         end if

         call this%inq_var_attributes(var, varid, rc=status)
         _VERIFY(status)

         call cf%add_variable(trim(var_name), var,rc=status)
         _VERIFY(status)

         deallocate(var)

      end do
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine inq_variables

   ! INT32
#define _VARTYPE 1
#  define _RANK 0
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 1
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 2
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 3
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#undef _VARTYPE

   ! INT64
#define _VARTYPE 2
#  define _RANK 0
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 1
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 2
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 3
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#undef _VARTYPE


   ! REAL32
#define _VARTYPE 4
#  define _RANK 0
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 1
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 2
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 3
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 4
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 5
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#undef _VARTYPE
   
   ! REAL64
#define _VARTYPE 5
#  define _RANK 0
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 1
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 2
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 3
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#  define _RANK 4
#    include "NetCDF4_get_var.H"
#    include "NetCDF4_put_var.H"
#  undef _RANK
#undef _VARTYPE
   
   
#undef _TYPE


   ! Kludge to support parallel write with UNLIMITED dimension
   integer function inq_dim(this, dim_name, unusable, rc) result(length)
      class (NetCDF4_FileFormatter), intent(in) :: this
      character(len=*), intent(in) :: dim_name
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: dimid
      integer :: status


      !$omp critical
      status = nf90_inq_dimid(this%ncid, name=dim_name, dimid=dimid)
      !$omp end critical
      _VERIFY(status)
      
      length = 0
      !$omp critical
      status = nf90_inquire_dimension(this%ncid, dimid, len=length)
      !$omp end critical
      _VERIFY(status)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end function inq_dim


   logical function is_coordinate_dimension(this, name)
      class (NetCDF4_FileFormatter), intent(in) :: this
      character(*), intent(in) :: name

      integer :: dimid
      integer :: status

      !$omp critical
      status = nf90_inq_dimid(this%ncid, name=name, dimid=dimid)
      !$omp end critical

      ! Sucess means that a dimension exists of the name name
      is_coordinate_dimension = (status == 0)
      
   end function is_coordinate_dimension

end module pFIO_NetCDF4_FileFormatterMod

module pFIO_FormatterPtrVectorMod
  use pFIO_NetCDF4_FileFormatterMod

#define _type type(NetCDF4_FileFormatter)

#define _vector FormatterPtrVector
#define _iterator FormatterPtrVectorIterator
#include "templates/vector.inc"

#undef _iterator
#undef _vector
#undef _type
end module pFIO_FormatterPtrVectorMod

module pFIO_StringNetCDF4_FileFormatterMapMod
   use pFIO_NetCDF4_FileFormatterMod

#include "types/key_deferredLengthString.inc"
#define _value type (NetCDF4_FileFormatter)
#define _value_equal_defined

#define _map StringNetCDF4_FileFormatterMap
#define _iterator StringNetCDF4_FileFormatterMapIterator

#define _alt

#include "templates/map.inc"

#undef _alt
#undef _iterator
#undef _map
#undef _value
#undef _key
#undef _value_equal_defined
end module pFIO_StringNetCDF4_FileFormatterMapMod

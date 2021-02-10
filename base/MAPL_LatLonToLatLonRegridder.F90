#include "MAPL_Generic.h"
module MAPL_LatLonToLatLonRegridderMod
   use MAPL_AbstractRegridderMod
   use MAPL_GridSpecMod
   use MAPL_RegridderSpec
   use mapl_RegridMethods
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use ESMF
   
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: LatLonToLatLonRegridder

   type :: Weights
      real(kind=REAL32), allocatable :: f(:)
   end type Weights

   type :: Mapping
      type (Weights), pointer :: WeightList(:)
   end type Mapping

   integer, parameter :: NUM_DIMS = 2

   type, extends(AbstractRegridder) :: LatLonToLatLonRegridder
      private
      type (ESMF_Grid) :: grid_in
      type (ESMF_Grid) :: grid_out
      integer :: num_points_in(NUM_DIMS+1)
      integer :: num_points_out(NUM_DIMS+1)
      type (Mapping) :: mappings(NUM_DIMS)
   contains
      procedure :: initialize_subclass
      procedure :: regrid_scalar_2d_real32 => apply_weights_real32
      procedure :: regrid_scalar_2d_real64 => apply_weights_real64
      procedure :: regrid_scalar_3d_real32 
   end type LatLonToLatLonRegridder

   interface LatLonToLatLonRegridder
      module procedure newLatLonToLatLonRegridder
   end interface LatLonToLatLonRegridder

  real, parameter :: WEIGHT_THRESHOLD = 0.7
  character(len=*), parameter :: MOD_NAME = 'MAPL_LatLonToLatLonRegridder::'

contains

   function newLatLonToLatLonRegridder(grid_in, grid_out, regrid_method, unusable, rc) result(regridder)
      use ESMF
      type (LatLonToLatLonRegridder) :: regridder
      type (ESMF_Grid), intent(in) :: grid_in
      type (ESMF_Grid), intent(in) :: grid_out
      integer :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: Iam = 'newLatLonToLatLonRegridder'

      type (RegridderSpec) :: regridder_spec

      _UNUSED_DUMMY(unusable)

      regridder_spec = RegridderSpec(grid_in, grid_out, regrid_method)
      call regridder%initialize(regridder_spec)

      _RETURN(_SUCCESS)
        
   end function newLatLonToLatLonRegridder


   subroutine compute_linear_weights(Weight, Xin, Xout, rc)
      type(Weights),  intent(inout) :: Weight(:)
      real(kind=REAL32), intent(in) :: Xin(:)
      real(kind=REAL32), intent(in) :: Xout(:)
      integer, optional, intent(out) :: rc
      
      ! Compute weights for binned interpolation along a dimension.
      ! Xout are the N_in + 1 input bin edges.
      ! Xin  are the N_out + 1 output bin edges
      ! Weigths are the mapping
      
      
      integer :: j_out, j0, j1
      integer :: N_in
      integer :: status
      character(len=*), parameter :: Iam = 'compute_linear_weights'

      N_in  = size(Xin )

      do j_out=1,size(Weight)
         j0 = 1
         do
            if(Xout(j_out  ) <= Xin(j0+1)) exit
            j0=j0+1
            _ASSERT(j0 < N_in, 'index error')
         end do
         j1 = j0 + 1

         allocate(weight(j_out)%f(j0:j1), stat=status)
         _VERIFY(status)

         associate (b => weight(j_out)%f)
           b(j0  ) = (Xin(j1)-Xout(j_out))/(Xin(j1)-Xin(j0))
           b(j0+1) = 1.0 - b(j0)
         end associate
         
      end do
    
   end subroutine compute_linear_weights


  subroutine compute_binning_weights(Weight,Xin,Xout,HasPoles,rc)
    
    type(Weights),     intent(INOUT) :: Weight(:)
    real(kind=REAL32), intent(IN   ) :: Xin(:), Xout(:)
    logical,           intent(IN   ) :: HasPoles
    integer, optional, intent(OUT  ) :: rc


    ! Compute weights for binned interpolation along a dimension.
    ! Xout are the N_in + 1 input bin edges.
    ! Xin  are the N_out + 1 output bin edges
    ! Weigths are the mapping


    integer :: j_out, j0, j1, j
    integer :: N_in, N_out
    integer :: status
    real    :: dx, ff
    character(len=*), parameter :: Iam = 'compute_binning_weights'

    N_in  = size(Xin )-1
    N_out = size(Weight)

    do j_out=1,N_out
       j0 = 1
       do           
          if(Xout(j_out  )>=Xin(j0) .and. Xout(j_out  )<=Xin(j0+1)) exit
          j0=j0+1
          _ASSERT(j0 <= N_in, 'index error')
       end do

       j1 = j0
       do
          if(Xout(j_out+1)>=Xin(j1) .and. Xout(j_out+1)<=Xin(j1+1)) exit
          j1=j1+1
          _ASSERT(j1 <= N_in, 'index error')
       end do

       allocate(weight(j_out)%f(j0:j1), stat=status)
       _VERIFY(status)
       associate (b => weight (j_out)%f)

         if(j0==j1) then
            b(j0) = 1.
         else
            dx    = Xin(j0+1)-Xout(j_out)
            ff    = dx
            b(j0) = dx
            do j=j0+1,j1-1
               dx   = Xin(j+1) - Xin(j)
               ff   = ff + dx
               b(j) = dx
            end do
            dx    = Xout(j_out+1)-Xin(j1)
            ff    = ff + dx
            b(j1) = dx
            b     = b/ff
         end if
         
       end associate

    end do

    if(HasPoles) then
       deallocate(Weight(    1)%f)
       deallocate(Weight(N_out)%f)
       allocate  (Weight(    1)%f(1   :1   ))
       allocate  (Weight(N_out)%f(N_in:N_in))
       Weight(    1)%f  =  1.
       Weight(N_out)%f  =  1.
    endif

 end subroutine compute_binning_weights

   subroutine regrid_scalar_3d_real32(this, q_in, q_out, rc)
      use MAPL_CommsMod
      use MAPL_BaseMod

      class (LatLonToLatLonRegridder), intent(in) :: this
      real (kind=REAL32), intent(in) :: q_in(:,:,:)
      real (kind=REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real32'
      integer :: k

      type (RegridderSpec) :: spec

      logical :: redistribute

      _ASSERT(size(q_in,3) == size(q_out,3), 'inconsistent array shape')

      spec = this%get_spec()

      block
        integer :: N_in(NUM_DIMS)
        integer :: dims(5)

        call MAPL_GridGet(spec%grid_in, globalCellCountPerDim=dims, rc=status)
        _VERIFY(status)
        N_in = dims(1:2)

        if ((N_in(1) /= size(q_in,1)) .or. (N_in(2) /= size(q_in,2))) then
           redistribute = .true.
        else
           redistribute = .false.
        end if

      end block
      if (redistribute) then
         block
           real (kind=REAL32), pointer :: q_in_global(:,:,:)
           real (kind=REAL32), allocatable :: q_out_global(:,:,:)
           integer :: N_out(NUM_DIMS)
           integer :: dims(5)

           q_in_global=> null()

           call MAPL_CollectiveGather3D(spec%grid_in, q_in, q_in_global, rc=status)
           _VERIFY(status)

           call MAPL_GridGet(spec%grid_out, globalCellCountPerDim=dims, rc=status)
           _VERIFY(status)
           N_out = dims(1:2)

           allocate(q_out_global(n_out(1), n_out(2), size(q_in_global,3)))

           if (size(q_in_global) > 1) then
              do k = 1, size(q_in_global,3)
                 call this%regrid(q_in_global(:,:,k), q_out_global(:,:,k), rc=status)
                 _VERIFY(status)
              end do
           end if

           deallocate(q_in_global)

           call MAPL_CollectiveScatter3D(spec%grid_out, q_out_global, q_out, rc=status)
           _VERIFY(status)

           deallocate(q_out_global)
         end block
      else
         if (size(q_in) > 1) then
            do k = 1, size(q_in,3)
               call this%regrid(q_in(:,:,k), q_out(:,:,k), rc=status)
               _VERIFY(status)
            end do
         end if
      end if

      _RETURN(_SUCCESS)

   end subroutine regrid_scalar_3d_real32   

   subroutine apply_weights_real32(this, q_in, q_out, rc)
      class (LatLonToLatLonRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: j
      integer :: jj, jx, ix, ii
      real :: q, w, f

      real(kind=REAL32) :: undef

      _UNUSED_DUMMY(rc)

      undef = -HUGE(1.)

      do j = 1, this%num_points_out(2)
         associate(weights_y => this%mappings(2)%WeightList(j)%f)
  
           do i = 1, this%num_points_out(1)
              
              associate(weights_x => this%mappings(1)%WeightList(i)%f)
                
                q = 0.0
                w = 0.0
            
                do jj = lbound(weights_y,1), ubound(weights_y,1)
                   if(jj > this%num_points_in(2)) then
                      jx = jj - this%num_points_in(2)
                   else
                      jx = jj
                   end if
                   
                   do ii = lbound(weights_x,1), ubound(weights_x,1)
                      if(ii>this%num_points_in(1)) then
                         ix = ii - this%num_points_in(1)
                      else
                         ix = ii
                      end if
                      
                      if(q_in(ix,jx) /= undef) then
                         f = weights_x(ii) *  weights_y(jj)
                         q = q + f*q_in(ix,jx)
                         w = w + f           
                      end if
                   end do
                end do
                
                if ( w >= WEIGHT_THRESHOLD ) then
                   q_out(i,j) = q / w
                else
                   q_out(i,j) = undef
                end if
                
              end associate
              
           end do
         end associate
      end do

   end subroutine apply_weights_real32


   subroutine apply_weights_real64(this, q_in, q_out, rc)
      class (LatLonToLatLonRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      integer :: i, j
      integer :: jj, jx, ix, ii
      real :: q, w, f

      real(kind=REAL64) :: undef

      _UNUSED_DUMMY(rc)

      undef = -HUGE(1.d0)

      do j = 1, this%num_points_out(2)

         associate(weights_y => this%mappings(2)%WeightList(j)%f)
  
           do i=1,this%num_points_out(1)
              
              associate(weights_x => this%mappings(1)%WeightList(i)%f)
                
                q = 0.0
                w = 0.0
            
                do jj = lbound(weights_y,1), ubound(weights_y,1)
                   if(jj > this%num_points_in(2)) then
                      jx = jj - this%num_points_in(2)
                   else
                      jx = jj
                   end if
                   
                   do ii = lbound(weights_x,1), ubound(weights_x,1)
                      if(ii>this%num_points_in(1)) then
                         ix = ii - this%num_points_in(1)
                      else
                         ix = ii
                      end if
                      
                      if(q_in(ix,jx) /= undef) then
                         f = weights_x(ii) *  weights_y(jj)
                         q = q + f*q_in(ix,jx)
                         w = w + f           
                      end if
                   end do
                end do
                
                if ( w >= WEIGHT_THRESHOLD ) then
                   q_out(i,j) = q / w
                else
                   q_out(i,j) = undef
                end if
                
              end associate
              
           end do
         end associate
      end do

   end subroutine apply_weights_real64


   ! Derived from getX() originally in MAPL_HorzTransform
   function get_coordinates(spec, input, stagger, rc) result(x)
      real, allocatable :: x(:)
      type (DimensionSpec), intent(in) :: spec
      logical, intent(in) :: input
      logical, optional, intent(in) :: stagger
      integer, optional, intent(out) :: rc

      logical :: stagger_
      integer :: j, jm
      real    :: dx

      _UNUSED_DUMMY(rc)


      if (present(stagger)) then
         stagger_ = stagger
      else
         stagger_ = .false.
      end if

      block
        integer n
        n = spec%num_points
        if (spec%topology == MAPL_DimTopoCyclic) then
           if (input) then
              jm = 2*n + 1
           else
              jm = n + 1
           end if
        else if (stagger_) then
           jm = n + 1
        else
           jm = n
        end if
      end block
      allocate(x(jm))

      if (spec%num_points > 1) then
         dx = (spec%x_max - spec%x_min) / (spec%num_points - 1)
      else
         dx = 0
      end if

      if (stagger_ ) then
         x(1)       = spec%x_min-0.5*dx
      else
         x(1)       = spec%x_min
      end if

      do j = 2, jm
         x(j) = x(1) + (j - 1)*dx
      end do
      
      if(spec%topology == MAPL_DimTopoEdge  ) then
         x(1) = spec%x_min
         x(jm) = spec%x_max
      end if

   end function get_coordinates


   real function get_range(spec) result(range)
      type (DimensionSpec), intent(in) :: spec

      range = (spec%x_max - spec%x_min) * spec%num_points / (spec%num_points - 1)

   end function get_range


   subroutine initialize_subclass(this, unusable, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_RegridderSpec
      use MAPL_BaseMod, only: MAPL_GridGet
      use MAPL_GetLatLonCoordMod
      use MAPL_ConstantsMod, only: MAPL_PI_R8
      class (LatLonToLatLonRegridder), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status      
      character(len=*), parameter :: Iam = 'initialize_subclass'
      type (RegridderSpec) :: spec

      logical :: cyclic_dim,hasPoles,stagger
      integer :: dim,nsize,nin
      type(Weights), pointer :: WeightList(:) => null()
      real(kind=REAL64), allocatable :: xg_in(:),xg_out(:)
      real(kind=REAL32), allocatable :: xf_in(:),xf_out(:)
      real(kind=REAL64) :: xMaxIn,xMaxOut,xMinIn,xMinOut,rngIn,rngOut
      type(dimensionSpec) :: dimspec
      character(len=ESMF_MAXSTR) :: grid_type

      _UNUSED_DUMMY(unusable)

      spec = this%get_spec()

      ! Verify that grids are of the support type: 'LatLon'
      call ESMF_AttributeGet(spec%grid_in , name="GridType", value=grid_type, rc=status)
      _VERIFY(status)
      _ASSERT(trim(grid_type) == 'LatLon', 'unsupported grid_in type: '//trim(grid_type))
        
      call ESMF_AttributeGet(spec%grid_out , name="GridType", value=grid_type, rc=status)
      _VERIFY(status)
      _ASSERT(trim(grid_type) == 'LatLon', 'unsupported grid_out type: '//trim(grid_type))
      
      call MAPL_GridGet(spec%grid_in, globalCellCountPerDim=this%num_points_in, rc=status)
      _VERIFY(status)

      call MAPL_GridGet(spec%grid_out, globalCellCountPerDim=this%num_points_out, rc=status)
      _VERIFY(status)

      do dim = 1, NUM_DIMS

         nin = this%num_points_in(dim)
         nsize = this%num_points_out(dim)
         allocate(this%mappings(dim)%weightlist(nsize), stat=status)
         _VERIFY(status)
         Weightlist => this%mappings(dim)%WeightList

         allocate(xg_out(nsize),stat=status)
         _VERIFY(status)
         allocate(xg_in(nin),stat=status)
         _VERIFY(status)
            
         call MAPL_GetLatLonCoord(spec%grid_in,dim,xg_in,rc=status)
         _VERIFY(status)
         call MAPL_GetLatLonCoord(spec%grid_out,dim,xg_out,rc=status)
         _VERIFY(status)
         xMaxIn=maxval(xg_in)
         xMaxOut=maxval(xg_out)
         xMinIn=minval(xg_in)
         xMinOut=minval(xg_out)
  
         stagger=.false. 
         cyclic_dim = (dim==1)
         hasPoles = (dim==2)
         dimspec%topology = MAPL_DimTopoEdge
         if (cyclic_dim) dimspec%topology = MAPL_DimTopoCyclic
         if (spec%regrid_method == REGRID_METHOD_BILINEAR) then
            stagger=.false.
         else if (spec%regrid_method == REGRID_METHOD_CONSERVE) then
            stagger=.true.
         end if

         dimspec%x_min=xMinIn
         dimspec%x_max=xMaxIn
         dimspec%num_points = this%num_points_in(dim)
         xf_in = get_coordinates(dimspec,.true.,stagger,rc=status)
         _VERIFY(status)
         dimspec%x_min=xMinOut
         dimspec%x_max=xMaxOut
         dimspec%num_points = this%num_points_out(dim)
         xf_out = get_coordinates(dimspec,.false.,stagger,rc=status)
         _VERIFY(status)

         if (cyclic_dim) then
            if (this%num_points_in(dim) > 1) then
               rngIn = ((xMaxIn  - xMinIn)*this%num_points_in(dim))/(this%num_points_in(dim)-1)
            else
               rngIn = 0
            end if
            if (this%num_points_out(dim) > 1) then
               rngOut = ((xMaxOut  - xMinOut)*this%num_points_out(dim))/(this%num_points_out(dim)-1)
            else
               rngOut = 0
            end if
!!$            _ASSERT(abs( (rngIn-rngOut)/rngIn ) < 1.e-5, 'range to small')
            if(xf_out(1) < xf_in(1)) then
               xf_out  = xf_out + int((xf_in(1)-xf_out(1))/rngIn+(MAPL_PI_R8/180.0d0))*rngIn
            else
               xf_out  = xf_out + int((xf_in(1)-xf_out(1))/rngIn)*rngIn
            end if
         end if
         _ASSERT(xf_in(size(xf_in)) >= xf_out(size(xf_out)), 'incorrect bracketing?')
         _ASSERT(xf_in(1) <= xf_out(1),'incorrect bracketing?')
         select case (spec%regrid_method)
         case (REGRID_METHOD_BILINEAR)
            call compute_linear_weights(this%mappings(dim)%WeightList, xf_in, xf_out, rc=status)
            _VERIFY(status)
         case (REGRID_METHOD_CONSERVE)
            call compute_binning_weights(this%mappings(dim)%WeightList,xf_in,xf_out,hasPoles,rc=status)
            _VERIFY(status)
         case default
            _FAIL('unsupported regrid method')
         end select
         deallocate(xg_in,xg_out,xf_in,xf_out)
         
      end do

      _RETURN(_SUCCESS)

   end subroutine initialize_subclass
   
end module MAPL_LatLonToLatLonRegridderMod

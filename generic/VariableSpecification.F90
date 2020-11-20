module mapl_VariableSpecification
   use ESMF
   implicit none
   private

   public :: MAPL_VarSpec
   public :: MAPL_VarSpecType
   public :: MAPL_VarSpecPtr
   public :: MAPL_VarConnPoint
   public :: MAPL_VarConnType
   public :: MAPL_VarConn

   type :: MAPL_VarSpec
!!$      private
      type(MAPL_VarSpecType), pointer :: SpecPtr => null()
   end type MAPL_VarSpec

   type :: MAPL_VarSpecPtr
      type(MAPL_VarSpec), pointer :: Spec(:) => null()
   end type MAPL_VarSpecPtr

   type :: MAPL_VarSpecType
      character(len=ESMF_MAXSTR)               :: SHORT_NAME
      character(len=ESMF_MAXSTR)               :: LONG_NAME
      character(len=ESMF_MAXSTR)               :: UNITS
      character(len=ESMF_MAXSTR)               :: FRIENDLYTO
      character(len=ESMF_MAXSTR)               :: VECTOR_PAIR
      character(len=ESMF_MAXSTR), pointer      :: ATTR_INAMES(:) => null()
      character(len=ESMF_MAXSTR), pointer      :: ATTR_RNAMES(:) => null()
      integer,                    pointer      :: ATTR_IVALUES(:) => null()
      real,                       pointer      :: ATTR_RVALUES(:) => null()
      integer,                    pointer      :: UNGRIDDED_DIMS(:) => null()
      character(len=ESMF_MAXSTR)               :: UNGRIDDED_UNIT
      character(len=ESMF_MAXSTR)               :: UNGRIDDED_NAME
      real,                       pointer      :: UNGRIDDED_COORDS(:)
      integer                                  :: DIMS
      integer                                  :: LOCATION
      integer                                  :: NUM_SUBTILES
      integer                                  :: STAT
      integer                                  :: ACCMLT_INTERVAL
      integer                                  :: COUPLE_INTERVAL
      integer                                  :: OFFSET
      integer                                  :: LABEL
      integer                                  :: HALOWIDTH
      integer                                  :: PRECISION
      integer                                  :: FIELD_TYPE
      integer                                  :: VECTOR_ORDER
      integer                                  :: STAGGERING
      integer                                  :: ROTATION
      integer                                  :: RESTART
      logical                                  :: defaultProvided
      logical                                  :: doNotAllocate
      logical                                  :: alwaysAllocate ! meant for export specs
      real                                     :: DEFAULT
      type(ESMF_Field), pointer                :: FIELD => null()
      type(ESMF_FieldBundle), pointer          :: BUNDLE => null()
      type(ESMF_State), pointer                :: STATE => null()
      type(ESMF_Grid)                          :: GRID
   end type MAPL_VarSpecType

   type MAPL_VarConnPoint
!!$      private
      character(len=ESMF_MAXSTR)               :: SHORT_NAME
      integer                                  :: IMPORT
      integer                                  :: EXPORT
   end type MAPL_VarConnPoint

   type MAPL_VarConnType
!!$      private
      type (MAPL_VarConnPoint)                 :: FROM
      type (MAPL_VarConnPoint)                 :: TO
      logical                                  :: used = .false.
      logical                                  :: notRequired = .false.
   end type MAPL_VarConnType

   type :: MAPL_VarConn
!!$      private
      type(MAPL_VarConnType), pointer :: ConnPtr => null()
   end type MAPL_VarConn

end module mapl_VariableSpecification

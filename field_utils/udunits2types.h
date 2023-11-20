!=================================== TYPES =====================================

!=================== TYPE: UT_UNIT - type to wrap C union ut_unit ==============
   type :: ut_unit
       type(c_ptr) :: ptr
   end type ut_unit

!================================ END UT_UNIT ==================================

!============== TYPE: CV_CONVERTER - type to wrap C union cv_converter =========
   type :: cv_converter
      type(c_ptr) :: ptr
   end type cv_converter

!============================== END CV_CONVERTER ===============================

!================================= TYPE: UT_SYSTEM =============================
! unit system
   type, bind(c, name='ut_system') :: ut_system 
      type(ut_unit)  :: second
      type(ut_unit)  :: one
      integer(UnitType) :: basicUnits(:)
      type(c_int), value :: basicCount
   end type ut_system
!=============================== END UT_SYSTEM =================================

!================================== TYPE: UNITOPTS =============================
! unit operations
   type, bind(c, name='UnitOps') :: UnitOps
      type(c_funptr) :: getProduct ! ProductUnit* :: (const ut_unit*)
      type(c_funptr) :: clone ! ut_unit* :: (ut_unit*)
      type(c_funptr) :: free ! void :: (ut_unit*)
      type(c_funptr) :: compare ! int :: (ut_unit*, ut_unit*)
      type(c_funptr) :: multiply ! ut_unit* :: (const ut_unit*, const ut_unit*)
      type(c_funptr) :: raise ! ut_unit* :: (const ut_unit*, const int power)
      type(c_funptr) :: root ! ut_unit* :: (const ut_unit*, const int root)
      type(c_funptr) :: initConverterToProduct ! int :: (ut_unit*)
      type(c_funptr) :: initConverterFromProduct ! int :: (ut_unit*)
      type(c_funptr) :: acceptVisitor ! ut_status :: (const ut_unit*, const ut_visitor*, void*)
   end type UnitOps
!================================ END UNITOPS ==================================

!================================== TYPE: COMMON_ ==============================
! COMMON_ is used instead of COMMON to avoid collision with Fortran "common"
   type, bind(c, name='Common') :: Common_
       type(ut_system)  :: system
       type(UnitOps)   :: ops
       integer(UnitType), value   :: type_ ! type_ is used to avoid collision
       type(cv_converter)   :: toProduct
       type(cv_converter)   :: fromProduct
   end type Common_
!================================ END COMMAND_ =================================

!============================== TYPE: BASICUNIT ================================
! common__ is used to avoid collision with derived type Command_
   type, bind(c, name='BasicUnit') :: BasicUnit
       type(Common_), value :: common__
       type(ProductUnit)    :: product_
       type(c_int), value   :: index_
       type(c_int), value   :: isDimensionless
   end type BasicUnit
!=============================== END BASICUNIT =================================

!============================= TYPE: PRODUCTUNIT ===============================
! common__ is used to avoid collision with derived type Command_
   type, bind(c, name='ProductUnit') :: ProductUnit
       type(Common_), value :: common__
       type(c_short), value   :: indexes(:)
       type(c_short), value   :: powers(:)
       type(c_int), value   :: count_
   end type ProductUnit
!============================== END PRODUCTUNIT ================================
   
!============================= TYPE: GALILEANUNIT ==============================
! common__ is used to avoid collision with derived type Command_
   type, bind(c, name='GalileanUnit') :: GalileanUnit
       type(Common_), value  :: common__
       type(ut_unit)   :: unit_
       type(c_double), value :: scale_
       type(c_double), value :: offset_
   end type GalileanUnit
!============================= END GALILEANUNIT ================================

!============================ TYPE: TIMESTAMPUNIT ==============================
! common__ is used to avoid collision with derived type Command_
   type, bind(c, name='TimestampUnit') :: TimestampUnit
       type(Common_), value   :: common__
       type(ut_unit)   :: unit_
       type(c_double), value   :: origin
   end type TimestampUnit
!============================= END TIMESTAMPUNIT ===============================
    
!=============================== TYPE: LOGUNIT =================================
! common__ is used to avoid collision with derived type Command_
   type, bind(c, name='LogUnit') :: LogUnit
       type(Common_), value :: common__
       type(ut_unit*)   :: reference
       type(c_double)   :: base
   end type LogUnit
!================================ END LOGUNIT ==================================

!================================= END TYPES ===================================
! vim: filetype=fortran

module mapl3g_GeomIO_API

   ! Procedures get MAPL_ prefix via rename
   use mapl3g_GeomCatagorizer, only: MAPL_make_geom_pfio => make_geom_pfio
   use mapl3g_SharedIO, only: MAPL_bundle_to_metadata        => bundle_to_metadata
   use mapl3g_SharedIO, only: MAPL_add_variables             => add_variables
   use mapl3g_SharedIO, only: MAPL_add_variable              => add_variable
   use mapl3g_SharedIO, only: MAPL_create_time_variable      => create_time_variable
   use mapl3g_SharedIO, only: MAPL_esmf_to_pfio_type         => esmf_to_pfio_type
   use mapl3g_SharedIO, only: MAPL_add_vertical_dimensions   => add_vertical_dimensions
   use mapl3g_SharedIO, only: MAPL_get_vertical_dimension_name_from_field => get_vertical_dimension_name_from_field
   use mapl3g_SharedIO, only: MAPL_add_ungridded_dimensions  => add_ungridded_dimensions
   use mapl3g_SharedIO, only: MAPL_ungridded_dim_names       => ungridded_dim_names
   use mapl3g_DataCollectionManager, only: MAPL_AddDataCollection => AddDataCollection
   use mapl3g_FieldBundleRead, only: MAPL_FieldBundleRead    => FieldBundleRead
   use mapl3g_FieldBundleRead, only: MAPL_FieldBundlePopulate => FieldBundlePopulate
   use mapl3g_FieldBundleWrite, only: MAPL_FieldBundleWrite  => FieldBundleWrite

   ! Types, constants, and module-level variables re-exported without renaming
   use mapl3g_GeomPFIO
   use mapl3g_DataCollection
   use mapl3g_DataCollectionVector
   use mapl3g_DataCollectionManager, only: DataCollections
   use mapl3g_pFIOServerBounds

   implicit none

   private

   public :: MAPL_make_geom_pfio
   public :: MAPL_bundle_to_metadata
   public :: MAPL_add_variables
   public :: MAPL_add_variable
   public :: MAPL_create_time_variable
   public :: MAPL_esmf_to_pfio_type
   public :: MAPL_add_vertical_dimensions
   public :: MAPL_get_vertical_dimension_name_from_field
   public :: MAPL_add_ungridded_dimensions
   public :: MAPL_ungridded_dim_names
   public :: MAPL_AddDataCollection
   public :: MAPL_FieldBundleRead
   public :: MAPL_FieldBundlePopulate
   public :: MAPL_FieldBundleWrite

   ! Types and constants (no MAPL_ prefix, following field/API.F90 pattern)
   public :: GeomPFIO
   public :: DataCollection
   public :: new_DataCollection
   public :: DataCollectionVector
   public :: DataCollectionVectorIterator
   public :: DataCollections
   public :: pFIOServerBounds
   public :: PFIO_BOUNDS_READ
   public :: PFIO_BOUNDS_WRITE

end module mapl3g_GeomIO_API

module mapl_geomio_api

   use mapl_CompressionSettings_mod, only: mapl_CompressionSettings => CompressionSettings
   use mapl_SharedIO_mod, only: mapl_bundle_to_metadata => bundle_to_metadata
   use mapl_SharedIO_mod, only: mapl_esmf_to_pfio_type => esmf_to_pfio_type
   use mapl_GeomPFIO_mod, only: mapl_GeomPFIO => GeomPFIO
   use mapl_GeomCategorizer_mod, only: mapl_make_geom_pfio => make_geom_pfio
   use mapl_pFIOServerBounds_mod, only: mapl_pFIOServerBounds => pFIOServerBounds
   use mapl_pFIOServerBounds_mod, only: mapl_PFIO_BOUNDS_READ => PFIO_BOUNDS_READ
   use mapl_DataCollection_mod, only: mapl_DataCollection => DataCollection
   use mapl_DataCollectionManager_mod, only: MAPL_AddDataCollection => AddDataCollection
   use mapl_DataCollectionManager_mod, only: mapl_DataCollections => DataCollections

   use mapl_FieldBundleWrite_mod, only: mapl_FieldBundleWriter => FieldBundleWriter
   use mapl_FieldBundleRead_mod, only: mapl_read_bundle => read_bundle
   implicit none
   private


   public :: mapl_CompressionSettings
   public :: mapl_GeomPFIO
   public :: mapl_pFIOServerBounds
   public :: MAPL_PFIO_BOUNDS_READ
   public :: mapl_DataCollection
   public :: mapl_DataCollections
   public :: mapl_AddDataCollection
   public :: mapl_FieldBundleWriter

   public :: mapl_make_geom_pfio
   public :: mapl_bundle_to_metadata
   public :: mapl_esmf_to_pfio_type
   public :: mapl_read_bundle
   
end module mapl_geomio_api

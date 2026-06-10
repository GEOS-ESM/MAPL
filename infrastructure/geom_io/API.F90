module mapl_geomio_api

   use mapl_CompressionSettings_mod, only: CompressionSettings
   use mapl_SharedIO_mod, only: bundle_to_metadata, esmf_to_pfio_type
   use mapl_GeomPFIO_mod, only: GeomPFIO
   use mapl_GeomCategorizer_mod, only: make_geom_pfio
   use mapl_pFIOServerBounds_mod, only: pFIOServerBounds, PFIO_BOUNDS_READ
   use mapl_DataCollection_mod, only: DataCollection
   use mapl_DataCollectionManager_mod, only: MAPL_AddDataCollection => AddDataCollection
   use mapl_DataCollectionManager_mod, only: DataCollections

   use mapl_GeomPFIO_mod
   use mapl_GeomCategorizer_mod
   use mapl_DataCollectionVector_mod
   use mapl_FieldBundleWrite_mod, only: FieldBundleWriter
   use mapl_FieldBundleWrite_mod, only: write_bundle
   use mapl_FieldBundleRead_mod, only: mapl_read_bundle => read_bundle
   use mapl_FieldBundleRead_mod, only: FieldBundlePopulate
   implicit none
   private


   public :: CompressionSettings
   public :: GeomPFIO
   public :: pFIOServerBounds
   public :: PFIO_BOUNDS_READ
   public :: DataCollection
   public :: DataCollections
   public :: mapl_AddDataCollection
   public :: FieldBundleWriter

   public :: make_geom_pfio
   public :: bundle_to_metadata
   public :: esmf_to_pfio_type
   public :: mapl_read_bundle
   
end module mapl_geomio_api

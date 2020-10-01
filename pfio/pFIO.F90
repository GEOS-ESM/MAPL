module pFIO
   use pFIO_ConstantsMod
   use pFIO_UnlimitedEntityMod
   use pFIO_AttributeMod
   use pFIO_VariableMod
   use pFIO_CoordinateVariableMod
   use pFIO_FileMetadataMod
   use pFIO_NetCDF4_FileFormatterMod
   use pFIO_AbstractDirectoryServiceMod
   use pFIO_DirectoryServiceMod
   use pFIO_AbstractServerMod
   use pFIO_BaseServerMod
   use pFIO_MpiServerMod
   use pFIO_MultiLayerServerMod
   use pFIO_MultiCommServerMod
   use pFIO_MultiGroupServerMod
   use pFIO_UtilitiesMod
!!$   use pFIO_OpenMPServerMod
   use pFIO_ServerThreadMod
   use pFIO_ClientThreadMod
   use pFIO_ClientThreadVectorMod
   use pFIO_ClientManagerMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractSocketVectorMod
   use pFIO_MpiSocketMod
   use pFIO_SimpleSocketMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_ArrayReferenceMod
   use pFIO_CoordinateVariableMod
   use pFIO_AttributeMod
   use pFIO_StringAttributeMapMod
   use pFIO_StringVariableMapMod
   use pFIO_DownBitMod
   use pFIO_LocalMemReferenceMod
   use pFIO_FormatterPtrVectorMod

   integer, save :: debug_unit = 0

end module pFIO

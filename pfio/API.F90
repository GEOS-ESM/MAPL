module mapl_pfio_api
  use pfio, only: FileMetadata
  use pfio, only: i_clients
  use pfio, only: o_clients
  use pfio, only: NetCDF4_FileFormatter
  use pfio, only: pfio_read
  use pfio, only: ArrayReference
  use pfio, only: string_in_stringvector

  use pfio, only: StringVariableMap
  use pfio, only: StringVariableMapIterator
  use pfio, only: operator(==), operator(/=)
end module mapl_pfio_api

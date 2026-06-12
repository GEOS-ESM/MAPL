module mapl_pfio_api
  use pfio, only: mapl_FileMetadata => FileMetadata
  use pfio, only: mapl_Variable => Variable
  use pfio, only: mapl_StringVariableMap => StringVariableMap
  use pfio, only: mapl_StringVariableMapIterator => StringVariableMapIterator
  use pfio, only: mapl_NetCDF4_FileFormatter => NetCDF4_FileFormatter
  use pfio, only: mapl_ArrayReference => ArrayReference
  use pfio, only: operator(==), operator(/=)

  use pfio, only: mapl_i_clients => i_clients
  use pfio, only: mapl_o_clients => o_clients
  use pfio, only: mapl_pfio_read => pfio_read
  use pfio, only: mapl_string_in_stringvector => string_in_stringvector

  implicit none
  private

  public :: mapl_FileMetadata
  public :: MAPL_Variable
  public :: mapl_StringVariableMap
  public :: mapl_StringVariableMapIterator
  public :: mapl_NetCDF4_FileFormatter

  public :: mapl_i_clients
  public :: mapl_o_clients
  public :: mapl_pfio_read
  public :: mapl_ArrayReference
  public :: mapl_string_in_stringvector

  public :: operator(==), operator(/=)

end module mapl_pfio_api

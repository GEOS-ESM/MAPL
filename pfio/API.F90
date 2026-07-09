module mapl_pfio_api
  use pfio, only: mapl_FileMetadata => FileMetadata
  use pfio, only: mapl_Variable => Variable
  use pfio, only: mapl_StringVariableMap => StringVariableMap
  use pfio, only: mapl_StringVariableMapIterator => StringVariableMapIterator
  use pfio, only: mapl_NetCDF4_FileFormatter => NetCDF4_FileFormatter
  use pfio, only: mapl_ArrayReference => ArrayReference
  use mapl_DefaultServerNames_mod, only: MAPL_DEFAULT_INPUT_SERVER, MAPL_DEFAULT_OUTPUT_SERVER
  use pfio, only: operator(==), operator(/=)

  use pfio, only: mapl_get_client => get_client
  use pfio, only: mapl_add_client => add_client
  use pfio, only: ClientThread
  use pfio, only: mapl_pfio_read => pfio_read
  use pfio, only: mapl_string_in_stringvector => string_in_stringvector

  implicit none
  private

  public :: mapl_FileMetadata
  public :: MAPL_Variable
  public :: mapl_StringVariableMap
  public :: mapl_StringVariableMapIterator
  public :: mapl_NetCDF4_FileFormatter
  public :: MAPL_DEFAULT_INPUT_SERVER
  public :: MAPL_DEFAULT_OUTPUT_SERVER

  public :: mapl_get_client
  public :: ClientThread
  public :: mapl_pfio_read
  public :: mapl_ArrayReference
  public :: mapl_string_in_stringvector

  public :: operator(==), operator(/=)

end module mapl_pfio_api

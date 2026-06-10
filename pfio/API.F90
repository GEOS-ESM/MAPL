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

  implicit none
  private

  public :: FileMetadata
  public :: i_clients
  public :: o_clients
  public :: NetCDF4_FileFormatter
  public :: pfio_read
  public :: ArrayReference
  public :: string_in_stringvector

  public :: StringVariableMap
  public :: StringVariableMapIterator
  public :: operator(==), operator(/=)

end module mapl_pfio_api

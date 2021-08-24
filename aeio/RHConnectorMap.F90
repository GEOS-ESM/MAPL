module AEIO_RHConnectorMap
   use AEIO_RHConnector

#include "types/key_deferredLengthString.inc"
#define _value class(RHConnector)
#define _value_allocatable class(RHConnector)

#define _map RHConnectorMap
#define _iterator RHConnectorMapIterator
#define _pair RHConnectorPair
#define _alt
#include "templates/map.inc"
end module AEIO_RHConnectorMap

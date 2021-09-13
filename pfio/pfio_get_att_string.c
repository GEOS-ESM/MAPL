#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <netcdf.h>

void pfio_check(int stat) {
  if (stat != NC_NOERR) {
    printf("NetCDF error: %s\n", nc_strerror(stat));
    exit(1);
  }
}

int pfio_get_att_string(int ncid, int varid, const char* name, char** value, size_t* attlen)
{
  int stat;
 
  *attlen = 0;
  stat = nc_inq_attlen(ncid, varid, "attribute", attlen); pfio_check(stat);
 
  char **string_attr = (char**)malloc(*attlen * sizeof(char*));
  memset(string_attr, 0, *attlen * sizeof(char*));

  stat = nc_get_att_string(ncid, varid, name, value); pfio_check(stat);

  return stat;
}

void pfio_free_string(char** string_attr)
{
  free(string_attr);
}

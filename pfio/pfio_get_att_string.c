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

int pfio_get_att_string(int ncid, const char* name, char* value, int *attlen)
{
  int stat;
  size_t alen;

  stat = nc_inq_attlen(ncid, NC_GLOBAL, name, &alen); pfio_check(stat);
 
  if (alen > 1) {
   printf("pfio doesnot support multi-dimentional strings");
   exit(-1);
  }

  char **string_attr = (char**)malloc( sizeof(char*));
  memset(string_attr, 0, sizeof(char*));

  stat = nc_get_att_string(ncid, NC_GLOBAL, name, string_attr); pfio_check(stat);

  *attlen = 0;
  alen = 0;
  char *p = string_attr[0];
  for(;;){
    if (alen >=511) {
       printf("pfio doesnot support string longer than 512");
       exit(-1);
    }
    *(value+alen) = (*(p+alen));
    if (*(p + alen) == '\0'){
       break;
    }
    alen = alen + 1;
  }
  *attlen = alen;
  stat = nc_free_string(1, string_attr); pfio_check(stat);
  free(string_attr);
  return stat;
}

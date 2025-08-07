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

int pfio_get_att_string(int ncid, int varid, const char* name, char* value, int *attlen)
{
  int stat;
  size_t alen;

  /* note: C-varid starts from 0, Fortran from 1 */
  int varid_C = varid - 1;

  stat = nc_inq_attlen(ncid, varid_C, name, &alen); pfio_check(stat);
 
  if (alen > 1) {
   printf("pfio doesnot support multi-dimentional strings");
   exit(-1);
  }

  char **string_attr = (char**)malloc( sizeof(char*));
  memset(string_attr, 0, sizeof(char*));

  stat = nc_get_att_string(ncid, varid_C, name, string_attr); pfio_check(stat);

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

// 
int pfio_get_var_string_len(int ncid, int varid, int *str_len, int str_size)
{
  int stat;
  
  // note: C-varid starts from 0, Fortran from 1 
  int varid_C = varid - 1;
  char *p ;
  int i, j;
  if (str_size == 0) str_size = 1; // for 0d string length
  char *string[str_size];
  stat = nc_get_var(ncid, varid_C, string ); pfio_check(stat);
  *str_len = 0;
  for (i = 0; i<str_size; i++){
    p = string[i];
    j = 0;
    for (;;){ 
       if (*(p+j) == '\0') break;
       j++;
    }
    if ( *str_len < j) *str_len = j;
  }

  stat = nc_free_string(str_size, string); pfio_check(stat);
  return stat;
}

int pfio_get_var_string(int ncid, int varid, char* value, int max_len, const size_t *start, const size_t *count)
{
  int stat;

  // note: C-varid starts from 0, Fortran from 1 
  int varid_C = varid - 1;

  int S0 = start[0];
  int str_size = count[0];
  unsigned long start_C = S0-1;
  unsigned long count_C = str_size;
  char *string[str_size];

  stat = nc_get_vara_string(ncid, varid_C, &start_C, &count_C, string ); pfio_check(stat);

  char *p;
  // re-arrange string
  int alen = 0;
  int j;
  for (int i=0; i<str_size; i++){
    p = string[i];
    j = 0;
    for (;;){
       if (*(p+j) == '\0'){
          for (; j<max_len; j++){
            *(value+alen) = ' ';
            alen++;
          }
          break;
       }
       *(value+alen) = *(p+j);
       alen ++;
       if (j == max_len-1 ) break;
       j++;
    }
  }

  stat = nc_free_string(str_size, string); pfio_check(stat);
  return stat;
}

int pfio_put_vara_string(int ncid, int varid, char* value, int max_len, int str_size, const size_t *start, const size_t *count)
{
  int stat;

  // note: C-varid starts from 0, Fortran from 1 
  int varid_C = varid - 1;

  char *string_in[str_size];
  // re-arrange string
  for (int i=0; i<str_size; i++){
    string_in[i] = value + i*max_len;
  }

  int S0 = start[0];
  int C0 = count[0];
  unsigned long start_C = S0-1;
  unsigned long count_C = C0; 
  stat = nc_put_vara_string(ncid, varid_C, &start_C, &count_C, (const char **) string_in ); pfio_check(stat);
  //stat = nc_free_string(str_size, string_in); pfio_check(stat);
  return stat;
}


int pfio_put_var_string(int ncid, int varid, char* value, int len_ )
{
  int stat;

  // note: C-varid starts from 0, Fortran from 1 
  int varid_C = varid - 1;

  char *string_in[1];
  // re-arrange string
  for (int i=0; i<1; i++){
    string_in[i] = value + i*len_;
  }

  stat = nc_put_var_string(ncid, varid_C,  (const char **) string_in ); pfio_check(stat);
  //stat = nc_free_string(str_size, string_in); pfio_check(stat);
  return stat;
}

#include <stdio.h>
#include <string.h>
#include <glob.h>

int glob_C (char*, char*, int*);

int glob_C (char *pattern, char *filename, int *stringlen)
{
  glob_t  globlist;
  int error = 1;
  int failure = -1;
  char *s;
  int MAXLEN = 512;  // set path length limit

  int j = glob( pattern, GLOB_ERR, NULL, &globlist );
  if ( j == GLOB_NOSPACE || j == GLOB_NOMATCH )
    return (failure);
  if ( j == GLOB_ABORTED)
    return (error);

  int i = 0;
  for (; globlist.gl_pathv[i] ; i++)
    // printf("f = %s\n", globlist.gl_pathv[i]);
    ;
  s = globlist.gl_pathv[--i];
  for (i=0; *(s+i) != '\0'; i++)
    *(filename+i) = *(s+i);
  *stringlen = i;

  if ( i > MAXLEN ) return error;
  return 0;
}

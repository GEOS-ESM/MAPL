#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// A quicksort implementation for intergers with Fortran bindings.

// It sorts a list of either int's (integer*4) or long long's (integer*8),
// and optionally, uses this sort as a key to reorder either one or two
// 2-D arrays of ints.

// The reordered dimension of the 2-D array can be either the inner or
// outer dimension, which must have the same size as the key's single
// dimension. 


// Utility functions to swap and shuffle arrays;
// These are needed for long and short types
//==============================================

#define SWAP(A,B) {s=B; B=A; A=s;}

#define IS_LONG
#include "swap_shuffle.c"
#undef  IS_LONG
#include "swap_shuffle.c"


// Overloads of routines that sort a list integers and
//  and reorder up to two 2-D arrays. 
//====================================================

// Overloads for long long (Integer*8) key

#define LENA 4

  #define LENB 4

    #define LENC 4
    #include "quicksort.c"
    #undef  LENC
    #define LENC 8
    #include "quicksort.c"
    #undef  LENC

  #undef  LENB
  #define LENB 8

    #define LENC 4
    #include "quicksort.c"
    #undef  LENC
    #define LENC 8
    #include "quicksort.c"
    #undef  LENC

  #undef  LENB

#undef LENA

// Overloads for long long (Integer*8) key

#define LENA 8

  #define LENB 4

    #define LENC 4
    #include "quicksort.c"
    #undef  LENC
    #define LENC 8
    #include "quicksort.c"
    #undef  LENC

  #undef  LENB
  #define LENB 8

    #define LENC 4
    #include "quicksort.c"
    #undef  LENC
    #define LENC 8
    #include "quicksort.c"
    #undef  LENC

  #undef  LENB

#undef  LENA




// Fortran interfaces


// Extra aliases for other loaders
// The Lxx and Sxx overloads refer to the kind of a, b, and c, respectively
// r is the length of the sorted list
// n is the number of reordered lists in b
// q is the number of reordered lists in c

void qsortl44 (long long a[],       int b[],       int c[], int *r, int *n, int *q) { (void)QSORTL44(a,b,c,r,n,q); }
void qsortl48 (long long a[],       int b[], long long c[], int *r, int *n, int *q) { (void)QSORTL48(a,b,c,r,n,q); }
void qsortl84 (long long a[], long long b[],       int c[], int *r, int *n, int *q) { (void)QSORTL84(a,b,c,r,n,q); }
void qsortl88 (long long a[], long long b[], long long c[], int *r, int *n, int *q) { (void)QSORTL88(a,b,c,r,n,q); }

void qsorts44 (      int a[],       int b[],       int c[], int *r, int *n, int *q) { (void)QSORTS44(a,b,c,r,n,q); }
void qsorts48 (      int a[],       int b[], long long c[], int *r, int *n, int *q) { (void)QSORTS48(a,b,c,r,n,q); }
void qsorts84 (      int a[], long long b[],       int c[], int *r, int *n, int *q) { (void)QSORTS84(a,b,c,r,n,q); }
void qsorts88 (      int a[], long long b[], long long c[], int *r, int *n, int *q) { (void)QSORTS88(a,b,c,r,n,q); }

void qsortl44_(long long a[],       int b[],       int c[], int *r, int *n, int *q) { (void)QSORTL44(a,b,c,r,n,q); }
void qsortl48_(long long a[],       int b[], long long c[], int *r, int *n, int *q) { (void)QSORTL48(a,b,c,r,n,q); }
void qsortl84_(long long a[], long long b[],       int c[], int *r, int *n, int *q) { (void)QSORTL84(a,b,c,r,n,q); }
void qsortl88_(long long a[], long long b[], long long c[], int *r, int *n, int *q) { (void)QSORTL88(a,b,c,r,n,q); }

void qsorts44_(      int a[],       int b[],       int c[], int *r, int *n, int *q) { (void)QSORTS44(a,b,c,r,n,q); }
void qsorts48_(      int a[],       int b[], long long c[], int *r, int *n, int *q) { (void)QSORTS48(a,b,c,r,n,q); }
void qsorts84_(      int a[], long long b[],       int c[], int *r, int *n, int *q) { (void)QSORTS84(a,b,c,r,n,q); }
void qsorts88_(      int a[], long long b[], long long c[], int *r, int *n, int *q) { (void)QSORTS88(a,b,c,r,n,q); }

void QSORTL44_(long long a[],       int b[],       int c[], int *r, int *n, int *q) { (void)QSORTL44(a,b,c,r,n,q); }
void QSORTL48_(long long a[],       int b[], long long c[], int *r, int *n, int *q) { (void)QSORTL48(a,b,c,r,n,q); }
void QSORTL84_(long long a[], long long b[],       int c[], int *r, int *n, int *q) { (void)QSORTL84(a,b,c,r,n,q); }
void QSORTL88_(long long a[], long long b[], long long c[], int *r, int *n, int *q) { (void)QSORTL88(a,b,c,r,n,q); }

void QSORTS44_(      int a[],       int b[],       int c[], int *r, int *n, int *q) { (void)QSORTS44(a,b,c,r,n,q); }
void QSORTS48_(      int a[],       int b[], long long c[], int *r, int *n, int *q) { (void)QSORTS48(a,b,c,r,n,q); }
void QSORTS84_(      int a[], long long b[],       int c[], int *r, int *n, int *q) { (void)QSORTS84(a,b,c,r,n,q); }
void QSORTS88_(      int a[], long long b[], long long c[], int *r, int *n, int *q) { (void)QSORTS88(a,b,c,r,n,q); }

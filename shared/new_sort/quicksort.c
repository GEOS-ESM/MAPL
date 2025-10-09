

// This function swaps i-j items of the key and of any ancillary data contained
// in the b array. n is the number of columns in b and its sign
// determines which dimension holds the columns.


#if LENA==4
  #define INTa     int 
  #define SHUFFLEA Shuffle4
  #if LENB==4
    #define INTb     int 
    #define SHUFFLEB  Shuffle4
    #define QSSWAPB   QSswapArr4
    #if LENC==4
      #define INTc      int 
      #define SHUFFLEC  Shuffle4
      #define QSSWAPC   QSswapArr4
      #define QSWAP     QswapS44
      #define QUICKSORT QuickSortS44
      #define QSORT     QSORTS44
    #else
      #define INTc      long long
      #define SHUFFLEC  Shuffle8
      #define QSSWAPC   QSswapArr8
      #define QSWAP     QswapS48
      #define QUICKSORT QuickSortS48
      #define QSORT     QSORTS48
    #endif
  #else
    #define INTb      long long 
    #define SHUFFLEB  Shuffle8
    #define QSSWAPB   QSswapArr8
    #if LENC==4
      #define INTc      int 
      #define SHUFFLEC  Shuffle4
      #define QSSWAPC   QSswapArr4
      #define QSWAP     QswapS84
      #define QUICKSORT QuickSortS84
      #define QSORT     QSORTS84
    #else
      #define INTc      long long
      #define SHUFFLEC  Shuffle8
      #define QSSWAPC   QSswapArr8
      #define QSWAP     QswapS88
      #define QUICKSORT QuickSortS88
      #define QSORT     QSORTS88
    #endif
  #endif
#else
  #define INTa     long long
  #define SHUFFLEA Shuffle8
  #if LENB==4
    #define INTb      int 
    #define SHUFFLEB  Shuffle4
    #define QSSWAPB   QSswapArr4
    #if LENC==4
      #define INTc      int
      #define SHUFFLEC  Shuffle4
      #define QSSWAPC   QSswapArr4
      #define QSWAP     QswapL44
      #define QUICKSORT QuickSortL44
      #define QSORT     QSORTL44
    #else
      #define INTc      long long
      #define SHUFFLEC  Shuffle8
      #define QSSWAPC   QSswapArr8
      #define QSWAP     QswapL48
      #define QUICKSORT QuickSortL48
      #define QSORT     QSORTL48
    #endif
  #else
    #define INTb      long long
    #define SHUFFLEB  Shuffle8
    #define QSSWAPB   QSswapArr8
    #if LENC==4
      #define INTc      int
      #define SHUFFLEC  Shuffle4
      #define QSSWAPC   QSswapArr4
      #define QSWAP     QswapL84
      #define QUICKSORT QuickSortL84
      #define QSORT     QSORTL84
    #else
      #define INTc      long long
      #define SHUFFLEC  Shuffle8
      #define QSSWAPC   QSswapArr8
      #define QSWAP     QswapL88
      #define QUICKSORT QuickSortL88
      #define QSORT     QSORTL88
    #endif
  #endif
#endif

void QSWAP(INTa a[], INTb b[], INTc c[], 
	   int i, int j, int m, int n, int q)
{
   if(i!=j) {
     INTa s;

     SWAP(a[i],a[j]);

     if(b && n!=0) (void)QSSWAPB(b,i,j,m,n);
     if(c && q!=0) (void)QSSWAPC(c,i,j,m,q);
   }
}




void QUICKSORT(INTa a[], INTb b[], INTc c[], 
	       int l, int r, int m, int n, int q)
{
  int len=r-l+1;

  if (len<=1) {
    return;
  } 
  else if (len==2) {
    if (a[l]>a[r]) QSWAP(a,b,c,l,r,m,n,q);
    return;
  }
  else {
    int  j = r;
    int  i = l-1;
    INTa v = a[r];

    for(;;)  {
      while(a[++i]<v && i<r);
      while(a[--j]>v && j>l);
      if (j<=i) break;
      QSWAP(a,b,c,i  ,j,m,n,q);
    }
    QSWAP    (a,b,c,i  ,r,m,n,q);
    QUICKSORT(a,b,c,l  ,j,m,n,q);
    QUICKSORT(a,b,c,i+1,r,m,n,q);
  }
}


#define MAX_SORT_LEN 400000000



// If the length of the sort exceeds a critical value (MAX_SORT_LEN),
// it halves it repeatedly until it is below this value, does
// order LEN/MAX_SORT_LEN sorts, and shuffles these in ascending order.


void QSORT(INTa a[], INTb b[], INTc c[], int *r, int *n, int *q) {
    (void)QUICKSORT(a,b,c,0,*r-1,*r,*n,*q);
#if 0
  if(*r<MAX_SORT_LEN) {
    (void)QUICKSORT(a,b,c,0,*r-1,*r,*n,*q);
  }
  else {
    int lenm, lenp, i;
    INTa *ap, *am;
    INTb *bp;
    INTc *cp;

    lenm = *r/2;
    lenp = lenm + *r%2;

    ap   = a + lenm;
    bp   = (n<0) ? b+lenm : b+*n*lenm;
    cp   = (q<0) ? c+lenm : c+*q*lenm;

    (void)QSORT(a ,b ,c ,&lenm,n,q);
    (void)QSORT(ap,bp,cp,&lenp,n,q);

    (void)SHUFFLEA(a,lenm,lenp,1);

    for (i=0;i<abs(*n);i++) {
      if(*n<0) {
 	(void)SHUFFLEB(&b[*r*i],lenm,lenp,1);
      }
      else {
	(void)SHUFFLEB(&b[i],lenm,lenp,*n);
      }

      if(*q<0) {
 	(void)SHUFFLEC(&c[*r*i],lenm,lenp,1);
      }
      else {
	(void)SHUFFLEC(&c[i],lenm,lenp,*q);
      }
    }
  }
#endif
}


#undef INTa     
#undef INTb     
#undef INTc     
#undef SHUFFLEA 
#undef SHUFFLEB 
#undef SHUFFLEC 
#undef QSSWAPB  
#undef QSSWAPC
#undef QSWAP    
#undef QUICKSORT
#undef QSORT    

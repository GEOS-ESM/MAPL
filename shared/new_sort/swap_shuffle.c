
#ifdef IS_LONG
  #define INTt long long
  #define QSSWAPARR QSswapArr8
  #define SHUFFLE   Shuffle8
#endif

#ifndef  IS_LONG
  #define INTt int
  #define QSSWAPARR QSswapArr4
  #define SHUFFLE   Shuffle4
#endif




QSSWAPARR(INTt b[], int i, int j, int m, int n)
{
// This function swaps i-j items of a 2-D array of ints contained
// in b. n is the number of lists to be reordered and its sign
// determines which dimension of b spans the lists.

  INTt s;
  int k;
   
  if     (n> 1) { // reordering the outer Fortran dimension (inner C dimension)
    for(k=0;k< n;k++) SWAP(b[n*i+k],b[n*j+k]);
  } 
  else if(n<-1) { // reordering the inner Fortran dimension (outer C dimension)
    printf("In swap %d %d %d %d\n",i,j,m,n);
    for(k=0;k<-n;k++) SWAP(b[i+m*k],b[j+m*k]);
  }
  else          {
    SWAP(b[i],b[j]);
  }
}

void SHUFFLE(INTt *a, int lenm, int lenp, int stride)
{

  int r=lenm+lenp, i;
  INTt *aml, *apl, *am, *ap, *a0;

  a0  = (INTt *)malloc(lenm*sizeof(INTt));

  am  = a0;
  ap  = a  + lenm*stride;
 
  aml = am + lenm;
  apl = ap + lenp*stride;

  for(i=0;i<lenm;i++) am[i]=a[i*stride];

  while(r--){
    if(am<aml && ap<apl)
      if(*am<*ap)   *a=*am++;
      else         {*a=*ap;ap+=stride;}
    else if(am<aml) *a=*am++; 
    else           {*a=*ap;ap+=stride;}
    a+=stride;
  }

  free(a0);
} 


#undef INTt 
#undef QSSWAPARR
#undef SHUFFLE   

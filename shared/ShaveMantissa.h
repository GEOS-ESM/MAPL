/* Define float32/int32 as in HDF-4 for portability.  */

#if !defined(int32)
#    define  int32    int
#endif

#if !defined(uint32)
#    define  uint32   unsigned int
#endif

#if !defined(float32)
#    define  float32  float
#endif


 /* prototype */
 int MAPL_ShaveMantissa ( float32 a[], float32 ain[], int32 len, int xbits, 
                     int has_undef, float32 undef, int32 chunksize, int passed_minmax, float32 arr_min, float32 arrmax );


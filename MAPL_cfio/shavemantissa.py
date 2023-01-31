"""
  SImple python interface to the ShaveMantissa function
"""

MISSING = 1.0E15

from ShaveMantissa_ import shave32

def shave(a,xbits=12,has_undef=0,undef=MISSING,chunksize=-1):
    """
    Shaves bits from mantissa of float point array for better netCDF4 compression
    using gzip.

    a_shaved = shave(a,...)

    xbits      ---  number of bits to shave
    has_undef  ---  set to 1 if undefs are present
    undef      ---  undef value
    chunksize  ---  for scaling of array to be shaved: find mid-range value
                    over chunksizes. If negative, set to len(a)

    Typically this function is used for a single vertical slice at time.


    """

    n = len(a)
    if chunksize<0: chunksize = n

    a_shaved, rc = shave32(a,xbits,has_undef,undef,chunksize)

    if rc:
        raise ValueError, 'shave: error on return from ShaveMantissa_.shave32: %d'%rc

    return a_shaved


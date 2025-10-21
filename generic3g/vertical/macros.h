#include "macros_undef.h"
#include "meta_macros.h"

#ifdef DP_
#   define PREC_ 64
#   define SPARSE_MATRIX_ SparseMatrix_dp
#else
#   define PREC_ 32
#   define SPARSE_MATRIX_ SparseMatrix_sp
#endif

#define SUFFIX_ CONCAT(R, PREC_)
#define KIND_ CONCAT(REAL, PREC_)
#define INDEX_VALUE_PAIR_ CONCAT(IndexValuePair, SUFFIX_)

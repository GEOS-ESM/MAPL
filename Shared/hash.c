

#ifndef sysAIX
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>

#define HASHCHUNK (1024*4)
#define HEAPCHUNK 4

#define FREE(A) (void)free(A); A=NULL

// TLC - missing signatures were causing compilation warnings
static int hash1(int);
static int hash2(int,int);

typedef struct {
  int i,j,k,m;
} entry_t;

typedef struct{
  entry_t *entry_list;
  int next_entry, size;
}  bucket_t;
  
typedef struct {
  bucket_t *bucket_list;
  int num_entries, num_buckets;
}  hash_t;

hash_t *hash_heap=(hash_t *)NULL;
int hash_heap_size = HEAPCHUNK;

void init_hash(hash_t *h, int nbuckets) {
  int l;
  bucket_t *bucket;

  h->bucket_list = (bucket_t *)malloc((size_t)(nbuckets*sizeof(bucket_t)));
  if(!h->bucket_list) {
    printf("hash.c line=%d : Could not allocate bucket list\n",__LINE__);
    exit(1);
  }
  for(l=0; l<nbuckets; l++) {
    bucket=h->bucket_list;
    (bucket+l)->entry_list=(entry_t *)NULL;
  }

  h->num_entries = 0;
  h->num_buckets = nbuckets;
}

void init_bucket(bucket_t *b) {
  b->size       = HASHCHUNK;
  b->next_entry = 0;
  b->entry_list = (entry_t *)malloc((size_t)(b->size*sizeof(entry_t)));
  if(!b->entry_list) {
    printf("hash.c line=%d : Could not allocate entry list\n",__LINE__);
    exit(1);
  }
}

// Create an empty hash with nbuckets

int create_hash(int nbuckets)
{
  int i, newhash;

  // If first time allocate a new heap and clear its bucket list

  if(!hash_heap) {
    hash_heap = (hash_t *)malloc(hash_heap_size*sizeof(hash_t));
    if(!hash_heap) {
      printf("hash.c line=%d : Could not allocate hash_heap\n",__LINE__);
      exit(1);
    }
    for(i=0;i<hash_heap_size;i++) hash_heap[i].bucket_list=(bucket_t *)NULL;
  }

  // Find an unused hash on the heap, call it newhash.

  for(i=0;i<hash_heap_size;i++) {
    if(!hash_heap[i].bucket_list) break;
  }

  newhash = i;

  // If newhash requires bigger heap, reallocate it

  if(newhash==hash_heap_size) {
    hash_heap = 
      (hash_t *)realloc(hash_heap,sizeof(hash_t)*(hash_heap_size+=HEAPCHUNK));

    if(!hash_heap) {
      printf("hash.c line=%d : Could not expand hash_heap\n",__LINE__);
      exit(1);
    }

    for(i=newhash;i<hash_heap_size;i++) 
      hash_heap[i].bucket_list=(bucket_t *)NULL;
  }

  (void)init_hash(hash_heap+newhash,nbuckets);

  return newhash;
}

// Destroy the hash identified by handle h, releasing all its space

void destroy_hash(int h)
{
  int i;

  if(!hash_heap) {
    printf("hash.c line=%d : Attempt to destroy hash from empty heap\n",__LINE__);
    exit(1);
  } else if(!hash_heap[h].bucket_list) {
    printf("hash.c line=%d : Attempt to destroy uninitalized hash\n",__LINE__);
    exit(1);
  } else { 
    for(i=0;i<hash_heap[h].num_buckets;i++) {
	FREE(hash_heap[h].bucket_list[i].entry_list);
    }
    FREE(hash_heap[h].bucket_list);
  }
}



int hash_size(int h)
{
  int l, num, maxb, minb, ct, nb;
  bucket_t *bucket;
  hash_t   *hash;

  hash   = hash_heap+h;
  bucket = hash->bucket_list;
  nb     = hash->num_buckets;

  num    = 0;
  maxb   = bucket->next_entry;
  minb   = bucket->next_entry;

  for(l=0; l<nb; l++,bucket++)
    if(bucket->entry_list) {
      ct   = bucket->next_entry;
      num  = num + ct;
      maxb = (ct>maxb)?ct:maxb;
      minb = (ct<minb)?ct:minb;
    }

  printf("==> minb: %d maxb: %d Total: %d\n",minb,maxb,num);
  return num;
}

void dump_hash(int h, int *i, int *j, int *k)
{
  int l, m, num;
  bucket_t *bucket;
  entry_t  *entry;
  hash_t   *hash;

  hash = hash_heap+h;
  num  = 0;

  for(l=0; l<hash->num_buckets; l++) {
    bucket = hash->bucket_list + l;
    for(m=0; m<bucket->next_entry; m++) {
      entry = (bucket->entry_list) + m;
      i[num]==entry->i;
      j[num]==entry->j;
      k[num]==entry->k;
      num++;
    }
  }

}


// Main hashing method checks hash for (i,j) pair.
// If present it returns its Id. If not it adds
// to the hash, giving the new entry the next id
// value, and returns the Id. Currently the next Id
// is determined by incrementing a counter.

int increment_hash(int h, int i, int j, int k)
{

  if(!hash_heap) {

    printf("hash.c line=%d : Attempt to increment hash from empty heap\n",__LINE__);
    exit(1);

  } else if(!(hash_heap[h].bucket_list)) {

    printf("hash.c line=%d : Attempt to increment uninitalized hash %d i=%d j=%d %ld\n",
	   __LINE__,h,i,j,hash_heap[h].bucket_list);
    exit(1);

  } else {

    bucket_t *bucket;
    entry_t  *entry;
    hash_t   *hash;
    int key;

    // Start


    if(j==INT_MAX && k==INT_MAX)
      key=hash1(i);
    else if(k==INT_MAX)
      key=hash2(i,j);
    else 
      key=hash2(hash2(i,j),k);

    hash   = hash_heap+h;
    bucket = hash->bucket_list + (key & (hash->num_buckets - 1));


    if(!(bucket->entry_list)) {

      init_bucket(bucket);

    } else {

      int m;
      for(m=bucket->next_entry-1; m>=0; m--) {
	entry = (bucket->entry_list) + m;
	if(entry->i==i && entry->j==j && entry->k==k)
	  return entry->m;
      }

      if(bucket->next_entry == bucket->size) {
	bucket->size      += HASHCHUNK;
	bucket->entry_list = 
	  (entry_t *)realloc(bucket->entry_list,sizeof(entry_t)*bucket->size);
	if(!bucket->entry_list) {
	  printf("hash.c line=%d : Could not reallocate entry list\n",__LINE__);
	  exit(1);
	}
      }

    }


    ++(hash->num_entries);
    entry = bucket->entry_list + bucket->next_entry++;

    entry->i = i;
    entry->j = j;
    entry->k = k;
    entry->m = hash->num_entries;

    return entry->m;
  }  
}

// Hash function from 2 ints to 1 int

int hash2(int i, int j)
{ unsigned long long key;
  key = (unsigned long long)i << 32 | (unsigned long long)j;
  key = (~key) + (key << 18); // key = (key << 18) - key - 1;
  key = key ^ (key >> 31);
  key = key * 21; // key = (key + (key << 2)) + (key << 4);
  key = key ^ (key >> 11);
  key = key + (key << 6);
  key = key ^ (key >> 22);
  return (int)key;
}

// Hash function from 1 ints to 1 int

int hash1(int key)
{
  key = ~key + (key << 15); // key = (key << 15) - key - 1;
  key = key ^ (key >> 12);
  key = key + (key << 2);
  key = key ^ (key >> 4);
  key = key * 2057; // key = (key + (key << 3)) + (key << 11);
  key = key ^ (key >> 16);
  return key;
}








// Fortran bindings

void DESTROYHASH (int *h){destroy_hash(*h);}
void DESTROYHASH_(int *h){destroy_hash(*h);}
void destroyhash (int *h){destroy_hash(*h);}
void destroyhash_(int *h){destroy_hash(*h);}

int INCREMENTHASH (int *h,int *i, int *j, int *k){return increment_hash(*h,*i,*j,*k);}
int INCREMENTHASH_(int *h,int *i, int *j, int *k){return increment_hash(*h,*i,*j,*k);}
int incrementhash (int *h,int *i, int *j, int *k){return increment_hash(*h,*i,*j,*k);}
int incrementhash_(int *h,int *i, int *j, int *k){return increment_hash(*h,*i,*j,*k);}

int CREATEHASH (int *nbuckets){return create_hash(*nbuckets);}
int CREATEHASH_(int *nbuckets){return create_hash(*nbuckets);}
int createhash (int *nbuckets){return create_hash(*nbuckets);}
int createhash_(int *nbuckets){return create_hash(*nbuckets);}

int HASHSIZE (int *h){return hash_size(*h);}
int HASHSIZE_(int *h){return hash_size(*h);}
int hashsize (int *h){return hash_size(*h);}
int hashsize_(int *h){return hash_size(*h);}

void DUMPHASH (int *h, int *i,int *j,int *k){dump_hash(*h,i,j,k);}
void DUMPHASH_(int *h, int *i,int *j,int *k){dump_hash(*h,i,j,k);}
void dumphash (int *h, int *i,int *j,int *k){dump_hash(*h,i,j,k);}
void dumphash_(int *h, int *i,int *j,int *k){dump_hash(*h,i,j,k);}

#endif

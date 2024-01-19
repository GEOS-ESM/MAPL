#if defined ESMF_HCONFIG_AS
#undef ESMF_HCONFIG_AS
#endif

#if (TYPE_ == TYPE_I4)
#define ESMF_HCONFIG_AS ESMF_HConfigAsI4
#elif (TYPE_ == TYPE_I8)
#define ESMF_HCONFIG_AS ESMF_HConfigAsI8
#elif (TYPE_ == TYPE_R4)
#define ESMF_HCONFIG_AS ESMF_HConfigAsR4
#elif (TYPE_ == TYPE_R8)
#define ESMF_HCONFIG_AS ESMF_HConfigAsR8
#elif (TYPE_ == TYPE_LOGICAL)
#define ESMF_HCONFIG_AS ESMF_HConfigAsLogical
#elif (TYPE_ == TYPE_CHARACTER)
#define ESMF_HCONFIG_AS ESMF_HConfigAsString
#else
#define ESMF_HCONFIG_AS
#endif

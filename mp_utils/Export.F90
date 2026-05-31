! Public Export umbrella for the MAPL.mp_utils layer.
! Re-exports only the symbols that are part of MAPL's public API
! (i.e., entities carrying the MAPL_ prefix).
! This is what mapl/MAPL.F90 imports from.
! For internal MAPL use, see mapl_mp_utils_internal.
module mapl_mp_utils_export
   use mapl_mp_utils_internal
   
   private

   
   ! PackedTime functions with MAPL_ prefix

   ! Backward compatibility: Unprefixed names (TODO: remove after client repos migrated)
   ! See issue #5011 - these should be removed once GEOSgcm and other clients updated

end module mapl_mp_utils_export

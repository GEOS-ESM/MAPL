module MAPL_EASEConversion

  ! Re-export wrapper. The canonical implementation now lives in
  ! mapl3g_EASEConversion (geom/EASE/EASEConversion.F90).
  ! MAPL.base callers continue to work unchanged via the MAPL_-prefixed aliases.

  use mapl3g_EASEConversion, &
       MAPL_ease_convert              => ease_convert,              &
       MAPL_ease_inverse              => ease_inverse,              &
       MAPL_ease_extent               => ease_extent,               &
       MAPL_get_ease_gridname_by_cols => get_ease_gridname_by_cols

  implicit none

  public :: MAPL_ease_convert
  public :: MAPL_ease_inverse
  public :: MAPL_ease_extent
  public :: MAPL_get_ease_gridname_by_cols

end module MAPL_EASEConversion

#include "MAPL.h"

module mapl3g_EASEConversion

  ! =====================================================================================
  ! Re-export wrapper around the canonical implementation in MAPL_EASEConversion
  ! (base/MAPL_EASEConversion.F90).
  !
  ! MAPL.geom depends on MAPL.base, so MAPL_EASEConversion is already available
  ! at geom compile time.  This module provides the mapl3g_* prefixed name used
  ! within geom/EASE/ and avoids duplicating the math.
  ! =====================================================================================

  use MAPL_EASEConversion, &
       ease_convert              => MAPL_ease_convert,             &
       ease_inverse              => MAPL_ease_inverse,             &
       ease_extent               => MAPL_ease_extent,              &
       get_ease_gridname_by_cols => MAPL_get_ease_gridname_by_cols

  implicit none

  public :: ease_convert
  public :: ease_inverse
  public :: ease_extent
  public :: get_ease_gridname_by_cols

end module mapl3g_EASEConversion

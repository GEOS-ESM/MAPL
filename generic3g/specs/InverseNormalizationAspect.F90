#include "MAPL.h"

module mapl3g_InverseNormalizationAspect

   use mapl3g_AspectId
   use mapl3g_NormalizationAspect
   use mapl_ErrorHandling
   
   implicit none
   private

   public :: InverseNormalizationAspect

   ! Thin subclass of NormalizationAspect for inverse (denormalization)
   ! Inherits all implementation from parent, only overrides aspect ID
   type, extends(NormalizationAspect) :: InverseNormalizationAspect
   contains
      procedure, nopass :: get_aspect_id
   end type InverseNormalizationAspect

   interface InverseNormalizationAspect
      procedure new_InverseNormalizationAspect
   end interface

contains

   function new_InverseNormalizationAspect(aux_field_name, scale_factor, source_units, target_units, is_time_dependent) result(aspect)
      type(InverseNormalizationAspect) :: aspect
      character(*), optional, intent(in) :: aux_field_name
      real, optional, intent(in) :: scale_factor
      character(*), optional, intent(in) :: source_units
      character(*), optional, intent(in) :: target_units
      logical, optional, intent(in) :: is_time_dependent

      ! Call parent constructor
      aspect%NormalizationAspect = NormalizationAspect(aux_field_name, scale_factor, &
                                                       source_units, target_units, is_time_dependent)
      
      ! Set is_inverse flag for this subclass
      aspect%is_inverse = .true.

   end function new_InverseNormalizationAspect

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = INVERSE_NORMALIZATION_ASPECT_ID
   end function get_aspect_id

end module mapl3g_InverseNormalizationAspect

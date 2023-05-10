module mapl_RegridderFactory
   use mapl_MaplRegridder
   implicit none
   private

   public :: RegridderFactory

   type, abstract :: RegridderFactory
   contains
      procedure(I_make_geom_spec_from_config), deferred :: make_geom_spec_from_config
      procedure(I_make_geom_spec_from_metadata), deferred :: make_geom_spec_from_metadata
      generic :: make_geom_spec => make_geom_spec_from_config
      generic :: make_geom_spec => make_geom_spec_from_metadata
   end type RegridderFactory


   abstract interface

     function I_make_geom_spec_from_config(this, config, rc) result(spec)
         use esmf, only: ESMF_Config
         import RegridderFactory
         class(RegridderSpec), allocatable :: spec
         class(RegridderFactory), intent(in) :: this
         type(ESMF_Config), intent(inout) :: config
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_config

      function I_make_geom_spec_from_config(this, file_metadata, rc) result(spec)
         use mapl_FileMetadata
         import RegridderFactory
         class(RegridderSpec), allocatable :: spec
         class(RegridderFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_config


      function I_make_mapl_geom_from_geom_spec(this, geom_spec, rc) result(mapl_geom)
         use esmf, only: ESMF_Config
         import RegridderFactory
         type(MaplRegridder) :: mapl_geom
         class(RegridderFactory), intent(in) :: this
         class(RegridderSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function I_make_mapl_geom_from_geom_spec

   end interface

end module mapl_RegridderFactory

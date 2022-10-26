module mapl3g_GenericConfig
   use esmf, only: Esmf_Config
   use yaFyaml, only: YAML_Node
   implicit none
   private
  
   public :: GenericConfig
  
   type :: GenericConfig
      type(ESMF_Config), allocatable :: esmf_cfg
      class(YAML_Node), allocatable :: yaml_cfg
   contains
      procedure :: has_yaml
      procedure :: has_esmf
   end type GenericConfig


   interface GenericConfig
      module procedure new_GenericConfig
   end interface GenericConfig

contains

   function new_GenericConfig(esmf_cfg, yaml_cfg) result(config)
      type(GenericConfig) :: config
      type(ESMF_Config), optional, intent(in) :: esmf_cfg
      class(YAML_Node), optional, intent(in) :: yaml_cfg

      if (present(esmf_cfg)) config%esmf_cfg = esmf_cfg
      if (present(yaml_cfg)) config%yaml_cfg = yaml_cfg

   end function new_GenericConfig

   pure logical function has_yaml(this)
      class(GenericConfig), intent(in) :: this
      has_yaml = allocated(this%yaml_cfg)
   end function has_yaml

   pure logical function has_esmf(this)
      class(GenericConfig), intent(in) :: this
      has_esmf = allocated(this%esmf_cfg)
   end function has_esmf

end module mapl3g_GenericConfig

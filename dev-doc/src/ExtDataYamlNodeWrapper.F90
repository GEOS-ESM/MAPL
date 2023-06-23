module MAPL_ExtDataYamlNodeWrapper
   use yaFyaml
   implicit none

   public YamlNodeWrapper

   type :: YamlNodeWrapper
      class(YAML_Node), allocatable :: a_yaml_node
   end type

end module MAPL_ExtDataYamlNodeWrapper

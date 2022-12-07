module MAPL_ExtDataYamlNodeStack
   use yaFyaml
   use MAPL_ExtDataYamlNodeWrapper
   implicit none

   public :: stack_depth
   public :: yaml_node_stack

   integer, save :: stack_depth = 0
   type(YamlNodeWrapper), save :: yaml_node_stack(20)

end module MAPL_ExtDataYamlNodeStack

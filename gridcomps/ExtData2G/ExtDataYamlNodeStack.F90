module MAPL_ExtDataYamlNodeStack
   use yaFyaml
   use MAPL_ExtDataYamlNodeWrapper
   implicit none

   public :: max_file_depth 
   public :: stack_depth
   public :: yaml_node_stack

   integer, save :: stack_depth = 0
   integer, parameter :: max_file_depth = 20
   type(YamlNodeWrapper), save :: yaml_node_stack(max_file_depth)

end module MAPL_ExtDataYamlNodeStack

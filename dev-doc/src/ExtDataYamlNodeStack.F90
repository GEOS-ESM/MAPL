module MAPL_ExtDataYamlNodeStack
   use yaFyaml
   use MAPL_ExtDataYamlNodeWrapper
   implicit none

   public :: max_file_depth 
   public :: stack_depth
   public :: yaml_node_stack

   integer, save :: stack_depth = 0
   integer, parameter :: MAX_FILE_DEPTH = 20
   type(YamlNodeWrapper), save :: yaml_node_stack(MAX_FILE_DEPTH)

end module MAPL_ExtDataYamlNodeStack

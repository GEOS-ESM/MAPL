module MAPL_ExtDataYamlNodeStack
   use yaFyaml
   implicit none

   integer, save :: stack_depth = 0
   type(Configuration), save :: yaml_node_stack(10)

end module MAPL_ExtDataYamlNodeStack

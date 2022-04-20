module mapl3g_ComponentSpec
   implicit none

   type :: ComponentSpec
      type(StateSpec) :: import_state_spec
      type(StateSpec) :: export_state_spec
      type(StateSpec) :: internal_state_spec
      type(ChildrenSpecMap) :: child_specs

   end type ComponentSpec
end module mapl3g_ComponentSpec

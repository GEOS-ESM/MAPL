submodule makeItemSpec_smod

   use mapl3g_FieldSpec, only: FieldSpec
   use mapl3g_ServiceSpec, only: ServiceSpec
   use mapl3g_WildcardSpec, only: WildcardSpec
   use mapl3g_BracketSpec, only: BracketSpec
   use mapl3g_InvalidSpec, only: InvalidSpec
   implicit none

contains

   module function make_itemSpec

      select case (variable_spec%itemtype%ot)
      case (MAPL_STATEITEM_FIELD%ot)
         allocate(FieldSpec :: item_spec)
         item_spec = FieldSpec(variable_spec)
      case (MAPL_STATEITEM_SERVICE%ot)
         allocate(ServiceSpec :: item_spec)
         item_spec = ServiceSpec()
      case (MAPL_STATEITEM_WILDCARD%ot)
         allocate(WildcardSpec :: item_spec)
         item_spec = WildcardSpec(variable_spec)
      case (MAPL_STATEITEM_BRACKET%ot)
         allocate(BracketSpec :: item_spec)
         item_spec = BracketSpec(variable_spec)
      case default
         allocate(InvalidSpec :: item_spec)
         _FAIL('Unsupported type.')
      end select

   end function make_itemSpec

end submodule makeItemSpec_smod

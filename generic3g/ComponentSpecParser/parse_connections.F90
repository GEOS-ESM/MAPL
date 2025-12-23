#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_connections_smod
   
contains

   module function parse_connections(hconfig, rc) result(connections)
      type(ConnectionVector) :: connections
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: conn_specs, conn_spec
      class(Connection), allocatable :: conn
      integer :: status, i, num_specs
      logical :: has_connections

      has_connections = ESMF_HConfigIsDefined(hconfig,keyString=COMPONENT_CONNECTIONS_SECTION,_RC)
      _RETURN_UNLESS(has_connections)

      conn_specs = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_CONNECTIONS_SECTION, _RC)

      num_specs = ESMF_HConfigGetSize(conn_specs, _RC)
      do i = 1, num_specs
         conn_spec = ESMF_HConfigCreateAt(conn_specs, index=i, _RC)
         conn = parse_connection(conn_spec, _RC)
         call connections%push_back(conn)
         deallocate(conn)
      enddo 

      _RETURN(_SUCCESS)

   contains

      function parse_connection(config, rc) result(conn)
         class(Connection), allocatable :: conn
         type(ESMF_HConfig), optional, intent(in) :: config
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: src_name, dst_name
         character(:), allocatable :: src_comp, dst_comp
         character(:), allocatable :: src_intent, dst_intent

         call get_comps(config, src_comp, dst_comp, _RC)

         if (ESMF_HConfigIsDefined(config,keyString='all_unsatisfied')) then
            conn = MatchConnection( &
                 ConnectionPt(src_comp, VirtualConnectionPt(state_intent='export', short_name='^.*$')), &
                 ConnectionPt(dst_comp, VirtualConnectionPt(state_intent='import', short_name='^.*$'))  &
                 )
            _RETURN(_SUCCESS)
         end if
            
         call get_names(config, src_name, dst_name, _RC)
         call get_intents(config, src_intent, dst_intent, _RC)

         associate ( &
              src_pt => VirtualConnectionPt(state_intent=src_intent, short_name=src_name), &
              dst_pt => VirtualConnectionPt(state_intent=dst_intent, short_name=dst_name) )

           if (dst_intent == 'export') then
              conn = ReexportConnection( &
                ConnectionPt(src_comp, src_pt), &
                ConnectionPt(dst_comp, dst_pt))
           else
              conn = SimpleConnection( &
                   ConnectionPt(src_comp, src_pt), &
                   ConnectionPt(dst_comp, dst_pt))
           end if

         end associate

         _RETURN(_SUCCESS)
      end function parse_connection

      subroutine get_names(config, src_name, dst_name, rc)
         type(ESMF_HConfig), intent(in) :: config
         character(:), allocatable :: src_name
         character(:), allocatable :: dst_name
         integer, optional, intent(out) :: rc

         integer :: status

         associate (provides_names => &
              ESMF_HConfigIsDefined(config,keyString='name') .or. &
              (ESMF_HConfigIsDefined(config,keyString='src_name') .and. ESMF_HConfigIsDefined(config,keyString='dst_name')) &
              )
           _ASSERT(provides_names, "Must specify 'name' or 'src_name' .and. 'dst_name' in connection.")
         end associate

         if (ESMF_HConfigIsDefined(Config,keystring='name')) then ! replicate for src and dst
            src_name = ESMF_HConfigAsString(config,keyString='name',_RC)
            dst_name = src_name
            _RETURN(_SUCCESS)
         end if

         src_name = ESMF_HConfigAsString(config,keyString='src_name',_RC)
         dst_name = ESMF_HConfigAsString(config,keyString='dst_name',_RC)

         _RETURN(_SUCCESS)
      end subroutine get_names

      subroutine get_comps(config, src_comp, dst_comp, rc)
         type(ESMF_HConfig), intent(in) :: config
         character(:), allocatable :: src_comp
         character(:), allocatable :: dst_comp
         integer, optional, intent(out) :: rc

         integer :: status
         
         _ASSERT(ESMF_HConfigIsDefined(config,keyString='src_comp'), 'Connection must specify a src component')
         _ASSERT(ESMF_HConfigIsDefined(config,keyString='dst_comp'), 'Connection must specify a dst component')
         src_comp = ESMF_HConfigAsString(config,keyString='src_comp',_RC)
         dst_comp = ESMF_HConfigAsString(config,keyString='dst_comp',_RC)
         _RETURN(_SUCCESS)
      end subroutine get_comps

      subroutine get_intents(config, src_intent, dst_intent, rc)
         type(ESMF_HConfig), intent(in) :: config
         character(:), allocatable :: src_intent
         character(:), allocatable :: dst_intent
         integer, optional, intent(out) :: rc

         integer :: status

         ! defaults
         src_intent = 'export'
         dst_intent = 'import'

         if (ESMF_HConfigIsDefined(config,keyString='src_intent')) then
            src_intent = ESMF_HConfigAsString(config,keyString='src_intent',_RC)
         end if
         if (ESMF_HConfigIsDefined(config,keyString='dst_intent')) then
            dst_intent = ESMF_HConfigAsString(config,keyString='dst_intent',_RC)
         end if

         _RETURN(_SUCCESS)
      end subroutine get_intents

   end function parse_connections

end submodule parse_connections_smod


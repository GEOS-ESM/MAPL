#include "MAPL.h"

! Actions procedures for StateRegistry:
! - allocate: Allocate memory for active specs
! - add_to_states: Add items to ESMF states
! - filter: Filter virtual points by pattern matching
! - get_export_couplers: Get couplers that produce exports
! - get_import_couplers: Get couplers that consume imports

submodule (mapl3g_StateRegistry) StateRegistry_Actions_smod
   use mapl3g_MultiState
   use mapl3g_ActualConnectionPt
   use mapl3g_VirtualPtFamilyMap, only: VirtualPtFamilyMapIterator
   use mapl3g_StateItemExtensionVector, only: StateItemExtensionVectorIterator
   use mapl3g_StateItemExtensionPtrVector, only: StateItemExtensionPtrVectorIterator
   use esmf
   implicit none(type,external)

contains

   module subroutine allocate(this, rc)
      class(StateRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension), pointer :: extension
      integer :: i
      type(StateItemSpec), pointer :: item_spec

      do i = 1, this%owned_items%size()
         extension => this%owned_items%of(i)
         item_spec => extension%get_spec()
         if (item_spec%is_active()) then
            call item_spec%allocate(_RC)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine allocate

   module subroutine add_to_states(this, multi_state, mode, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(MultiState), intent(inout) :: multi_state
      character(*), intent(in) :: mode
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(VirtualPtFamilyMapIterator) :: family_iter
      type(VirtualConnectionPt), pointer :: v_pt
      type(ActualConnectionPt) :: a_pt
      type(ExtensionFamily), pointer :: family
      type(StateItemExtensionPtrVector), pointer :: extensions
      type(StateItemExtensionPtr), pointer :: extension
      type(StateItemExtension), pointer :: primary
      type(StateItemExtensionPtrVectorIterator) :: ext_iter
      type(StateItemSpec), pointer :: spec
      integer :: i, label

      _ASSERT(any([mode == 'user', mode == 'outer']), 'invalid mode: <' // mode // '>')
      associate (e => this%family_map%ftn_end())

        family_iter = this%family_map%ftn_begin()
        do while (family_iter /= e)
           call family_iter%next()
           v_pt => family_iter%first()
           family => family_iter%second()
           extensions => family%get_extensions()

           select case (mode)
           case ('user') ! only add if both primary and not a substate item
              if (v_pt%get_comp_name() /= '') cycle
              if (.not. family%has_primary()) cycle
              primary => family%get_primary(_RC)
              a_pt = ActualConnectionPt(v_pt)
              spec => primary%get_spec()
              call spec%add_to_state(multi_state, a_pt, _RC)
           case ('outer')
              associate (ext_e => extensions%ftn_end())
                ext_iter = extensions%ftn_begin()
                i = 0
                do while (ext_iter /= ext_e)
                   call ext_iter%next()
                   i = i + 1

                   extension => ext_iter%of()
                   spec => extension%ptr%get_spec()

                   label = i
                   if (family%has_primary()) label = i-1

                   a_pt = ActualConnectionPt(v_pt)
                   if (label /= 0) a_pt = ActualConnectionPt(v_pt, label=label)
                   call spec%add_to_state(multi_state, a_pt, _RC)
                end do
              end associate
           case default
              _FAIL("Illegal mode in StateRegistry::add_to_states()")
           end select

        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine add_to_states

   ! Used by connection subclasses to allow wildcard matches in names.
   module function filter(this, pattern) result(matches)
      type(VirtualConnectionPtVector) :: matches
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: pattern

      type(VirtualConnectionPt), pointer :: v_pt
      type(VirtualPtFamilyMapIterator) :: iter

      matches = VirtualConnectionPtVector()
      associate (e => this%family_map%ftn_end())
        iter = this%family_map%ftn_begin()
        do while (iter /= e)
           call iter%next()
           v_pt => iter%first()

           if (pattern%matches(v_pt)) then
              call matches%push_back(v_pt)
           end if

        end do
      end associate

   end function filter

   ! An item has a user-level export coupler iff:
   !  - it is owned
   !  - has a consumer
   !  - has no producers
   ! The export couplers are all consumers.

   module function get_export_couplers(this) result(export_couplers)
      type(ComponentDriverPtrVector) :: export_couplers
      class(StateRegistry), target, intent(in) :: this

      type(StateItemExtension), pointer :: extension
      type(StateItemExtensionVectorIterator) :: iter
      type(ComponentDriverVector), pointer :: consumers
      type(ComponentDriverPtr) :: wrapper
      integer :: i
      
      associate (e => this%owned_items%ftn_end())
        iter = this%owned_items%ftn_begin()
        do while (iter /= e)
           call iter%next()
           extension => iter%of()

           if (extension%has_producer()) cycle
           consumers => extension%get_consumers()
           do i = 1, consumers%size()
              wrapper%ptr => consumers%of(i) ! copy ptr
              call export_couplers%push_back(wrapper)
           end do

        end do
      end associate

   end function get_export_couplers

   ! An item has an import coupler iff:
   !   - is has a producer
   !   - it has no consumers
   !   - it is NOT an extension
   !
   ! That last condition is to prevent treating "ultimate" extensions
   ! as having an import coupler.   These would be the same couplers
   ! but would be activate at the connection level rather than
   ! the owning grid comp. 

   module function get_import_couplers(this) result(import_couplers)
      type(ComponentDriverPtrVector) :: import_couplers
      class(StateRegistry), target, intent(in) :: this

      type(VirtualPtFamilyMapIterator) :: family_iter
      type(ExtensionFamily), pointer :: family
      type(VirtualConnectionPt), pointer :: v_pt
      type(ComponentDriverPtr) :: wrapper
      type(StateItemExtension), pointer :: primary

      associate (e => this%family_map%ftn_end())
        family_iter = this%family_map%ftn_begin()
        do while (family_iter /= e)
           call family_iter%next()
           v_pt => family_iter%first()
           family => family_iter%second()

           if (v_pt%get_comp_name() /= '') cycle
           if (.not. family%has_primary()) cycle
           primary => family%get_primary()

           if (primary%has_producer() .and. .not. primary%has_consumers()) then
              wrapper%ptr => primary%get_producer()
              call import_couplers%push_back(wrapper)
           end if
              
        end do
      end associate
 
  end function get_import_couplers

end submodule StateRegistry_Actions_smod

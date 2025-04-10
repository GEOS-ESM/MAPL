#include "MAPL_Generic.h"

module mapl3g_StateRegistry

   use mapl3g_AbstractRegistry
   use mapl3g_RegistryPtr
   use mapl3g_RegistryPtrMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ConnectionPt
   use mapl3g_StateItemExtension
   use mapl3g_StateItemExtensionVector
   use mapl3g_StateItemExtensionPtrVector
   use mapl3g_ExtensionFamily
   use mapl3g_VirtualPtFamilyMap
   use mapl3g_StateItemVector
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_GriddedComponentDriver
   use mapl3g_VerticalGrid
   use mapl_ErrorHandling
   use esmf, only: ESMF_Geom, ESMF_TimeInterval

   implicit none
   private

   public :: StateRegistry

   type, extends(AbstractRegistry) :: StateRegistry
      private
      character(:), allocatable :: name
      type(StateItemExtensionVector) :: owned_items ! specs and couplers
      type(RegistryPtrMap) :: subregistries

      type(VirtualPtFamilyMap) :: family_map

   contains

      procedure :: add_subregistry
      procedure :: add_virtual_pt
      procedure :: add_primary_spec
      procedure :: link_extension
      procedure :: add_extension
      procedure :: add_spec
      procedure :: add_family

      procedure :: propagate_unsatisfied_imports_all
      procedure :: propagate_unsatisfied_imports_subregistry
      procedure :: propagate_unsatisfied_imports_virtual_pt
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_all
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_subregistry
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_virtual_pt

      procedure :: propagate_exports_all
      procedure :: propagate_exports_subregistry
      procedure :: propagate_exports_virtual_pt
      generic :: propagate_exports => propagate_exports_all
      generic :: propagate_exports => propagate_exports_subregistry
      generic :: propagate_exports => propagate_exports_virtual_pt

      procedure :: get_name
      procedure :: has_virtual_pt
      procedure :: num_owned_items
      procedure :: get_extension_family
      procedure :: get_extensions
      procedure :: get_primary_extension

      procedure :: has_subregistry
      procedure :: get_subregistry_by_name
      procedure :: get_subregistry_by_conn_pt
      generic :: get_subregistry => get_subregistry_by_name
      generic :: get_subregistry => get_subregistry_by_conn_pt

      ! Actions on specs
      procedure :: allocate
      procedure :: add_to_states

      procedure :: filter ! for MatchConnection

      procedure :: get_export_couplers
      procedure :: get_import_couplers

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: extend

   end type StateRegistry

  interface StateRegistry
      procedure new_StateRegistry
   end interface StateRegistry

   character(*), parameter :: SELF = "<self>"
   
contains

   function new_StateRegistry(name) result(r)
      type(StateRegistry) :: r
      character(*), intent(in) :: name

      r%name = name
   end function new_StateRegistry

   logical function has_virtual_pt(this, virtual_pt)
      class(StateRegistry), intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      has_virtual_pt = (this%family_map%count(virtual_pt) > 0)
   end function has_virtual_pt

   subroutine add_virtual_pt(this, virtual_pt, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%has_virtual_pt(virtual_pt), "Virtual connection point already exists in registry")
      call this%family_map%insert(virtual_pt, ExtensionFamily())

      _RETURN(_SUCCESS)
   end subroutine add_virtual_pt


   integer function num_owned_items(this)
      class(StateRegistry), intent(in) :: this
      num_owned_items = this%owned_items%size()
   end function num_owned_items

   subroutine add_family(this, virtual_pt, family, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(ExtensionFamily), intent(in) :: family
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: new_family
      
      call this%add_virtual_pt(virtual_pt, _RC)
      new_family => this%family_map%at(virtual_pt, _RC)
#ifndef __GFORTRAN__      
      new_family = family
#else
      call ridiculous(new_family, family)
#endif

      _RETURN(_SUCCESS)

#ifdef __GFORTRAN__      
   contains

      subroutine ridiculous(a, b)
         type(ExtensionFamily), intent(out) :: a
         type(ExtensionFamily), intent(in) :: b
         a = b
      end subroutine ridiculous
#endif

   end subroutine add_family


   subroutine add_primary_spec(this, virtual_pt, spec, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension) :: extension
      type(ExtensionFamily) :: family

      extension = StateItemExtension(spec)
      call this%owned_items%push_back(extension)
      family = ExtensionFamily(this%owned_items%back())
      call this%add_family(virtual_pt, family, _RC)
      
      _RETURN(_SUCCESS)

   end subroutine add_primary_spec

   function get_primary_extension(this, virtual_pt, rc) result(primary)
      type(StateItemExtension), pointer :: primary
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family

      primary => null()
      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")
      family => this%family_map%at(virtual_pt,_RC)
      primary => family%get_primary()


      _RETURN(_SUCCESS)
   end function get_primary_extension

   function add_extension(this, virtual_pt, extension, rc) result(new_extension)
      type(StateItemExtension), pointer :: new_extension
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemExtension), intent(in) :: extension
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      call this%owned_items%push_back(extension)
      new_extension => this%owned_items%back()
      call this%link_extension(virtual_pt, this%owned_items%back(), _RC)

      _RETURN(_SUCCESS)
   end function add_extension

   subroutine add_spec(this, virtual_pt, spec, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension) :: extension

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      extension = StateItemExtension(spec)
      call this%owned_items%push_back(extension)
      call this%link_extension(virtual_pt, this%owned_items%back(), _RC)

      _RETURN(_SUCCESS)
   end subroutine add_spec

   subroutine link_extension(this, virtual_pt, extension, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemExtension), pointer, intent(in) :: extension
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      family => this%family_map%at(virtual_pt, _RC)
      call family%add_extension(extension)

      _RETURN(_SUCCESS)
   end subroutine link_extension

   function get_extension_family(this, virtual_pt, rc) result(family)
      type(ExtensionFamily), pointer :: family
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      family => this%family_map%at(virtual_pt, _RC)

      _RETURN(_SUCCESS)
   end function get_extension_family

   function get_extensions(this, virtual_pt, rc) result(extensions)
      type(StateItemExtensionPtr), allocatable :: extensions(:)
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family
      integer :: i, n

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")
      family => this%family_map%at(virtual_pt, _RC)
      n = family%num_variants()
      allocate(extensions(n))
      do i = 1, n
         extensions(i)%ptr => family%get_extension(i)
      end do

      _RETURN(_SUCCESS)
   end function get_extensions

   function get_name(this) result(name)
      character(:), allocatable :: name
      class(StateRegistry), intent(in) :: this
      name = this%name
   end function get_name

   subroutine add_subregistry(this, subregistry, rc)
      class(StateRegistry), target, intent(inout) :: this
      class(StateRegistry), target, intent(in) :: subregistry
      integer, optional, intent(out) :: rc

      character(:), allocatable :: name
      type(RegistryPtr) :: wrap

      name = subregistry%get_name()
      _ASSERT(.not. this%has_subregistry(name), 'Duplicate subregistry entry.')
      wrap%registry => subregistry
      call this%subregistries%insert(name, wrap)

      _RETURN(_SUCCESS)
   end subroutine add_subregistry

   function get_subregistry_by_name(this, name, rc) result(subregistry)
      type(StateRegistry), pointer :: subregistry
      class(StateRegistry), target, intent(in) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(RegistryPtr), pointer :: wrap
      integer :: status

      subregistry => null()
      if (name == this%get_name() .or. name == SELF) then
         subregistry => this
         _RETURN(_SUCCESS)
      end if

      wrap => this%subregistries%at(name,_RC)
      _ASSERT(associated(wrap%registry), 'null pointer encountered for subregistry.')

      select type (q => wrap%registry)
      type is (StateRegistry)
         subregistry => q
         _RETURN(_SUCCESS)
      class default
         _FAIL('Illegal subtype of AbstractRegistry encountered.')
      end select

   end function get_subregistry_by_name

   function get_subregistry_by_conn_pt(this, conn_pt, rc) result(subregistry)
      type(StateRegistry), pointer :: subregistry
      class(StateRegistry), target, intent(in) :: this
      type(ConnectionPt), intent(in) :: conn_pt
      integer, optional, intent(out) :: rc

      integer :: status

      subregistry => this%get_subregistry(conn_pt%component_name,_RC)

      _RETURN(_SUCCESS)
   end function get_subregistry_by_conn_pt

   logical function has_subregistry(this, name)
      class(StateRegistry), intent(in) :: this
      character(len=*), intent(in) :: name
      has_subregistry = (this%subregistries%count(name) > 0)
   end function has_subregistry


   subroutine propagate_unsatisfied_imports_all(this, rc)
      class(StateRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateRegistry), pointer :: subregistry
      type(RegistryPtrMapIterator) :: iter

      associate (e => this%subregistries%ftn_end())
        iter = this%subregistries%ftn_begin()
        do while (iter /= e)
           call iter%next()
           subregistry => this%get_subregistry(iter%first(), _RC)
           call this%propagate_unsatisfied_imports(subregistry, _RC)
        end do
      end associate
   
      _RETURN(_SUCCESS)
   end subroutine propagate_unsatisfied_imports_all

   subroutine propagate_unsatisfied_imports_subregistry(this, subregistry, rc)
      class(StateRegistry), target, intent(inout) :: this
      class(StateRegistry), target, intent(in) :: subregistry
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualPtFamilyMapIterator) :: iter
      type(VirtualConnectionPt), pointer :: virtual_pt
      type(ExtensionFamily), pointer :: family

      associate (e => subregistry%family_map%ftn_end())
        iter = subregistry%family_map%ftn_begin()
        do while (iter /= e)
           call iter%next()
           virtual_pt => iter%first()
           if (.not. virtual_pt%is_import()) cycle
           family => iter%second()
           call this%propagate_unsatisfied_imports(virtual_pt, family, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_unsatisfied_imports_subregistry

   subroutine propagate_unsatisfied_imports_virtual_pt(this, virtual_pt, family, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(ExtensionFamily), intent(in) :: family
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtensionPtrVector) :: extensions
      type(StateItemExtensionPtr), pointer :: extension
      integer :: i

      extensions = family%get_extensions()
      do i = 1, extensions%size()
         extension => extensions%of(i)
         call link(extension%ptr, _RC)
      end do

      _RETURN(_SUCCESS)
   contains

      subroutine link(extension, rc)
         type(StateItemExtension), target :: extension
         integer, optional, intent(out) :: rc

         integer :: status
         type(StateItemSpec), pointer :: spec
         
         spec => extension%get_spec()
         _RETURN_IF(spec%is_active())

         if (.not. this%has_virtual_pt(virtual_pt)) then
            call this%add_virtual_pt(virtual_pt, _RC)
         end if
         call this%link_extension(virtual_pt, extension, _RC)

         _RETURN(_SUCCESS)
      end subroutine link
      
      
   end subroutine propagate_unsatisfied_imports_virtual_pt

   ! Loop over subregistry and propagate unsatisfied imports of each
   subroutine propagate_exports_all(this, rc)
      class(StateRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateRegistry), pointer :: subregistry
      type(RegistryPtrMapIterator) :: iter

      associate (e => this%subregistries%ftn_end())
        iter = this%subregistries%ftn_begin()
        do while (iter /= e)
           call iter%next()
           subregistry => this%get_subregistry(iter%first(), _RC)
           call this%propagate_exports(subregistry, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_all


   subroutine propagate_exports_subregistry(this, subregistry, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(StateRegistry), target, intent(in) :: subregistry
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualPtFamilyMapIterator) :: iter

     associate (e => subregistry%family_map%ftn_end())
        iter = subregistry%family_map%ftn_begin()
        do while (iter /= e)
           call iter%next()
           call this%propagate_exports(subregistry%get_name(), iter, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_subregistry

   subroutine propagate_exports_virtual_pt(this, subregistry_name, iter, rc)
      class(StateRegistry), target, intent(inout) :: this
      character(*), intent(in) :: subregistry_name
      type(VirtualPtFamilyMapIterator), intent(in) :: iter
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualConnectionPt), pointer :: virtual_pt
      type(VirtualConnectionPt) :: new_virtual_pt
      type(ExtensionFamily), pointer :: family
      type(ExtensionFamily), pointer :: parent_family

      virtual_pt => iter%first()
      _RETURN_UNLESS(virtual_pt%is_export())

      new_virtual_pt = virtual_pt
      if (virtual_pt%get_comp_name() == '') then
         new_virtual_pt = VirtualConnectionPt(virtual_pt, comp_name=subregistry_name)
      end if

      if (.not. this%has_virtual_pt(new_virtual_pt)) then
         call this%add_virtual_pt(new_virtual_pt)
      end if

      family => iter%second()
      parent_family => this%get_extension_family(new_virtual_pt)
      call parent_family%merge(family)

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_virtual_pt

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(StateRegistry), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit,*,iostat=iostat,iomsg=iomsg) new_line('a')
      if (iostat /= 0) return

      call write_header(this, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return

      call write_virtual_pts(this, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return
      _UNUSED_DUMMY(v_list)
      _UNUSED_DUMMY(iotype)
   contains
      
      subroutine write_header(this, iostat, iomsg)
         class(StateRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         integer :: total
         type(VirtualPtFamilyMapIterator) :: iter
         type(ExtensionFamily), pointer :: family

         total = 0
         associate (e => this%family_map%ftn_end())
           iter = this%family_map%ftn_begin()
           do while (iter /= e)
              call iter%next()
              family => iter%second()
              total = total + family%num_variants()
           end do
         end associate

         write(unit,'(a,a, a,i0, a,i0, a,i0,a)',iostat=iostat,iomsg=iomsg) &
              'Registry(name=', this%name, &
              ', n_owned=', this%num_owned_items(), &
              ', n_virtual=', this%family_map%size(), &
              ', n_extensions=', total, ')' // new_line('a')
         if (iostat /= 0) return
         write(unit,*,iostat=iostat,iomsg=iomsg) '   extensions: '// new_line('a')
      end subroutine write_header

      subroutine write_virtual_pts(this, iostat, iomsg)
         class(StateRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(VirtualPtFamilyMapIterator) :: virtual_iter
         type(ExtensionFamily), pointer :: family
         type(StateItemExtension), pointer :: extension
         type(StateItemSpec), pointer :: spec
         logical :: is_active

         write(unit,*,iostat=iostat,iomsg=iomsg) '   virtuals: '// new_line('a')
         if (iostat /= 0) return
         associate (e => this%family_map%ftn_end())
           virtual_iter = this%family_map%ftn_begin()
           do while (virtual_iter /= e)
              call virtual_iter%next()
              associate (virtual_pt => virtual_iter%first())
                family => virtual_iter%second()
                is_active = .false.
                if (family%has_primary()) then
                   extension => family%get_primary()
                   spec => extension%get_spec()
                   is_active = spec%is_active()
                end if
                write(unit,*,iostat=iostat,iomsg=iomsg)'        ',virtual_pt,  &
                     ': ',family%num_variants(), ' variants ', &
                     ' is primary? ', family%has_primary(),  ' is active? ', is_active, new_line('a')
                if (iostat /= 0) return
              end associate
           end do
         end associate
      end subroutine write_virtual_pts

      
   end subroutine write_formatted

   subroutine allocate(this, rc)
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

   subroutine add_to_states(this, multi_state, mode, rc)
      use mapl3g_MultiState
      use mapl3g_ActualConnectionPt
      use esmf
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
   function filter(this, pattern) result(matches)
      type(VirtualConnectionPtVector) :: matches
      class(StateRegistry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: pattern

      type(VirtualConnectionPt), pointer :: v_pt
      type(VirtualPtFamilyMapIterator) :: iter
      
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

   function get_export_couplers(this) result(export_couplers)
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

   function get_import_couplers(this) result(import_couplers)
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

   ! Repeatedly extend family at v_pt until extension can directly
   ! connect to goal_spec.
   recursive function extend(registry, v_pt, goal_spec, rc) result(extension)
      use mapl3g_MultiState
      use mapl3g_ActualConnectionPt, only: ActualConnectionPt
      type(StateItemExtension), pointer :: extension
      class(StateRegistry), target, intent(inout) :: registry
      type(VirtualConnectionPt), intent(in) :: v_pt
      type(StateItemSpec), intent(in) :: goal_spec
      integer, optional, intent(out) :: rc

      type(StateItemExtension), pointer :: closest_extension, new_extension
      type(StateItemExtension) :: tmp_extension
      type(ExtensionFamily), pointer :: family
      class(ComponentDriver), pointer :: producer
      integer :: iter_count
      integer, parameter :: MAX_ITERATIONS = 10
      integer :: status
      type(MultiState) :: coupler_states
      type(ActualConnectionPt) :: a_pt
      type(StateItemSpec), pointer :: last_spec, new_spec

      family => registry%get_extension_family(v_pt, _RC)
      
      closest_extension => family%find_closest_extension(goal_spec, _RC)
      iter_count = 0
      do
         iter_count = iter_count + 1
         _ASSERT(iter_count <= MAX_ITERATIONS, "StateItem extensions for v_pt did not converge.")

         tmp_extension = closest_extension%make_extension(goal_spec, _RC)
         if (.not. associated(tmp_extension%get_producer())) exit ! no further extensions needed

         ! Add permanent copy of extension to registry and retrieve a valid pointer:
         new_extension => registry%add_extension(v_pt, tmp_extension, _RC)
         producer => new_extension%get_producer()

         coupler_states = producer%get_states()
         a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='import', short_name='import[1]'))
         last_spec => closest_extension%get_spec()
         call last_spec%set_active()
         call last_spec%add_to_state(coupler_states, a_pt, _RC)
         a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='export', short_name='export[1]'))
         new_spec => new_extension%get_spec()
         call new_spec%add_to_state(coupler_states, a_pt, _RC)

         closest_extension => new_extension
      end do

      extension => closest_extension

      _RETURN(_SUCCESS)
   end function extend

end module mapl3g_StateRegistry


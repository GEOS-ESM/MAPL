#include "MAPL_Generic.h"


module mapl3g_Registry
   use mapl3g_AbstractRegistry
   use mapl3g_RegistryPtr
   use mapl3g_RegistryPtrMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_ConnectionPt
   use mapl3g_StateItemExtension
   use mapl3g_StateItemExtensionVector
   use mapl3g_StateItemExtensionPtrVector
   use mapl3g_ExtensionFamily
   use mapl3g_VirtualPtFamilyMap
   use mapl3g_StateItemVector
   use mapl3g_StateItemSpec
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_ComponentDriverVector
   use mapl3g_GriddedComponentDriver
   use mapl_ErrorHandling
   implicit none
   private

   public :: Registry
   public :: newConnection

   type, abstract, extends(Connection) :: newConnection
   contains
      procedure(I_connect_new), deferred :: connect_new
      generic :: connect => connect_new
   end type newConnection

   type, extends(AbstractRegistry) :: Registry
      private
      character(:), allocatable :: name
      type(StateItemExtensionVector) :: owned_items ! specs and couplers
      type(RegistryPtrMap) :: subregistries

      type(VirtualPtFamilyMap) :: family_map

      type(ComponentDriverVector) :: export_couplers ! invalidate() after run
      type(ComponentDriverVector) :: import_couplers ! update() before run

   contains

      procedure :: add_subregistry
      procedure :: add_virtual_pt
      procedure :: add_primary_spec
      procedure :: link_extension
      procedure :: add_extension
      procedure :: add_spec


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

      procedure :: add_connection

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

      procedure :: add_import_coupler
      procedure :: add_export_coupler
      procedure :: allocate
      procedure :: add_to_states

      procedure :: filter ! for MatchConnection


      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

   end type Registry

    abstract interface
      subroutine I_connect_new(this, with_registry, rc)
         import newConnection
         import Registry
         class(newConnection), intent(in) :: this
         type(Registry), target, intent(inout) :: with_registry
         integer, optional, intent(out) :: rc
      end subroutine I_connect_new
   end interface

  interface Registry
      procedure new_Registry
   end interface Registry

   character(*), parameter :: SELF = "<self>"
   
contains

   function new_Registry(name) result(r)
      type(Registry) :: r
      character(*), intent(in) :: name

      r%name = name
   end function new_Registry

   logical function has_virtual_pt(this, virtual_pt)
      class(Registry), intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      has_virtual_pt = (this%family_map%count(virtual_pt) > 0)
   end function has_virtual_pt

   subroutine add_virtual_pt(this, virtual_pt, rc)
      class(Registry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      _ASSERT(.not. this%has_virtual_pt(virtual_pt), "Virtual connection point already exists in registry")
      call this%family_map%insert(virtual_pt, ExtensionFamily())

      _RETURN(_SUCCESS)
   end subroutine add_virtual_pt


   integer function num_owned_items(this)
      class(Registry), intent(in) :: this
      num_owned_items = this%owned_items%size()
   end function num_owned_items

   subroutine add_primary_spec(this, virtual_pt, spec, rc)
      class(Registry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(StateItemSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension) :: extension
      type(ExtensionFamily), pointer :: family

      extension = StateItemExtension(spec)
      call this%owned_items%push_back(extension)

      ! New family (or else!)
      call this%add_virtual_pt(virtual_pt, _RC)
      family => this%family_map%at(virtual_pt, _RC)
      family = ExtensionFamily(this%owned_items%back())

      _RETURN(_SUCCESS)
   end subroutine add_primary_spec

   function get_primary_extension(this, virtual_pt, rc) result(primary)
      type(StateItemExtension), pointer :: primary
      class(Registry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family

      primary => null()
      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")
      family => this%family_map%at(virtual_pt,_RC)
      primary => family%get_primary()
   end function get_primary_extension

   subroutine add_extension(this, virtual_pt, extension, rc)
      class(Registry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(StateItemExtension), intent(in) :: extension
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")

      call this%owned_items%push_back(extension)
      call this%link_extension(virtual_pt, this%owned_items%back(), _RC)

      _RETURN(_SUCCESS)
   end subroutine add_extension

   subroutine add_spec(this, virtual_pt, spec, rc)
      class(Registry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      class(StateItemSpec), intent(in) :: spec
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
      class(Registry), target, intent(inout) :: this
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
      class(Registry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status

      family => this%family_map%at(virtual_pt, _RC)

      _RETURN(_SUCCESS)
   end function get_extension_family

   function get_extensions(this, virtual_pt, rc) result(extensions)
      type(StateItemExtensionPtr), allocatable :: extensions(:)
      class(Registry), target, intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      integer, optional, intent(out) :: rc

      integer :: status
      type(ExtensionFamily), pointer :: family
      integer :: i

      _ASSERT(this%has_virtual_pt(virtual_pt), "Virtual connection point does not exist in registry")
      family => this%family_map%at(virtual_pt, _RC)
      associate (n => family%num_variants())
        allocate(extensions(n))
        do i = 1, n
           extensions(i)%ptr => family%get_extension(i)
        end do
      end associate

      _RETURN(_SUCCESS)
   end function get_extensions

   function get_name(this) result(name)
      character(:), allocatable :: name
      class(Registry), intent(in) :: this
      name = this%name
   end function get_name

   subroutine add_subregistry(this, subregistry, rc)
      class(Registry), target, intent(inout) :: this
      class(Registry), target, intent(in) :: subregistry
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
      type(Registry), pointer :: subregistry
      class(Registry), target, intent(in) :: this
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
      type is (Registry)
         subregistry => q
         _RETURN(_SUCCESS)
      class default
         _FAIL('Illegal subtype of AbstractRegistry encountered.')
      end select

   end function get_subregistry_by_name

   function get_subregistry_by_conn_pt(this, conn_pt, rc) result(subregistry)
      type(Registry), pointer :: subregistry
      class(Registry), target, intent(in) :: this
      type(ConnectionPt), intent(in) :: conn_pt
      integer, optional, intent(out) :: rc

      integer :: status

      subregistry => this%get_subregistry(conn_pt%component_name,_RC)

      _RETURN(_SUCCESS)
   end function get_subregistry_by_conn_pt

   logical function has_subregistry(this, name)
      class(Registry), intent(in) :: this
      character(len=*), intent(in) :: name
      has_subregistry = (this%subregistries%count(name) > 0)
   end function has_subregistry


   subroutine propagate_unsatisfied_imports_all(this, rc)
      class(Registry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(Registry), pointer :: subregistry
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
      class(Registry), target, intent(inout) :: this
      class(Registry), target, intent(in) :: subregistry
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
      class(Registry), target, intent(inout) :: this
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
         class(StateItemSpec), pointer :: spec

         spec => extension%get_spec()
         _RETURN_IF(spec%is_active())
         
         if (.not. this%has_virtual_pt(virtual_pt)) then
            call this%add_virtual_pt(virtual_pt, _RC)
         end if
         call this%link_extension(virtual_pt, extension, _RC)

         _RETURN(_SUCCESS)
      end subroutine link
      
      
   end subroutine propagate_unsatisfied_imports_virtual_pt

   ! Loop over subregistryren and propagate unsatisfied imports of each
   subroutine propagate_exports_all(this, rc)
      class(Registry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(Registry), pointer :: subregistry
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
      class(Registry), target, intent(inout) :: this
      type(Registry), target, intent(in) :: subregistry
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
      class(Registry), target, intent(inout) :: this
      character(*), intent(in) :: subregistry_name
      type(VirtualPtFamilyMapIterator), intent(in) :: iter
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualConnectionPt), pointer :: virtual_pt
      type(VirtualConnectionPt) :: new_virtual_pt
      type(ExtensionFamily), pointer :: family

      virtual_pt => iter%first()
      _RETURN_UNLESS(virtual_pt%is_export())

      new_virtual_pt = VirtualConnectionPt(virtual_pt, subregistry_name)
      call this%add_virtual_pt(new_virtual_pt, _RC)
      family => iter%second()
      call this%family_map%insert(new_virtual_pt, family)

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_virtual_pt

   ! Connect two _virtual_ connection points.
   recursive subroutine add_connection(this, conn, rc)
      class(Registry), target, intent(inout) :: this
      class(newConnection), intent(in) :: conn
      integer, optional, intent(out) :: rc

      integer :: status

      call conn%connect(this, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_connection

   subroutine add_import_coupler(this, coupler)
      class(Registry), target, intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: coupler
      call this%import_couplers%push_back(coupler)
   end subroutine add_import_coupler

   subroutine add_export_coupler(this, coupler)
      class(Registry), target, intent(inout) :: this
      type(GriddedComponentDriver), intent(in) :: coupler
      call this%export_couplers%push_back(coupler)
   end subroutine add_export_coupler

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(Registry), intent(in) :: this
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
         class(Registry), target, intent(in) :: this
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
         class(Registry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(VirtualPtFamilyMapIterator) :: virtual_iter
         type(ExtensionFamily), pointer :: family

         write(unit,*,iostat=iostat,iomsg=iomsg) '   virtuals: '// new_line('a')
         if (iostat /= 0) return
         associate (e => this%family_map%ftn_end())
           virtual_iter = this%family_map%ftn_begin()
           do while (virtual_iter /= e)
              call virtual_iter%next()
              associate (virtual_pt => virtual_iter%first())
                family => virtual_iter%second()
                write(unit,*,iostat=iostat,iomsg=iomsg)'        ',virtual_pt,  &
                     ': ',family%num_variants(), ' variants ', &
                     ' is primary? ', family%has_primary(),  new_line('a')
                if (iostat /= 0) return
              end associate
           end do
         end associate
      end subroutine write_virtual_pts

      
   end subroutine write_formatted

   subroutine allocate(this, rc)
      class(Registry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension), pointer :: extension
      integer :: i
      class(StateItemSpec), pointer :: item_spec

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
      use esmf
      use mapl3g_MultiState
      class(Registry), target, intent(inout) :: this
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
      class(StateItemSpec), pointer :: spec
      integer :: label

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
                label = 0
                do while (ext_iter /= ext_e)
                   call ext_iter%next()
                   label = label + 1
                   extension => ext_iter%of()
                   spec => extension%ptr%get_spec()
                   if (label == 1 .and. family%has_primary()) then
                      a_pt = ActualConnectionPt(v_pt)
                      call spec%add_to_state(multi_state, a_pt, _RC)
                      cycle
                   end if
                   a_pt = ActualConnectionPt(v_pt, label=label)
                   call spec%add_to_state(multi_state, a_pt, _RC)
                end do
              end associate
           case default
              _FAIL("Illegal mode in Registry::add_to_states()")
           end select

        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine add_to_states

   ! Used by connection subclasses to allow wildcard matches in names.
   function filter(this, pattern) result(matches)
      type(VirtualConnectionPtVector) :: matches
      class(Registry), target, intent(in) :: this
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

end module mapl3g_Registry


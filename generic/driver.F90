program main
   use ESMF
   use mapl_MaplGenericComponent
   use mapl_ConcreteComposite
   use UserComponent_mod
   use mapl_AbstractComponent
   use mapl_AbstractFrameworkComponent
   use mapl_CompositeComponent
   implicit none

   type(MaplGenericComponent), target :: tmp
   class(AbstractFrameworkComponent), pointer :: child_o => null()
   class(AbstractFrameworkComponent), pointer :: child_a => null()
   class(AbstractFrameworkComponent), pointer :: grandchild
   type(UserComponent) :: gcm
   type(UserComponent) :: agcm, ogcm
   type(UserComponent) :: dynamics, physics
   type(ESMF_Clock) :: clock
   integer :: status

   class(AbstractComponent), pointer :: component
   class(AbstractComponent), pointer :: generic

   type(ConcreteComposite), target :: root_composite
   class(AbstractFrameworkComponent), pointer :: root

   call gcm%set_name('GCM')
   call agcm%set_name('AGCM')
   call ogcm%set_name('OGCM')
   call dynamics%set_name('DYN')
   call physics%set_name('PHYSICS')


   root_composite = ConcreteComposite(tmp)
   root => root_composite%get_component()
   call root%set_composite(root_composite) ! close the circular structure

   child_a => root%add_child_component('agcm', agcm)
   grandchild => child_a%add_child_component('dynamics', dynamics)
   grandchild => child_a%add_child_component('physics', physics)

   child_o => root%add_child_component('ogcm', ogcm)

   call root%run_child('agcm', clock, 'phase', rc=status)

   call child_a%run_child('dynamics', clock, 'phase', rc=status)
   call child_a%run_child('physics', clock, 'phase', rc=status)

   call root%run_child('ogcm', clock, 'phase', rc=status)
   
end program main

   

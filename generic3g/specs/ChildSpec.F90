#include "MAPL_ErrLog.h"

module mapl3g_ChildSpec
   use mapl3g_UserSetServices
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: ChildSpec
   public :: operator(==)
   public :: operator(/=)
   
   type :: ChildSpec
      character(:), allocatable :: name
      character(:), allocatable :: yaml_config_file
      character(:), allocatable :: esmf_config_file
      class(AbstractUserSetServices), allocatable :: user_setservices
      ! Prevent default structure constructor
      integer, private ::  hack
   end type ChildSpec

   interface ChildSpec
      module procedure new_ChildSpec
   end interface ChildSpec

   interface operator(==)
      module procedure equal
   end interface operator(==)
      
   interface operator(/=)
      module procedure not_equal
   end interface operator(/=)


contains

   pure function new_ChildSpec(name, user_setservices, unusable, yaml_config, esmf_config) result(spec)
      type(ChildSpec) :: spec
      character(*), intent(in) :: name
      class(AbstractUserSetServices), intent(in) :: user_setservices
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: yaml_config
      character(*), optional, intent(in) :: esmf_config

      spec%name = name
      spec%user_setservices = user_setservices

      if (present(yaml_config)) spec%yaml_config_file = yaml_config
      if (present(esmf_config)) spec%esmf_config_file = esmf_config

   end function new_ChildSpec
      

   pure logical function equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      equal = a%name == b%name
      if (.not. equal) return

      equal = equal_config(a%yaml_config_file, b%yaml_config_file)
      if (.not. equal) return

      equal = equal_config(a%esmf_config_file, b%esmf_config_file)
      if (.not. equal) return

      equal = (a%user_setservices == b%user_setservices)
      if (.not. equal) return
      
   contains

      pure logical function equal_config(a, b) result(equal)
         character(:), allocatable, intent(in) :: a
         character(:), allocatable, intent(in) :: b

         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) equal = (a == b)

      end function equal_config

   end function equal

   pure logical function not_equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

end module mapl3g_ChildSpec

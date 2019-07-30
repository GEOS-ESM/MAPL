!#include "MAPL_ErrLog.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"

module MAPL_NUOPCWrapperMod

  !-----------------------------------------------------------------------------
  ! ATM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
       model_routine_SS    => SetServices, &
       model_label_Advance => label_Advance
  use MAPL_CapGridCompMod
  use MAPL_CapMod
  use MAPL_BaseMod, only: MAPL_AllocateCoupling, MAPL_Communicators
  use MAPL_ErrorHandlingMod

  implicit none

  private

  public SetServices, cap_parameters, &
       get_cap_parameters_from_gc, set_cap_parameters_on_gc
  
  character(*), parameter :: internal_parameters_name = "cap_parameters"

  type :: Field_Attributes
     type(ESMF_Field) :: field
     character(len=ESMF_MAXSTR) :: short_name, long_name, units
  end type Field_Attributes

  type Cap_Wrapper
     type(MAPL_Cap), pointer :: ptr
  end type Cap_Wrapper  

  abstract interface 
     subroutine set_services_interface(gc, rc)
       import ESMF_GridComp
       type(ESMF_GridComp), intent(inout) :: gc
       integer, intent(out) :: rc
     end subroutine set_services_interface
  end interface

  ! Values needed to create CapGridComp. 
  type :: cap_parameters
     character(len=:), allocatable :: name, cap_rc_file
     procedure(set_services_interface), nopass, pointer :: set_services
  end type cap_parameters


  type :: cap_parameters_wrapper
     type(cap_parameters), pointer :: ptr
  end type cap_parameters_wrapper

#include "mpif.h"

contains


  ! set the class variables to default values since the wrapper doesn't interact with command-line
  ! arguments
  subroutine initialize_cap_default_values(cap)
    class(MAPL_Cap), intent(inout) :: cap
    integer :: ierror, npes_model
    call cap%set_n_members(1)
    call cap%set_npes_input_server(0)
    call cap%set_npes_output_server(0)
    call cap%set_nodes_input_server(0)
    call cap%set_nodes_output_server(0)

    call MPI_Comm_size(cap%get_comm_world(), npes_model, ierror)
    call cap%set_npes_model(npes_model)

    call cap%set_npes_member(cap%get_npes_model() / cap%get_n_members())
    call cap%set_ensemble_subdir_prefix("mem")
  end subroutine initialize_cap_default_values
  

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out

    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
         userRoutine = initialize_p0, phase = 0, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out


    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=["IPDv01p1"], userRoutine=advertise_fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out

    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=["IPDv01p2"], userRoutine=realize_fields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out


    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
         specRoutine = model_advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return  ! bail out

  end subroutine SetServices


  subroutine initialize_p0(model, import_state, export_state, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: import_state, export_state
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(MAPL_Cap), pointer :: cap
    type(Cap_Wrapper) :: wrapped_cap
    type(cap_parameters) :: cap_params

    type(ESMF_VM) :: vm
    integer :: my_rank, npes, mpi_comm, dup_comm, status, subcommunicator
    
    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(export_state)
    _UNUSED_DUMMY(clock)
    
    call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
         acceptStringList=(/"IPDv01p"/), rc=rc)

    call ESMF_GridCompGet(model, vm = vm, rc = status); _VERIFY(status)
    call ESMF_VMGet(vm, localPet = my_rank, petCount  = npes, mpiCommunicator = mpi_comm, rc = status); _VERIFY(status)
      
    call MPI_Comm_dup(mpi_comm, dup_comm, status); _VERIFY(status)

    cap_params = get_cap_parameters_from_gc(model, rc)

    allocate(cap)
    cap = MAPL_Cap(name = cap_params%name, set_services = cap_params%set_services, &
         comm = dup_comm, cap_rc_file = cap_params%cap_rc_file)
    
    call initialize_cap_default_values(cap)
    
    wrapped_cap%ptr => cap
    

    call ESMF_UserCompSetInternalState(model, "MAPL_Cap", wrapped_cap, status); _VERIFY(status)

    call cap%initialize_mpi(rc = status); _VERIFY(status)

    subcommunicator = cap%create_member_subcommunicator(cap%get_comm_world(), rc=status); _VERIFY(status)
    call cap%initialize_io_servers(subcommunicator, rc = status); _VERIFY(status)

    call cap%initialize_cap_gc(cap%get_mapl_comm())
    
    call cap%cap_gc%set_services(rc = status); _VERIFY(status)
    call cap%cap_gc%initialize(rc = status); _VERIFY(status)
        
    _RETURN(ESMF_SUCCESS)

  end subroutine initialize_p0


  subroutine advertise_fields(model, import_state, export_state, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: import_state, export_state
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(MAPL_Cap), pointer :: cap
    type(Field_Attributes), allocatable :: export_attributes(:), import_attributes(:)

    _UNUSED_DUMMY(clock)
    
    rc = ESMF_SUCCESS

    cap => get_cap_from_gc(model)
    export_attributes = get_field_attributes_from_state(cap%cap_gc%export_state)
    import_attributes = get_field_attributes_from_state(cap%cap_gc%import_state)

    call advertise_to_state(import_state, import_attributes)
    call advertise_to_state(export_state, export_attributes)

  contains

    subroutine advertise_to_state(state, fields)
      type(ESMF_State), intent(inout) :: state
      type(field_attributes), intent(in) :: fields(:)
      integer :: i, status

      do i = 1, size(fields)
         associate(short_name => fields(i)%short_name, units => fields(i)%units)
           if (.not. NUOPC_FieldDictionaryHasEntry(short_name)) then
              call NUOPC_FieldDictionaryAddEntry(standardName = trim(short_name), &
                   canonicalUnits = trim(units), rc = status)
              if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
                   line=__LINE__, file=__FILE__)) return
           end if

           call NUOPC_Advertise(state, StandardName = trim(short_name), &
                units = trim(units), rc = status)
           if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=__FILE__)) return
         end associate
      end do
    end subroutine advertise_to_state

  end subroutine advertise_fields


  subroutine realize_fields(model, import_state, export_state, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: import_state, export_state
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    type(MAPL_Cap), pointer :: cap
    type(Field_Attributes), allocatable :: export_attributes(:), import_attributes(:)
    integer :: i, status

    _UNUSED_DUMMY(clock)

    rc = ESMF_SUCCESS

    cap => get_cap_from_gc(model)
    export_attributes = get_field_attributes_from_state(cap%cap_gc%export_state)
    import_attributes = get_field_attributes_from_state(cap%cap_gc%import_state)

    do i = 1, size(export_attributes)
       associate(export => export_attributes(i))
         call MAPL_AllocateCoupling(export%field, status)
         call NUOPC_Realize(export_state, field = export%field, rc = status)
         if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return
       end associate
    end do

    do i = 1, size(import_attributes)
       associate(import => import_attributes(i))
         call ESMF_FieldValidate(import%field, rc = status)
         if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return
         call NUOPC_Realize(import_state, field = import%field, rc = status)
         if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=__FILE__)) return
       end associate
    end do

  end subroutine realize_fields


  subroutine fill_fields(model, import_state, export_state, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: import_state, export_state
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    integer :: num_items, i
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR), allocatable :: item_names(:)

    _UNUSED_DUMMY(model)
    _UNUSED_DUMMY(import_state)
    _UNUSED_DUMMY(clock)

    rc = ESMF_SUCCESS

    call ESMF_StateGet(export_state, itemcount = num_items, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return 
    allocate(item_names(num_items))

    call ESMF_StateGet(export_state, itemnamelist = item_names, rc = rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return 

    do i = 1, num_items
       call ESMF_StateGet(export_state, item_names(i), field, rc = rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return 
       call ESMF_FieldValidate(field, rc = rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
    end do

  end subroutine fill_fields


  subroutine model_advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    type(MAPL_Cap), pointer :: cap

    rc = ESMF_SUCCESS

    cap => get_cap_from_gc(model)
    call cap%step_model(rc = rc)

  end subroutine model_advance


  function get_cap_from_gc(gc) result(cap)
    type(ESMF_GridComp), intent(inout) :: gc
    type(MAPL_Cap), pointer :: cap
    type(Cap_Wrapper) :: wrapped_cap
    integer :: rc
    call ESMF_UserCompGetInternalState(gc, "MAPL_Cap", wrapped_cap, rc)
    cap => wrapped_cap%ptr
  end function get_cap_from_gc


  function get_field_attributes_from_state(state) result(attributes)
    type(Field_Attributes), allocatable :: attributes(:)
    type(ESMF_State), intent(in) :: state

    integer :: num_items, status, i
    type(ESMF_Field) :: field
    character(len=ESMF_MAXSTR), allocatable :: item_names(:)
    character(len=ESMF_MAXSTR) :: str

    call ESMF_StateGet(state, itemcount = num_items, rc = status)
    if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return 
    allocate(item_names(num_items))
    allocate(attributes(num_items))

    call ESMF_StateGet(state, itemnamelist = item_names, rc = status)
    if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return 

    do i = 1, num_items
       call ESMF_StateGet(state, item_names(i), field, rc = status)
       if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
       call ESMF_FieldValidate(field, rc = status)
       if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return
       attributes(i)%field = field        

       call ESMF_AttributeGet(field, name = "LONG_NAME", value = str, rc = status)
       if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return 
       attributes(i)%long_name = trim(str)

       call ESMF_FieldGet(field, name = str, rc = status)
       if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return 
       attributes(i)%short_name = trim(str)

       call ESMF_AttributeGet(field, name = "UNITS", value = str, rc = status)
       if (ESMF_LogFoundError(rcToCheck=status, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=__FILE__)) return 
       attributes(i)%units = trim(str)
    end do

  end function get_field_attributes_from_state


  function get_cap_parameters_from_gc(gc, rc) result(cap_params)
    type(cap_parameters) :: cap_params
    type(ESMF_GridComp), intent(inout) :: gc
    integer, intent(out) :: rc

    type(cap_parameters_wrapper) :: parameters_wrapper

    rc = ESMF_SUCCESS

    call ESMF_UserCompGetInternalState(gc, internal_parameters_name, parameters_wrapper, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return    
    cap_params = parameters_wrapper%ptr
  end function get_cap_parameters_from_gc


  subroutine set_cap_parameters_on_gc(gc, name, cap_rc_file, set_services, rc)
    type(ESMF_GridComp), intent(inout) :: gc
    character(*), intent(in) :: name, cap_rc_file
    procedure(set_services_interface) :: set_services
    integer, intent(out) :: rc

    type(cap_parameters_wrapper) :: wrapper

    rc = ESMF_SUCCESS

    allocate(wrapper%ptr)
    wrapper%ptr = cap_parameters(name, cap_rc_file, set_services)

    call ESMF_UserCompSetInternalState(gc, internal_parameters_name, wrapper, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, file=__FILE__)) return
  end subroutine set_cap_parameters_on_gc

end module MAPL_NUOPCWrapperMod

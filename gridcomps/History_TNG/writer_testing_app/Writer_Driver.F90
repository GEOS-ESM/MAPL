#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module synthetic_driver
    use ESMF
    use NUOPC
    use NUOPC_Driver, &
            driverSS => SetServices, &
            modelSS  => label_SetModelServices, &
            runSS    => label_SetRunSequence

    use MAPL
    use MAPL_NUOPCWrapperMod, only: wrapperSS => SetServices, init_wrapper
    ! use NUOPC_MAPLcapMod, only: wrapperSS => SetServices, init_internal_wrapper
    use NUOPC_Connector,  only: cplSS     => SetServices

    use NUOPC_HistoryCapMod, only: GeosSS => setServices, initialize_HistoryCap_wrapper
    use HistoryWriter_GridComp, HistWtrSS => setServices
    use Provider_GridCompMod, only: providerSS => SetServices
    !use Reciever_GridCompMod, only: recieverSS => SetServices

    use FieldRegistryMod
    use CollectionRegistryMod
    use COllectionMod
    use HistoryConfigMod
    use yafYaml
    use gFTL_StringVector
    implicit none
    private

    public SetServices
contains
    subroutine SetServices(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc
        type(ESMF_Config)    :: config

        print*, "Driver start SetServices"
        call ESMF_LogWrite("Driver start SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        ! NUOPC_Driver registers the generic methods
        print*,"Driver add Generic SetServices"
        call ESMF_LogWrite("Driver add Generic SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call NUOPC_CompDerive(driver, driverSS, rc=rc)
        VERIFY_NUOPC_(rc)

        ! attach specializing method(s)
        print*,"Driver add SetModelServices"
        call ESMF_LogWrite("Driver add SetModelServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call NUOPC_CompSpecialize(driver, specLabel=modelSS, &
                specRoutine=SetModelServices, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Driver add Set Run Sequence"
        call ESMF_LogWrite("Driver add Set Run Sequence", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call NUOPC_CompSpecialize(driver, specLabel=runSS, &
                specRoutine=SetRunSequence, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Driver create config"
        call ESMF_LogWrite("Driver create config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        config = ESMF_ConfigCreate(rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Driver read config"
        call ESMF_LogWrite("Driver read config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_ConfigLoadFile(config, "NUOPC_run_config.txt", rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Driver add config"
        call ESMF_LogWrite("Driver add config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_GridCompSet(driver, config=config, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "Driver finish SetServices"
        call ESMF_LogWrite("Driver finish SetServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine SetServices

    subroutine SetModelServices(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc

        type(ESMF_GridComp) :: provider!, reciever
        type(ESMF_GridComp), allocatable :: reciever(:)
        type(ESMF_CplComp)  :: connector
        type(ESMF_VM)       :: vm
        type(ESMF_Config)   :: config

        logical              :: seq
        integer              :: i, n, npes, n_provider_pes, n_reciever_pes
        integer, allocatable :: provider_petlist(:), reciever_petlist(:,:)
        type(FieldRegistry)  :: registry
        type(CollectionRegistry)  :: coll_registry
        type(Collection) :: hist_coll
        type(HistoryConfig) :: hist_config
        type(Configuration) :: yaml_config
        type(Parser)     :: P
        type(FileStream) :: file_stream
        integer :: n_collection
        type(StringVector) :: enabled
        type(StringVectorIterator) :: iter
        character(len=1) :: ci
        character(:), allocatable :: key

        P           = Parser('core')
        file_stream  = FileStream("driver_hist.yaml")
        yaml_config = P%load(file_stream)
        call hist_config%import_yaml(yaml_config,rc=rc)
        VERIFY_ESMF_(rc)
        call hist_config%finish_import()
        registry=hist_config%get_fields()
        call registry%register()
        enabled=hist_config%get_enabled()
        n_collection=enabled%size()
        coll_registry=hist_config%get_collections()
        
        
        print*, "Driver start Set Model Services"
        call ESMF_LogWrite("Driver start SetModelServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*,"Driver run set_clock"
        call ESMF_LogWrite("Driver run set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call set_clock(driver)

        print*,"Driver read the config"
        call ESMF_LogWrite("Driver read the config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_GridCompGet(driver, vm=vm, config=config, rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Driver read npes"
        call ESMF_LogWrite("Driver read npes", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_VMGet(vm, petCount=npes, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Driver read values from config"
        call ESMF_LogWrite("Driver values from config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        print*,"Driver read mode"
        call ESMF_ConfigGetAttribute(config, seq, label="sequential:", rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Driver read n providers"
        call ESMF_ConfigGetAttribute(config, n_provider_pes, &
                label="AGCM_pets:", rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Driver read n recievers"
        call ESMF_ConfigGetAttribute(config, n_reciever_pes, &
                label="reciever_pets:", rc=rc)
        VERIFY_NUOPC_(rc)
        ! call NUOPC_CompAttributeSet(driver, 'Diagnostic', 'max', rc=rc)
        ! VERIFY_NUOPC_(rc)

        print*,"Driver create pet lists"
        call ESMF_LogWrite("Driver create pet lists", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        allocate(provider_petlist(n_provider_pes))
        allocate(reciever_petlist(n_collection,n_reciever_pes))

        if (seq) then
            !_ASSERT((n_provider_pes == n_reciever_pes), "provider_pets must be equal to reciever_pets in sequential")

            !provider_petlist = [(i, i = 0, n_provider_pes - 1)]
            !reciever_petlist = [(i, i = 0, n_reciever_pes - 1)]
        else
            _ASSERT(((n_provider_pes + n_reciever_pes*n_collection) == npes), "provider_pets + reciever_pets must be equal to number of pets")

            provider_petlist = [(i, i = 0, n_provider_pes - 1)]
            reciever_petlist(1,:) = [(i,i=n_provider_pes,n_provider_pes+n_reciever_pes-1)]
            do n=2,n_collection
               reciever_petlist(2,:) = [(i, i = reciever_petlist(n-1,n_reciever_pes)+1 , &
                            reciever_petlist(n-1,n_reciever_pes)  + n_reciever_pes)]
            enddo

            print*,"Conncurrent mode"
        end if

        print*, "Sequential? :", seq
        print*, "Provider_petlist:", provider_petlist
        print*, "Reciever_petlist:", reciever_petlist

        print*,"Driver add provider"
        call ESMF_LogWrite("Driver add provider", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call NUOPC_DriverAddComp(driver, "AGCM", GeosSS, comp=provider, &
                petlist=provider_petlist, rc=rc)
        VERIFY_NUOPC_(rc)
        print*,"Driver wrap provider MAPL"
        call ESMF_LogWrite("Driver wrap provider MAPL", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        call NUOPC_CompAttributeSet(provider, 'Verbosity', 'high', rc=rc)
        VERIFY_NUOPC_(rc)

        call initialize_HistoryCap_wrapper(provider,"AGCM","PROVIDER_CAP.rc",providerSS,registry,.true.,rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Driver add reciever"
        allocate(reciever(n_collection))
        iter=enabled%begin()
        i=0
        do while (iter /= enabled%end())
           i=i+1
           key=iter%get()
           hist_coll=coll_registry%at(key)
           write(*,*)"bmaa writing: ","reciever_"//trim(key)
           call NUOPC_DriverAddComp(driver, "reciever_"//trim(key), HistWtrSS, comp=reciever(i), &
                   petlist=reciever_petlist(i,:), rc=rc)
           VERIFY_NUOPC_(rc)
           print*,"Driver add reciever"
           !call NUOPC_DriverAddComp(driver, "reciever", HistWtrSS, comp=reciever, &
                   !petlist=reciever_petlist(i,:), rc=rc)
           call initialize_WriterGC_Wrapper(reciever(i),hist_coll,rc=rc)
           VERIFY_NUOPC_(rc)
           call NUOPC_DriverAddComp(driver, srcCompLabel="AGCM", dstCompLabel="reciever_"//trim(key), &
                   compSetServicesRoutine=cplSS, comp=connector, rc=rc)
           VERIFY_NUOPC_(rc)
           call iter%next()
        enddo

        print*, "Driver finish SetModelServices"
        call ESMF_LogWrite("Driver finish SetModelServices", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine SetModelServices

    subroutine set_clock(driver)
        type(ESMF_GridComp), intent(inout) :: driver

        type(ESMF_Time)         :: startTime
        type(ESMF_Time)         :: stopTime
        type(ESMF_TimeInterval) :: timeStep
        type(ESMF_Clock)        :: internalClock
        type(ESMF_Config)       :: config

        integer :: start_date_and_time(2), end_date_and_time(2), dt, file_unit, yy, mm, dd, h, m, s, rc

        print*, "Driver start set_clock"
        call ESMF_LogWrite("Driver start set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*, "Driver read start time"
        call ESMF_LogWrite("Driver read start time", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        ! Read the start time
        open(newunit = file_unit, file = "cap_restart", form = 'formatted', &
                status = 'old', action = 'read')
        read(file_unit, *) start_date_and_time
        close(file_unit)

        print*, "Driver unpack start time"
        call ESMF_LogWrite("Driver unpack start time", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call UnpackDateTime(start_date_and_time, yy, mm, dd, h, m, s)
        ! Set the start time
        print*, "Driver set start time"
        call ESMF_LogWrite("Driver set start time", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_TimeSet(startTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, &
                calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "Driver read the config"
        call ESMF_LogWrite("Driver read the config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_GridCompGet(driver, config = config, rc = rc)
        VERIFY_NUOPC_(rc)

        ! Read the end time
        print*, "Driver read end time"
        call ESMF_LogWrite("Driver read end time", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_ConfigGetAttribute(config, end_date_and_time(1), &
                label="end_date:", rc=rc)
        VERIFY_NUOPC_(rc)
        call ESMF_ConfigGetAttribute(config, end_date_and_time(2), &
                label="end_time:", rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "Driver unpack end time"
        call ESMF_LogWrite("Driver unpack end time", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call UnpackDateTime(end_date_and_time, yy, mm, dd, h, m, s)
        ! Set the end time
        print*, "Driver set end time"
        call ESMF_LogWrite("Driver set end time", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_TimeSet(stopTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, &
                calkindflag=ESMF_CALKIND_GREGORIAN, rc=rc)
        VERIFY_NUOPC_(rc)

        ! Read the interpolation time interval
        print*, "Driver read interpolation interval"
        call ESMF_LogWrite("Driver read interpolation interval", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_ConfigGetAttribute(config, dt, label="interpolation_dt:", rc = rc)
        VERIFY_NUOPC_(rc)

        ! Set the time interval
        print*, "Driver create time interval"
        call ESMF_LogWrite("Driver create time interval", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_TimeIntervalSet(timeStep, s=dt, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "Driver create the clock"
        call ESMF_LogWrite("Driver create the clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        internalClock = ESMF_ClockCreate(name="Driver Clock", timeStep=timeStep, &
                startTime=startTime, stopTime=stopTime, rc=rc)
        VERIFY_NUOPC_(rc)

        ! Set the driver clock
        print*, "Driver set the clock"
        call ESMF_LogWrite("Driver set the clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_GridCompSet(driver, clock=internalClock, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "Driver finish set_clock"
        call ESMF_LogWrite("Driver finish set_clock", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
    contains
        subroutine UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
            integer, intent(IN   ) :: DATETIME(:)
            integer, intent(  OUT) :: YY, MM, DD, H, M, S

            YY =     datetime(1)/10000
            MM = mod(datetime(1),10000)/100
            DD = mod(datetime(1),100)
            H  =     datetime(2)/10000
            M  = mod(datetime(2),10000)/100
            S  = mod(datetime(2),100)
            return
        end subroutine UnpackDateTime
    end subroutine set_clock

    subroutine SetRunSequence(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Time)         :: startTime, stopTime
        type(ESMF_TimeInterval) :: timeStep
        type(ESMF_Config)       :: config
        type(NUOPC_FreeFormat)  :: run_sequence_ff

        print*, "Driver start SetRunSequence"
        call ESMF_LogWrite("Driver start SetRunSequence", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        print*,"Driver read the config"
        call ESMF_LogWrite("Driver read the config", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call ESMF_GridCompGet(driver, config=config, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Driver read run sequence"
        call ESMF_LogWrite("Driver read run sequence", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        run_sequence_ff = NUOPC_FreeFormatCreate(config, label="run_sequence::", rc=rc)
        VERIFY_NUOPC_(rc)

        ! ingest FreeFormat run sequence
        print*,"Driver ingest run sequence"
        call ESMF_LogWrite("Driver ingest run sequence", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call NUOPC_DriverIngestRunSequence(driver, run_sequence_ff, rc=rc)
        VERIFY_NUOPC_(rc)

        print*,"Driver destroy run sequence"
        call ESMF_LogWrite("Driver destroy run sequence", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)
        call NUOPC_FreeFormatDestroy(run_sequence_ff, rc=rc)
        VERIFY_NUOPC_(rc)

        print*, "Driver finish SetRunSequence"
        call ESMF_LogWrite("Driver finish SetRunSequence", ESMF_LOGMSG_INFO, rc=rc)
        VERIFY_ESMF_(rc)

        _RETURN(_SUCCESS)
    end subroutine SetRunSequence
end module synthetic_driver

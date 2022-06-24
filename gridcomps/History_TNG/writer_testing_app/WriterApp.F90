#include "NUOPC_ErrLog.h"

program SyntheticApp
    use ESMF
    use NUOPC
    use, intrinsic :: iso_fortran_env,  only: INT64

    use synthetic_driver, only: driverSS => SetServices

    implicit none
    include "mpif.h"

    integer             :: rc, urc, rank, file_unit
    integer(kind=INT64) :: t0, t1, count_rate
    real                :: elapsed_time
    type(ESMF_GridComp) :: esmComp

    print*,"start"

    call mpi_init(rc)
    VERIFY_ESMF_(rc)

    call mpi_comm_rank(MPI_COMM_WORLD, rank, rc)
    VERIFY_ESMF_(rc)

    if (rank == 0) then
        call system_clock(t0)
    end if

    ! Initialize ESMF
    print*,"ESMF initialize"
    call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, rc=rc)
    VERIFY_ESMF_(rc)

    print*,"ESMF log start"
    call ESMF_LogWrite("esmApp STARTING", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)

    ! Create the earth system Component
    print*,"ESMF create ESM"
    call ESMF_LogWrite("esmApp create ESM", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)
    esmComp = ESMF_GridCompCreate(name="esm", rc=rc)
    VERIFY_ESMF_(rc)

    ! SetServices for the earth system Component
    print*,"ESM Set Services"
    call ESMF_LogWrite("esmApp ESM Set Services", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)
    call ESMF_GridCompSetServices(esmComp, driverSS, userRc=urc, rc=rc)
    VERIFY_ALL_ESMF_(rc, urc)

    ! Call Initialize for the earth system Component
    print*,"ESM Initialize"
    call ESMF_LogWrite("esmApp ESM Initalize", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)
    call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
    VERIFY_ALL_ESMF_(rc, urc)

    ! Call Run  for earth the system Component
    print*,"ESM Run"
    call ESMF_LogWrite("esmApp ESM Run", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)
    call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)
    VERIFY_ALL_ESMF_(rc, urc)

    if (rank == 0) then
        call system_clock(t1, count_rate)
        open(newunit = file_unit, file = "elapsed_time.txt", &
                status = "replace", action = "write")
        elapsed_time = (t1 - t0) / real(count_rate)
        write(file_unit, '(f0.0)') elapsed_time
        close(file_unit)
    end if

    ! Call Finalize for the earth system Component
    print*,"ESM Finalize"
    call ESMF_LogWrite("esmApp ESM Finalize", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)
    call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)
    VERIFY_ALL_ESMF_(rc, urc)

    ! Destroy the earth system Component
    print*,"ESM Destroy"
    call ESMF_LogWrite("esmApp ESM Destroy", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)
    call ESMF_GridCompDestroy(esmComp, rc=rc)
    VERIFY_ESMF_(rc)

    CALL MPI_Comm_set_errhandler(MPI_COMM_WORLD,MPI_ERRORS_RETURN,rc)
    print*,"ESMF log finish"
    call ESMF_LogWrite("esmApp FINISHED", ESMF_LOGMSG_INFO, rc=rc)
    VERIFY_ESMF_(rc)

    ! Finalize ESMF
    print*,"ESMF finalize"
    call ESMF_Finalize()

    print*,"complete"
end program SyntheticApp

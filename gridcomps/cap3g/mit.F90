program main

   ...

   mode = get_mode(...)  ! geos, pfio, or mit (server)


   call mode%run()


   ...


   subroutine run_mit_server(...)
      call mit_entry_point(comm_mit_plus_geos)
      call mit_hconfig%set(shared_comm, comm_mit_plus_geos)
   end subroutine run_mit_server


   subroutine run_geos(...)
      ...
      call hconfig%get(comm_mit_plus_geos)
      call mit_entry_point(comm_mit_plus_geos)


      call ESMF_Initialize(cap_gc)
          ...
          call init_GuestOcean(...)
             call mit_entry_point(comm_mit_plus_geos)


          
      call ESMF_Run(cap_gc)
      call ESMF_Finalize(cap_gc)
   end subroutine run_geos

   subroutine pfio(...)

     

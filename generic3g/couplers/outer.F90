

   type(ObserverPtrVector) :: export_couplers
   type(ObserverPtrVector) :: import_couplers

   ! Connect E --> I

   sequence = cplr(E, I)
   call src_comp%add_export_coupler(sequence%first())
   call dst_comp%add_import_coupler(sequence%last())
   

   ! (1) Trivial case:
   ! No need to add coupler
   ! I and E share field

   ! (2) Regrid

   cplr = Regrid(E, I)
   call src_comp%add_export_coupler(cplr)
   call dst_comp%add_import_coupler(cplr)


   ! (3) Change units and then regrid

   cplr1 = ChangeUnits(E, E1)
   cplr2 = Regrid(E1, I)
   call cplr2%add_import(cplr1)
   call cplr1%add_export(cplr2)

   call src_comp%add_export_coupler(cplr1)
   call dst_comp%add_import_coupler(cplr2)

   ! dst comp runs
   call update_all(dst_comp%import_couplers)
     ! triggers
     call update(cplr1) ! change units
     call update(cplr2) ! regrid


     ! parent is "this"
     coupler = this%registry%connect(C1:E, C2:I)

     export_cplrs = this%get_export_couplers(c1)
     import_cplrs => this%get_import_couplers(c2)

     export_cplr => export_cplrs(E)
     import_cplr => import_cplrs(I)

     call import_cplr%add_import(export_cplr) ! does not work for complex sequence
     call export_cplr%add_import(import_cplr) 


     ! coupler includes import dependencies

     ! always a new cplr for given import - it can only connect once.
     ! (except wildcards)
     import_cplrs = this%get_import_couplers(C2) ! imports of child C2
     call import_cplrs%push_back(coupler) ! careful not to break internal pointers!
     
     call i
     cplr => this%export_couplers%at(E, _RC) ! extends mapping
     if (cplr%size() == 0) then
        cplr%
        call cplr%add_export(new_couplers%first())

     ! Child C1 gets the extensions
     
     


        couplers is 




        subroutine connect(C_e, e, C_i, i)

           coupler_0 => C_e%export_couplers(e) ! possibly null()

           e_0 = e
           do while (e_0 /= i)
              e_1 => connect_one_step(e_0, i)
              coupler_1 => NewCoupler(e_0, e_1)
              call coupler_1%add_import(coupler_0)
              call coupler_0%add_export(coupler_1)

              e_0 => e_1
              coupler_0 => coupler_1  ! memory leak
           end do

           if (.associated(coupler_c)) then
              call C_i%import_couplers%push_back(Ptr(last_coupler)
           end if

        

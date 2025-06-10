!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF, DS
!
integer function io_VPRJ_pwscf(ID)
 !
 use pars,             ONLY: LP, SP, DP, IP_YIO, LP_YIO, CR_YIO
 use stderr,           ONLY: intc
 use IO_m,             ONLY: io_sec,read_is_on,write_is_on
 use IO_int,           ONLY: io_connect,io_disconnect, &
&                            io_elemental,io_bulk,io_header, &
&                            def_variable_bulk, io_variable_bulk
 use D_lattice,        ONLY: n_atoms,n_atomic_species
 use electrons,        ONLY: l_spin_orbit,n_spinor,n_sp_pol
 !
 use pseudo,           ONLY: PP_uspp_alloc,PP_uspp_free,qe_pseudo_alloc
 use qe_pseudo_m,      ONLY: nspin, nspin_mag, noncolin, npol, nat, nsp, ityp, tau, zv, omega, &
&                            npwx, gcutw, gcutm, ecutwfc, ecutrho, lspinorb, tpiba, alat
 use us,               ONLY: nqxq, nqx, tab, qrad
 use uspp,             ONLY: nhtol, nhtoj, nhtolm, ijtoh, dvan, qq_at, indv,&
&                            qq_so, dvan_so, indv_ijkb0, nkb, nkbus, okvan,&
                             lpl, lpx, ap
 use uspp_param,       ONLY: upf, lmaxq, nbetam, nh, nhm, lmaxkb 
 !
 implicit none
 !
 integer,intent(in) :: ID
 ! 
 ! Work Space
 !
 integer :: is
 integer(LP) :: lvar
 logical :: ltmp
 integer :: var_id  ! Variable counter
 !
 ! Initialize variable counter
 var_id = 100
 !
 !io_VPRJ_pwscf=io_connect(desc="vproj_pp_pwscf",type=0,ID=ID)
 io_VPRJ_pwscf=io_connect(desc="uspp_pp_pwscf",type=0,ID=ID)
 if (io_VPRJ_pwscf/=0) goto 1
 !
 io_VPRJ_pwscf=io_header(ID,IMPOSE_SN=.true.)
 if (io_VPRJ_pwscf/=0) goto 1
 !
 ! Dimensions
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=5,MENU=0)
   call io_elemental(ID,L0=l_spin_orbit,DB_L0=lspinorb,CHECK=.true.,OP=(/"=="/))
   !
   ltmp=(n_spinor==2)
   call io_elemental(ID,L0=ltmp,DB_L0=noncolin,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=n_sp_pol,DB_I0=nspin_mag,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=n_atoms,DB_I0=nat,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,I0=n_atomic_species,DB_I0=nsp,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0)
 !
 ! system and lattice
 call def_variable_bulk(ID,"nspin",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nspin)
 var_id = var_id + 1
 call def_variable_bulk(ID,"npol",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=npol)
 var_id = var_id + 1
 call def_variable_bulk(ID,"npwx",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=npwx)
 var_id = var_id + 1
 call def_variable_bulk(ID,"omega",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=omega)
 var_id = var_id + 1
 call def_variable_bulk(ID,"alat",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=alat)
 var_id = var_id + 1
 call def_variable_bulk(ID,"tpiba",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=tpiba)
 var_id = var_id + 1
 call def_variable_bulk(ID,"gcutw",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=gcutw)
 var_id = var_id + 1
 call def_variable_bulk(ID,"ecutwfc",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=ecutwfc)
 var_id = var_id + 1
 call def_variable_bulk(ID,"gcutm",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=gcutm)
 var_id = var_id + 1
 call def_variable_bulk(ID,"ecutrho",var_id,(/1/),DP)
 call io_variable_bulk(ID,var_id,D0=ecutrho)
 var_id = var_id + 1

 !
 ! USPP param
 call def_variable_bulk(ID,"lmaxq",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=lmaxq) 
 var_id = var_id + 1
 call def_variable_bulk(ID,"lmaxkb",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=lmaxkb) 
 var_id = var_id + 1
 call def_variable_bulk(ID,"nbetam",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nbetam) 
 var_id = var_id + 1
 call def_variable_bulk(ID,"nhm",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nhm) 
 var_id = var_id + 1

 !
 ! USPP module
 call def_variable_bulk(ID,"nkb",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nkb)
 var_id = var_id + 1
 call def_variable_bulk(ID,"nkbus",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nkbus)
 var_id = var_id + 1
 !
 if (read_is_on(ID)) okvan=.true.
 !
 ! US module
 call def_variable_bulk(ID,"nqxq",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nqxq)
 var_id = var_id + 1
 call def_variable_bulk(ID,"nqx",var_id,(/1/),IP_YIO)
 call io_variable_bulk(ID,var_id,I0=nqx)
 var_id = var_id + 1
   !
endif
!
if (.not. any((/io_sec(ID,:)==2/))) goto 1
!
 !
 ! qe_pseudo_m
 call def_variable_bulk(ID,"ityp",var_id,shape(ityp),IP_YIO)
 call io_variable_bulk(ID,var_id,I1=ityp)
 var_id = var_id + 1
 call def_variable_bulk(ID,"tau",var_id,shape(tau),DP)
 call io_variable_bulk(ID,var_id,D2=tau)
 var_id = var_id + 1
 call def_variable_bulk(ID,"zv",var_id,shape(zv),DP)
 call io_variable_bulk(ID,var_id,D1=zv)
 var_id = var_id + 1
 !
 ! uspp_param
 call def_variable_bulk(ID,"nh",var_id,shape(nh),IP_YIO)
 call io_variable_bulk(ID,var_id,I1=nh)
 var_id = var_id + 1
 !
 specie_loop:&
 do is = 1, nsp
   !
   if (write_is_on(ID)) then
     lvar=0
     if (upf(is)%tvanp) lvar=1
   endif
   call def_variable_bulk(ID,"tvanp_"//trim(intc(is)),var_id,(/1/),LP_YIO)
   call io_variable_bulk(ID,var_id,L0=lvar)
   var_id = var_id + 1
   if (read_is_on(ID)) upf(is)%tvanp= (lvar==1)
   !
   if (write_is_on(ID)) then
     lvar=0
     if (upf(is)%has_so) lvar=1
   endif
   call def_variable_bulk(ID,"has_so_"//trim(intc(is)),var_id,(/1/),LP_YIO)
   call io_variable_bulk(ID,var_id,L0=lvar)
   var_id = var_id + 1
   if (read_is_on(ID)) upf(is)%has_so= (lvar==1)
   !
   call def_variable_bulk(ID,"nbeta_"//trim(intc(is)),var_id,(/1/),IP_YIO)
   call io_variable_bulk(ID,var_id,I0=upf(is)%nbeta)
   var_id = var_id + 1
   !
   if (read_is_on(ID)) then
     allocate(upf(is)%lll(upf(is)%nbeta))
   endif
   call def_variable_bulk(ID,"lll_"//trim(intc(is)),var_id,shape(upf(is)%lll),IP_YIO)
   call io_variable_bulk(ID,var_id,I1=upf(is)%lll)
   var_id = var_id + 1
   !
   if (l_spin_orbit) then
     if (read_is_on(ID)) then
       allocate(upf(is)%jjj(upf(is)%nbeta))
     endif
     call def_variable_bulk(ID,"jjj_"//trim(intc(is)),var_id,shape(upf(is)%jjj),DP)
     call io_variable_bulk(ID,var_id,D1=upf(is)%jjj)
     var_id = var_id + 1
   endif
   !
 enddo specie_loop
 !
 ! [FP]
#ifndef _USPP
 nhtoj=0.d0 ! This is set to zero since not initialised in non-USPP case
#endif
 ! [FP]
 !
 ! uspp
 call def_variable_bulk(ID,"indv",var_id,shape(indv),IP_YIO)
 call io_variable_bulk(ID,var_id,I2=indv)
 var_id = var_id + 1
 call def_variable_bulk(ID,"nhtol",var_id,shape(nhtol),IP_YIO)
 call io_variable_bulk(ID,var_id,I2=nhtol)
 var_id = var_id + 1
 call def_variable_bulk(ID,"nhtolm",var_id,shape(nhtolm),IP_YIO)
 call io_variable_bulk(ID,var_id,I2=nhtolm)
 var_id = var_id + 1
 call def_variable_bulk(ID,"ijtoh",var_id,shape(ijtoh),IP_YIO)
 call io_variable_bulk(ID,var_id,I3=ijtoh)
 var_id = var_id + 1
 call def_variable_bulk(ID,"indv_ijkb0",var_id,shape(indv_ijkb0),IP_YIO)
 call io_variable_bulk(ID,var_id,I1=indv_ijkb0)
 var_id = var_id + 1
 !
 call def_variable_bulk(ID,"qq_at",var_id,shape(qq_at),DP)
 call io_variable_bulk(ID,var_id,D3=qq_at)
 var_id = var_id + 1
 call def_variable_bulk(ID,"nhtoj",var_id,shape(nhtoj),DP)
 call io_variable_bulk(ID,var_id,D2=nhtoj)
 var_id = var_id + 1
 call def_variable_bulk(ID,"lpx",var_id,shape(lpx),IP_YIO)
 call io_variable_bulk(ID,var_id,I2=lpx)
 var_id = var_id + 1
 call def_variable_bulk(ID,"lpl",var_id,shape(lpl),IP_YIO)
 call io_variable_bulk(ID,var_id,I3=lpl)
 var_id = var_id + 1
 call def_variable_bulk(ID,"ap",var_id,shape(ap),DP)
 call io_variable_bulk(ID,var_id,D3=ap)
 var_id = var_id + 1
 !
 if (lspinorb) then
   call def_variable_bulk(ID,"dvan_so",var_id,[2, shape(dvan_so)],DP)
   call io_variable_bulk(ID,var_id,Z4=dvan_so)
   var_id = var_id + 1
   call def_variable_bulk(ID,"qq_so",var_id,[2, shape(dvan_so)],DP)
   call io_variable_bulk(ID,var_id,Z4=qq_so)
   var_id = var_id + 1
 else
   call def_variable_bulk(ID,"dvan",var_id,shape(dvan),DP)
   call io_variable_bulk(ID,var_id,D3=dvan)
   var_id = var_id + 1
 endif
 !
 ! us
 if (lmaxq > 0 ) then
   call def_variable_bulk(ID,"qrad",var_id,shape(qrad),DP)
   call io_variable_bulk(ID,var_id,D4=qrad)
   var_id = var_id + 1
 endif
 call def_variable_bulk(ID,"tab",var_id,shape(tab),DP)
 call io_variable_bulk(ID,var_id,D3=tab)
 var_id = var_id + 1
 !
1 call io_disconnect(ID=ID)
 !
end function io_VPRJ_pwscf

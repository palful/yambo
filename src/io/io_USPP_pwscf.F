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
 call def_variable_bulk(ID,"nspin",100,(/1/),IP_YIO)
 call io_variable_bulk(ID,100,I0=nspin)
 call def_variable_bulk(ID,"npol",101,(/1/),IP_YIO)
 call io_variable_bulk(ID,101,I0=npol)
 call def_variable_bulk(ID,"npwx",102,(/1/),IP_YIO)
 call io_variable_bulk(ID,102,I0=npwx)
 call def_variable_bulk(ID,"omega",103,(/1/),DP)
 call io_variable_bulk(ID,103,D0=omega)
 call def_variable_bulk(ID,"alat",104,(/1/),DP)
 call io_variable_bulk(ID,104,D0=alat)
 call def_variable_bulk(ID,"tpiba",105,(/1/),DP)
 call io_variable_bulk(ID,105,D0=tpiba)
 call def_variable_bulk(ID,"gcutw",106,(/1/),DP)
 call io_variable_bulk(ID,106,D0=gcutw)
 call def_variable_bulk(ID,"ecutwfc",107,(/1/),DP)
 call io_variable_bulk(ID,107,D0=ecutwfc)
 call def_variable_bulk(ID,"gcutm",108,(/1/),DP)
 call io_variable_bulk(ID,108,D0=gcutm)
 call def_variable_bulk(ID,"ecutrho",109,(/1/),DP)
 call io_variable_bulk(ID,109,D0=ecutrho)

 !
 ! USPP param
 call def_variable_bulk(ID,"lmaxq",110,(/1/),IP_YIO)
 call io_variable_bulk(ID,110,I0=lmaxq) 
 call def_variable_bulk(ID,"lmaxkb",111,(/1/),IP_YIO)
 call io_variable_bulk(ID,111,I0=lmaxkb) 
 call def_variable_bulk(ID,"nbetam",112,(/1/),IP_YIO)
 call io_variable_bulk(ID,112,I0=nbetam) 
 call def_variable_bulk(ID,"nhm",113,(/1/),IP_YIO)
 call io_variable_bulk(ID,113,I0=nhm) 

 !
 ! USPP module
 call def_variable_bulk(ID,"nkb",114,(/1/),IP_YIO)
 call io_variable_bulk(ID,114,I0=nkb)
 call def_variable_bulk(ID,"nkbus",115,(/1/),IP_YIO)
 call io_variable_bulk(ID,115,I0=nkbus)
 !
 if (read_is_on(ID)) okvan=.true.
 !
 ! US module
 call def_variable_bulk(ID,"nqxq",116,(/1/),IP_YIO)
 call io_variable_bulk(ID,116,I0=nqxq)
 call def_variable_bulk(ID,"nqx",117,(/1/),IP_YIO)
 call io_variable_bulk(ID,117,I0=nqx)
   !
endif
!
if (.not. any((/io_sec(ID,:)==2/))) goto 1
!
 !
 ! qe_pseudo_m
 call def_variable_bulk(ID,"ityp",118,shape(ityp),IP_YIO)
 call io_variable_bulk(ID,118,I1=ityp)
 call def_variable_bulk(ID,"tau",119,shape(tau),DP)
 call io_variable_bulk(ID,119,D2=tau)
 call def_variable_bulk(ID,"zv",120,shape(zv),DP)
 call io_variable_bulk(ID,120,D1=zv)
 !
 ! uspp_param
 call def_variable_bulk(ID,"nh",121,shape(nh),IP_YIO)
 call io_variable_bulk(ID,121,I1=nh)
 !
 specie_loop:&
 do is = 1, nsp
   !
   if (write_is_on(ID)) then
     lvar=0
     if (upf(is)%tvanp) lvar=1
   endif
   call def_variable_bulk(ID,"tvanp_"//trim(intc(is)),120+is,(/1/),LP_YIO)
   call io_variable_bulk(ID,120+is,L0=lvar)
   if (read_is_on(ID)) upf(is)%tvanp= (lvar==1)
   !
   if (write_is_on(ID)) then
     lvar=0
     if (upf(is)%has_so) lvar=1
   endif
   call def_variable_bulk(ID,"has_so_"//trim(intc(is)),130+is,(/1/),LP_YIO)
   call io_variable_bulk(ID,130+is,L0=lvar)
   if (read_is_on(ID)) upf(is)%has_so= (lvar==1)
   !
   call def_variable_bulk(ID,"nbeta_"//trim(intc(is)),140+is,(/1/),IP_YIO)
   call io_variable_bulk(ID,140+is,I0=upf(is)%nbeta)
   !
   if (read_is_on(ID)) then
     allocate(upf(is)%lll(upf(is)%nbeta))
   endif
   call def_variable_bulk(ID,"lll_"//trim(intc(is)),150+is,shape(upf(is)%lll),IP_YIO)
   call io_variable_bulk(ID,150+is,I1=upf(is)%lll)
   !
   if (l_spin_orbit) then
     if (read_is_on(ID)) then
       allocate(upf(is)%jjj(upf(is)%nbeta))
     endif
     call def_variable_bulk(ID,"jjj_"//trim(intc(is)),160+is,shape(upf(is)%jjj),DP)
     call io_variable_bulk(ID,160+is,D1=upf(is)%jjj)
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
 call def_variable_bulk(ID,"indv",200,shape(indv),IP_YIO)
 call io_variable_bulk(ID,200,I2=indv)
 call def_variable_bulk(ID,"nhtol",201,shape(nhtol),IP_YIO)
 call io_variable_bulk(ID,201,I2=nhtol)
 call def_variable_bulk(ID,"nhtolm",202,shape(nhtolm),IP_YIO)
 call io_variable_bulk(ID,202,I2=nhtolm)
 call def_variable_bulk(ID,"ijtoh",203,shape(ijtoh),IP_YIO)
 call io_variable_bulk(ID,203,I3=ijtoh)
 call def_variable_bulk(ID,"indv_ijkb0",204,shape(indv_ijkb0),IP_YIO)
 call io_variable_bulk(ID,204,I1=indv_ijkb0)
 !
 call def_variable_bulk(ID,"qq_at",205,shape(qq_at),DP)
 call io_variable_bulk(ID,205,D3=qq_at)
 call def_variable_bulk(ID,"nhtoj",206,shape(nhtoj),DP)
 call io_variable_bulk(ID,206,D2=nhtoj)
 call def_variable_bulk(ID,"lpx",207,shape(lpx),IP_YIO)
 call io_variable_bulk(ID,207,I2=lpx)
 call def_variable_bulk(ID,"lpl",208,shape(lpl),IP_YIO)
 call io_variable_bulk(ID,208,I3=lpl)
 call def_variable_bulk(ID,"ap",209,shape(ap),DP)
 call io_variable_bulk(ID,209,D3=ap)
 !
 if (lspinorb) then
   call def_variable_bulk(ID,"dvan_so",210,shape(dvan_so),DP)
   call io_variable_bulk(ID,210,Z4=dvan_so)
   call def_variable_bulk(ID,"qq_so",211,shape(qq_so),DP)
   call io_variable_bulk(ID,211,Z4=qq_so)
 else
   call def_variable_bulk(ID,"dvan",212,shape(dvan),DP)
   call io_variable_bulk(ID,212,D3=dvan)
 endif
 !
 ! us
 if (lmaxq > 0 ) then
   call def_variable_bulk(ID,"qrad",213,shape(qrad),DP)
   call io_variable_bulk(ID,213,D4=qrad)
 endif
 call def_variable_bulk(ID,"tab",214,shape(tab),DP)
 call io_variable_bulk(ID,214,D3=tab)
 !
1 call io_disconnect(ID=ID)
 !
end function io_VPRJ_pwscf

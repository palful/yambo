yambo: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; $(mk_src)
	@+X2DO="yambo"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(MAIN_LIBS_LD)"; $(mk_yambo)
#
# Yambo PROJECTS #
# 
yambo_magnetic: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_MAGNETIC -D_SC -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_SCLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_MAGNETIC -D_SC"; $(mk_src)
	@+X2DO="yambo_magnetic"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_SCLIBS_LD)"; ADF="-D_MAGNETIC -D_SC"; $(mk_yambo)
yambo_electric: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_ELECTRIC -D_SC -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_SCLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_ELECTRIC -D_SC"; $(mk_src)
	@+X2DO="yambo_electric"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_SCLIBS_LD)"; ADF="-D_ELECTRIC -D_SC"; $(mk_yambo)
yambo_kerr: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_RT -D_ELPH -D_KERR -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_RTLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_RT -D_ELPH -D_KERR"; $(mk_src)
	@+X2DO="yambo_kerr"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_RTLIBS_LD)";ADF="-D_RT -D_ELPH -D_KERR"; $(mk_yambo)
yambo_sc: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_SC -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_SCLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_SC"; $(mk_src)
	@+X2DO="yambo_sc"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_SCLIBS_LD)"; ADF="-D_SC"; $(mk_yambo)
yambo_pl: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_RT -D_SC -D_ELPH -D_PL -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_PLLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_RT -D_SC -D_ELPH -D_PL"; $(mk_src)
	@+X2DO="yambo_pl"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_PLLIBS_LD)"; ADF="-D_RT -D_SC -D_ELPH -D_PL"; $(mk_yambo)
yambo_rt: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_RT -D_ELPH -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_RTLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_RT -D_ELPH"; $(mk_src)
	@+X2DO="yambo_rt"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_RTLIBS_LD)"; ADF="-D_RT -D_ELPH"; $(mk_yambo)
yambo_nl: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; ADF="-D_DOUBLE -D_SLEPC_OFF" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT -D_ELPH -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_NLLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT -D_ELPH"; $(mk_src)
	@+X2DO="yambo_nl"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_NLLIBS_LD)"; ADF="-D_DOUBLE -D_SLEPC_OFF -D_NL -D_RT -D_ELPH"; $(mk_yambo)
yambo_ph: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_ELPH -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(MAIN_LIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_ELPH"; $(mk_src)
	@+X2DO="yambo_ph"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(MAIN_LIBS_LD)"; ADF="-D_ELPH"; $(mk_yambo)
yambo_qed: ext-libs
	@+LIBS2DO="$(INT_LIBS)"; DIR2GO="lib" ; VPATH="$(topdir)/lib" ; $(mk_internal_lib)
	@+LIBS2DO="$(DRIVER_LIBS)"; XPATH="driver"; VPATH="$(topdir)/driver"; ADF="-D_QED -D_RT -D_ELPH -D_yambo"; $(mk_driver_src)
	@+LIBS2DO="$(PJ_RTLIBS)"; XPATH="src"; VPATH="$(topdir)/src"; ADF="-D_QED -D_RT -D_ELPH"; $(mk_src)
	@+X2DO="yambo_qed"; XPATH="driver"; VPATH="$(topdir)/driver"; XLIBS="$(PJ_RTLIBS_LD)"; ADF="-D_QED -D_RT -D_ELPH"; $(mk_yambo)


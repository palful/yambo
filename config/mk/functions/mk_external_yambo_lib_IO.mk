define mk_external_yambo_lib_IO
 for ldir in $$LIBS2DO; do \
  if test ! -f "$(libdir)/$$ldir.a" ; then \
   echo " " ; \
   echo ">>>[Making $$ldir]<<<" ; \
   if test ! -d "$$DIR2GO/$$ldir" ; then mkdir -p "$$DIR2GO/$$ldir" ; fi ; \
   if [ "$(topdir)" != "$(prefix)" ] && [ -f $$VPATH/$$ldir/.objects ] ; then \
    cp $$VPATH/$$ldir/.objects $$DIR2GO/$$ldir ; \
   fi ; \
   ./sbin/driver.sh -d $$DIR2GO/$$ldir -t lib$$ldir.a -o .objects -m l -- "$(xcpp) $(precision) $$ADF" ; \
   cd $$DIR2GO/$$ldir ; $(MAKE) VPATH=$$VPATH/$$ldir || exit "$$?" ; cd ../../; \
  fi \
 done
endef

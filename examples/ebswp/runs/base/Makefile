EXEC = amak
ifeq ($(SAFE),TRUE)
	DIST = ../../../../src/
else
	DIST = ../../../../src/
endif
ARGS = -nox -iprint 50 

ifdef ComSpec
    RM=del /F /Q
else
    RM=rm -rf
endif

all: mpd
$(EXEC): $(DIST)$(EXEC).tpl
	ln -sf $(DIST)$(EXEC) $@

$(DIST)$(EXEC).tpl:
	$(MAKE) --directory=../../src


mpd: $(EXEC)
	./$(EXEC) $(ARGS)

proj: $(EXEC)
	#./$(EXEC) $(ARGS)
	cp amak.prj proj/data/
	cd proj
	./main


debug: $(EXEC)
	./$(EXEC) $(ARGS)

plot:
	R CMD BATCH plot.R

clean:
	@$(RM) $(EXEC)
	@$(RM) $(EXEC)  $(EXEC).[brces]* $(EXEC).*[0123456789] *.rpt *.log variance gradient.* *tmp
	@$(RM) admodel.*
	@$(RM) checkfile.rep
	@$(RM) mcout.rep
	@$(RM) plot.Rout
	@$(RM) Rplots.pdf
	@$(RM) For_R.rep
	@$(RM) Fprof.yld
	@$(RM) amak.prj
	@$(RM) amak.par
	@$(RM) SIS_out.rep
	@$(RM) mceval.dat


ifdef ComSpec
    RM=del /F /Q
    COPY=copy
else
    RM=rm -rf
    COPY=cp
endif

all: amak ms 

ms: ms.tpl 
	@admb -f ms.tpl 

amak: amak.tpl 
	@admb  amak.tpl 

clean:
	@$(RM) *.cpp
	@$(RM) *.htp
	@$(RM) *.obj

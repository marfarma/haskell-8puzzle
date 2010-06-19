PACKAGES=-package HGL-3.2.0.2 -package pure-priority-queue-0.14 \
		 -package array-0.3.0.0 -package containers-0.3.0.0

DIRS=-hidir bin -Isrc -dbin
O_LEVEL=-O2

HMFLAGS=$(PACKAGES) $(O_LEVEL) $(DIRS) 

default:
	hmake $(HMFLAGS) Main

clean:
	rm bin/*.hi bin/*.o bin/Main

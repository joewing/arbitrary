
#OPTIONS = -O2 -gnatwa -gnatybm

.PHONY:	all,	clean

all:
	gprbuild -p -PBuild.gpr

clean:
	gprclean -PBuild.gpr

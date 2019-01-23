
OPTIONS = -O2 -gnatwa -gnatybm

all:
	gprbuild -PBuild.gpr

clean:
	gprclean -PBuild.gpr

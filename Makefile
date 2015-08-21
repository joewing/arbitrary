
OPTIONS = -O2 -gnatwa -gnatybm

all:	e golden_ratio ln2 pi root2

e:
	gnatmake $(OPTIONS) e

golden_ratio:
	gnatmake $(OPTIONS) golden_ratio

ln2:
	gnatmake $(OPTIONS) ln2

pi:
	gnatmake $(OPTIONS) pi

root2:
	gnatmake $(OPTIONS) root2


clean:
	rm *.o *.ali e golden_ratio ln2 pi root2

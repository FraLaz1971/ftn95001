# ftn95001
fortran programs following F95 standard (and later)
### circle001.f95
!program which will ask the user for the x and y coordinates of three points and
!which will calculate the equation of the circle passing through those three points, namely
!(x − x0)**2 + (y − y0)**2 = r**2
!and then display the coordinates (x0, y0) of the centre of the circle and its radius, r.
!1. Read three sets of coordinates (x(1) , y(1) ), (x(2), y(2) ) and (x(3) , y(3))
!2. Calculate the equation of the circle using the procedure calculate circle
!3. Display the values a, b and r
! examples: (x0,y0) = (0,0)  r=1
! (x(1),y(1))=(1,0),(x(2),y(2))=(0,1),(x(3),y(3))=(-1,0)
!(x0,y0) = (2,0)  r=2
! (x(1),y(1))=(4,0),(x(2),y(2))=(2,2),(x(3),y(3))=(0,0)

### primo.f
C uses the simpleplot SIMPLE.dll FTN95 library (MS Windows or WINE)
C specify plotting scales
			CALL SCALES(0.0,10.0,1, 0.0,1.0,1)
C start picture and draw pair of axes
			CALL AXES7('x-axis','y-axis')
C draw curve of data
			CALL BRKNCV(XARR,YARR,NARR,0)
C terminate plotting
			CALL ENDPLT
### How to run the programs:
- on linux:
	make
	./circle001
- on android/termux
	make -f Makefile.termux
	./circle001
- on windows
  mingw32-make -f Makefile.ftn95 

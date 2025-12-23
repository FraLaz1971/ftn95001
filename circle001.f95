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
PROGRAM circle
IMPLICIT NONE
!
! This program calculates the equation of a circle passing
! through three points
!
! Variable declarations
!
REAL :: x(3), y(3), a, b, r
!
! Step 1
!
PRINT *, "Please type the coordinates of three points"
PRINT *, "in the order x(1), y(1), x(2), y(2), x(3), y(3)"
PRINT *, "separated by commas"
READ *, x(1), y(1), x(2), y(2), x(3), y(3)
 ! Read the three points
!
! Step 2
!
CALL calculate_circle(x, y, a, b, r)
!
! Step 3
!
PRINT *, "The centre of the circle through these points is &
&(", a, ",", b, ")"
PRINT *, "Its radius is ", r
!
END PROGRAM circle

SUBROUTINE calculate_circle(x, y, a, b, r)
IMPLICIT NONE
!(x - x0)**2 + (y - y0)**2 = r**2
!(x +2a)**2 + (y +2b)**2 = r**2
! x(1)**2 + x0**2 -2*x0*x(1) + y(1)**2 + y0**2 -2*y0*y(1) = r**2
! x(2)**2 + x0**2 -2*x0*x(2) + y(2)**2 + y0**2 -2*y0*y(2) = r**2
! x(3)**2 + x0**2 -2*x0*x(3) + y(3)**2 + y0**2 -2*y0*y(3) = r**2
!
! -2*x0*x(1) -2*y0*y(1) + y(1)**2 + x(1)**2 + x0**2 + y0**2  =  r**2
! -2*x0*x(2) -2*y0*y(2) + y(2)**2 + x(2)**2 + x0**2 + y0**2  =  r**2
! -2*x0*x(3) -2*y0*y(3) + y(3)**2 + x(3)**2 + x0**2 + y0**2  =  r**2
! * x^2 + y^2 + x0*x + y0*y + c = 0 
! Circle of centre C(x0, y0) and radius r
! (x - x0)**2 + (y - y0)**2 = r**2
! a = -2*x0 ---> x0 = -a/2
! b = -2*y0 ---> y0 = -b/2
! c = x0**2  + y0**2 -r**2 ---> r**2 = x0**2  + y0**2 - c --> r = sqrt(x0**2  + y0**2 - c) 
! x**2 + y**2 + a*x + b*y + c = 0 
! x(1)**2 + y(1)**2 +a*x(1)+b*y(1) + c = 0
! x(2)**2 + y(2)**2 +a*x(2)+b*y(2) + c = 0
! x(3)**2 + y(3)**2 +a*x(3)+b*y(3) + c = 0
REAL :: x(3), y(3), x0, y0, r
REAL :: a,b,c,m(4,3)
INTEGER :: i
do i=1,3
  print 100,x(i),y(i),1.0,-1*(x(i)**2 + y(i)**2)
end do
do i=1,3
  m(1,i)=x(i)
  m(2,i)=y(i)
  m(3,i)=1.0
  m(4,i)=-1*(x(i)**2 + y(i)**2)
end do
call show_matrix(m,4,3)
100 format(F8.2,'*a+',F8.2,'*b +',F8.2,'*c =',F8.2)
stop
END SUBROUTINE calculate_circle
!
!
!
SUBROUTINE show_matrix(m,ncols,nrows)
implicit none
integer ncols,nrows,i,j
character*36 line
REAL :: m(ncols,nrows)
do j=1,nrows
  do i=1,ncols
	!    print *,'writing on ',(i-1)*9+1,':',(i-1)*9+9
	write(line((i-1)*9+1:(i-1)*9+9),fmt='(F8.2,1X)') m(i,j)
  end do
    print *,line
    line = ' '
end do
END SUBROUTINE

!program which will ask the user for the x and y coordinates of three points and
!which will calculate the equation of the circle passing through those three points, namely
!(x − a)**2 + (y − b)**2 = r**2
!and then display the coordinates (a, b) of the centre of the circle and its radius, r.
!Analysis (2.1)
!Structure plan:
!1. Read three sets of coordinates (x(1) , Y(1) ), (x(2), Y(2) ) and (x(3) , Y(3))
!2. Calculate the equation of the circle using the procedure calculate circle
!3. Display the values a, b and r
! examples: (a,b) = (0,0)  r=1
! (x(1),Y(1))=(1,0),(x(2),Y(2))=(0,1),(x(3),Y(3))=(-1,0)
!(a,b) = (2,0)  r=2
! (x(1),Y(1))=(4,0),(x(2),Y(2))=(0,2),(x(3),Y(3))=(0,-2)
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
PRINT *, "in the order x(1), Y(1), x(2), Y(2), x(3), Y(3)"
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
!(x - a)**2 + (y - b)**2 = r**2
!(x +2alf)**2 + (y +2bet)**2 = r**2
! x(1)**2 + a**2 -2*a*x(1) + Y(1)**2 + b**2 -2*b*Y(1) = r**2
! x(2)**2 + a**2 -2*a*x(2) + Y(2)**2 + b**2 -2*b*Y(2) = r**2
! x(3)**2 + a**2 -2*a*x(3) + Y(3)**2 + b**2 -2*b*Y(3) = r**2
!
! -2*a*x(1) -2*b*Y(1) + Y(1)**2 + x(1)**2 + a**2 + b**2  =  r**2
! -2*a*x(2) -2*b*Y(2) + Y(2)**2 + x(2)**2 + a**2 + b**2  =  r**2
! -2*a*x(3) -2*b*Y(3) + Y(3)**2 + x(3)**2 + a**2 + b**2  =  r**2
! * x^2 + y^2 + a*x + b*y + c = 0 
! Circle of centre C(x_0, y_0) and radius r
! (x - a)**2 + (y - b)**2 = r**2
! alf = -2a ---> a = -alf/2
! bet = -2b ---> b = -bet/2
! gam = a**2  + b**2 -r**2 ---> r**2 = a**2  + b**2 - gam --> r = sqrt(a**2  + b**2 - gam) 
! x**2 + y**2 + alf*x + bet*y + gam = 0 
! x(1)**2 + Y(1)**2 +alf*x(1)+bet*Y(1) + gam = 0
! x(2)**2 + Y(2)**2 +alf*x(2)+bet*Y(2) + gam = 0
! x(3)**2 + Y(3)**2 +alf*x(3)+bet*Y(3) + gam = 0
REAL :: x(3), y(3), a, b, r
REAL :: alf,bet,gam,m(4,3)
INTEGER :: i
print 100,x(1),Y(1),1.0,-1*(x(1)**2 + Y(1)**2)
print 100,x(2),Y(2),1.0,-1*(x(2)**2 + Y(2)**2)
print 100,x(3),Y(3),1.0,-1*(x(3)**2 + Y(3)**2)
do i=1,3
  m(1,i)=x(i)
  m(2,i)=y(i)
  m(3,i)=1.0
  m(4,i)=-1*(x(i)**2 + Y(i)**2)
end do
call show_matrix(m,4,3)
100 format(F8.2,'*alf+',F8.2,'*bet +',F8.2,'*gam =',F8.2)
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

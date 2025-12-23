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
REAL :: x(3), y(3), x0, y0, r
INTEGER :: i,j
LOGICAL DEBUG
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
CALL calculate_circle(x, y, x0, y0, r)
!
! Step 3
!
PRINT *, "The centre of the circle through these points is &
&(", x0, ",", y0, ")"
PRINT *, "Its radius is ", r
!
END PROGRAM circle

SUBROUTINE calculate_circle(x, y, x0, y0, r)
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
! c = x0**2  + y0**2 -r**2 ---> r**2 = x0**2  + y0**2 - c --> 
! r = sqrt(x0**2  + y0**2 - c) 
! x**2 + y**2 + a*x + b*y + c = 0 
! x(1)**2 + y(1)**2 +a*x(1)+b*y(1) + c = 0
! x(2)**2 + y(2)**2 +a*x(2)+b*y(2) + c = 0
! x(3)**2 + y(3)**2 +a*x(3)+b*y(3) + c = 0
! m(3,3) is the coefficients matrix and v(3)
! is the coefficient of the known terms 
REAL :: x(3), y(3), x0, y0, r
REAL :: a,b,c,m(3,3),v(3),det,mydet,mydet1,mydet2,mydet3
! arrays for the solutions
REAL :: ms1(3,3),ms2(3,3),ms3(3,3)
INTEGER :: i,j
do i=1,3
  print 100,x(i),y(i),1.0,-1*(x(i)**2 + y(i)**2)
end do
do i=1,3
  m(1,i)=x(i)
  m(2,i)=y(i)
  m(3,i)=1.0
  v(i)=-1*(x(i)**2 + y(i)**2)
end do
call show_system(m,3,3,v)
mydet=det(m,3,3)
print 200,mydet
if (mydet.eq.0) then
  print *,'The equation system cannot be solved'
else
	!        /* create matrix of the first unknown variable */
	do j=1,3
		ms1(1,j)=v(j)
	end do
	do j=1,3
	  do i=2,3
	    ms1(i,j)=m(i,j)
	  end do
	end do
	!        /* create matrix of the second unknown variable */
	do j=1,3
		ms2(1,j)=m(1,j)
	end do
	do j=1,3
		ms2(2,j)=v(j)
	end do
	do j=1,3
		ms2(3,j)=m(3,j)
	end do
	!        /* create matrix of the third unknown variable */
	do j=1,3
	  do i=1,2
	    ms3(i,j)=m(i,j)
	  end do
	end do
	do j=1,3
		ms3(3,j)=v(j)
	end do
	!        /* compute and shows determinant of first unknown variable a */
	mydet1=det(ms1,3,3)
	print 200,mydet1
	!        /* compute and shows determinant of second unknown variable b */
	mydet2=det(ms2,3,3)
	print 200,mydet2
	!        /* compute and shows determinant of third unknown variable c */
	mydet3=det(ms3,3,3)
	print 200,mydet3
	! compute solutions
	a=mydet1/mydet;
	b=mydet2/mydet;
	c=mydet3/mydet;
	print 300,a
	print 310,b
	print 320,c
	x0=a/(-2.0)
	y0=b/(-2.0)
	print 330,x0,y0
	r=sqrt(x0**2+y0**2-c)
	print 340,r
end if

100 format(F5.1,'*a+',F5.1,'*b +',F5.1,'*c =',F5.1)
200 format('determinant of m =',F6.1)
300 format('the value of a is ',F5.1)
310 format('the value of b is ',F5.1)
320 format('the value of c is ',F5.1)
330 format('the centre is (',F5.1,',',F5.1,')')
340 format('the radius r is ',F5.1)
stop
END SUBROUTINE calculate_circle
!
!
!
SUBROUTINE show_system(m,ncols,nrows,v)
implicit none
integer ncols,nrows,i,j
character*50 line
REAL :: m(ncols,nrows), v(nrows)
do j=1,nrows
  write(line,'(A)') '('
  do i=1,ncols
	write(line(1+(i-1)*6+1:1+(i-1)*6+6),'(F5.1,1X)') m(i,j)
  end do
	if(j.eq.1) then
		write(line((i-1)*6:),'(A)') ') (a)=('
	else if(j.eq.2) then
		write(line((i-1)*6:),'(A)') ') (b)=('
	else if(j.eq.3) then
		write(line((i-1)*6:),'(A)') ') (c)=('
	end if
	write(line((i-1)*6+7:),'(F5.1,1X)') v(j)
    write(line((i-1)*6+13:),'(A)') ')'
    print *,line
    line = ' '
end do
END SUBROUTINE

REAL FUNCTION det(m,ncols,nrows)
  implicit none
  INTEGER :: ncols,nrows,i,j
  REAL :: m(ncols,nrows)
  det = m(1,1)*m(2,2)*m(3,3) + &
  &     m(2,1)*m(3,2)*m(1,3) + &
  &     m(3,1)*m(1,2)*m(2,3) - &
  &     m(3,1)*m(2,2)*m(1,3) - &
  &     m(2,1)*m(1,2)*m(3,3) - &
  &     m(1,1)*m(3,2)*m(2,3)
END

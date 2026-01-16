PROGRAM ICIRCLE
  implicit none
  real :: radius
  print *,'enter the radius'
  read *,radius
  print *,'the circle of radius ', radius
  print *,'has perimeter ',perimeter(radius),' and'
  print *,'area ',area(radius)
  stop
  contains
  real function area(radius)
	implicit none
    real, intent(in) :: radius
    real pi
    parameter(pi=3.141592653589793)
    area = pi*radius**2
  end function area
  real function perimeter(radius)
	implicit none
    real, intent(in) :: radius
    real pi
    parameter(pi=3.141592653589793)
    perimeter = 2*pi*radius
  end function perimeter
END PROGRAM ICIRCLE

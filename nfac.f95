RECURSIVE FUNCTION NFACT(n) RESULT(fac)
! compute the factorial of n n!
  implicit none
  ! result
  integer :: fac
  ! dummy arguments
  integer,intent(in) :: n
  select case(n)
  case(0)
    fac=1
    return
  case(1)
    fac=1
    return
  case default
	fac = n*nfact(n-1)
  end select
END FUNCTION NFACT

PROGRAM use_nfact
  implicit none
  integer :: n,nfact
50  print *,'enter an integer number in [0,+inf]'
  read *,n
  if (n.lt.0) goto 50
  print *,'the factorial of n is ',NFACT(n)
  stop
END PROGRAM use_nfact


program assumed_char
	implicit none
	character (len=5) :: name
	character (len=128) :: name2
	name = "Tanja"
	call print_string (name)
	print *,'enter a name'
	read *,name2
	call print_string(name2)
end program assumed_char
subroutine print_string (name)
	implicit none
	! dummy arguments
	character (len=*), intent (in) :: name
	print*, name
end subroutine print_string

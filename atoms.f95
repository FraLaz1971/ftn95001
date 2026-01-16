PROGRAM ATOMS1
  TYPE :: ATOM
    CHARACTER*2 :: label
    REAL :: x,y,z
  END TYPE ATOM
  INTEGER :: i,ai,ra,posi
  LOGICAL debug
! i=atoms counter, ai=index of the atoms list,ra=random atom z
  CHARACTER*80 ::  ifnam,ofnam
  CHARACTER*2 FS
  PARAMETER(FS=',')
  CHARACTER*1024 ::  LINE
! procedure part of the program
  type(ATOM) :: a
  debug=.true.
  PRINT *,'ENTER THE OUTPUT FILE NAME'
  READ *,ofnam
  ifnam = 'atoms_symbols.csv'
  CALL INI_RAN
  DO i=1,20
      open(11,FILE=ofnam,ERR=9300) ! output file
      WRITE(11,*) '#label,   x  ,   y  ,   z  '
      open(12,FILE=ifnam,ERR=9200) ! input file containing atoms data
!skip the 2 rows header
      read(12,'(A)') LINE
      read(12,'(A)') LINE
      ra=nint(getran(118)+59)
      if(debug) print *,'i=',i,' ra=',ra
60      read(12,'(A)',ERR=9000,END=80) LINE
      if(debug) print *,'ra=',ra,' LINE=',LINE
      posi=INDEX(LINE,FS)
      read(LINE(:posi-1),'(I2)') ai
      if(debug) print *,' ai=',ai
      LINE=LINE(posi+1:)
      posi=INDEX(LINE,FS)
      read(LINE(:posi-1),'(A2)') a%label
      if(debug) print *,'a%label=',a%label
      if (ra.eq.ai) then
        if(debug) print *,'ra=',ra,' ai=',ai
        a%x=getran(20)
        a%y=getran(20)
        a%z=getran(20)
        !write the atom row
        WRITE(11,'(A2,3(A1,F4.2))')a%label,FS,a%x,FS,a%y,FS,a%z
      else
        rewind(12)
        goto 60
      end if
80    continue
  END DO
      close(11)
      close(12)
GOTO 9999
9000      PRINT *,'ERROR IN READING LINE',LINE,'FROM FILE ',IFNAM
      GOTO 9999
9200      PRINT *,'ERROR IN OPENING INPUT FILE',IFNAM
      GOTO 9999
9300      PRINT *,'ERROR IN OPENING OUTPUT FILE',OFNAM
9999  STOP
END PROGRAM ATOMS1

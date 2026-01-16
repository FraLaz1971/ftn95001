SUBROUTINE PLOT(infile, driver)
  CHARACTER*80 infile,gpfile,cmd
  CHARACTER*10 driver
  gpfile='atoms.gp'
  cmd='gnuplot -p '//gpfile
  open(11,file=gpfile)
  write(11, *)'set datafile separator '','''
  write(11,*)'splot ''atoms_list.csv'' using 2:3:4 with points pt 7 &
&  ps 5 lc rgb ''yellow'' title ''spatial distribution of atoms'', \'
  write(11,*)'''atoms_list.csv'' using 2:3:4:1 with labels notitle'
  close(11)
  CALL RUN(cmd)
  RETURN
END SUBROUTINE PLOT

PROGRAM ATOMS1
  TYPE :: ATOM
    CHARACTER*2 :: label
    REAL :: x,y,z
  END TYPE ATOM
  INTEGER :: i,ai,ra,posi
  LOGICAL debug
! i=atoms counter, ai=index of the atoms list,ra=random atom z
  CHARACTER*80 ::  ifnam,ofnam
  CHARACTER*1024 ::  LINE
! procedure part of the program
  type(ATOM) :: a
  debug=.false.
  PRINT *,'ENTER THE OUTPUT FILE NAME'
  READ *,ofnam
  ifnam = 'atoms_symbols.csv'
  CALL INI_RAN
      open(11,FILE=ofnam,ERR=9300) ! output file
      WRITE(11,*) '#label, x  ,   y  ,   z  '
      open(12,FILE=ifnam,ERR=9200) ! input file containing atoms data
  DO i=1,20
!skip the 2 rows header
      read(12,'(A)') LINE
      read(12,'(A)') LINE
	   ra=nint(getran(118.0)+59)
      if(debug) print *,'i=',i,' ra=',ra
60      read(12,'(A)',ERR=9000,END=80) LINE
      !if(debug) print *,'ra=',ra,' LINE=',LINE
      posi=INDEX(LINE,',')
      read(LINE(:posi-1),'(I2)') ai
      !if(debug) print *,' ai=',ai
      LINE=LINE(posi+1:)
      posi=INDEX(LINE,',')
      if(debug)print *,'now posi= ',posi,'now LINE= ',LINE(:posi-1)
      read(LINE(:posi-1),'(A2)') a%label
      if(debug) print *,'a%label=',a%label,' LINE= ',LINE
      if (ra.eq.ai) then
        if(debug) print *,'ra=',ra,' ai=',ai,'a%label= ',a%label
        a%x=getran(20.0)
        a%y=getran(20.0)
        a%z=getran(20.0)
        !write the atom row
        WRITE(11,'(A2,3(A1,F8.2))')a%label,',',a%x,',',a%y,',',a%z
      else
        goto 60
      end if
80    continue
      rewind(12)
  END DO
      close(11)
      close(12)
      call PLOT(ofnam, 'qt')
GOTO 9999
9000      PRINT *,'ERROR IN READING LINE',LINE,'FROM FILE ',IFNAM
      GOTO 9999
9200      PRINT *,'ERROR IN OPENING INPUT FILE',IFNAM
      GOTO 9999
9300      PRINT *,'ERROR IN OPENING OUTPUT FILE',OFNAM
9999  STOP
END PROGRAM ATOMS1

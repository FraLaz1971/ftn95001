REAL FUNCTION RANK23(M)
  IMPLICIT NONE
! rank of a matrix 2 rows x 3 columns
  REAL,INTENT(IN) :: M(2,3)
  RANK23=2
END FUNCTION RANK23
SUBROUTINE PLOT(infile, driver)
  IMPLICIT NONE
  CHARACTER*80 :: infile,gpfile,cmd
  CHARACTER*10 :: driver
  gpfile='dihedral.gp'
  cmd='gnuplot -p '//gpfile
  open(11,file=gpfile)
  write(11, *)'set datafile separator '','''
  write(11, *)'set terminal wxt'
  write(11,*)'splot ''points.csv'' using 1:2:3 with points pt 7 &
&  ps 2 lc rgb ''red'' title ''dihedral angle'''
  close(11)
  CALL RUN(cmd)
  RETURN
END SUBROUTINE PLOT
PROGRAM DIHEDRAL
  IMPLICIT NONE
  CHARACTER*80 :: OFNAM
  CHARACTER*10 DRIV
  REAL :: RANK23
  REAL :: M(2,3)
  TYPE :: POINT
  ! point's cohordinates
    REAL :: x,y,z
  END TYPE POINT
  TYPE(POINT) A,B,C,D
  TYPE(POINT) AB,AC
  OFNAM='points.csv'
  DRIV='qt'
  PRINT *,'ENTER 4 POINTS (x,y,z) comma separated'
  READ *, A%x,A%y,A%z
  READ *, B%x,B%y,B%z
  READ *, C%x,C%y,C%z
  READ *, D%x,D%y,D%z
  OPEN(11,FILE=OFNAM,ERR=9000)
    WRITE(11, '(2(F12.4,A),F12.4)', ERR=9100) A%x,',',A%y,',',A%z
    WRITE(11, '(2(F12.4,A),F12.4)', ERR=9200) B%x,',',B%y,',',B%z
    WRITE(11, '(2(F12.4,A),F12.4)', ERR=9300) C%x,',',C%y,',',C%z
    WRITE(11, '(2(F12.4,A),F12.4)', ERR=9400) D%x,',',D%y,',',D%z
  CLOSE(11)
  ! check if A,B,C are allineated
  ! compute the vectors AB and AC
  AB%x = B%x-A%x
  AB%y = B%y-A%y
  AB%z = B%z-A%z
  AC%x = C%x-A%x
  AC%y = C%y-A%y
  AC%z = C%z-A%z
  ! build the matrix M and find RANK(M)
  M(1,1)=AB%x
  M(1,2)=AB%y
  M(1,3)=AB%z
  M(2,1)=AC%x
  M(2,2)=AC%y
  M(2,3)=AC%z
  CALL PLOT(OFNAM,DRIV)
  print *,'rank of M is ',RANK23(M)
  GOTO 9999
9000 PRINT *,'ERROR IN OPENING OUTPUT FILE ',OFNAM
  GOTO 9999
9100 PRINT *,'ERROR IN WRITING POINT A COHORDINATES '
  GOTO 9999
9200 PRINT *,'ERROR IN WRITING POINT B COHORDINATES '
  GOTO 9999
9300 PRINT *,'ERROR IN WRITING POINT C COHORDINATES '
  GOTO 9999
9400 PRINT *,'ERROR IN WRITING POINT D COHORDINATES '
9999 STOP
END PROGRAM DIHEDRAL

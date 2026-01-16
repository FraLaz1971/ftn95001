      SUBROUTINE RUN(CMD)
        CHARACTER*80 CMD,batchfile
        INTEGER K
        batchfile='cmd.bat'
        open(11,file=batchfile)
        write(11,*) CMD
        close(11)
        CALL CISSUE(batchfile,K)
        IF(K.NE.0)PRINT *,'ERROR IN EXECUTING COMMAND',CMD
      END

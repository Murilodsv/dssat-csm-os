    SUBROUTINE READ_LINES(CONTROL, ROWS, IND, FILEKF, KF)
    !-------------------------------------------------------------------------
    ! Subroutine used to read a SCK file and return the number of rows with 
    ! information to be assimilated in the model and the the KF key 
    !
    ! ROWS - number of rows with data for assimilation 
    ! KF   - key that return with the data assimilation will be done
    !        (Y -YES, N - NO)
    !-------------------------------------------------------------------------  
        USE ModuleDefs
        USE SAM_ModuleDefs
    
        IMPLICIT NONE
        SAVE

        INTEGER ROWS, IO, IND, LUNIO
        CHARACTER*12  FILEKF, FILEA
        CHARACTER*80  ROW80
        CHARACTER*30  FILEIO
        CHARACTER*80  PATHEX
        CHARACTER* 1 KF
        CHARACTER* 3 SPACE
        CHARACTER* 2 MOD
        
        TYPE(CONTROLTYPE) CONTROL
        INTEGER DYNAMIC, ERRNUM
    
        FILEIO = CONTROL % FILEIO
        LUNIO  = CONTROL % LUNIO
        
        OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
        READ (LUNIO,'(4(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA, PATHEX
        CLOSE (LUNIO)
        
        FILEKF = FILEA(:8)//'.SCK'
        MOD = 'KF'
        
        OPEN(67, FILE = FILEKF, STATUS = 'UNKNOWN')
        
        IND = 0
        ROWS = 0 !Count the number of lines in a file
        DO WHILE (IO.EQ.0)
            READ(67,'(A80)',iostat=IO) ROW80
            !IF (io/=0) EXIT
            IF (ROW80(1:2) .EQ. "KF") THEN
                READ (ROW80, '(A3, A1)', IOSTAT=IO) SPACE, KF
                IND = IND + 1
            ELSE IF (ROW80(1:1) .NE. "*" .AND. ROW80(1:20) .NE. "                    " .AND. ROW80(1:1) .NE. "@") THEN
                ROWS = ROWS + 1
            ELSE
                IND = IND + 1
            END IF
        END DO
        CLOSE(67)
    END SUBROUTINE READ_LINES
    
!    SUBROUTINE READ_SCT(TRNOKF, DATEKF, DASKF, LAIKF, LAISD, FILEKF, ROWS)
!        USE ModuleDefs
!        USE SAM_ModuleDefs
!    
!        IMPLICIT NONE
!        SAVE
!    
!        INTEGER LUNIO, ROWS, IO, I, ISECT, IND, R
!    
!        INTEGER,ALLOCATABLE,DIMENSION(:) :: TRNOKF, DATEKF, DASKF
!        REAL,ALLOCATABLE,DIMENSION(:) ::  LAIKF, LAISD
!
!        CHARACTER* 1 KF
!        CHARACTER* 3 SAPCE
!        CHARACTER*12 FILEA, FILEKF
!        !CHARACTER*30 FILEIO
!        !CHARACTER*80 PATHEX
!        CHARACTER*80 ROW80
!        !TYPE(CONTROLTYPE) CONTROL
!        INTEGER DYNAMIC, ERRNUM
!    
!        !FILEIO = CONTROL % FILEIO
!        !LUNIO  = CONTROL % LUNIO
!        
!        !OPEN (LUNIO, FILE = FILEIO, STATUS = 'OLD', IOSTAT=ERRNUM)
!        !READ (LUNIO,'(4(/),15X,A12,1X,A80)',IOSTAT=ERRNUM) FILEA, PATHEX
!        !CLOSE (LUNIO)
!    
!        !FILEA = 'ESAL1401.SCA'
!        !PATHEX = 'C:\DSSAT47\Sugarcane\'
!        
!        !FILEKF = FILEA(:8)//'.SCK'
!    
!        !CALL READ_LINES(FILEKF, ROWS, IND)
!    
!        OPEN(67, FILE = FILEKF, STATUS = 'UNKNOWN')

        !IF (ROWS.GT.0) THEN
        !    ALLOCATE (TRNOKF(ROWS), DATEKF(ROWS), DASKF(ROWS))
        !    ALLOCATE (LAIKF(ROWS), LAISD(ROWS))
        !ELSE
        !    ALLOCATE(LAIKF(1))
        !    LAIKF(1) = -99
        !    RETURN
        !END IF
    
!        ISECT = 0
!        I = 1
!        DO WHILE (ISECT.EQ.0)
!            READ(67, *, IOSTAT=ISECT) ROW80
!            IF (ROW80(1:1) .NE. "*" .AND. ROW80(1:20) .NE. "                    " .AND. ROW80(1:1) .NE. "@") THEN
!                !READ (67,'(2I6, 6F6.2)',IOSTAT=ISECT) TRNO(I), DATE(I), SMDMD(I), SMFMD(I), TAD(I), LAIGD(I), SHTD(I), SUCMD(I)
!                READ (ROW80,'(2I6, 6F6.2)',IOSTAT=ISECT) TRNOKF(I), DATEKF(I), DASKF(I), LAIKF(I), LAISD(I)
!                I = I + 1
!                IF (I .GT. ROWS) THEN
!                    ISECT = 1
!                END IF
!            END IF
!        END DO
!        CLOSE(67)
        
!    END SUBROUTINE READ_SCT
    
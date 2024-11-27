PROGRAM symmetrize

  USE sub_plot
  IMPLICIT NONE
  INTEGER,     ALLOCATABLE, DIMENSION(:,:) :: jj, neigh, jjs
  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: i_d, sides, neighs
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_lect, rr_new

  REAL(KIND=8)      :: r, theta, r0, exp, pi
  CHARACTER(len=64) :: directory, file_name
  INTEGER           :: np,  nw,  me,  nws,  mes, m, n, ms, type_fe, d_end, f_end
  LOGICAL           :: mesh_formatted !formatted <=> mesh_formatted=.true.

  OPEN(21,FILE='data_rescale',FORM='formatted', STATUS = 'unknown')
  READ (21, *) mesh_formatted
  READ(21,*) exp
  READ (21, *) directory, file_name

  pi = ACOS(-1.d0)
  d_end = last_c_leng (64, directory)
  f_end = last_c_leng (64, file_name)
  IF (mesh_formatted) THEN
     OPEN(30,FILE=directory(1:d_end)//'/'//file_name(1:f_end),FORM='formatted')
  ELSE
     OPEN(30,FILE=directory(1:d_end)//'/'//file_name(1:f_end),FORM='unformatted')
  END IF

  IF (mesh_formatted) THEN
     OPEN(40,FILE=directory(1:d_end)//'/resc_'//file_name(1:f_end),FORM='formatted')
  ELSE
     OPEN(40,FILE=directory(1:d_end)//'/resc_'//file_name(1:f_end),FORM='unformatted')
  END IF

111  FORMAT(10(I6,3x))
112  FORMAT(10(e25.16,3x))

  DO type_fe = 1, 2
     WRITE (*,*) 'Loading mesh-file ...'
     !---READ GRID DATA AND ARRAY ALLOCATION ----------------------------------------
     IF (mesh_formatted) THEN
        READ  (30, *)  np,  nw,  me,  nws,  mes
     ELSE
        READ(30)       np,  nw,  me,  nws,  mes
     END IF

     ALLOCATE(jj(nw,me),neigh(3,me),i_d(me))
     ALLOCATE(jjs(nws,mes), neighs(mes), sides(mes))
     ALLOCATE(rr_lect(2,np), rr_new(2,np))

     IF (mesh_formatted) THEN
        DO m = 1, me
           READ(30,*) jj(:,m), neigh(:,m), i_d(m)
        END DO
        DO ms = 1, mes
           READ(30,*)  jjs(:,ms), neighs(ms), sides(ms)
        END DO
        DO n = 1, np
           READ(30,*) rr_lect(1:2,n)
        END DO
     ELSE
        READ(30)      jj,  neigh,  i_d   
        READ(30)      jjs, neighs, sides
        READ(30)      rr_lect
     END IF
     !-------------------------------------------------------------------------------
 
    !Begin Rescaling
     DO n = 1, np
        !==Rescaling for VKS (Nov 2014)
        rr_new(1,n) =  rr_lect(1,n)*(2-rr_lect(1,n))/2
        rr_new(2,n) =  rr_lect(2,n)
        CYCLE
        !==Rescaling for VKS (Nov 2014)
           r = SQRT(SUM(rr_lect(:,n)**2))
           theta = ATAN2(rr_lect(2,n),rr_lect(1,n))
           IF (r.le..5d0) THEN
           r = r *SIN(r*pi/(2*0.5d0))**(exp)
           END IF
           rr_new(1,n) = r*COS(theta)
           rr_new(2,n) = r*SIN(theta)
           CYCLE
        IF (rr_lect(1,n) > 0.d0 .AND. rr_lect(2,n).le. rr_lect(1,n)) THEN
           r = SQRT(SUM(rr_lect(:,n)**2))
           theta = ATAN2(rr_lect(2,n),rr_lect(1,n))
           r0 = 1.d0/COS(theta)
           r = r *min(1.d0,SIN(r*pi/2))**(exp)
           rr_new(1,n) = r*COS(theta)
           rr_new(2,n) = r*SIN(theta)
        ELSE IF (rr_lect(2,n).ge. rr_lect(1,n) .AND. rr_lect(2,n) .ge. -rr_lect(1,n)) THEN
           r = SQRT(SUM(rr_lect(:,n)**2))
           theta = ATAN2(rr_lect(2,n),rr_lect(1,n))
           r0 = 1.d0/SIN(theta)
           r = r *min(1.d0,r)**(exp)
           rr_new(1,n) = r*COS(theta)
           rr_new(2,n) = r*SIN(theta)
        ELSE IF (rr_lect(2,n) .le. -rr_lect(1,n) .AND. rr_lect(2,n) .ge. rr_lect(1,n)) THEN
           r = SQRT(SUM(rr_lect(:,n)**2))
           theta = ATAN2(rr_lect(2,n),rr_lect(1,n))
           r0 = 1.d0/ABS(COS(theta))
           r = r *min(1.d0,r)**(exp)
           rr_new(1,n) = r*COS(theta)
           rr_new(2,n) = r*SIN(theta)
        ELSE
           r = SQRT(SUM(rr_lect(:,n)**2))
           theta = ATAN2(rr_lect(2,n),rr_lect(1,n))
           r0 = 1.d0/ABS(SIN(theta))
           r = r *min(1.d0,r)**(exp)
           rr_new(1,n) = r*COS(theta)
           rr_new(2,n) = r*SIN(theta)
        END IF
     END DO
     !End rescaling 


     IF (mesh_formatted) THEN
        WRITE (40, *)  np,  nw,  me,  nws,  mes
     ELSE
        WRITE (40)     np,  nw,  me,  nws,  mes
     END IF

     IF (mesh_formatted) THEN
        DO m = 1, me
           WRITE(40,111) jj(:,m), neigh(:,m), i_d(m)
        END DO
     ELSE
        WRITE(40) jj, neigh, i_d
     END IF

     IF (mesh_formatted) THEN
        DO ms = 1, mes
           WRITE(40,111) jjs(:,ms), neighs(ms), sides(ms)
        END DO
     ELSE
        WRITE(40)      jjs,       neighs,     sides
     END IF

     IF (mesh_formatted) THEN
        DO n = 1, np
           WRITE(40,112) rr_new(:,n)
        END DO
     ELSE
        WRITE(40)      rr_new
     END IF
     CALL plot_pressure_p2_label(jj, rr_new, rr_new(1,:), 'verif_p2_rescale.plt')
     !---Cleaning------------------------------------------------------------
     DEALLOCATE (rr_lect, rr_new, jj, i_d, neigh, jjs, neighs, sides)

  END DO

  CLOSE (30)
  CLOSE (40)

CONTAINS

  FUNCTION last_c_leng (len_str, string) RESULT (leng)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: len_str
    CHARACTER (LEN=len_str), INTENT(IN) :: string
    INTEGER :: leng

    INTEGER :: i

    leng = len_str

    DO i=1,len_str
       IF ( string(i:i) .EQ. ' ' ) THEN
          leng = i-1; EXIT
       ENDIF
    ENDDO

  END FUNCTION last_c_leng
END PROGRAM symmetrize






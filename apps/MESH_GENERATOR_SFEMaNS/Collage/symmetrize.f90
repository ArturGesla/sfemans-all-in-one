PROGRAM symmetrize

  USE sub_plot
  IMPLICIT NONE
  INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: jj_lect, neigh_lect, jjs_lect
  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: i_d_lect, sides_lect, neighs_lect
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_lect

  INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: jj_new, neigh_new, jjs_new
  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: i_d_new, sides_new, neighs_new
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_new

  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: new_el, new_node
  LOGICAl,      ALLOCATABLE, DIMENSION(:)   :: virgin, mark_rr, mark_el, mark_els

  CHARACTER(len=64) :: directory, file_name
  INTEGER           :: np, nw, me, nws, mes, m, ms, n, n1, n2, n3, type_fe, els, els_max, &
       d_end, f_end, node, np_new, me_new, mes_new, new_points
  REAL(KIND=8)      :: diam, epsilon =1.d-12, eps_diam, norm, a_coef ,b_coef ,c_coef, d 
  LOGICAL           :: mesh_formatted !formatted <=> mesh_formatted=.true.
  LOGICAL           :: if_keep_sides
  INTEGER           :: add_to_sides
  OPEN(21,FILE='data_symmetrize',FORM='formatted', STATUS = 'unknown')
  READ (21, *) mesh_formatted
  READ (21, *) directory, file_name
  READ (21, *) a_coef, b_coef, c_coef ! Coefficents of the symmetry line  
  READ (21, *) if_keep_sides

  WRITE (*,*) 'Loading mesh-file ...'
  d_end = last_c_leng (64, directory)
  f_end = last_c_leng (64, file_name)
  IF (mesh_formatted) THEN
     OPEN(30,FILE=directory(1:d_end)//'/'//file_name(1:f_end),FORM='formatted')
  ELSE
     OPEN(30,FILE=directory(1:d_end)//'/'//file_name(1:f_end),FORM='unformatted')
  END IF

  IF (mesh_formatted) THEN
     OPEN(40,FILE=directory(1:d_end)//'/symm_'//file_name(1:f_end),FORM='formatted')
  ELSE
     OPEN(40,FILE=directory(1:d_end)//'/symm_'//file_name(1:f_end),FORM='unformatted')
  END IF

111  FORMAT(10(I6,3x))
112  FORMAT(10(e25.16,3x))

  DO type_fe = 1, 2
     !---READ GRID DATA AND ARRAY ALLOCATION ----------------------------------------
     IF (mesh_formatted) THEN
        READ  (30, *)  np,  nw,  me,  nws,  mes
     ELSE
        READ(30)       np,  nw,  me,  nws,  mes
     END IF

     ALLOCATE(jj_lect(nw,me),neigh_lect(3,me),i_d_lect(me))
     ALLOCATE(jjs_lect(nws,mes), neighs_lect(mes), sides_lect(mes))
     ALLOCATE(rr_lect(2,np))

     IF (mesh_formatted) THEN
        DO m = 1, me
           READ(30,*) jj_lect(1:nw,m), neigh_lect(1:3,m), i_d_lect(m)
        END DO
        DO ms = 1, mes
           READ(30,*) jjs_lect(1:nws,ms), neighs_lect(ms), sides_lect(ms)
        END DO
        DO n = 1, np
           READ(30,*) rr_lect(1:2,n)
        END DO
     ELSE
        READ(30)      jj_lect,      neigh_lect,      i_d_lect
        READ(30)      jjs_lect,     neighs_lect,     sides_lect
        READ(30)      rr_lect
     END IF
     !-------------------------------------------------------------------------------

     !-------------------------------------------------------------------------------
     diam = ABS(MAXVAL(rr_lect(1,:))-MINVAL(rr_lect(1,:)))&
          +ABS(MAXVAL(rr_lect(2,:))-MINVAL(rr_lect(2,:)))
     eps_diam = epsilon*diam
     norm = SQRT(a_coef**2+b_coef**2)
     !-------------------------------------------------------------------------------

     !-------------------------------------------------------------------------------
     ALLOCATE(mark_rr(np), virgin(np), new_node(np), new_el(me), mark_el(me), mark_els(mes))
     mark_rr  = .FALSE.
     mark_el  = .FALSE.
     mark_els = .FALSE.

     node = 0
     DO n = 1, np
        d = ABS(c_coef+a_coef*rr_lect(1,n)+b_coef*rr_lect(2,n))/norm
        IF (d .le. eps_diam) THEN
           mark_rr(n) = .TRUE. ! Node on symmetry line
        ELSE
           node = node + 1
        END IF
     END DO
     new_points = node ! Number of new nodes 

     DO m = 1, me
        DO n = 1, 3
           n1 = MODULO(n,3)+1
           n2 = MODULO(n+1,3)+1
           IF (mark_rr(jj_lect(n1,m)) .AND. mark_rr(jj_lect(n2,m))) THEN  
              mark_el(m) = .TRUE. ! Element touches symmetry line   
           END IF
        END DO
     END DO

     DO ms = 1, mes
        n1 = jjs_lect(1,ms)
        n2 = jjs_lect(2,ms)
        IF (mark_rr(n1) .AND. mark_rr(n2)) THEN  
           mark_els(ms) = .TRUE. ! Segment on symmetry line     
        END IF
     END DO

     virgin = .TRUE.
     node = np ! Initialize node
     DO m = 1, me
        new_el(m) = m + me
        DO n = 1, nw
           IF (virgin(jj_lect(n,m))) THEN
              virgin(jj_lect(n,m)) = .FALSE.
              IF (mark_rr(jj_lect(n,m))) THEN
                 new_node(jj_lect(n,m)) = jj_lect(n,m) ! Node on symmetry line
              ELSE 
                 node = node + 1
                 new_node(jj_lect(n,m)) = node  ! New node
              END IF
           END IF
        END DO
     END DO
     np_new = node
     me_new = 2*me
     IF (np_new /= np+new_points) THEN
        WRITE(*,*) ' BUG in counting new nodes',np_new, np+new_points
        STOP
     END IF

     ALLOCATE(rr_new(2,np_new),jj_new(nw,me_new), neigh_new(3,me_new), i_d_new(me_new))

     rr_new(:,1:np)    = rr_lect
     jj_new(:,1:me)    = jj_lect
     neigh_new(:,1:me) = neigh_lect
     i_d_new(1:me)     = i_d_lect
     i_d_new(new_el)   = i_d_lect

     node = np
     DO n = 1, np
        IF (mark_rr(n)) CYCLE
        d = -(c_coef+a_coef*rr_lect(1,n)+b_coef*rr_lect(2,n))/norm
        node = node + 1
        rr_new(1,new_node(n)) = rr_lect(1,n) + 2*d*a_coef/norm
        rr_new(2,new_node(n)) = rr_lect(2,n) + 2*d*b_coef/norm
     END DO
     IF (node /= np_new) THEN
        WRITE(*,*) ' BUG in counting new nodes',node, np_new, np
        STOP
     END IF

     DO m = 1, me
        jj_new(:, new_el(m)) = new_node(jj_lect(:,m))
        DO n = 1, 3
           IF (neigh_lect(n,m) ==0 .AND. mark_el(m)) THEN
              neigh_new(n,new_el(m)) = m
              !Bug corrected June 26, 2007, JLG
              neigh_new(n,m) = new_el(m)
           ELSE
              IF (neigh_lect(n,m)==0) THEN
                 neigh_new(n,new_el(m)) = 0
              ELSE
                 neigh_new(n,new_el(m)) = new_el(neigh_lect(n,m))
              END IF
           END IF
        END DO
     END DO

     DO m = me+1, me_new ! Switch for anticlockwise enumeration
        n1 = jj_new(1, m) ! Switch 1 and 2 for anticlockwise enumeration
        jj_new(1, m) = jj_new(2, m)
        jj_new(2, m) = n1

        n1 = neigh_new(1, m) ! Switch 1 and 2 for anticlockwise enumeration
        neigh_new(1, m) = neigh_new(2, m)
        neigh_new(2, m) = n1

        IF (type_fe==2) THEN
           n1 = jj_new(4, m) ! Switch 4 and 5 for anticlockwise enumeration
           jj_new(4, m) = jj_new(5, m)
           jj_new(5, m) = n1
        END IF
     END DO

     els = 0
     DO ms = 1, mes
        IF (.NOT.mark_els(ms)) THEN
           els = els + 1
        END IF
     END DO

     els_max = els
     mes_new = 2*els
     ALLOCATE(jjs_new(nws,mes_new), neighs_new(mes_new), sides_new(mes_new))

     !-------------------------------------------------------------------------------
     IF (if_keep_sides) THEN
        add_to_sides = 0
     ELSE
        add_to_sides = MAXVAL(sides_lect)
     END IF


     els = 0
     DO ms = 1, mes
        IF (.NOT.mark_els(ms)) THEN
           els = els + 1
           jjs_new(:,els)          =          jjs_lect(:,ms)
           jjs_new(:,els+els_max)  = new_node(jjs_lect(:,ms))
           sides_new(els)          = sides_lect(ms)
           sides_new(els+els_max)  = sides_lect(ms) + add_to_sides
           neighs_new(els)         =        neighs_lect(ms)
           neighs_new(els+els_max) = new_el(neighs_lect(ms))
        END IF
     END DO

     DO ms = els_max+1, mes_new ! Switch for enumeration compatibility
        n1 = jjs_new(1,ms)
        jjs_new(1,ms) =  jjs_new(2,ms)
        jjs_new(2,ms) = n1
     END DO


     IF (mesh_formatted) THEN
        WRITE  (40, *)  np_new,  nw,  me_new,  nws,  mes_new
     ELSE
        WRITE(40)       np_new,  nw,  me_new,  nws,  mes_new
     END IF

     IF (mesh_formatted) THEN
        DO m = 1, me_new
           WRITE(40,111) jj_new(:,m), neigh_new(:,m), i_d_new(m)
        END DO
     ELSE
        WRITE(40)      jj_new,      neigh_new,      i_d_new
     END IF

     IF (mesh_formatted) THEN
        DO ms = 1, mes_new
           WRITE(40,111) jjs_new(:,ms), neighs_new(ms), sides_new(ms)
        END DO
     ELSE
        WRITE(40)      jjs_new,       neighs_new,     sides_new
     END IF

     IF (mesh_formatted) THEN
        DO n = 1, np_new
           WRITE(40,112) rr_new(:,n)
        END DO
     ELSE
        WRITE(40)      rr_new
     END IF

     !---Cleaning------------------------------------------------------------
     DEALLOCATE (rr_lect, jj_lect, i_d_lect, neigh_lect, jjs_lect, neighs_lect, sides_lect)
     DEALLOCATE (mark_rr, virgin, new_node, new_el, mark_el, mark_els)

     !---Verifications--------------------------------------------------------
     ALLOCATE(rr_lect(2,me_new))
     rr_lect = 0
     rr_lect(1,neighs_new(1:mes_new)) = sides_new(1:mes_new) 
     rr_lect(2,:) = i_d_new
     IF (type_fe==1) THEN
        CALL plot_const_p1_label(jj_new, rr_new, rr_lect(1,:), 'sides_p1.plt')
        CALL plot_const_p1_label(jj_new, rr_new, rr_lect(2,:), 'i_d_p1.plt')
     ELSE 
        CALL plot_const_p1_label(jj_new, rr_new, rr_lect(1,:), 'sides_p2.plt')
        CALL plot_const_p1_label(jj_new, rr_new, rr_lect(2,:), 'i_d_p2.plt') 

        OPEN(20,FILE='t.plt',FORM='formatted', STATUS = 'unknown')
        WRITE (20, *) '$ DATA = CONTCURVE'
        WRITE (20, *) '% contstyle = 2'
        WRITE (20, *) '% nsteps = 50'
        WRITE (20, *) '% meshplot  = true'
        WRITE (20, *) '% pointid=true'

        WRITE (20, *)
        do n = 1, 2
           m = (n-1)*me + 1
           n1 = jj_new(1, m)
           n2 = jj_new(6, m)
           n3 = jj_new(5, m)
           WRITE (20, 100) rr_new(1,n1), rr_new(2,n1), 0., 1
           WRITE (20, 100) rr_new(1,n2), rr_new(2,n2), 0., 6
           WRITE (20, 100) rr_new(1,n3), rr_new(2,n3), 0., 6
           WRITE (20, 100)


           n1 = jj_new(2, m)
           n2 = jj_new(4, m)
           n3 = jj_new(6, m)
           WRITE (20, 100) rr_new(1,n1), rr_new(2,n1), 0., 2
           WRITE (20, 100) rr_new(1,n2), rr_new(2,n2), 0., 4
           WRITE (20, 100) rr_new(1,n3), rr_new(2,n3), 0., 6
           WRITE (20, 100)

           n1 = jj_new(4, m)
           n2 = jj_new(3, m)
           n3 = jj_new(5, m)
           WRITE (20, 100) rr_new(1,n1), rr_new(2,n1), 0., 4
           WRITE (20, 100) rr_new(1,n2), rr_new(2,n2), 0., 3
           WRITE (20, 100) rr_new(1,n3), rr_new(2,n3), 0., 5
           WRITE (20, 100)

           n1 = jj_new(5, m)
           n2 = jj_new(6, m)
           n3 = jj_new(4, m)
           WRITE (20, 100) rr_new(1,n1), rr_new(2,n1), 0., 5
           WRITE (20, 100) rr_new(1,n2), rr_new(2,n2), 0., 6
           WRITE (20, 100) rr_new(1,n3), rr_new(2,n3), 0., 4
           WRITE (20, 100)
        end do
        CLOSE(20)
100     FORMAT(3(e11.5,3x),i5)

     END IF
     DEALLOCATE(rr_lect)
     !---End Verifications--------------------------------------------------------

     DEALLOCATE (rr_new,  jj_new,  i_d_new,  neigh_new,  jjs_new,  neighs_new,  sides_new)

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






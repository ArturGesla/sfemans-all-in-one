PROGRAM symmetrize

  USE sub_plot
  IMPLICIT NONE
  INTEGER,     ALLOCATABLE, DIMENSION(:,:)  :: jj, neigh, jjs
  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: i_d, sides, neighs
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_lect, rr_new, rr_p2
  INTEGER, ALLOCATABLE, DIMENSION(:,:)      :: jj_p2, neigh_p2, jjs_p2
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: id_p2, sides_p2, neighs_p2 

  REAL(KIND=8)      :: r, theta, r0, exp, pi, &
       ratiox0, ratioy0, alphax0, alphay0, xmin, xmax, ymin, ymax, &
       ratiox1, ratioy1, alphax1, alphay1, &
       x0, homothx, y0, homothy
  CHARACTER(len=64) :: directory, file_name
  INTEGER           :: np,  nw,  me,  nws,  mes, m, n, ms, type_fe, d_end, f_end
  LOGICAL           :: mesh_formatted !formatted <=> mesh_formatted=.true.

  OPEN(21,FILE='data_rescale',FORM='formatted', STATUS = 'unknown')
  READ (21, *) mesh_formatted
  READ(21,*) ratiox0, ratiox1, x0, homothx
  READ(21,*) ratioy0, ratioy1, y0, homothy
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

111 FORMAT(10(I6,3x))
112 FORMAT(10(e25.16,3x))

  !DO type_fe = 1, 2
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
  alphax0 = -(1.d0-ratiox0)/(1.d0+ratiox0)
  alphax1 = (1.d0-ratiox1)/(1.d0+ratiox1)
  alphay0 = -(1.d0-ratioy0)/(1.d0+ratioy0)
  alphay1 = (1.d0-ratioy1)/(1.d0+ratioy1)
  xmin = MINVAL(rr_lect(1,:))
  xmax = MAXVAL(rr_lect(1,:))
  ymin = MINVAL(rr_lect(2,:))
  ymax = MAXVAL(rr_lect(2,:))
  rr_lect(1,:) =  (rr_lect(1,:)-xmin)/(xmax-xmin)
  rr_lect(2,:) =  (rr_lect(2,:)-ymin)/(ymax-ymin)
  DO n = 1, np
     !==Rescaling
     rr_new(1,n) =  rr_lect(1,n) + alphax0*rr_lect(1,n)*(1-rr_lect(1,n))**2 &
          + alphax1*rr_lect(1,n)**2*(1-rr_lect(1,n)) 
     rr_new(2,n) =  rr_lect(2,n) + alphay0*rr_lect(2,n)*(1-rr_lect(2,n))**2 &
          + alphay1*rr_lect(2,n)**2*(1-rr_lect(2,n)) 
  END DO
  rr_new(1,:) = xmin + (xmax-xmin)*rr_new(1,:)
  rr_new(2,:) = ymin + (ymax-ymin)*rr_new(2,:)
  !End rescaling 

  ! Shift and homothety
  rr_new(1,:) = x0 + homothx*(rr_new(1,:)-xmin)
  rr_new(2,:) = y0 + homothy*(rr_new(2,:)-ymin)
  ! End Shift and homothety

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
  rr_lect(1,:) = 1.d0
  CALL plot_scalar_field(jj, rr_new, rr_lect(1,:), 'verif_p1_rescale.plt')



  WRITE (*,*) 'Creating P2 mesh'
  !---READ GRID DATA AND ARRAY ALLOCATION ----------------------------------------
  IF (mesh_formatted) THEN
     READ  (30, *)  np,  nw,  me,  nws,  mes
  ELSE
     READ(30)       np,  nw,  me,  nws,  mes
  END IF

  ALLOCATE(jj_p2(nw,me),neigh_p2(3,me),id_p2(me))
  ALLOCATE(jjs_p2(nws,mes), neighs_p2(mes), sides_p2(mes))
  ALLOCATE(rr_p2(2,np))
  neigh_p2 = neigh
  neighs_p2 = neighs
  id_p2 = i_d
  sides_p2 = sides
  CALL create_grid_iso_p1p2(jj, jjs, rr_new, neigh_p2, neighs_p2, jj_p2, jjs_p2, rr_p2, i_d)

  IF (mesh_formatted) THEN
     WRITE (40, *)  np,  nw,  me,  nws,  mes
  ELSE
     WRITE (40)     np,  nw,  me,  nws,  mes
  END IF


  IF (mesh_formatted) THEN
     DO m = 1, me
        WRITE(40,111) jj_p2(:,m), neigh_p2(:,m), id_p2(m)
     END DO
  ELSE
     WRITE(40) jj_p2, neigh_p2, id_p2
  END IF

  IF (mesh_formatted) THEN
     DO ms = 1, mes
        WRITE(40,111) jjs_p2(:,ms), neighs_p2(ms), sides_p2(ms)
     END DO
  ELSE
     WRITE(40)      jjs_p2,       neighs_p2,     sides_p2
  END IF

  IF (mesh_formatted) THEN
     DO n = 1, np
        WRITE(40,112) rr_p2(:,n)
     END DO
  ELSE
     WRITE(40)      rr_p2
  END IF
  DEALLOCATE (rr_lect)
  ALLOCATE(rr_lect(2,np))
  rr_lect(1,:) = 1.d0
  CALL plot_scalar_field(jj_p2, rr_p2, rr_lect(1,:), 'verif_p2_rescale.plt')

  !---Cleaning------------------------------------------------------------
  DEALLOCATE (rr_lect, rr_new, jj, i_d, neigh, jjs, neighs, sides)
  DEALLOCATE(jj_p2,neigh_p2,id_p2)
  DEALLOCATE(jjs_p2,neighs_p2,sides_p2)
  !END DO

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

  SUBROUTINE create_grid_iso_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el,&
       jj_f,  jjs_f,  rr_f, id)
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !  jj(:, :)    nodes of the  volume_elements of the input grid
    !  jjs(:, :)    nodes of the surface_elements of the input grid
    !  rr(:, :)    cartesian coordinates of the nodes of the input grid
    !  m_op(:,:)   volume element opposite to each node 
    !  neigh_el(:) volume element ajacent to the surface element 
    !
    !  jj_f(:, :)  nodes of the  volume_elements of the output p2 grid
    !  jjs_f(:, :)  nodes of the surface_elements of the output p2 grid
    !  rr_f(:, :)  cartesian coordinates of the nodes of the output p2 grid

    IMPLICIT NONE

    INTEGER,      DIMENSION(:,:), INTENT(INOUT) :: jj_in, jjs_in, m_op
    INTEGER,      DIMENSION(:),   INTENT(IN)    :: neigh_el, id
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN)    :: rr_in

    INTEGER,      DIMENSION(:,:), INTENT(OUT)   :: jj_f, jjs_f
    REAL(KIND=8), DIMENSION(:,:), INTENT(OUT)   :: rr_f

    LOGICAL, DIMENSION(:),   ALLOCATABLE        :: virgin
    INTEGER, DIMENSION(:,:), ALLOCATABLE        :: j_mid
    INTEGER, DIMENSION(:),   ALLOCATABLE        :: jjs_mid

    INTEGER      :: np, me, nw, kd, n, m, k, i_d
    INTEGER      :: n1, n2, n3, n4, i1, i2, ms, m2, m3 
    INTEGER      :: k1, k2, m_op_k, kk, i, mm
    REAL(KIND=8) :: a
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: r_mid
    !Iso P2
    INTEGER, DIMENSION(2) :: ms3, ns3
    REAL(KIND=8), DIMENSION(2) :: scos
    REAL(KIND=8) :: epsilon=1.d-13, dist, d1, d2, d3, s1, s2, s3, shalf, ref, scc
    INTEGER      :: bord, ns, ns1, index, ms_bord, nb_angle
    LOGICAL :: iso, iso_test, test
    !Iso P2

    nb_angle = 0
    nw  = SIZE(jj_in,1)   ! nodes in each volume element
    me  = SIZE(jj_in,2)
    kd  = SIZE(rr_in,1)   ! space dimensions
    np  = SIZE(rr_in,2)

    ALLOCATE(virgin(me),j_mid(nw,me),&
         jjs_mid(SIZE(jjs_in,2)),r_mid(kd))

    IF (kd == 3) THEN 
       WRITE(*,*) ' CREATE_GRID_P1P2: 3D case not programmed yet !'
       STOP
    END IF

    !  GENERATION OF THE P2 GRID

    rr_f(:, 1:np) = rr_in

    jj_f(1:nw, :) = jj_in

    virgin = .TRUE.

    n = np

    DO m = 1, me ! loop on the elements

       DO k = 1, nw ! loop on the nodes (sides) of the element

          m_op_k = m_op(k, m)
          k1 = MODULO(k,   nw) + 1;  n1 = jj_in(k1, m)
          k2 = MODULO(k+1, nw) + 1;  n2 = jj_in(k2, m)
          r_mid = (rr_in(:, n1) + rr_in(:, n2))/2

          !TEST October 26 2005
          iso_test =.FALSE.
          IF (m_op_k == 0) THEN  !  the side is on the boundary
             iso = .TRUE.
          ELSE IF (id(m)/=id(m_op_k) .AND. virgin(m_op_k)) THEN
             iso = .TRUE.
             iso_test =.TRUE.
          ELSE
             iso = .FALSE.
          END IF
          !TEST
          !iso = .false.
          !TEST



          !TEST October 26 2005
          IF (iso) THEN
             n = n + 1
             j_mid(k, m) = n
             !            rr_f(:, n) = r_mid
             ! Noeuds P2 isoparametriques
             !
             DO ms = 1, SIZE(jjs_in,2)
                DO ns = 1, SIZE(jjs_in,1) 
                   dist = SQRT(SUM((rr_in(:,n1)-rr_in(:,jjs_in(ns,ms)))**2))   
                   IF (dist.LE.epsilon) THEN
                      ns1 = MODULO(ns,   SIZE(jjs_in,1)) + 1 
                      dist = SQRT(SUM((rr_in(:,n2)-rr_in(:,jjs_in(ns1,ms)))**2))
                      IF (dist.LE.epsilon) THEN
                         bord = sides(ms)
                         ms_bord = ms
                         GO TO 100
                      END IF
                   END IF
                END DO
             END DO
             !WRITE(*,*) ' BUG: pas de segment trouve on side', n1, n2, r_mid, dist, iso, iso_test
             rr_f(:, n) = r_mid
             CYCLE ! This side must be forgotten I guess, it is an internal side. Cycling is the best option.
             STOP

100          index = 1

             ref = SQRT(SUM((rr_in(:,n1)-rr_in(:,n2))**2))
             DO ms = 1, SIZE(jjs_in,2)
                IF (ms==ms_bord) CYCLE  
                d1 = SQRT(SUM((rr_in(:,n1)-rr_in(:,jjs_in(1,ms)))**2))/ref
                d2 = SQRT(SUM((rr_in(:,n1)-rr_in(:,jjs_in(2,ms)))**2))/ref
                IF (d1.LE.epsilon .AND. d2.GT.2*epsilon) THEN
                   scc=SUM((rr_in(:,n1)-rr_in(:,jjs_in(2,ms)))*              (rr_in(:,n1)-rr_in(:,n2)))/ &
                        (SQRT(SUM((rr_in(:,n1)-rr_in(:,jjs_in(2,ms)))**2))*SQRT(SUM((rr_in(:,n1)-rr_in(:,n2))**2)))
                   IF (index.GE.3) THEN
                      IF (scc .GE. MINVAL(scos)) CYCLE
                      index = 2
                   END IF
                   ms3(index)  = sides(ms)
                   ns3(index)  = jjs_in(2,ms)
                   scos(index) = scc
                   index = index + 1
                ELSE IF (d2.LE.epsilon .AND. d1.GT.2*epsilon) THEN
                   scc=SUM((rr_in(:,n1)-rr_in(:,jjs_in(1,ms)))*              (rr_in(:,n1)-rr_in(:,n2)))/ &
                        (SQRT(SUM((rr_in(:,n1)-rr_in(:,jjs_in(1,ms)))**2))*SQRT(SUM((rr_in(:,n1)-rr_in(:,n2))**2)))
                   IF (index.GE.3) THEN
                      IF (scc .GE. MINVAL(scos)) CYCLE
                      index = 2
                   END IF
                   ms3(index) = sides(ms)
                   ns3(index)  = jjs_in(1,ms)
                   scos(index) = scc
                   index = index + 1
                END IF
                d1 = SQRT(SUM((rr_in(:,n2)-rr_in(:,jjs_in(1,ms)))**2))/ref
                d2 = SQRT(SUM((rr_in(:,n2)-rr_in(:,jjs_in(2,ms)))**2))/ref
                IF (d1.LE.epsilon .AND. d2.GT.2*epsilon) THEN
                   scc=SUM((rr_in(:,n2)-rr_in(:,jjs_in(2,ms)))*              (rr_in(:,n2)-rr_in(:,n1)))/ &
                        (SQRT(SUM((rr_in(:,n2)-rr_in(:,jjs_in(2,ms)))**2))*SQRT(SUM((rr_in(:,n2)-rr_in(:,n1))**2)))
                   IF (index.GE.3) THEN
                      IF (scc .GE. MINVAL(scos)) CYCLE
                      index = 2
                   END IF
                   ms3(index)  = sides(ms)
                   ns3(index)  = jjs_in(2,ms)
                   scos(index) = scc
                   index = index + 1
                ELSE IF (d2.LE.epsilon .AND. d1.GT.2*epsilon) THEN
                   scc=SUM((rr_in(:,n2)-rr_in(:,jjs_in(1,ms)))*              (rr_in(:,n2)-rr_in(:,n1)))/ &
                        (SQRT(SUM((rr_in(:,n2)-rr_in(:,jjs_in(1,ms)))**2))*SQRT(SUM((rr_in(:,n2)-rr_in(:,n1))**2)))
                   IF (index.GE.3) THEN
                      IF (scc .GE. MINVAL(scos)) CYCLE
                      index = 2
                   END IF
                   ms3(index) = sides(ms)
                   ns3(index)  = jjs_in(1,ms)
                   scos(index) = scc
                   index = index + 1
                END IF

             END DO

             !DO index = 1, 2 
             !   IF (bord .EQ. ms3(index)) THEN
             !      n3 = ns3(index) 
             !      EXIT
             !   END IF
             !END DO
             !
             !The above test does not work if indices of sides a identical on both sides of a corner. 
             !
             IF (index.LT.2) THEN
                WRITE(*,*) SIZE(jjs_in,2), ms_bord
                WRITE(*,*) ' BUG: mauvais index', rr_in(1,jjs_in(1,ms_bord)), rr_in(1,jjs_in(2,ms_bord))
                WRITE(*,*) ' BUG: suite   index', rr_in(2,jjs_in(1,ms_bord)), rr_in(2,jjs_in(2,ms_bord))
                STOP
             END IF

             IF (ABS(scos(1)) > ABS(scos(2))) THEN
                n3 = ns3(1)
             ELSE
                n3 = ns3(2)
             END IF

             IF (MINVAL(ABS(scos)) < 0.95) THEN 
                nb_angle = nb_angle + 1
             END IF

             d1 = SQRT(SUM((rr_in(:,n1)-rr_in(:,n3))**2))
             d2 = SQRT(SUM((rr_in(:,n2)-rr_in(:,n3))**2))
             IF (d1 .LT. d2) THEN 
                n4 = n2
                n2 = n1
                n1 = n4
             END IF

             d1 = SQRT(SUM((rr_in(:,n1)-rr_in(:,n3))**2)) 
             d2 = SQRT(SUM((rr_in(:,n1)-rr_in(:,n2))**2))
             s3 = d1 / d2
             s2 = 1.d0
             s1 = 0.d0
             shalf = 0.5d0

             r_mid = rr_in(:,n1)*(shalf - s2)*(shalf - s3)/((s1 - s2)*(s1 - s3)) & 
                  + rr_in(:,n2)*(shalf - s3)*(shalf - s1)/((s2 - s3)*(s2 - s1)) & 
                  + rr_in(:,n3)*(shalf - s1)*(shalf - s2)/((s3 - s1)*(s3 - s2))

             rr_f(:, n) = r_mid
             !if (iso_test) write(*,*) 'test r_mid', abs(SQRT(SUM(r_mid**2))-1), abs(SQRT(SUM(rr_in(:,n1)**2))-1)
             !---Fin P2 iso-parametrique

             ! surface elements of the p2 grid are defined later

          ELSE  !  the side is internal

             IF ( virgin(m_op_k) ) THEN  !  the side is new

                n = n + 1
                j_mid(k, m) = n
                rr_f(:, n) = r_mid

             ELSE  !  the side has been already considered

                mm = m_op_k
                DO i = 1, nw
                   IF (m_op(i, mm) == m) kk = i
                ENDDO
                j_mid(k, m) = j_mid(kk, mm)

             ENDIF

          ENDIF

       ENDDO

       virgin(m) = .FALSE.

    ENDDO

    !Write(*,*) ' The mesh has ', nb_angle/2, ' angles; is that right? Yup?'

    !  connectivity matrix of the p2 grid

    jj_f(nw+1 : SIZE(jj_f,1), :) = j_mid


    !  connectivity matrix of the surface elements of the p2 grid

    DO ms = 1, SIZE(jjs_in,2);  mm = neigh_el(ms)
       DO i = 1,nw 
          IF (m_op(i, mm) == 0) THEN
             kk = i
          ELSE IF (id(m_op(i, mm)) /= id(mm)) THEN
             kk = i
          END IF
       ENDDO
       jjs_mid(ms) = j_mid(kk, mm)
    ENDDO

    jjs_f(1:SIZE(jjs_in,1), :) = jjs_in

    jjs_f(3, :) = jjs_mid

    DEALLOCATE(virgin,j_mid,jjs_mid,r_mid)

  END SUBROUTINE  create_grid_iso_p1p2

END PROGRAM symmetrize






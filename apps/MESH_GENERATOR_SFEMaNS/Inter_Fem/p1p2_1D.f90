PROGRAM p1p2_1d

  USE sub_plot

  IMPLICIT NONE

  INTEGER :: me_1d, np_c_1d, me, np_c, nps_c, mes
  INTEGER, ALLOCATABLE, DIMENSION(:,:)      :: jj_c, neigh, jjs_c
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: neighs, sides 
  REAL(KIND=8), POINTER, DIMENSION(:,:)     :: rr_c

  INTEGER:: k, m, ms, n, kd=2, nw_c=3, nws_c=2, nw=6, nws=3
  CHARACTER(len=64) :: directory,file_name
  INTEGER,      POINTER, DIMENSION(:,:) :: jj, jjs
  REAL(KIND=8), POINTER, DIMENSION(:,:) :: rr
  INTEGER                                   :: np, nps
  INTEGER, ALLOCATABLE, DIMENSION(:,:)      :: jj_c_fin, neigh_fin, jjs_c_fin
  INTEGER, ALLOCATABLE, DIMENSION(:)        :: neighs_fin, sides_fin, id
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_c_fin
  INTEGER                                   :: me_fin, mes_fin , np_c_fin
  INTEGER                                   :: m_new, m1, m2, m3, m4, ms_new, ms1, ms2
  INTEGER :: isopar
  LOGICAL :: file_form

  REAL(KIND=8) :: xmin, xmax, ymin, ymax, h

  !-------------INITIAL MESH OPERATIONS
  WRITE(*,'(A)',ADVANCE='NO') ' xmin, xmax, h, format '
  READ(*,*)  xmin, xmax, h, file_form
  ymin = 0.d0
  ymax = 2*h
  me_1d = INT((xmax-xmin)/h)
  h  = (xmax-xmin)/me_1d
  np_c_1d = me_1d + 1

  np_c = 2*np_c_1d
  ALLOCATE(rr_c(2,np_c))
  DO n = 1, np_c_1d
     rr_c(1,2*(n-1)+1) = xmin + (n-1)*h
     rr_c(1,2*(n-1)+2) = xmin + (n-1)*h
     rr_c(2,2*(n-1)+1) = ymin
     rr_c(2,2*(n-1)+2) = ymax
  END DO

  me = 2*me_1d
  ALLOCATE(jj_c(3,me), neigh(3,me))
  DO m = 1, me_1d
     jj_c(1,2*(m-1)+1) = 2*m-1 
     jj_c(2,2*(m-1)+1) = 2*m+1 
     jj_c(3,2*(m-1)+1) = 2*m 
     jj_c(1,2*(m-1)+2) = 2*m 
     jj_c(2,2*(m-1)+2) = 2*m+1 
     jj_c(3,2*(m-1)+2) = 2*m+2 
     neigh(1,2*(m-1)+1) = 2*(m-1)+2
     neigh(2,2*(m-1)+1) = 2*(m-2)+2
     neigh(3,2*(m-1)+1) = 0
     neigh(1,2*(m-1)+2) = 2*(m-0)+1
     neigh(2,2*(m-1)+2) = 0 
     neigh(3,2*(m-1)+2) = 2*(m-1)+1 
  END DO

  DO m = 1, me
     DO n = 1, 3
        IF ((neigh(n,m) < 0) .OR. (neigh(n,m) > me)) neigh(n,m) = 0
     END DO
  END DO

  mes = me + 2
  ALLOCATE(jjs_c(2,mes), sides(mes), neighs(mes))
  DO m = 1,  me_1d
     jjs_c(1,2*(m-1)+1) = jj_c(2,2*(m-1)+1)
     jjs_c(2,2*(m-1)+1) = jj_c(1,2*(m-1)+1)
     jjs_c(1,2*(m-1)+2) = jj_c(1,2*(m-1)+2)
     jjs_c(2,2*(m-1)+2) = jj_c(3,2*(m-1)+2)
     sides(2*(m-1)+1) = 4
     sides(2*(m-1)+2) = 2
     neighs(2*(m-1)+1) = 2*(m-1)+1
     neighs(2*(m-1)+2) = 2*(m-1)+2
  END DO
  jjs_c(1,me+1) = jj_c(1,1)
  jjs_c(2,me+1) = jj_c(3,1)
  jjs_c(1,me+2) = jj_c(3,me)
  jjs_c(2,me+2) = jj_c(2,me)
  sides(me+1) = 1 
  sides(me+2) = 3 
  neighs(me+1) = 1
  neighs(me+2) = me 


  directory = '.'
  WRITE(*,'(A)',ADVANCE='NO')  ' File name? '
  READ(*,*)  file_name

  isopar = 0 

  CALL prep_maill_p1p2(jj_c, jjs_c, rr_c, neigh, neighs, sides, &
       jj,  jjs,  rr)

  WRITE(*,*) ' ==== Maillage grossier ===='
  WRITE (*,*)  'np_c',np_c,'nwc',nw_c,'nwsc',nws_c,'me',me,'mes',mes
  WRITE (*,*)  'np  ',np,  'nw ',nw ,'nws ',nws

  CALL plot_pressure_p1_label(jj_c,rr_c,rr_c(1,:), &
       'maill_G_grossier.plt')
  CALL plot_pressure_p2_label(jj, rr, rr(1,:), 'maill_G_fin.plt')

  !--------Creation du maillage P1/P2 fin
  IF (kd.NE.2) THEN 
     PRINT*, ' 3D pas encore programme' 
     STOP
  END IF
  me_fin = 4*me
  mes_fin = 2*mes
  np_c_fin = np

  ALLOCATE(jj_c_fin(nw_c,me_fin),    neigh_fin(nw_c,me_fin), &
       jjs_c_fin(nws_c,mes_fin), neighs_fin(mes_fin),    &
       sides_fin(mes_fin), rr_c_fin(kd, np))

  rr_c_fin = rr

  m_new = 0
  DO m = 1, me

     m1 = m_new + 1;  m2 = m_new + 2;  m3 = m_new + 3;  m4 = m_new + 4
     m_new = m_new + 4

     !triangle 1
     jj_c_fin(1,m1) = jj(1,m)
     jj_c_fin(2,m1) = jj(6,m)
     jj_c_fin(3,m1) = jj(5,m)
     neigh_fin(1,m1) = m4 
     neigh_fin(2,m1) = voisin(jj(1,m),neigh(2,m),jj)
     neigh_fin(3,m1) = voisin(jj(1,m),neigh(3,m),jj)

     !triangle 2
     jj_c_fin(1,m2) = jj(2,m)
     jj_c_fin(2,m2) = jj(4,m)
     jj_c_fin(3,m2) = jj(6,m)
     neigh_fin(1,m2) = m4 
     !      neigh_fin(2,m2) = neigh(3,m) 
     !      neigh_fin(3,m2) = neigh(1,m) 
     neigh_fin(2,m2) = voisin(jj(2,m),neigh(3,m),jj)
     neigh_fin(3,m2) = voisin(jj(2,m),neigh(1,m),jj)

     !triangle 3
     jj_c_fin(1,m3) = jj(3,m)
     jj_c_fin(2,m3) = jj(5,m)
     jj_c_fin(3,m3) = jj(4,m)
     neigh_fin(1,m3) = m4 
     !      neigh_fin(2,m3) = neigh(1,m) 
     !      neigh_fin(3,m3) = neigh(2,m) 
     neigh_fin(2,m3) = voisin(jj(3,m),neigh(1,m),jj)
     neigh_fin(3,m3) = voisin(jj(3,m),neigh(2,m),jj)

     !triangle 4
     jj_c_fin(1,m4) = jj(6,m)
     jj_c_fin(2,m4) = jj(4,m)
     jj_c_fin(3,m4) = jj(5,m)
     neigh_fin(1,m4) = m3 
     neigh_fin(2,m4) = m1
     neigh_fin(3,m4) = m2 

  END DO

  ms_new = 0
  DO ms = 1, mes

     ms1 = ms_new + 1;  ms2 = ms_new + 2; 
     ms_new = ms_new + 2 

     ! segment 1
     jjs_c_fin(1,ms1) = jjs(1,ms) 
     jjs_c_fin(2,ms1) = jjs(3,ms) 
     sides_fin(ms1) = sides(ms)

     m = neighs(ms) 
     IF (m.NE.0 ) THEN
        DO n = 1, nw_c
           IF (jjs(1,ms) == jj(n,m)) THEN 
              neighs_fin(ms1) = 4*(m-1) + n 
              EXIT
           END IF
        END DO
     END IF

     ! segment 2
     jjs_c_fin(1,ms2) = jjs(3,ms) 
     jjs_c_fin(2,ms2) = jjs(2,ms) 
     sides_fin(ms2) = sides(ms)

     m = neighs(ms) 
     IF (m.NE.0 ) THEN
        DO n = 1, nw_c
           IF (jjs(2,ms) == jj(n,m)) THEN 
              neighs_fin(ms2) = 4*(m-1) + n 
              EXIT
           END IF
        END DO
     END IF

  END DO

  !   DEALLOCATE(jj_c,neigh,jjs_c,neighs,sides,rr_c)
  !   DEALLOCATE(neigh,jjs_c,neighs,sides)
  !   NULLIFY(jj_c,rr_c)

  me = me_fin
  mes = mes_fin
  np_c = np
  !   ALLOCATE(jj_c(nw_c,me),    neigh(nw_c,me), &
  !            jjs_c(nws_c,mes), neighs(mes),    sides(mes),& 
  !            rr_c(kd, np_c))
  !   jj_c = jj_c_fin
  !   jjs_c = jjs_c_fin
  !   neigh = neigh_fin
  !   neighs = neighs_fin
  !   sides = sides_fin
  !   rr_c = rr_c_fin

  !   DEALLOCATE(jj,jjs,rr)

  CALL prep_maill_p1p2(jj_c_fin, jjs_c_fin, rr_c_fin, neigh_fin, neighs_fin, sides_fin, &
       jj,  jjs,  rr)

  IF (file_form) THEN
     OPEN(UNIT=10,FILE='FEM.'//file_name,FORM='formatted',STATUS='unknown')

     write(10,*) np_c_fin, nw_c, me_fin, nws_c, mes_fin

     DO m = 1, SIZE(jj_c_fin, 2)
        WRITE(10, *)  (jj_c_fin(n,m), n = 1,nw_c),  (neigh_fin(n,m), n = 1,nw_c), 1
     END DO

     DO m = 1, SIZE(jjs_c_fin, 2)
        WRITE(10, *)  (jjs_c_fin(n,m), n = 1,nws_c),  neighs_fin(m), sides_fin(m)
     END DO

     DO n = 1, SIZE(rr_c_fin, 2)
        WRITE(10, *)  (rr_c_fin(k,n), k = 1,kd)
     END DO

     WRITE  (10, *) np, nw, me_fin, nws, mes_fin

     DO m = 1, me_fin
        WRITE(10,*) jj(:,m), (neigh_fin(n,m), n = 1,nw_c), 1
     END DO

     DO ms = 1, mes_fin
        WRITE(10,*) jjs(:,ms), neighs_fin(m) , sides_fin(ms)
     END DO

     DO n = 1, SIZE(rr,2)
        WRITE(10,*) rr(:,n)
     END DO
  ELSE
     ALLOCATE(id(me_fin))
     id = 1
     OPEN(UNIT=10,FILE='FEM.'//file_name,FORM='unformatted',STATUS='unknown')
     write(10) np_c_fin, nw_c, me_fin, nws_c, mes_fin
     WRITE(10) jj_c_fin, neigh_fin, id
     WRITE(10) jjs_c_fin, neighs_fin, sides_fin
     WRITE(10) rr_c_fin
     WRITE(10) np, nw, me_fin, nws, mes_fin
     WRITE(10) jj, neigh_fin, id
     WRITE(10) jjs, neighs, sides_fin
     WRITE(10) rr
  END IF
  WRITE(*,*) ' ==== Maillage fin ===='
  WRITE (*,*)  'np_c',np_c,'nwc',nw_c,'nwsc',nws_c,'me',me,'mes',mes
  WRITE (*,*)  'np',np,'nw',nw,'nws ',nws
  CALL plot_pressure_p1_label(jj_c_fin, rr_c_fin, rr_c_fin(1,:), 'maill_grossier.plt')
  CALL plot_pressure_p2_label(jj, rr, rr(1,:), 'maill_fin.plt')

CONTAINS

  SUBROUTINE prep_maill_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el, i_front,&
       jj_f,  jjs_f,  rr_f)

    IMPLICIT NONE
    INTEGER,      DIMENSION(:,:), INTENT(INOUT) :: jj_in, jjs_in, m_op
    INTEGER,      DIMENSION(:),   INTENT(IN)    :: neigh_el, i_front
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN)    :: rr_in
    INTEGER,      DIMENSION(:,:), POINTER       :: jj_f, jjs_f
    REAL(KIND=8), DIMENSION(:,:), POINTER       :: rr_f

    INTEGER              :: m, n_sides, k, n, j_size, ntrou
    INTEGER              :: n_faces, n_edges   !  3D only
    REAL(KIND=8)         :: dummy

    !INTERFACE
    !  SUBROUTINE create_grid_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el,&
    !		    jj_f,  jjs_f,  rr_f)
    !  INTEGER,      DIMENSION(:,:), INTENT(INOUT) :: jj_in, jjs_in, m_op
    !  INTEGER,      DIMENSION(:),   INTENT(IN)    :: neigh_el
    !  REAL(KIND=8), DIMENSION(:,:), INTENT(IN)    :: rr_in
    !  INTEGER,      DIMENSION(:,:), POINTER       :: jj_f, jjs_f
    !  REAL(KIND=8), DIMENSION(:,:), POINTER       :: rr_f
    !  END SUBROUTINE create_grid_p1p2
    !ND INTERFACE
    !-------------END OF DECLARATIONS----------------------------------------------

    !-------------CALCULATION OF nps AND np FOR THE P2 GRIG------------------------
    SELECT CASE(kd)  !Viva Eulero
    CASE(2)
       WRITE(*,'(A)',ADVANCE='NO') ' Attention aux trous: nombre de trou> '
       READ(*,*) ntrou 
       np    = np_c + (me + np_c -1 + ntrou ) ! in 2D
       nps_c = mes  
       nps   = 2 * mes 

    CASE(3)
       n_faces = 2*me + mes/2
       n_edges = n_faces + np_c - 1 - me    ! = me + mes/2 + np - 1 
       np      = np_c + n_edges             ! in 3D

       !         nps_c   = mes/2 + 2
       nps     = nps_c + mes + nps_c - 2

    END SELECT
    WRITE (*,*) '  In create_p1p2: nps_c = ', nps_c, '  np = ', np, '  nps = ', nps
    WRITE (*,*) '  In create_p1p2: me = ', me, '  mes = ', mes
    !-------------END CALCULATION OF nps AND np FOR THE P2 GRIG--------------------


    !--------------------ARRAY ALLOCATION------------------------------------------
    IF (ASSOCIATED(jj_f)) NULLIFY(jj_f,jjs_f,rr_f)
    ALLOCATE (jj_f(nw, me), jjs_f(nws, mes)) 
    ALLOCATE (rr_f(kd,np))
    !   npdir = SIZE(idir_c) + mes ! 2D only
    !   ALLOCATE (idir(npdir), jjdir(npdir)) ! I am unable to cope with numbering 
    !------------------------------------------------------------------------------

    !--------------------CREATE p1/P2 GRID-----------------------------------------
    CALL create_grid_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el, i_front, jj_f, jjs_f, rr_f)
    !------------------------------------------------------------------------------

  END SUBROUTINE  prep_maill_p1p2

  !--------------------------------------------------------------------
  !
  !     This program creates the P2 mesh from the P1 mesh.
  !
  !--------------------------------------------------------------------

  SUBROUTINE create_grid_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el, sides, &
       jj_f,  jjs_f,  rr_f)
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
    INTEGER,      DIMENSION(:),   INTENT(IN)    :: neigh_el, sides
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN)    :: rr_in

    INTEGER,      DIMENSION(:,:), INTENT(OUT)   :: jj_f, jjs_f
    REAL(KIND=8), DIMENSION(:,:), INTENT(OUT)   :: rr_f

    LOGICAL, DIMENSION(:),   ALLOCATABLE        :: virgin
    INTEGER, DIMENSION(:,:), ALLOCATABLE        :: j_mid
    INTEGER, DIMENSION(:),   ALLOCATABLE        :: jjs_mid

    INTEGER      :: np, me, nw, kd, n, m, k
    INTEGER      :: n1, n2, n3, n4, i1, i2, ms, m2, m3 
    INTEGER      :: k1, k2, m_op_k, kk, i, mm
    REAL(KIND=8) :: a
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: r_mid
    !Iso P2
    INTEGER, DIMENSION(2) :: ms3, ns3
    REAL(KIND=8) :: epsilon=1.d-9, dist, d1, d2, d3, s1, s2, s3, shalf
    INTEGER      :: bord, ns, ns1, index
    !Iso P2

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

    virgin = .true.

    n = np

    DO m = 1, me ! loop on the elements

       DO k = 1, nw ! loop on the nodes (sides) of the element

          m_op_k = m_op(k, m)

          k1 = MODULO(k,   nw) + 1;  n1 = jj_in(k1, m)
          k2 = MODULO(k+1, nw) + 1;  n2 = jj_in(k2, m)
          r_mid = (rr_in(:, n1) + rr_in(:, n2))/2
          !if (ABS(rr_in(1, n1)-rr_in(1, n2)) + ABS(rr_in(2, n1)-rr_in(2, n2)) .GE. .5) THEN
          !           write(*,*) 'Probleme: m',m,'k',k,'m_op_k',m_op_k,'n',n
          !           write(*,*) '  r1x',rr_in(1, n1),'r2x',rr_in(1, n2)
          !           write(*,*) '  r1y',rr_in(2, n1),'r2y',rr_in(2, n2)
          !end if

          IF (m_op_k == 0) THEN  !  the side is on the boundary

             n = n + 1
             j_mid(k, m) = n
             !            rr_f(:, n) = r_mid
             IF (isopar.NE.1) THEN
                rr_f(:, n) = r_mid
                go to 1000
             END IF
             !
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
                         GO TO 100
                      END IF
                   END IF
                END DO
             END DO
             WRITE(*,*) ' BUG: pas de segment trouve'
             STOP

100          index = 1

             DO ms = 1, SIZE(jjs_in,2)  
                d1 = SQRT(SUM((rr_in(:,n1)-rr_in(:,jjs_in(1,ms)))**2))
                d2 = SQRT(SUM((rr_in(:,n2)-rr_in(:,jjs_in(2,ms)))**2))
                IF (d1.LE.epsilon .AND. d2.GT.2*epsilon) THEN
                   ms3(index) = sides(ms)
                   ns3(index)  = jjs_in(2,ms)
                   index = index + 1
                   IF (index.GE. 3) EXIT
                ELSE IF (d2.LE.epsilon .AND. d1.GT.2*epsilon) THEN
                   ms3(index) = sides(ms)
                   ns3(index)  = jjs_in(1,ms)
                   index = index + 1
                   IF (index.GE. 3) EXIT
                END IF
             END DO

             DO index = 1, 2 
                IF (bord .EQ. ms3(index)) THEN
                   n3 = ns3(index) 
                   EXIT
                END IF
             END DO

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
             !---Fin P2 iso-parametrique

             ! surface elements of the p2 grid are defined later
1000         continue

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

    !  connectivity matrix of the p2 grid

    !   jj_f(nw+1 : SIZE(jj_f,1), :) = j_mid
    jj_f(4 : 6, 1:me) = j_mid(1:3,1:me)
    CALL plot_pressure_p2_label(jj_f, rr_f, rr_f(1,:), 'maill_create.plt')


    !  connectivity matrix of the surface elements of the p2 grid


    DO ms = 1, SIZE(jjs_in,2);  mm = neigh_el(ms)
       !      DO i = 1,nw 
       !         IF (m_op(i, mm) == 0) kk = i
       !      ENDDO
       !TEST
       DO i = 1,nw 
          IF (jjs_in(1,ms).NE.jj_in(i,mm) .AND. jjs_in(2,ms).NE.jj_in(i,mm)) THEN
             kk = i
             EXIT
          END IF
       ENDDO
       !TEST
       jjs_mid(ms) = j_mid(kk, mm)
    ENDDO


    jjs_f(1:SIZE(jjs_in,1), :) = jjs_in

    jjs_f(3, :) = jjs_mid

    DEALLOCATE(virgin,j_mid,jjs_mid,r_mid)


  END SUBROUTINE  create_grid_p1p2

  FUNCTION voisin(j_in,neigh_in,jj_in)  RESULT(n_vois)

    IMPLICIT NONE
    INTEGER,                 INTENT(IN) :: j_in, neigh_in
    INTEGER, DIMENSION(:,:), INTENT(IN) :: jj_in
    INTEGER                             :: n_vois, n, nw

    nw = 3
    IF (neigh_in .EQ. 0) THEN
       n_vois = 0
       RETURN
    END IF
    DO n = 1, nw
       IF (jj_in(n,neigh_in) .EQ. j_in) THEN
          n_vois = 4*(neigh_in-1) + n
          EXIT
       END IF
    END DO
  END FUNCTION voisin

END PROGRAM p1p2_1d

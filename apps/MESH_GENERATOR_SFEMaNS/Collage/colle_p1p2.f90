PROGRAM colle_p1

  USE sub_plot
  IMPLICIT NONE

  !------------------------------------------------------------------------------
  !  jj(nw,   me_1)    nodes of the  volume_elements
  !  jjs(nws, me_1s)   nodes of the surface_eleme_1nts --> volume numbering
  !  iis(nws, me_1s)   nodes of the surface_eleme_1nts --> surface numbering 
  !  mm(me_1)          (possibly sub) set of elements for quadrature
  ! mms(me_1s)         (possibly sub) set of surface_elements for surf_quadrature
  !------------------------------------------------------------------------------

  INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: jj_1, jj_2, jjs_1, jjs_2, jjs, &
       jj_c, jj, jjs_c, jjs_tmp
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_1, rr_2, rr, rr_c
  INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: neigh_1, neigh_2, neigh_c, neigh
  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: sides_1, sides_2, sides,&
       neighs_1, neighs_2, neighs, &
       id_1, id_2, id12, &
       neighs_tmp, sides_tmp, &
       NEW, nouv, list, inter_save
  LOGICAL,      ALLOCATABLE, DIMENSION(:)    :: virgin, inter_1, inter_2

  INTEGER :: kd, nw, nws, me_1, me_2, me, mes_1, mes_2, mes, np_1, np_2, np, &
       i, k, n, n1, n2, m, ns, ms, ns1, ns2, ms1, ms2, count, &
       ms_inter, ms_oubli, n_inter, mes_12, ntrou, nps_c, np_c, nps, mm

  INTEGER             :: d_end, f_end, nb_sd, it, itmax
  INTEGER, DIMENSION(1) :: n_loc
  REAL(KIND=8) :: epsilon = 5.d-13, r_norm
  CHARACTER(len=64) :: dir_1,fil_1, dir_2,fil_2
  CHARACTER(len=1) :: tdom    
  LOGICAL :: test_form

  OPEN(UNIT=20,file='data_colle',FORM='formatted',STATUS='unknown')
  WRITE(*,*) ' Formatted true/false ' !true=formatted, false=non formatted
  READ(20,*) test_form
  WRITE(*,'(A)',ADVANCE='NO') ' Nombre de sous-domaines a coller > '
  READ(20,*) nb_sd

  IF (nb_sd == 1) THEN
     itmax = 1
  ELSE 
     itmax = nb_sd - 1
  END IF

  DO it = 1, itmax

     IF (it == 1) THEN
        WRITE(*,'(A)',ADVANCE='NO') ' Repertoire et nom du 1er fichier', &
             ' a coller > '
        READ(20,*) dir_1, fil_1
     ELSE
        dir_1 = '.'
        fil_1 = 'COLLE.FEM'
     END IF

     IF (nb_sd>1) THEN
        !--Gestion des interfaces------------------------------------------------------
        WRITE(*,'(A)',ADVANCE='NO') &
             ' Combien d''interface a conserver > '
        READ(20,*) n_inter
        ALLOCATE(inter_save(n_inter))

        WRITE(*,'(A)',ADVANCE='NO') ' Liste des index des interfaces > '
        READ(20,*) inter_save
        !------------------------------------------------------------------------------

        WRITE(*,'(A)',ADVANCE='NO') &
             ' Repertoire et nom du 2eme fichier a coller > '
        READ(20,*) dir_2, fil_2

        !CLOSE(20)

     END IF

     d_end = last_c_leng (64, dir_1)
     f_end = last_c_leng (64, fil_1)

     WRITE (*,*) 'Loading mesh-file 1 ...'

     IF (it > 1) THEN
     !IF (it .GE. 1) THEN

        OPEN  (30,FILE=dir_1(1:d_end)//'/'//fil_1(1:f_end),&
             FORM='unformatted')

        !--READ GRID DATA AND ARRAY ALLOCATION FOR 1RST GRID---------------------------

        READ  (30)  np_1,  nw,  me_1,  nws,  mes_1

        SELECT CASE(nw)
        CASE(3); kd=2
        CASE(4); kd=3
        END SELECT

        ALLOCATE (jj_1(nw,me_1),neigh_1(nw,me_1), id_1(me_1))
        ALLOCATE (jjs_1(nws,mes_1),neighs_1(mes_1),sides_1(mes_1))
        ALLOCATE (rr_1(kd,np_1))

        READ(30) jj_1, neigh_1, id_1
        READ(30) jjs_1, neighs_1, sides_1
        READ(30) rr_1 

        WRITE(*,*) 'Done'

        CLOSE(30)   

     ELSE

        OPEN(30,FILE=dir_1(1:d_end)//'/'//fil_1(1:f_end),&
             FORM='formatted')

        !--READ GRID DATA AND ARRAY ALLOCATION FOR 1RST GRID---------------------------
        READ  (30, *)  np_1,  nw,  me_1,  nws,  mes_1

        SELECT CASE(nw)
        CASE(3); kd=2
        CASE(4); kd=3
        END SELECT
        ALLOCATE (jj_1(nw,me_1),neigh_1(nw,me_1), id_1(me_1))
        ALLOCATE (jjs_1(nws,mes_1),neighs_1(mes_1),sides_1(mes_1))
        ALLOCATE (rr_1(kd,np_1))
        DO m = 1, me_1
           READ(30,*) jj_1(:,m), neigh_1(:,m), id_1(m)
        END DO
        DO ms = 1, mes_1
           READ(30,*) jjs_1(:,ms), neighs_1(ms), sides_1(ms)
        END DO
        DO n = 1, np_1
           READ(30,*) rr_1(:,n) 
        END DO
        WRITE(*,*) 'Done'
        CLOSE(30)
     END IF
     !--END OF GRID READING -------------------------------------------------------- 

     IF (nb_sd==1) THEN

        IF (test_form .AND. it==itmax) THEN
           OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',&
                FORM='formatted',STATUS='unknown')
           WRITE(10,111) np_1, nw, me_1, nws, mes_1
           DO m = 1, me_1 
              WRITE(10, 111)  jj_1(:,m), neigh_1(:,m), id_1(m)
           END DO
           DO m = 1, mes_1
              WRITE(10, 111)  jjs_1(:,m), neighs_1(m),  sides_1(m)
           END DO
           DO n = 1, np_1
              WRITE(10, 112)  rr_1(:,n)
           END DO
        ELSE
           OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',&
                FORM='unformatted',STATUS='unknown')
           WRITE(10) np_1, nw, me_1, nws, mes_1
           WRITE(10) jj_1, neigh_1, id_1
           WRITE(10) jjs_1, neighs_1,  sides_1
           WRITE(10) rr_1
        END IF
        WRITE(*,*) ' Maillage dans ',dir_1(1:d_end)//'/COLLE.FEM'
        CLOSE(10)

        !========= Now we create the P2 mesh
        me = me_1
        mes = mes_1
        np = np_1  

        ALLOCATE(id12(1:me_1))
        id12 = id_1

        ALLOCATE(jj_c(3,me),jjs_c(2,mes),neigh_c(3,me),rr_c(2,np),neighs(mes),sides(mes))
        jj_c  = jj_1
        neigh_c = neigh_1
        rr_c = rr_1
        jjs_c = jjs_1
        neighs= neighs_1
        sides = sides_1

     ELSE

        !--READ GRID DATA AND ARRAY ALLOCATION FOR 2ND GRID---------------------------

        d_end = last_c_leng (64, dir_2)
        f_end = last_c_leng (64, fil_2)

        WRITE (*,*) 'Loading mesh-file 2 ...'

        IF (test_form) THEN
           OPEN  (30,FILE=dir_2(1:d_end)//'/'//fil_2(1:f_end),&
                FORM='formatted')

           READ  (30, *)  np_2,  nw,  me_2,  nws,  mes_2

           ALLOCATE (jj_2(nw,me_2),neigh_2(nw,me_2), id_2(me_2))
           ALLOCATE (jjs_2(nws,mes_2),neighs_2(mes_2),sides_2(mes_2))
           ALLOCATE (rr_2(kd,np_2))

           DO m = 1, me_2
              READ(30,*) jj_2(:,m), neigh_2(:,m), id_2(m)
           END DO

           DO ms = 1, mes_2
              READ(30,*) jjs_2(:,ms), neighs_2(ms), sides_2(ms)
           END DO

           DO n = 1, np_2
              READ(30,*) rr_2(:,n) 
           END DO

        ELSE

           OPEN  (30,FILE=dir_2(1:d_end)//'/'//fil_2(1:f_end),&
                FORM='unformatted')
           READ(30)  np_2,  nw,  me_2,  nws,  mes_2
           ALLOCATE (jj_2(nw,me_2),neigh_2(nw,me_2), id_2(me_2))
           ALLOCATE (jjs_2(nws,mes_2),neighs_2(mes_2),sides_2(mes_2))
           ALLOCATE (rr_2(kd,np_2))
           READ(30) jj_2, neigh_2, id_2
           READ(30) jjs_2, neighs_2, sides_2
           READ(30) rr_2 
        END IF

        WRITE(*,*) 'Done'

        CLOSE(30)

        count = 0
        DO m = 1, me_1
           IF (MINVAL(neigh_1(:,m))==0) count = count+1
        END DO
        DO m = 1, me_2
           IF (MINVAL(neigh_2(:,m))==0) count = count+1
        END DO
        mes_12 = count
        !--END OF GRID READING --------------------------------------------------------

        ALLOCATE(list(nws),virgin(np_2),inter_1(mes_1),inter_2(mes_2),NEW(np_2),nouv(np_2))

        !--Make list of nodes at the interface
        DO n = 1, np_2
           NEW(n) = n
        END DO
        nouv = 0

        DO m = 1, me_2
           DO n =1, nw
              IF (neigh_2(n,m) /=0) neigh_2(n,m) = neigh_2(n,m) + me_1
           END DO
        END DO
        neighs_2 = neighs_2 + me_1

        virgin = .TRUE.
        inter_1= .FALSE.
        inter_2= .FALSE.

        count = 0
        r_norm = MAX(MAXVAL(rr_2(1,:))-MINVAL(rr_2(1,:)),&
             MAXVAL(rr_2(2,:))-MINVAL(rr_2(2,:)),&
             MAXVAL(rr_1(1,:))-MINVAL(rr_1(1,:)),&
             MAXVAL(rr_1(2,:))-MINVAL(rr_1(2,:)))
        epsilon = epsilon * r_norm

        DO ms2 = 1, mes_2
           r_norm = MAXVAL(ABS(rr_2(:,jjs_2(1,ms2))-rr_2(:,jjs_2(2,ms2))))
           loop_ms1: DO ms1 = 1, mes_1
              DO k = 1, nws
                 DO ns = 1, nws
                    list(ns) = MODULO(nws-ns+k-1,nws) + 1 
                 END DO
                 IF (MAXVAL(ABS(rr_1(:,jjs_1(list,ms1))-rr_2(:,jjs_2(:,ms2)))).GT.epsilon) CYCLE 
                 DO ns2 = 1, nws
                    n2 = jjs_2(ns2,ms2)
                    IF (.NOT.virgin(n2)) CYCLE
                    DO ns1 = 1, nws
                       n1 = jjs_1(ns1,ms1)
                       IF (MAXVAL(ABS(rr_1(:,n1)-rr_2(:,n2))) .LE. epsilon) THEN
                          nouv(n2) = n1
                          NEW(n2+1:np_2) = NEW(n2+1:np_2) - 1
                          NEW(n2) = 0
                          virgin(n2) = .FALSE.
                          EXIT
                       END IF
                    END DO
                 END DO

                 list = jjs_1(:,ms1) 
                 DO n = 1, nw
                    IF (MINVAL(ABS(list -jj_1(n,neighs_1(ms1)))) /= 0) THEN
                       neigh_1(n,neighs_1(ms1)) = neighs_2(ms2)
                       EXIT
                    END IF
                 END DO
                 list = jjs_2(:,ms2) 
                 DO n = 1, nw
                    IF (MINVAL(ABS(list -jj_2(n,neighs_2(ms2)-me_1))) /= 0) THEN
                       neigh_2(n,neighs_2(ms2)-me_1) = neighs_1(ms1) 
                       EXIT
                    END IF
                 END DO

                 inter_1(ms1) = .TRUE.
                 inter_2(ms2) = .TRUE.
                 count = count + 1
                 EXIT loop_ms1 

              END DO
           END DO loop_ms1
        END DO

        WRITE(*,*) ' Il y a ',count,' segments a coller'

        NEW = NEW + np_1
        DO m = 1, me_2
           DO n = 1, nw
              i = jj_2(n,m)
              IF (virgin(i)) THEN
                 jj_2(n,m) = NEW(i)
              ELSE
                 jj_2(n,m) = nouv(i)
              END IF
           END DO
        END DO
        DO ms2 = 1, mes_2
           DO ns2 = 1, nws
              i = jjs_2(ns2,ms2)
              IF (virgin(i)) THEN
                 jjs_2(ns2,ms2) = NEW(i)
              ELSE
                 jjs_2(ns2,ms2) = nouv(i)
              END IF
           END DO
        END DO


        !  ALLOCATE(sides(mes_1+mes_2), jjs(nws,mes_1+mes_2), neighs(mes_1+mes_2))
        ALLOCATE(sides_tmp(mes_1+mes_2), jjs_tmp(nws,mes_1+mes_2), neighs_tmp(mes_1+mes_2))
        ms = 0
        DO ms1 = 1, mes_1
           IF (inter_1(ms1)) CYCLE ! C'est une interface
           ms = ms + 1
           sides_tmp(ms) = sides_1(ms1)
           jjs_tmp(:,ms) = jjs_1(:,ms1)
           neighs_tmp(ms) = neighs_1(ms1)
        END DO
        DO ms2 = 1, mes_2
           IF (inter_2(ms2)) CYCLE ! C'est une interface
           ms = ms + 1
           sides_tmp(ms) = sides_2(ms2)
           jjs_tmp(:,ms) = jjs_2(:,ms2)
           neighs_tmp(ms) = neighs_2(ms2)
        END DO

        ms_inter = 0
        ms_oubli = 0
        DO ms1 = 1, mes_1
           IF (.NOT.inter_1(ms1)) CYCLE ! Ce n'est pas une interface
           IF (MINVAL(ABS(inter_save-sides_1(ms1))) /= 0) THEN
              ms_oubli = ms_oubli + 1
              CYCLE                     ! Interface a oublier
           ELSE
              ms_inter = ms_inter + 1   ! On conserve l'interface
           END IF
           ms = ms + 1
           sides_tmp(ms) = sides_1(ms1)
           jjs_tmp(:,ms) = jjs_1(:,ms1)
           neighs_tmp(ms) = neighs_1(ms1)
        END DO

        mes = ms
        me = me_1 + me_2
        WRITE(*,*) ' Il y a ', mes - ms_inter,' segments de frontiere exterieure'
        WRITE(*,*) ' Il y a ', ms_inter,      ' segments de frontiere interieure'

        !==Test
        count = 0
        DO m = 1, me_1
           IF (MINVAL(neigh_1(:,m))==0) count = count+1
        END DO
        DO m = 1, me_2
           IF (MINVAL(neigh_2(:,m))==0) count = count+1
        END DO
        WRITE(*,*) ' Il y a ',count,' voisins de frontiere exterieure'
        m = mes_1 + mes_2 - ms_inter -2*ms_oubli 
        ms = mes_12 - 2*(ms_inter + ms_oubli)
        WRITE(*,*) ' mes_1 ', mes_1
        WRITE(*,*) ' mes_2 ', mes_2
        WRITE(*,*) ' ms_inter ', ms_inter
        WRITE(*,*) ' ms_oubli ', ms_oubli
        WRITE(*,*) ' mes ', mes, ' verif ', m
        WRITE(*,*) ' count ', count, ' verif ', ms
        IF (mes /= m .OR. count/=ms) THEN
           WRITE(*,*) ' Il y a eu un probleme dans le collage !'
           STOP
        END IF
        !==Test

        np = MAXVAL(jj_2) 

        ALLOCATE (rr(kd,np))
        rr(:,1:np_1) = rr_1 
        DO i = 1, np_2 
           IF (NEW(i) .LE. np_1) CYCLE 
           rr(:,NEW(i)) = rr_2(:,i)
        END DO

        i = me_1 + me_2
        IF(ALLOCATED(id12)) THEN
           DEALLOCATE(id12)
        END IF
        ALLOCATE(id12(i))
        id12(1:me_1) = id_1(1:me_1)
        id12(me_1+1:) = id_2    

        ALLOCATE(jj_c(3,me),neigh_c(3,me))
        jj_c(:,1:me_1)    = jj_1(:,:)
        jj_c(:,1+me_1:me) = jj_2(:,:)
        neigh_c(:,1:me_1)    = neigh_1(:,:)
        neigh_c(:,1+me_1:me) = neigh_2(:,:)
        !-------------------------------------

        ALLOCATE(jjs(nws,mes), neighs(mes), sides(mes))
        jjs = jjs_tmp(:,1:mes)
        neighs = neighs_tmp(1:mes)
        sides= sides_tmp(1:mes)

        !ATTENTION A LA LONGUEUR DES VECTEURS
        IF (test_form .AND. it==itmax) THEN
           OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',&
                FORM='formatted',STATUS='unknown')
           WRITE(10,111) np, nw, me, nws, mes
           DO m = 1, me 
              WRITE(10, 111)  jj_c(:,m), neigh_c(:,m), id12(m)
           END DO
           DO m = 1, mes 
              WRITE(10, 111)  jjs(:,m), neighs(m),  sides(m)
           END DO
           DO n = 1, np 
              WRITE(10, 112)  rr(:,n)
           END DO
        ELSE
           OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',&
                FORM='unformatted',STATUS='unknown')
           WRITE(*,*) ' Maillage dans ',dir_1(1:d_end)//'/COLLE.FEM'
           WRITE(10) np, nw, me, nws, mes
           WRITE(10) jj_c, neigh_c, id12
           WRITE(10) jjs, neighs, sides
           WRITE(10) rr
        END IF
        CLOSE(10)

        IF (it==itmax) THEN
           DO i = 1, MIN(MAXVAL(id12),9)
              WRITE(tdom,'(i1)') i
              CALL plot_scalar_field_domain(jj_c, rr, id12, i, &
                   'colle_dom_p1_'//tdom//'.plt') 
           END DO
        END IF

        DEALLOCATE(jjs_tmp,neighs_tmp,sides_tmp)
        DEALLOCATE(jj_c,neigh_c)

     END IF

111  FORMAT(10(I6,3x))
112  FORMAT(10(e25.16,3x))

     IF (it==itmax) THEN

        IF (nb_sd> 1) THEN
           !========= Now we create the P2 mesh
           ALLOCATE(jj_c(3,me),jjs_c(2,mes),neigh_c(3,me),rr_c(2,np))
           jj_c(:,1:me_1)    = jj_1(:,:)
           jj_c(:,1+me_1:me) = jj_2(:,:)
           neigh_c(:,1:me_1)    = neigh_1(:,:)
           neigh_c(:,1+me_1:me) = neigh_2(:,:)
           rr_c = rr
           jjs_c = jjs(:,1:mes)
        END IF


        !-------------CALCULATION OF nps AND np FOR THE P2 GRIG------------------------
        !Viva Eulero
        np_c = np
        nps_c = nps
        WRITE(*,'(A)',ADVANCE='NO') &
             ' Attention aux trous: nombre de trou> '
        READ(*,*) ntrou 
        np    = np_c + (me + np_c -1 + ntrou ) ! in 2D
        nps   = 2 * mes 

        nw = 6
        nws = 3

        !--------------------ARRAY ALLOCATION------------------------------------------
        IF (ALLOCATED(jjs)) DEALLOCATE(jjs)
        IF (ALLOCATED(rr)) DEALLOCATE(rr)
        ALLOCATE (jj(nw, me), jjs(nws, mes), neigh(3,me))
        ALLOCATE (rr(2,np))
        neigh = neigh_c

        !TEST
        DO m = 1, me ! loop on the elements
           DO n = 1, 3 
              mm = neigh(n,m)
              IF (mm/=0) THEN
                 IF (MINVAL(ABS(neigh(:,mm)-m)) /=0) THEN
                    WRITE(*,*) ' BUG  sur l''element ', m
                    WRITE(*,*) ' m ', m, 'neigh(:,mm)', neigh(:,mm)
                    STOP
                 END IF
              END IF
           END DO
        END DO
        !TEST
        !------------------------------------------------------------------------------
        WRITE(*,*) ' Allocation ok'
        WRITE(*,*) ' nw',nw,',nws',nws,'me',me,'mes',mes,'np',np
        WRITE(*,*) ' Size(sides)', SIZE(sides), ' Size(jjs,2)', SIZE(jjs,2)
        !--------------------CREATE p1/P2 GRID-----------------------------------------
        CALL create_grid_iso_p1p2(jj_c, jjs_c, rr_c, neigh, neighs, jj, jjs, rr, id12)
        !CALL create_grid_p1p2(jj_c, jjs_c, rr_c, neigh, neighs, jj, jjs, rr, id12)
        !------------------------------------------------------------------------------
        WRITE(*,*) ' P2 mesh constructed'

        IF (test_form) THEN
           OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',&
                FORM='formatted',STATUS='unknown')
           ! Skip P1 mesh
           READ(10,111) np_c, n1, n2, ns1, mes_1
           DO m = 1, n2
              READ(10, 111) 
           END DO
           DO m = 1, mes_1
              READ(10, 111)  
           END DO
           DO n = 1, np_c
              READ(10, 112)
           END DO
           ! End Skip P1 mesh
           WRITE(10,111) np, nw, me, nws, mes
           DO m = 1, me
              WRITE(10, 111)  jj(:,m), neigh(:,m), id12(m)
           END DO
           DO m = 1, mes
              WRITE(10, 111)  jjs(:,m), neighs(m),  sides(m)
           END DO
           DO n = 1, np
              WRITE(10, 112)  rr(:,n)
           END DO
        ELSE
           ! Skip P1 mesh
           OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',&
                FORM='unformatted',STATUS='unknown')
           READ(10) 
           READ(10)
           READ(10)
           READ(10)
           ! End Skip P1 mesh
           WRITE(10) np, nw, me, nws, mes
           WRITE(10)  jj, neigh, id12
           WRITE(10)  jjs, neighs,  sides
           WRITE(10)  rr
        END IF

        CLOSE(10)

        WRITE(*,*) ' Finished writing mesh; start verifications'

        !==================verifications
        DEALLOCATE(neigh_1,rr_1)
        ALLOCATE(neigh_1(3,me),rr_1(1,me))

        DO i = 1, MIN(MAXVAL(id12),9)
           WRITE(tdom,'(i1)') i
           CALL plot_scalar_field_domain(jj, rr, id12, i, 'colle_dom'//tdom//'.plt') 
        END DO

        neigh_1(:,1:me_1)    = jj_1(:,:)
        IF (me_1/=me) neigh_1(:,1+me_1:me) = jj_2(:,:)
        rr_1(1,1:me_1)    = id_1
        IF (me_1/=me) rr_1(1,1+me_1:me) = id_2
        CALL plot_const_p1_label(neigh_1, rr, rr_1(1,:), 'colle.plt')

        rr_1 = 0
        rr_1(1,neighs(1:mes)) = sides(1:mes)
        CALL plot_const_p1_label(neigh_1, rr, rr_1(1,:), 'sides.plt')

        CALL plot_pressure_p2_label(jj, rr, rr(1,:), 'verif_p2.plt')

        !======Mesure des distances
        !      DEALLOCATE(rr_1)
        !      ALLOCATE(rr_1(2,np))
        !      DO n = 1, np
        !         DO k = 1, 2
        !            rr_1(k,:) = rr(k,:)- rr(k,n)
        !         END DO
        !         rr_1(:,n) = -1.d10
        !         IF (MINVAL(ABS(rr_1(1,:)) +ABS(rr_1(2,:))) .LE. 1.d-4) THEN
        !            n_loc = MINLOC(ABS(rr_1(1,:)) +ABS(rr_1(2,:)))
        !            WRITE(*,*) ' BUG sur la numerotation', n, &
        !                       n_loc(1), rr(:,n), rr(:,n_loc(1))
        !         END IF
        !      END DO

        !===================Plot
        GO TO 1000
        OPEN (40, FILE='cav_face.plt', FORM='formatted', &
             STATUS='unknown')
        WRITE(40,*) '$ DATA=CURVE3D'
        WRITE(40,*) ' '
        WRITE(40,*) '% dfilltype = 1'
        WRITE(40,*) '% pointid=TRUE'
        WRITE(40,*) '% hiddenline=TRUE'
        WRITE(40,*) '% axisscale=FALSE'

        DO m = 1, SIZE(jj,2)
           WRITE(40,*) '% fillcolor=',1
           WRITE(40,*) rr(:,jj(1,m)), jj(1,m)
           WRITE(40,*) rr(:,jj(6,m)), jj(6,m)
           WRITE(40,*) rr(:,jj(5,m)), jj(5,m)
           WRITE(40,*) ' '
           WRITE(40,*) '% fillcolor=',2
           WRITE(40,*) rr(:,jj(6,m)), jj(6,m)
           WRITE(40,*) rr(:,jj(2,m)), jj(2,m)
           WRITE(40,*) rr(:,jj(4,m)), jj(4,m)
           WRITE(40,*) ' '
           WRITE(40,*) '% fillcolor=',3
           WRITE(40,*) rr(:,jj(4,m)), jj(4,m)
           WRITE(40,*) rr(:,jj(3,m)), jj(3,m)
           WRITE(40,*) rr(:,jj(5,m)), jj(5,m)
           WRITE(40,*) ' '
           WRITE(40,*) '% fillcolor=',4
           WRITE(40,*) rr(:,jj(6,m)), jj(6,m)
           WRITE(40,*) rr(:,jj(4,m)), jj(4,m)
           WRITE(40,*) rr(:,jj(5,m)), jj(5,m)
           WRITE(40,*) ' '
        END DO

        WRITE(40,*) '$ END'
1000    CONTINUE
        WRITE(*,*) ' Verifications done'



        !TEST
        WRITE(50,*) ' $DATA=CURVE3D'
        DO m = 1, SIZE(jjs,2)
           WRITE(50,*) rr(1,jjs(1,m)), rr(2,jjs(1,m)), 0.d0
           WRITE(50,*) rr(1,jjs(2,m)), rr(2,jjs(2,m)), 0.d0
           WRITE(50,*) ' '
        END DO
        !TEST

     END IF


     IF (nb_sd>1) THEN
        DEALLOCATE (jj_1,neigh_1, id_1)
        DEALLOCATE (jjs_1,neighs_1,sides_1)
        DEALLOCATE (rr_1)
        DEALLOCATE (jj_2,neigh_2, id_2)
        DEALLOCATE (jjs_2,neighs_2,sides_2)
        DEALLOCATE (rr_2)
        DEALLOCATE(inter_save)
        DEALLOCATE(list,virgin,inter_1,inter_2,NEW,nouv)
        DEALLOCATE(sides, jjs, neighs)
        DEALLOCATE (rr)
     END IF

  END DO



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

  SUBROUTINE create_grid_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el,&
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
    INTEGER      :: n1, n2, n3, i1, i2, ms, m2, m3 
    INTEGER      :: k1, k2, m_op_k, kk, i, mm
    REAL(KIND=8) :: a
    REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: r_mid


    nw  = SIZE(jj_in,1)   ! nodes in each volume element
    me  = SIZE(jj_in,2)
    kd  = SIZE(rr_in,1)   ! space dimensions
    np  = SIZE(rr_in,2)

    !TEST
    !  DO m = 1, me ! loop on the elements
    !     DO n = 1, nw 
    !        mm = m_op(n,m)
    !        IF (mm/=0) THEN
    !           IF (MINVAL(ABS(m_op(:,mm)-m)) /=0) THEN
    !              WRITE(*,*) ' BUG  sur l''element ', m
    !              STOP
    !           END IF
    !        END IF
    !     END DO
    !  END DO
    !TEST

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

          IF (m_op_k == 0) THEN  !  the side is on the boundary

             n = n + 1
             j_mid(k, m) = n
             rr_f(:, n) = r_mid

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

    DEALLOCATE(virgin)
    DEALLOCATE(j_mid)
    DEALLOCATE(jjs_mid)
    DEALLOCATE(r_mid)

  END SUBROUTINE  create_grid_p1p2


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

END PROGRAM colle_p1






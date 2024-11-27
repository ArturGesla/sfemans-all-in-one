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

  INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: jj_1, jj_2, jjs_1, jjs_2, jjs
  REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr_1, rr_2, rr
  INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: neigh_1, neigh_2
  INTEGER,      ALLOCATABLE, DIMENSION(:)   :: sides_1, sides_2, sides,&
                                               neighs_1, neighs_2, neighs, id_1, id_2, &
                                               new, nouv, list, inter_save
  LOGICAL,      ALLOCATABLE, DIMENSION(:)    :: virgin, inter_1, inter_2

  INTEGER :: kd, nw, nws, me_1, me_2, me, mes_1, mes_2, mes, np_1, np_2, np, &
              i, k, n, n1, n2, m, ns, ms, ns1, ns2, ms1, ms2, count, &
              ms_inter, ms_oubli, n_inter, mes_12

  INTEGER             :: d_end, f_end, nb_sd, it
  REAL(KIND=8) :: eps_save = 5.d-5, r_norm, epsilon
  CHARACTER(len=64) :: dir_1,fil_1, dir_2,fil_2

   OPEN(UNIT=20,file='data_colle',FORM='formatted',STATUS='unknown')
   WRITE(*,'(A)',ADVANCE='NO') ' Nombre de sous-domaines a coller > '
   READ(20,*) nb_sd

   DO it = 1, nb_sd - 1
   IF (it == 1) THEN
      WRITE(*,'(A)',ADVANCE='NO') ' Repertoire et nom du 1er fichier a coller > '
      READ(20,*) dir_1, fil_1
   ELSE
      dir_1 = '.'
      fil_1 = 'COLLE.FEM'
   END IF

!--Gestion des interfaces------------------------------------------------------
   WRITE(*,'(A)',ADVANCE='NO') ' Combien d''interface a conserver > '
   READ(20,*) n_inter
   ALLOCATE(inter_save(n_inter))

   WRITE(*,'(A)',ADVANCE='NO') ' Liste des index des interfaces concernees > '
   READ(20,*) inter_save
!------------------------------------------------------------------------------

   WRITE(*,'(A)',ADVANCE='NO') ' Repertoire et nom du 2eme fichier a coller > '
   READ(20,*) dir_2, fil_2

   d_end = last_c_leng (64, dir_1)
   f_end = last_c_leng (64, fil_1)

   WRITE (*,*) 'Loading mesh-file 1 ...'

   OPEN  (30,FILE=dir_1(1:d_end)//'/'//fil_1(1:f_end),FORM='formatted')

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
!--END OF GRID READING --------------------------------------------------------

!--READ GRID DATA AND ARRAY ALLOCATION FOR 2ND GRID---------------------------

   d_end = last_c_leng (64, dir_2)
   f_end = last_c_leng (64, fil_2)

   WRITE (*,*) 'Loading mesh-file 2 ...'

   OPEN  (30,FILE=dir_2(1:d_end)//'/'//fil_2(1:f_end),FORM='formatted')

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


   
ALLOCATE(list(nws),virgin(np_2),inter_1(mes_1),inter_2(mes_2),new(np_2),nouv(np_2))

!--Make list of nodes at the interface
  DO n = 1, np_2
     new(n) = n
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
  epsilon = eps_save * r_norm
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
                    new(n2+1:np_2) = new(n2+1:np_2) - 1
                    new(n2) = 0
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

  new = new + np_1
  DO m = 1, me_2
     DO n = 1, nw
        i = jj_2(n,m)
        IF (virgin(i)) THEN
           jj_2(n,m) = new(i)
        ELSE
           jj_2(n,m) = nouv(i)
        END IF
     END DO
  END DO
  DO ms2 = 1, mes_2
     DO ns2 = 1, nws
        i = jjs_2(ns2,ms2)
        IF (virgin(i)) THEN
           jjs_2(ns2,ms2) = new(i)
        ELSE
           jjs_2(ns2,ms2) = nouv(i)
        END IF
     END DO
  END DO


  ALLOCATE(sides(mes_1+mes_2), jjs(nws,mes_1+mes_2), neighs(mes_1+mes_2))
  ms = 0
  DO ms1 = 1, mes_1
     IF (inter_1(ms1)) CYCLE ! C'est une interface
     ms = ms + 1
     sides(ms) = sides_1(ms1)
     jjs(:,ms) = jjs_1(:,ms1)
     neighs(ms) = neighs_1(ms1)
  END DO
  DO ms2 = 1, mes_2
     IF (inter_2(ms2)) CYCLE ! C'est une interface
     ms = ms + 1
     sides(ms) = sides_2(ms2)
     jjs(:,ms) = jjs_2(:,ms2)
     neighs(ms) = neighs_2(ms2)
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
     sides(ms) = sides_1(ms1)
     jjs(:,ms) = jjs_1(:,ms1)
     neighs(ms) = neighs_1(ms1)
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
     IF (new(i) .LE. np_1) CYCLE 
     rr(:,new(i)) = rr_2(:,i)
  END DO
    
!-------------------------------------

      OPEN(UNIT=10,FILE=dir_1(1:d_end)//'/COLLE.FEM',FORM='formatted',STATUS='unknown')
      WRITE(*,*) ' Maillage dans ',dir_1(1:d_end)//'/COLLE.FEM'

      WRITE(10,*) np, nw, me, nws, mes

      DO m = 1, me_1 
         WRITE(10, *)  jj_1(:,m), neigh_1(:,m), id_1(m)
      END DO
      DO m = 1, me_2 
         WRITE(10, *)  jj_2(:,m), neigh_2(:,m), id_2(m)
      END DO

      DO m = 1, mes 
          WRITE(10, *)  jjs(:,m), neighs(m),  sides(m)
      END DO

      DO n = 1, np 
          WRITE(10, *)  rr(:,n)
      END DO

      CLOSE(10)

   IF (it==nb_sd-1) THEN
      DEALLOCATE(neigh_1,rr_1)
      ALLOCATE(neigh_1(nw,me),rr_1(1,me))
      neigh_1(:,1:me_1)    = jj_1(:,:)
      neigh_1(:,1+me_1:me) = jj_2(:,:)
      rr_1(1,1:me_1)    = id_1
      rr_1(1,1+me_1:me) = id_2

      CALL plot_const_p1_label(neigh_1, rr, rr_1(1,:), 'colle.plt')

      rr_1 = 0
      rr_1(1,neighs(1:mes)) = sides(1:mes)
      CALL plot_const_p1_label(neigh_1, rr, rr_1(1,:), 'sides.plt')
   END IF

   DEALLOCATE (jj_1,neigh_1, id_1)
   DEALLOCATE (jjs_1,neighs_1,sides_1)
   DEALLOCATE (rr_1)
   DEALLOCATE (jj_2,neigh_2, id_2)
   DEALLOCATE (jjs_2,neighs_2,sides_2)
   DEALLOCATE (rr_2)
   DEALLOCATE(inter_save)
   DEALLOCATE(list,virgin,inter_1,inter_2,new,nouv)
   DEALLOCATE(sides, jjs, neighs)
   DEALLOCATE (rr)

   END DO
       
   CLOSE(20)

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

END PROGRAM colle_p1

PROGRAM inter_mv_fem

!----------P1 mesh format------------------------------------------------------
USE grid_2d,  idir_c        => idir,        & ! Integer index for all boundary nodes 
                                !!!!!!!!!!interface nodes are duplicated
jjdir_c       => jjdir,       & ! Global index of boundary nodes
                                !!!!!!!!!!interface nodes are duplicated
sides         => iflux,       & ! Type of flux BC
jjs_c         => jjs,         & ! Global Connectivy of boundary elements
jj_c          => jj,          & ! Global connectivity of domain elements
rr_c          => rr,          & ! Coordinates of nodes
jj_previous_c => jj_previous, & ! Glob. connect. of elements of previous grid
rr_previous_c => rr_previous, & ! Coordinates of nodes of previous grid
np_c          => np_d,        & ! Number of domain nodes
nps_c         => np_b,        & ! Number of boundary nodes
me            => ns_d,        & ! Number of volume elements
mes           => ns_b           ! Number of boundary elements 
!
!Last generated grid
!   INTEGER, PUBLIC :: nd_d, np_d, ns_d, nd_b, np_b, ns_b
!
!   INTEGER, DIMENSION(:),   ALLOCATABLE, PUBLIC :: iparent, idir, jjdir, iflux, neighs
!   INTEGER, DIMENSION(:,:), ALLOCATABLE, PUBLIC :: jjs, neigh, jjs_b
!   REAL (KIND=8), DIMENSION(:,:),   ALLOCATABLE, PUBLIC :: rr_b
!   REAL (KIND=8), DIMENSION(:,:,:), ALLOCATABLE, PUBLIC :: hess
!
!   INTEGER,       DIMENSION(:,:), POINTER, PUBLIC :: jj
!   REAL (KIND=8), DIMENSION(:,:), POINTER, PUBLIC :: rr
!
!
!Previous grid
!   INTEGER, PUBLIC :: np_d_previous, ns_d_previous
!
!   INTEGER,       DIMENSION(:,:), POINTER, PUBLIC :: jj_previous
!   REAL (KIND=8), DIMENSION(:,:), POINTER, PUBLIC :: rr_previous
!------------------------------------------------------------------------------

USE sub_plot

   IMPLICIT NONE

   INTEGER:: k, m, ms, n, i_d=0,  kd=2, nw_c=3, nws_c=2, nw=6, nws=3
   CHARACTER(len=64) :: directory,file_name
   INTEGER,      ALLOCATABLE, DIMENSION(:,:) :: jj, jjs
   REAL(KIND=8), ALLOCATABLE, DIMENSION(:,:) :: rr
   INTEGER                                   :: np, nps

!-------------INITIAL MESH OPERATIONS
      directory = '.'
      WRITE(*,*)' Generate(1) or Read(2) the mesh?'
      read(*,*) m
      WRITE(*,*)' File name?'
      read(*,*)file_name
      if(m == 1) then
         CALL build_grid(1, 100, directory, file_name )
         CALL save_grid(1, directory, file_name )
         CALL plot_grid(1, directory, file_name )
      elseif(m == 2) then
        CALL load_grid(1, directory, file_name )
      endif

      CALL prep_maill_p1p2

      OPEN(UNIT=10,FILE='FEM.'//file_name,FORM='formatted',STATUS='unknown')

      write(10,*) np_c, nw_c, me, nws_c, mes

      DO m = 1, SIZE(jj_c, 2)
         WRITE(10, *)  (jj_c(n,m), n = 1,nw_c),  (neigh(n,m), n = 1,nw_c)
      END DO

      DO m = 1, SIZE(jjs_c, 2)
          WRITE(10, *)  (jjs_c(n,m), n = 1,nws_c),  neighs(m), sides(m)
      END DO

      DO n = 1, SIZE(rr_c, 2)
          WRITE(10, *)  (rr_c(k,n), k = 1,kd)
      END DO

   WRITE  (10, *) np, nw, me, nws, mes

   DO m = 1, me
      WRITE(10,*) jj(:,m)
   END DO

   DO ms = 1, mes
     WRITE(10,*) jjs(:,ms), m, sides(ms)
   END DO

   DO n = 1, SIZE(rr,2)
      WRITE(10,*) rr(:,n)
   END DO

   WRITE (*,*)  'np_c',np_c,'nwc',nw_c,'nwsc',nws_c,'me',me,'mes',mes
   WRITE (*,*)  'np  ',np,  'nw ',nw ,'nws ',nws

       CALL plot_pressure_p1_label(jj_c, rr_c, rr_c(1,:), 'maill_grossier.plt')
       CALL plot_pressure_p2_label(jj, rr, rr(1,:), 'maill_fin.plt')


CONTAINS

SUBROUTINE prep_maill_p1p2
  
   IMPLICIT NONE
   INTEGER              :: m, n_sides, k, n, j_size, ntrou
   INTEGER              :: n_faces, n_edges   !  3D only
   REAL(KIND=8)         :: dummy

!INTERFACE
!  SUBROUTINE create_grid_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el,&
!		    jj_f,  jjs_f,  rr_f)
!  INTEGER,      DIMENSION(:,:), INTENT(INOUT) :: jj_in, jjs_in, m_op
!  INTEGER,      DIMENSION(:),   INTENT(IN)    :: neigh_el
!  REAL(KIND=8), DIMENSION(:,:), INTENT(IN)    :: rr_in
!  INTEGER,      DIMENSION(:,:), INTENT(OUT)   :: jj_f, jjs_f
!  REAL(KIND=8), DIMENSION(:,:), INTENT(OUT)   :: rr_f
!  END SUBROUTINE create_grid_p1p2
!ND INTERFACE
!-------------END OF DECLARATIONS----------------------------------------------

!-------------CALCULATION OF nps AND np FOR THE P2 GRIG------------------------
   SELECT CASE(kd)  !Viva Eulero
      CASE(2)
         WRITE(*,'(A$)') ' Attention aux trous: nombre de trou> '
         READ(*,*) ntrou 
         np    = np_c + (me + np_c -1 + ntrou ) ! in 2D
!         nps_c = mes  
         nps   = 2 * mes 

      CASE(3)
	 n_faces = 2*me + mes/2
	 n_edges = n_faces + np_c - 1 - me    ! = me + mes/2 + np - 1 
	 np      = np_c + n_edges             ! in 3D

!         nps_c   = mes/2 + 2
	 nps     = nps_c + mes + nps_c - 2

   END SELECT
   WRITE (*,*) '  nps_c = ', nps_c, '  np = ', np, '  nps = ', nps
!-------------END CALCULATION OF nps AND np FOR THE P2 GRIG--------------------


!--------------------ARRAY ALLOCATION------------------------------------------
   IF (ALLOCATED(jj)) DEALLOCATE(jj,jjs,rr)
   ALLOCATE (jj(nw, me), jjs(nws, mes)) 
   ALLOCATE (rr(kd,np))
!   npdir = SIZE(idir_c) + mes ! 2D only
!   ALLOCATE (idir(npdir), jjdir(npdir)) ! I am unable to cope with numbering 
!------------------------------------------------------------------------------

!--------------------CREATE p1/P2 GRID-----------------------------------------
   CALL create_grid_p1p2(jj_c, jjs_c, rr_c, neigh, neighs, jj, jjs, rr)
!------------------------------------------------------------------------------

END SUBROUTINE  prep_maill_p1p2

!--------------------------------------------------------------------
!
!     This program creates the P2 mesh from the P1 mesh.
!
!--------------------------------------------------------------------

SUBROUTINE create_grid_p1p2(jj_in, jjs_in, rr_in, m_op, neigh_el,&
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
   INTEGER,      DIMENSION(:),   INTENT(IN)    :: neigh_el
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
         IF (m_op(i, mm) == 0) kk = i
      ENDDO
      jjs_mid(ms) = j_mid(kk, mm)
   ENDDO

   jjs_f(1:SIZE(jjs_in,1), :) = jjs_in

   jjs_f(3, :) = jjs_mid
   
  DEALLOCATE(virgin,j_mid,jjs_mid,r_mid)

END SUBROUTINE  create_grid_p1p2

END PROGRAM inter_mv_fem

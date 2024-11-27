PROGRAM inter_mv_fem 

!----------Mesh format dependent modules---------------------------------------
USE grid_2d, sides         => iflux,       & ! Type of flux BC
          np            => np_d,        & ! Number of domain nodes
          nps           => np_b,        & ! Number of boundary nodes
          me            => ns_d,        & ! Number of volume elements
          mes           => ns_b           ! Number of boundary elements
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

   IMPLICIT NONE

   INTEGER:: k, m, n, i_d=0, nw=3, nws=2, kd=2
   CHARACTER(len=64) directory, file_name
!-------------END OF DECLARATIONS----------------------------------------------

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

      OPEN(UNIT=10,FILE='FEM.'//file_name,FORM='formatted',STATUS='unknown')

      write(10,*) np, nw, me, nws, mes 

      DO m = 1, SIZE(jj, 2)
         WRITE(10, *)  (jj(n,m), n = 1,nw),  i_d,  (neigh(n,m), n = 1,nw)
      END DO

      DO m = 1, SIZE(jjs, 2)
          WRITE(10, *)  neighs(m),  (jjs(n,m), n = 1,nws),  sides(m) 
      END DO

      DO n = 1, SIZE(rr, 2)
          WRITE(10, *)  (rr(k,n), k = 1,kd)
      END DO

      CLOSE(10)

END PROGRAM  inter_mv_fem

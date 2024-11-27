PROGRAM unif_grid
  USE chaine_caractere
  IMPLICIT NONE
  INTEGER :: nx, ny, me, mes, np, nxp1, nyp1, i, j, k, l, m, n, ms, f_end, &
       type_id, np2, n1, n2, mop, nw, nws
  REAL(KIND=8), DIMENSION(2) :: x0
  REAL(KIND=8) :: lx, ly, dx, dy
  LOGICAL :: do_it, form

  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: rr, rr2
  INTEGER,      DIMENSION(:,:), ALLOCATABLE :: jj, jjs, neigh, jj2, jjs2
  INTEGER,      DIMENSION(:),   ALLOCATABLE :: id, neighs, sides
  LOGICAL,      DIMENSION(:),   ALLOCATABLE :: virgin
  INTEGER,      DIMENSION(4)                :: side
  CHARACTER(len=64) :: name

  OPEN(unit=21,file='data',FORM='formatted',STATUS='unknown')
  READ(21,*) form
  READ(21,*) name
  READ(21,*) x0
  READ(21,*) lx, ly 
  READ(21,*) NX, NY 
  READ(21,*) type_id
  READ(21,*) side

  ! Allocations  
  me = 2*nx*ny
  ALLOCATE(jj(3,me),neigh(3,me),id(me))
  mes = 2*nx + 2*ny
  ALLOCATE(jjs(2,mes), neighs(mes), sides(mes))
  nxp1 = nx + 1
  nyp1 = ny + 1
  np = nxp1*nyp1
  ALLOCATE(rr(2,np))

  ! Definition of id
  id = type_id

  ! Definition of jj and neigh
  neigh = 0
  DO i= 1, nx
     DO j = 1, ny
        m = (j-1)*2*nx + 2*i-1
        k = (j-1)*nxp1 + i 
        jj(1,m) = k
        jj(2,m) = k + 1 + nxp1
        jj(3,m) = k + nxp1
        neigh(3,m) = m + 1
        IF (j/=ny) neigh(1,m) = m + 1 + 2*nx
        IF (i/=1 ) neigh(2,m) = m - 1

        m = m + 1
        jj(1,m) = k
        jj(2,m) = k + 1
        jj(3,m) = k + 1 + nxp1
        neigh(2,m) = m - 1
        IF (j/=1 ) neigh(3,m) = m - 1 - 2*nx
        IF (i/=nx) neigh(1,m) = m + 1

     END DO
  END DO

  ! Definition of rr
  dx = lx/nx
  dy = ly/ny
  DO i= 1, nxp1
     DO j = 1, nyp1
        k = (j-1)*nxp1 + i 
        rr(1,k) = x0(1)+(i-1)*dx
        rr(2,k) = x0(2)+(j-1)*dy
     END DO
  END DO

  ! Definition of jjs and neighs
  k = 0
  DO j = 1, ny
     k = k + 1
     jjs(2,k)  = (j-1)*nxp1 + 1
     jjs(1,k)  = j*nxp1 + 1
     neighs(k) = (j-1)*2*nx + 1
     sides(k) = side(1)
  END DO

  DO i = 1, nx
     k = k + 1
     jjs(2,k)  = ny*nxp1 + i
     jjs(1,k)  = ny*nxp1 + i + 1
     neighs(k) = (ny-1)*2*nx + 2*(i-1) +1
     sides(k) = side(2)
  END DO

  DO j = ny, 1, -1
     k = k + 1
     jjs(2,k) = (j+1)*nxp1
     jjs(1,k) = j*nxp1
     neighs(k) = j*2*nx
     sides(k) = side(3)
  END DO

  DO i = nx, 1, -1
     k = k + 1
     jjs(2,k)  = i + 1
     jjs(1,k)  = i
     neighs(k) = 2*i
     sides(k) = side(4)
  END DO

  ! Creation of the P2 mesh
  ALLOCATE(jj2(6,me), virgin(me), jjs2(3,mes))
  np2 = np + nx*nyp1+ nxp1*ny + nx*ny
  ALLOCATE(rr2(2,np2))
  rr2(:,1:np) = rr
  jj2(1:3,:) = jj

  virgin = .TRUE.
  k = np 
  DO m = 1, me
     DO n = 1, 3
        mop = neigh(n,m)
        do_it = .FALSE.
        IF (mop==0) THEN
           do_it = .TRUE.
        ELSE IF (virgin(mop)) THEN
           do_it = .TRUE.
        END IF
        IF (do_it) THEN
           k = k + 1
           jj2(n+3,m) = k
           IF (mop/=0) THEN
              DO l = 1, 3
                 IF (neigh(l,mop)==m) EXIT
              END DO
              IF (neigh(l,mop)/=m) THEN
                 WRITE(*,*) ' BUG : neigh(l,mop)/=m'
                 STOP
              END IF
              jj2(l+3,mop)=k
           END IF
           n1 = MODULO(n,3) + 1
           n2 = MODULO(n,3) + 2
           rr2(:,k) = 0.5d0*(rr(:,jj(n1,m))+rr(:,jj(n2,m)))
        END IF
     END DO
     virgin(m) = .FALSE.
  END DO

  IF (k/=np2) THEN
     WRITE(*,*) ' BUG : k/=np2 '
     STOP
  END IF

  k = 0
  jjs2(1:2,:) = jjs
  DO j = 1, ny
     k = k + 1
     m = (j-1)*2*nx+1
     jjs2(3,k) = jj2(5,m)
  END DO
  DO i = 1, nx
     k = k + 1
     m = (ny-1)*2*nx + 2*(i-1) + 1
     jjs2(3,k) = jj2(4,m)
  END DO
  DO j = ny, 1, -1
     k = k + 1
     m = j*2*nx
     jjs2(3,k) = jj2(4,m) 
  END DO
  DO i = nx, 1, -1
     k = k + 1
     m = i*2
     jjs2(3,k) = jj2(6,m)
  END DO

  ! Storing mesh file
  WRITE (*,*) 'Writing mesh-file  ...'
  f_end = last_c_leng (64, name)
  IF (form) THEN
     OPEN(unit=30, FILE='FEM.'//name(1:f_end), FORM='formatted')
     ! Write P1 mesh
     WRITE (30,*) np,  3,  me,  2,  mes
     DO m = 1, me
        WRITE(30,*) jj(:,m), neigh(:,m), id(m)
     END DO
     DO ms = 1, mes
        WRITE(30,*) jjs(:,ms), neighs(ms), sides(ms)
     END DO
     DO n = 1, np
        WRITE(30,*) rr(:,n) 
     END DO
     ! Write P2 mesh
     WRITE (30,*) np2,  6,  me,  3,  mes
     DO m = 1, me
        WRITE(30,*) jj2(:,m), neigh(:,m), id(m)
     END DO
     DO ms = 1, mes
        WRITE(30,*) jjs2(:,ms), neighs(ms), sides(ms)
     END DO
     DO n = 1, np2
        WRITE(30,*) rr2(:,n) 
     END DO
  ELSE
     OPEN(unit=30, FILE='FEM.'//name(1:f_end), FORM='unformatted')
     ! Write P1 mesh
     nw = 3; nws = 2
     WRITE(30) np,  nw,  me,  nws,  mes
     WRITE(30) jj, neigh, id
     WRITE(30) jjs, neighs, sides
     WRITE(30) rr
     ! Write P2 mesh
     nw = 6; nws = 3
     WRITE(30) np2,  nw,  me,  nws,  mes
     WRITE(30) jj2, neigh, id
     WRITE(30) jjs2, neighs, sides
     WRITE(30) rr2 

  END IF
  CLOSE(30)
  WRITE(*,*) 'Done'

END PROGRAM unif_grid

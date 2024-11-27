PROGRAM main

   USE grid_2d
   USE sub_edit
   USE sub_geometry

   IMPLICIT NONE

   INTEGER, PARAMETER :: idf = 1
   CHARACTER (LEN=64) :: dir, fil
   INTEGER :: nw=3, nws=2, kd=2
   INTEGER :: steps, k, m, n, i_d
   INTEGER, DIMENSION(:), POINTER :: index_edge
! pour format avoir2D
   character (len=80) :: titre
   REAL (KIND=4) :: x,y
! endpour

   CALL edit(fil,i_d,index_edge)
   CALL geometry(fil)
   dir = '.'
   steps = 2500

!   WRITE (*,*) 'ENTER I/O DIRECTORY'
!   READ  (*,'(a64)') dir
!   WRITE (*,*) 'ENTER I/O FILE TYPE'
!   READ  (*,'(a64)') fil
!   WRITE (*,*) 'ENTER MAXIMUM NUMBER OF STEPS'
!   READ  (*,*) steps

   IF ( steps >= 0 ) THEN
      CALL build_grid  (idf, steps, dir, fil)
! AE 28/03/01      CALL save_grid   (idf, dir, fil)
      CALL plot_grid   (idf, dir, fil)
   ELSE
      CALL load_grid   (idf, dir, fil)
      CALL refine_grid
      CALL save_grid   (idf, dir, fil)
      CALL plot_grid   (idf, dir, fil)
   ENDIF

      OPEN(UNIT=11,FILE='knots.'//fil,FORM='formatted',STATUS='unknown')
      CLOSE(UNIT=11, STATUS='DELETE')
      OPEN(UNIT=10,FILE='FEM.'//fil,FORM='formatted',STATUS='unknown')

!  nb de sommets, 3, nb d'elements, 2, nb de segments de frontiere
      write(10,'(1x,5(i7,1x))') np_d, nw, ns_d, nws, ns_b 

!  tableau de connectivite, tableau de voisinage
      DO m = 1, SIZE(jj, 2)
         WRITE(10, '(1x,7(i7,1x))')  (jj(n,m), n = 1,nw),  (neigh(n,m), n = 1,nw), i_d
      END DO

! tableau de connectivite de frontiere, triangle dont le segment est une arete,
! numero de lacet auquel le segment appartient 
      DO m = 1, SIZE(jjs, 2)
          WRITE(10, '(1x,4(i7,1x))')  (jjs(n,m), n = 1,nws),  neighs(m), index_edge(iflux(m))
      END DO

! coordonnees des sommets
      DO n = 1, SIZE(rr, 2)
          WRITE(10, *)  (rr(k,n), k = 1,kd)
      END DO


!   WRITE (*,*) 'Saving Arrays ...'
!   OPEN  (idf,file='z.arrays.0')
!   CALL save_arrays (idf)
!   CLOSE (idf)

! format avoir2d

!      OPEN(UNIT=10,FILE='mesh.avoir2D',FORM='unformatted',STATUS='unknown')
!      titre = 'maillage'
!      write(10) titre
!      write(10) np_d, 8

!      do m = 1, np_d
!         x = rr(1,m)
!         y = rr(2,m)
!         write(10) x,y,0.d0,0,0,0,m,0,0,0,0,0
!      enddo

!      write(10) 1, 1, ns_d, 3
!      do m = 1, ns_d
!         write(10) m, (jj(n,m), n = 1,3)
!      enddo

!      CLOSE(10)

   WRITE (*,*) 'Done.'

END PROGRAM main




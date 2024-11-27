PROGRAM main

   USE grid_2d

   IMPLICIT NONE

   INTEGER, PARAMETER :: idf = 1
   CHARACTER (LEN=64) :: dir, fil
   INTEGER :: nw=3, nws=2, kd=2
   INTEGER :: steps, k, m, n


   WRITE (*,*) 'ENTER I/O DIRECTORY'
   READ  (*,'(a64)') dir
   WRITE (*,*) 'ENTER I/O FILE TYPE'
   READ  (*,'(a64)') fil
   WRITE (*,*) 'ENTER MAXIMUM NUMBER OF STEPS'
   READ  (*,*) steps

   IF ( steps >= 0 ) THEN
      CALL build_grid  (idf, steps, dir, fil)
      CALL save_grid   (idf, dir, fil)
      CALL plot_grid   (idf, dir, fil)
   ELSE
      CALL load_grid   (idf, dir, fil)
      CALL refine_grid
      CALL save_grid   (idf, dir, fil)
      CALL plot_grid   (idf, dir, fil)
   ENDIF


      OPEN(UNIT=10,FILE='FEM.'//fil,FORM='formatted',STATUS='unknown')

!  nb de sommets, 3, nb d'elements, 2, nb de segments de frontiere
      write(10,*) np_d, nw, ns_d, nws, ns_b 

!  tableau de connectivite, tableau de voisinage
      DO m = 1, SIZE(jj, 2)
         WRITE(10, *)  (jj(n,m), n = 1,nw),  (neigh(n,m), n = 1,nw)
      END DO

! tableau de connectivite de frontiere, triangle dont le segment est une arete,
! numero de lqcet auquel le segment qppqrtient 
      DO m = 1, SIZE(jjs, 2)
          WRITE(10, *)  (jjs(n,m), n = 1,nws),  neighs(m), iflux(m)
      END DO

! coordonnees des sommets
      DO n = 1, SIZE(rr, 2)
          WRITE(10, *)  (rr(k,n), k = 1,kd)
      END DO


!   WRITE (*,*) 'Saving Arrays ...'
!   OPEN  (idf,file='z.arrays.0')
!   CALL save_arrays (idf)
!   CLOSE (idf)

   WRITE (*,*) 'Done.'

END PROGRAM main

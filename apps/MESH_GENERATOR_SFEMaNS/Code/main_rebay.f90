PROGRAM main

   USE grid_2d

   IMPLICIT NONE

   INTEGER, PARAMETER :: idf = 1
   CHARACTER (LEN=64) :: dir, fil
   INTEGER :: steps


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


   WRITE (*,*) 'Saving Arrays ...'
   OPEN  (idf,file='z.arrays.0')
   CALL save_arrays (idf)
   CLOSE (idf)

   WRITE (*,*) 'Done.'

END PROGRAM main

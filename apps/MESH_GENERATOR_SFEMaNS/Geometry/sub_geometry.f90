MODULE sub_geometry

   !  Reads a geometrical data point file, computes
   !  the cubic spline passing through the nodes and
   !  save the results (hermite form).

   USE curves

   IMPLICIT NONE
  
   PUBLIC :: geometry

CONTAINS

SUBROUTINE geometry(name)

   CHARACTER(*) :: name
   LOGICAL, PARAMETER :: ascii = .TRUE.

   INTEGER :: sub_idf = 10
   INTEGER :: n


!   WRITE (*,*) 'ENTER PROBLEM NAME'
!   READ  (*,'(a10)') name

   n = first_blank ()

   OPEN  (sub_idf,form='formatted',file='knots.'//name(1:n))
   CALL new_curves (sub_idf, ascii)
   CLOSE (sub_idf)

   OPEN  (sub_idf,form='formatted',file='geometry.'//name(1:n))
   CALL save_curves (sub_idf, ascii)
   CLOSE (sub_idf)

!   WRITE (*,*) 'Done.'

CONTAINS

   FUNCTION first_blank () RESULT (n)

      IMPLICIT NONE

      INTEGER :: n
      INTEGER :: i

      n = 10

      DO i=1,10
         IF ( name(i:i) .EQ. ' ' ) THEN
            n = i; EXIT
         ENDIF
      ENDDO

   END FUNCTION first_blank

END SUBROUTINE geometry

END MODULE sub_geometry

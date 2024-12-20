MODULE sub_edit 

   !  "editor" to generate a geometrical data point file
   !
   !  generates a global file containing points uniformly
   !  distributed on 1) lines, 2) ellipses, 3) circles and
   !  4) points read form file.
   !
   !  The output format is suitable for program "geometry"


   IMPLICIT NONE

   PUBLIC :: edit

CONTAINS

   SUBROUTINE edit(fil,i_d,index_edge)

   CHARACTER(*), INTENT(OUT) :: fil 
   INTEGER, INTENT(OUT) :: i_d
   INTEGER, DIMENSION(:), POINTER :: index_edge
   CHARACTER (LEN=20)  :: name
   INTEGER, PARAMETER :: idf = 1, kdf = 2
   LOGICAL, PARAMETER :: ascii = .TRUE.

   REAL (KIND=8) :: pi

   CHARACTER (LEN=10) command
   INTEGER :: n, m, i, j, k
   REAL (KIND=8) :: r1, r2, a1, a2
   REAL (KIND=8), DIMENSION(2) :: xb, xe

  pi = ACOS(-1.d0)


!   WRITE (*,'(a18)') 'output file name ?'
   READ  (*,*) name, i_d
   fil = name

!   WRITE (*,'(a20)') 'number of segments ?'
   READ  (*,*) m
   ALLOCATE(index_edge(m))

   k = last_c_leng (10, name)

   OPEN  (idf,file='knots.'//name(1:k),form='formatted')

   WRITE (idf,'(4x,a6)') 'N_CURV'
   WRITE (idf,'(i10)') m

   DO i=1,m

!      WRITE (*,'(a8,i2,a11)') 'segment ',i,'; command ?'
      READ  (*,*) command, index_edge(i)

      SELECT CASE (command(1:3))

	 CASE ('lin')

!            WRITE (*,*) 'number of points and coordinates ?'
!JLG	    READ  (*,*) n, xb, xe
            n=1000
	    READ  (*,*) xb, xe


	    CALL line   (n, xb, xe)

	 CASE ('ell')

!            WRITE (*,*) 'number of points, center, axis, and angles ?'
!JLG	    READ  (*,*) n, xb, r1, r2, a1, a2
            n=1000
	    READ  (*,*) xb, r1, r2, a1, a2

	    a1 = pi*a1/180.d0
	    a2 = pi*a2/180.d0

	    CALL ellipse (n, xb, r1, r2, a1, a2)

	 CASE ('cir')

!            WRITE (*,*) 'number of points, center, radius, and angles ?'
!JLG	    READ  (*,*) n, xb, r1, a1, a2
            n=1000
	    READ  (*,*) xb, r1, a1, a2

	    a1 = pi*a1/180.d0
	    a2 = pi*a2/180.d0

	    CALL ellipse (n, xb, r1, r1, a1, a2)

	 CASE ('dat')

!            WRITE (*,*) 'file name ?'
	    READ  (*,*) name

	    OPEN  (kdf,file=name,form='formatted')
	    READ  (kdf,*) n
	    CALL header (n, 0)
	    DO j=1,n
	       READ  (kdf,*) xb
	       WRITE (idf,'(2e24.16)') xb
	    ENDDO
	    CLOSE (kdf)

	 CASE DEFAULT

	    WRITE (*,*) 'command not known; no action taken'
	    WRITE (*,*) 'the only command known are line, ellipse, circle, and data'

      END SELECT

   ENDDO

   CLOSE (idf)

CONTAINS

   SUBROUTINE ellipse (n, c, r1, r2, beg, end)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n
      REAL (KIND=8), DIMENSION(2), INTENT(IN) :: c
      REAL (KIND=8), INTENT(IN) :: r1, r2, beg, end

      INTEGER :: i
      REAL (KIND=8) :: delta

      delta = (end-beg)/float(n-1)

      CALL header (n, 2)

      WRITE (idf,'(2e24.16)') (c(1)+r1*cos(beg+delta*i),c(2)+r2*sin(beg+delta*i),i=0,n-1)

   END SUBROUTINE ellipse

   SUBROUTINE line (n, beg, end)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n
      REAL (KIND=8), DIMENSION(2), INTENT(IN) :: beg, end

      INTEGER :: i
      REAL (KIND=8), DIMENSION(2) :: delta

      delta = (end-beg)/float(n-1)

      CALL header (n, 0)

      WRITE (idf,'(2e24.16)') (beg+delta*i,i=0,n-1)

   END SUBROUTINE line

   SUBROUTINE header (n, m)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: n, m

      WRITE (idf,'(2a10)') '       DIM','    POINTS'
      WRITE (idf,'(2i10)') 2,n
      WRITE (idf,'( a10)') '       IDC'
      WRITE (idf,'( i10)') m
      WRITE (idf,'(2a10)') '         X','         Y'

   END SUBROUTINE header

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

END SUBROUTINE edit

END MODULE sub_edit

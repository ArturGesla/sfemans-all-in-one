MODULE grid_types

   IMPLICIT NONE


   INTEGER, PARAMETER, PUBLIC :: maxp = 2, maxs = maxp+1

   INTEGER, PARAMETER, PUBLIC :: nul_cnd = 0, nul_par = 0
   INTEGER, PARAMETER, PUBLIC :: frz_status = 0, und_status = 1,   &
				 int_status = 2, ext_status = 3,   &
				 stc_status = 4


   TYPE opposits
      TYPE (point),   POINTER :: pnt
      TYPE (simplex), POINTER :: spx
   END TYPE opposits

   TYPE point
      LOGICAL       :: visited
      INTEGER       :: index, cnd, ibk, parent
      REAL (KIND=8) :: leng

      TYPE (point),   POINTER  :: prev, next, old, ppp
      TYPE (simplex), POINTER  :: spx

      REAL (KIND=8), DIMENSION (maxp)      :: x
      REAL (KIND=8), DIMENSION (maxp,maxp) :: metric
   END TYPE point


   TYPE simplex
      INTEGER       :: index, status
      REAL (KIND=8) :: det

      TYPE (simplex), POINTER :: prev, next, int, ext

      INTEGER,         DIMENSION (maxs,maxs) :: cmat
      REAL (KIND=8),   DIMENSION (maxs,maxs) :: fmat
      TYPE (opposits), DIMENSION (maxs)      :: opp
   END TYPE simplex



CONTAINS


   FUNCTION new_point () RESULT (p)

      IMPLICIT NONE

      TYPE (point), POINTER :: p

      INTEGER :: i


      ALLOCATE ( p )

      p % visited = .FALSE.
      p % index   = 0
      p % cnd     = nul_cnd
      p % ibk     = 0
      p % parent  = nul_par

      NULLIFY ( p % prev )
      NULLIFY ( p % next )
      NULLIFY ( p % old )
      NULLIFY ( p % ppp )
      NULLIFY ( p % spx )

      p % leng    = 0.
      p % x       = 0.
      p % metric  = 0.

      DO i=1,maxp
         p % metric(i,i)  = 1.
      ENDDO

   END FUNCTION new_point


   FUNCTION new_simplex () RESULT (s)

      IMPLICIT NONE

      TYPE (simplex), POINTER :: s

      INTEGER :: i


      ALLOCATE ( s )

      s % index   = 0
      s % status  = und_status
      s % det     = 0.

      NULLIFY ( s % prev )
      NULLIFY ( s % next )
      NULLIFY ( s % int )
      NULLIFY ( s % ext )

      DO i=1,maxs
	 NULLIFY (s % opp(i) % pnt)
	 NULLIFY (s % opp(i) % spx)
      ENDDO

      s % cmat    = 0
      s % fmat    = 0.

   END FUNCTION new_simplex


END MODULE grid_types

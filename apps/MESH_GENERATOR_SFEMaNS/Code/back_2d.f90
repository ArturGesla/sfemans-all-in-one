MODULE back_2d

   USE convert

   PUBLIC   :: build_back, eval_back
   PRIVATE  :: fix_back

CONTAINS


   SUBROUTINE build_back (idf, grid, back)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf
      TYPE (domain), INTENT(INOUT)  :: grid
      TYPE (domain), INTENT(INOUT) :: back

      INTEGER :: i, j, n
      REAL (KIND=8) :: big, leng
      TYPE (point), POINTER :: hp, hq, p, q
      TYPE (simplex), POINTER :: hs, s


      !  Boundary grid lengths

      big = HUGE(big); hp  =>  grid % pnt_head; p  =>  hp
      DO; p  =>  p % next; IF ( ASSOCIATED (p, hp) ) EXIT
	 p % leng = big
      ENDDO

      DO i=1,grid % number_of_edges
	 hs  =>  grid % edges(i) % int_head; s  =>  hs
	 DO; s  =>  s % next; IF ( ASSOCIATED (s, hs) ) EXIT
	    p  =>  s % opp(2) % pnt % ppp
	    q  =>  s % opp(1) % pnt % ppp
	    leng = (p % x(1) - q % x(1))**2 + (p % x(2) - q % x(2))**2
	    leng = SQRT(leng)
	    p % leng = MIN( p % leng, leng )
	    q % leng = MIN( q % leng, leng )
	 ENDDO
      ENDDO


      !  Copy boundary grid into back grid

      CALL g_copy (grid, back)

      p  =>  hp; q  =>  back % pnt_head
      DO; p  =>  p % next; q  =>  q % next; IF ( ASSOCIATED (p, hp) ) EXIT
	 p % ppp  =>  q
	 q % leng = p % leng
      ENDDO


      !  Additional back grid points and lengths

      hs  =>  back % int_head

      READ (idf,*)
      READ (idf,*) n
      READ (idf,*)

      DO i=1,n
	 p  =>  new_point ()
	 READ (idf,*) (p % x(j),j=1,nd_d), p % leng
         CALL insert_point (nd_d, hs, p)
      ENDDO

   END SUBROUTINE build_back


   SUBROUTINE eval_back (p)

      IMPLICIT NONE

      TYPE (point), INTENT(INOUT) :: p

      INTEGER :: i, j, k
      REAL (KIND=8) :: a, b, c
      TYPE (simplex), POINTER :: s_back


      a = 0.0; c = 0.0; s_back  =>  p % old % ppp % spx
      
      CALL walking_search (nd_d, p, s_back)

      DO i=1,nd_d+1

	 b = s_back % fmat(i,nd_d+1)
	 DO j=1,nd_d
	    b = b + s_back % fmat(i,j) * p % x(j)
	 ENDDO
	 b = MAX(0d0,b)  !  p must be internal to s

	 IF ( b > c ) THEN
	    c = b
	    k = i
	 ENDIF

	 a = a + b * s_back % opp(i) % pnt % leng
      ENDDO

      p % leng = a; p % ppp  =>  s_back % opp(k) % pnt

   END SUBROUTINE eval_back


   SUBROUTINE fix_back (grid)

      IMPLICIT NONE

      TYPE (domain), INTENT(INOUT) :: grid

      LOGICAL :: okay
      INTEGER :: i, j, ci, cj, n
      REAL (KIND=8) :: f, lmax
      TYPE (point), POINTER :: p, q
      TYPE (simplex), POINTER :: s


      f = 1.0/float(nd_d+1); n = 0
      
      s  =>  grid % int_head
      DO; s  =>  s % next; IF ( ASSOCIATED (grid % int_head, s) ) EXIT

	 okay = .FALSE.

	 DO i=1,nd_d+1
	    ci = s % opp(i) % pnt % cnd
	    DO j=i+1,nd_d+1
	       cj = s % opp(j) % pnt % cnd
	       IF ( ABS(ci) == ABS(cj) ) THEN
		  okay = .TRUE.
		  EXIT
	       ENDIF
	    ENDDO
	    IF (okay) EXIT
	 ENDDO

	 IF ( .NOT. okay ) THEN
	    n = n+1

	    p  =>  grid % gar_head % next

	    IF ( ASSOCIATED (p, grid % gar_head) ) THEN
	       p  =>  new_point()
	       CALL pushtop_point (grid % add_head, p)
	    ELSE
	       CALL movetop_point (grid % add_head, p)
	    ENDIF

	    p % old  =>  s % opp(1) % pnt

	    lmax = 0.0
	    DO i=1,nd_d+1
	       lmax = MAX(lmax, s % opp(i) % pnt % leng)
	    ENDDO
	    p % leng = lmax

	    DO i=1,nd_d
	       p % x(i) = 0.
	       DO j=1,nd_d+1
		  p % x(i) = p % x(i) + s % opp(j) % pnt % x(i)
	       ENDDO
	       p % x(i) = f * p % x(i)
	    ENDDO
	 ENDIF

      ENDDO


      !  Add the fixing points

      p  =>  grid % add_head % next
      DO; IF ( ASSOCIATED (p, grid % add_head) ) EXIT
         q  =>  p % next
         CALL insert_point  (nd_d, grid % int_head, p)
         CALL movetop_point (grid % pnt_head, p)
	 p  =>  q
      END DO

   END SUBROUTINE fix_back


END MODULE back_2d

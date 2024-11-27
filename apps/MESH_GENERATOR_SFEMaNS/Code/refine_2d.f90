!ATTENTION ::dfloat change en float
!==================================
MODULE refine_2d

   USE domain_2d

   IMPLICIT NONE

   REAL (KIND=8), PARAMETER, PRIVATE :: ref_length = 1.01

   PUBLIC  :: refine_boundary, refine_domain, stitch_boundary, halve_bou, halve_dom


CONTAINS


   SUBROUTINE refine_boundary (grid, nadd)

      IMPLICIT NONE

      TYPE (domain), INTENT(INOUT) :: grid
      INTEGER, INTENT(OUT) :: nadd

      INTEGER :: e, g, n
      TYPE (simplex), POINTER :: head, s
      TYPE (point), POINTER :: p1, p2


      n = 0

      DO e=1,grid % number_of_edges
	 g = grid % edges(e) % index; head  =>  grid % edges(e) % int_head

	 s  =>  head
	 DO; s  =>  s % next; IF ( ASSOCIATED (s, head) ) EXIT

            p1  =>  s % opp(1) % pnt % ppp
            p2  =>  s % opp(2) % pnt % ppp

            IF ( segment_r_length (nd_d, p1, p2) > ref_length ) THEN
	       n = n+1; CALL halve_bou (grid, e, g, s)
            ENDIF

	 ENDDO
      ENDDO

      nadd = n

   END SUBROUTINE refine_boundary


   SUBROUTINE refine_domain (grid, nadd)

      IMPLICIT NONE

      TYPE (domain), INTENT(INOUT) :: grid
      INTEGER, INTENT(OUT) :: nadd

      INTEGER :: i, j, m
      TYPE (simplex), POINTER :: s, n
      TYPE (point), POINTER :: p1, p2


      m = 0; s  =>  grid % int_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % int_head) ) EXIT

	 s % status = -s % status

	 DO i=1,nd_d+1
	    n  =>  s % opp(i) % spx
	    IF ( ( n % status == int_status ) .AND. ( n % status > 0 ) ) THEN
               NULLIFY (p1)

               DO j=1,nd_d+1
	          IF (j .NE. i) THEN
	             IF ( .NOT. ASSOCIATED (p1) ) THEN
                        p1  =>  s % opp(j) % pnt
	             ELSE
                        p2  =>  s % opp(j) % pnt
	             ENDIF
	          ENDIF
               ENDDO

               IF ( segment_r_length (nd_d, p1, p2) > ref_length ) THEN
		  m = m+1; CALL halve_dom (grid, i, s)
               ENDIF
	    ENDIF
	 ENDDO
      ENDDO

      s  =>  grid % int_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % int_head) ) EXIT
	 s % status = ABS(s % status)
      ENDDO

      nadd = m

   END SUBROUTINE refine_domain


   SUBROUTINE stitch_boundary (grid, nadd)

      IMPLICIT NONE

      TYPE (domain), INTENT(INOUT) :: grid
      INTEGER, INTENT(OUT) :: nadd

      INTEGER :: e, g, n
      TYPE (simplex), POINTER :: head, s
      TYPE (point), POINTER :: p1, p2


      n = 0

      DO e=1,grid % number_of_edges
	 g = grid % edges(e) % index

         head  =>  grid % edges(e) % stc_head

	 s  =>  head
	 DO 
            s  =>  s % next 
            IF ( ASSOCIATED (s, head) ) EXIT
            n = n+1 
            CALL halve_bou (grid, e, g, s)
	 ENDDO
      ENDDO

      nadd = n

   END SUBROUTINE stitch_boundary


   SUBROUTINE halve_bou (grid, ibou, igeo, sbou)

      IMPLICIT NONE

      TYPE (domain), INTENT(INOUT) :: grid
      TYPE (simplex), TARGET, INTENT(INOUT) :: sbou
      INTEGER :: ibou, igeo

      INTEGER :: i
      REAL (KIND=8) :: factor
      TYPE (point), POINTER :: p_dom, p_bou


      factor = 1./float(nd_b+1)

      p_bou  =>  new_point()
      p_dom  =>  new_point()

      p_bou % cnd = ibou
      p_bou % parent = sbou % int % index
      p_bou % old  =>  sbou % opp(1) % pnt
      p_bou % ppp  =>  p_dom

      p_bou % x = 0.
      DO i=1,nd_b+1
         p_bou % x = p_bou % x + sbou % opp(i) % pnt % x
      ENDDO
      p_bou % x = factor * p_bou % x

      p_dom % cnd = ibou
      p_dom % parent = sbou % int % index
      p_dom % old  =>  sbou % opp(1) % pnt % ppp

      p_dom % metric = 0.
      DO i=1,nd_b+1
         p_dom % metric = p_dom % metric + sbou % opp(i) % pnt % ppp % metric
      ENDDO
      p_dom % metric = factor * p_dom % metric

      CALL evalc_0       (igeo, p_bou % x, p_dom % x)
      CALL pushtop_point (grid % edges(ibou) % add_head, p_bou)
      CALL pushtop_point (grid % add_head, p_dom)

   END SUBROUTINE halve_bou


   SUBROUTINE halve_dom (grid, idom, sdom)

      IMPLICIT NONE

      TYPE (domain), INTENT(INOUT) :: grid
      INTEGER, INTENT(IN) :: idom
      TYPE (simplex), TARGET, INTENT(INOUT) :: sdom

      INTEGER :: i
      REAL (KIND=8) :: factor
      TYPE (point), POINTER :: p


      factor = 1./float(nd_d)

      p  =>  new_point()

      p % cnd = nul_cnd
      p % parent = sdom % index
      p % old  =>  sdom % opp(1) % pnt

      p % x = 0.; p % metric = 0.
      DO i=1,nd_d+1
         IF (i .NE. idom) THEN
	    p % x = p % x + sdom % opp(i) % pnt % x
	    p % metric = p % metric + sdom % opp(i) % pnt % metric
	 ENDIF
      ENDDO
      p % x = factor * p % x; p % metric = factor * p % metric

      CALL pushtop_point (grid % add_head, p)

   END SUBROUTINE halve_dom


END MODULE refine_2d

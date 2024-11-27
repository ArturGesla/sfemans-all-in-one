!ATTENTION :: float change en float
!====================================
MODULE delaunay

   USE linsys
   USE list

   IMPLICIT NONE


   !               ###  G L O B A L   V A R I A B L E S  ###


   REAL (KIND=8), PARAMETER, PRIVATE :: neg_tol = -1.d-10

   TYPE (simplex), POINTER, PRIVATE  :: bbl_head, del_head, new_head, gar_head,   &
                                        new_spx_head, new_int_head,   &
				        new_ext_head, new_bbl_head

   INTEGER,       DIMENSION(maxs),      PRIVATE :: pivot
   REAL (KIND=8), DIMENSION(maxs),      PRIVATE :: vector, center
   REAL (KIND=8), DIMENSION(maxs,maxs), PRIVATE :: matrix



   !                  ###  R O U T I N E   I N D E X  ###


   !  1) GENERAL:

   PUBLIC  :: plotmtv_delaunay, delaunay_setup,   &
	      walking_search, trivial_search,   &
              simplex_facets, simplex_normal, eval_facets,   &
	      segment_length, simplex_center, delaunay_measure,   &
	      segment_r_length, simplex_r_center, delaunay_r_measure
	     

   !  2) BUBBLE AND SPLIT:

   PUBLIC  :: flag_shell, split_ie_1d, split_ie, merge_ie
   PRIVATE :: build_bubble, match_bubble_ndm1, swap_ie_ndm1, match_bubble_nd, swap_ie_nd

   !  3) INSERTION:

   PUBLIC  :: set_delaunay, insert_point, insert_far_point, update_geometry
   PRIVATE :: delete_first, delete_basis, delete_cavity, check_cavity,   &
              update_topology

   !  4) REMOVAL:

   PUBLIC  :: remove_point
   PRIVATE :: bubble_triang, split_bubble

CONTAINS


   ! ###  PLOT ROUTINES


   SUBROUTINE plotmtv_delaunay (idf, nd, spx_head)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf, nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head


      INTEGER, PARAMETER :: background = -1, foreground = 0,   &
			    yellow = 1, blue = 2, green = 3, red = 4,   &
			    dark_blue = 5, orange = 6, pink = 7,   &
			    light_pink = 8, cyan = 9, brown = 9   !  PLOTMTV COLORS

      INTEGER, PARAMETER :: none = 0, dot = 1, cross = 2, capital_x = 3,   &
			    white_square = 4, black_square = 5,   &
			    white_diamond = 6, black_diamond = 7,   &
			    white_triangle = 8, black_triangle = 9,   &
			    white_inv_triangle = 10, black_inv_triangle = 11,   &
			    white_circle = 12, black_circle = 13   !   PLOTMTV MARKERS

      INTEGER, PARAMETER :: line_color = green, point_marker = black_circle


      INTEGER :: i, j, k, l, n_lines
      TYPE (simplex), POINTER :: s


      IF ( nd == 2 ) THEN
         WRITE (idf,*) '$ DATA = CURVE2D'
         WRITE (idf,*) '% equalscale = TRUE'
         WRITE (idf,*) '% boundary = TRUE'
         WRITE (idf,*)

         s  =>  spx_head; n_lines = 0
         DO; s  =>  s % next; IF ( ASSOCIATED (s, spx_head) ) EXIT
	    IF ( s % status > 0 ) THEN
	       s % status = - s % status
	       DO i=1,nd+1
	          IF ( s % opp(i) % spx % status >= 0 ) THEN
	             k = 0; n_lines = n_lines+1
                     WRITE (idf,*)
                     WRITE (idf,*) '% linecolor = ',line_color,' markertype = ',point_marker
		     DO j=1,nd+1
		        IF (j /= i) THEN
			   k = k+1
                           WRITE (idf,'(2e15.7,i8)') (s % opp(j) % pnt % x(l),l=1,nd), k
		        ENDIF
		     ENDDO
	          ENDIF
	       ENDDO
	    ENDIF
         ENDDO

         s  =>  spx_head
         DO; s  =>  s % next; IF ( ASSOCIATED (s, spx_head) ) EXIT
	    s % status = ABS(s % status)
         ENDDO

         WRITE (*,*) 'plotmtv_delaunay: number of plotted lines = ',n_lines
      ELSE
         WRITE (*,*) 'plotmtv_delaunay: unable to plot for nd = ',nd
      ENDIF

   END SUBROUTINE plotmtv_delaunay


   ! ###  SETUP ROUTINES


   SUBROUTINE delaunay_setup

      ! +--------------------+
      ! |   INITIAL SET UP   |
      ! +--------------------+

      IMPLICIT NONE

      INTEGER :: i


      bbl_head      =>  newhead_simplex ()
      del_head      =>  newhead_simplex ()
      new_head      =>  newhead_simplex ()
      gar_head      =>  newhead_simplex ()

      new_spx_head  =>  newhead_simplex ()
      new_int_head  =>  newhead_simplex ()
      new_ext_head  =>  newhead_simplex ()
      new_bbl_head  =>  newhead_simplex ()


   END SUBROUTINE delaunay_setup


   ! ### GENERAL ROUTINES (SEARCH)


   SUBROUTINE walking_search (nd, pnew, simp)

      ! +------------------------------------------+
      ! |   SEARCH FOR A SIMPLEX CONTAINING PNEW   |
      ! +------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), TARGET, INTENT(INOUT) :: pnew
      TYPE (simplex), POINTER :: simp

      INTEGER :: i, j, n, m
      REAL (KIND=4):: h
      TYPE (simplex), POINTER :: s


      s  =>  simp; NULLIFY (simp)

      DO m=1,10000
         CALL eval_facets (nd, pnew, s, vector)

         n = 0
         DO i=1,nd+1
	    IF ( (vector(i) <= neg_tol) .AND. ( s % opp(i) % spx % status > 0 ) ) THEN
	       n = n+1
	       pivot(n) = i
	    ENDIF
         ENDDO

	 IF (n == 0) THEN
	    simp  =>  s; EXIT
         ELSE
	    CALL RANDOM_NUMBER (h); s  =>  s % opp(pivot(1+INT(n*h))) % spx
	 ENDIF
      ENDDO

      IF ( .NOT. ASSOCIATED (simp) ) THEN
	 WRITE (*,*) 'walking_search error, simplex not found after 10000 steps'
	 WRITE (*,*) '(stop)'
	 STOP
      ENDIF

   END SUBROUTINE walking_search


   SUBROUTINE trivial_search (nd, spx_head, pnew, simp)

      ! +------------------------------------------+
      ! |   SEARCH FOR A SIMPLEX CONTAINING PNEW   |
      ! +------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head
      TYPE (point), TARGET, INTENT(INOUT) :: pnew
      TYPE (simplex), POINTER :: simp

      INTEGER :: i, j, n
      REAL (KIND=8) :: c_maxmin, c_min
      TYPE (simplex), POINTER :: s, neig, s_maxmin


      NULLIFY (simp); c_maxmin = -HUGE(c_maxmin)

      s  =>  spx_head
      DO; s  =>  s % next; IF ( ASSOCIATED(s, spx_head) ) EXIT

	 IF ( s % status > 0 ) THEN
            CALL eval_facets (nd, pnew, s, vector)

            n = 0; c_min = +HUGE(c_min)
            DO i=1,nd+1
	       IF ( vector(i) >= neg_tol ) n = n+1
	       c_min = MIN(c_min, vector(i))
            ENDDO

	    IF (n == nd+1) THEN
	       simp  =>  s
               EXIT
            ENDIF

	    IF (c_maxmin < c_min) THEN
	       c_maxmin  =  c_min
               s_maxmin  =>  s
	    ENDIF
         ENDIF

      ENDDO

      IF ( .NOT. ASSOCIATED (simp) ) THEN
         WRITE (*,*) '!'; simp  =>  s_maxmin
      ENDIF

   END SUBROUTINE trivial_search


   ! ### GENERAL ROUTINES (GEOMETRICAL)


   SUBROUTINE simplex_facets (nd, simp)

      ! +---------------------------------------------------------+
      ! |   FACET NORMAL VECTORS (BARYCENTRIC COORDINATE MATRIX)  |
      ! +---------------------------------------------------------+

      ! The barycentric coordinate matrix contains the nd+1
      ! coefficient of the facet nd-planes.

      ! 3D example: the equation of a 3-plane is ax + by + cz + d = 0,
      ! (a, b, c, d defined up to a constant value).
      ! The coefficients for face k are stored in simp % fmat(k,:)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp
      INTEGER :: i, j


      simp % fmat = 0.

      DO j=1,nd+1
	 simp % fmat(j,j) = 1.
         DO i=1,nd
            matrix(i,j) = simp % opp(j) % pnt % x(i)
         ENDDO
         matrix(nd+1,j) = 1.
      ENDDO

      CALL lufact (nd+1, pivot, matrix, simp % det)

      DO j=1,nd+1
         CALL lusolv (nd+1, pivot, matrix, simp % fmat(:,j))
      ENDDO

   END SUBROUTINE simplex_facets


   SUBROUTINE simplex_normal (nd, simp_ndm1, normal)

      ! +---------------------------+
      ! |   SIMPLEX NORMAL VECTOR   |
      ! +---------------------------+

      ! WARNING: simp_ndm1 is a simplex in nd-1 dimensional space

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp_ndm1
      REAL (KIND=8), DIMENSION(nd), INTENT(INOUT) :: normal

      INTEGER :: i, j, k, m
      REAL (KIND=8), DIMENSION(:), POINTER :: xk

      DO j=1,nd
	 DO k=1,nd
	    xk  =>  simp_ndm1 % opp(k) % pnt % ppp % x
	    DO i=0,nd-1
	       m = MOD(i+j,nd+1)+1
	       IF (m <= nd) THEN
	          matrix(k,i+1) = xk(m)
	       ELSE
	          matrix(k,i+1) = 1.
	       ENDIF
	    ENDDO
	 ENDDO
         CALL lufact (nd, pivot, matrix, normal(j))
      ENDDO

   END SUBROUTINE simplex_normal


   SUBROUTINE eval_facets (nd, poin, simp, x)

      ! +---------------------+
      ! |   AREA COORDINATE   |
      ! +---------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), INTENT(INoUT) :: poin
      TYPE (simplex), INTENT(INOUT) :: simp
      REAL (KIND=8), DIMENSION(nd+1), INTENT(INOUT) :: x

      INTEGER :: i, j


      DO i=1,nd+1
	 x(i) = simp % fmat(i,nd+1)
	 DO j=1,nd
	    x(i) = x(i) + simp % fmat(i,j) * poin % x(j)
	 ENDDO
      ENDDO

   END SUBROUTINE eval_facets


   FUNCTION segment_length (nd, a, b) RESULT (leng2)

      ! +-------------------+
      ! |   EUCLID LENGTH   |
      ! +-------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), INTENT(INOUT) :: a, b
      REAL (KIND=8) :: leng2

      INTEGER :: i
      REAL (KIND=8) :: r


      r = 0.
      DO i=1,nd
	 r = r + (a % x(i) - b % x(i))**2
      ENDDO
      leng2 = r

   END FUNCTION segment_length


   SUBROUTINE simplex_center (nd, simp, centre, radius)

      ! +--------------------------------------+
      ! |   EUCLID SIMPLEX CENTER AND RADIUS   |
      ! +--------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp
      REAL (KIND=8), INTENT(INOUT) :: radius
      REAL (KIND=8), DIMENSION(ND), INTENT(INOUT) :: centre

      INTEGER :: i, j
      REAL (KIND=8) :: eps, sum, det
      REAL (KIND=8), DIMENSION(:), POINTER :: x1, x2
      PARAMETER (eps = 1.0d-12)


      DO i=1,nd
	 x1  =>  simp % opp(i) % pnt % x
	 x2  =>  simp % opp(i+1) % pnt % x
         sum = 0.
         DO j=1,nd
            matrix(i,j) = x2(j)-x1(j)
            sum = sum + x2(j)**2-x1(j)**2
         ENDDO
         centre(i) = 0.5*sum
      ENDDO

      CALL lufact (nd, pivot, matrix, det)

      IF ( ABS(det) <= eps ) THEN
         WRITE (*,'(a30,e12.5)') 'simplex_center warning, det = ',det
         IF ( ABS(det) <= 0.0 ) THEN
            WRITE (*,*) '(stop)'
            STOP
         ENDIF
      ENDIF

      CALL lusolv (nd, pivot, matrix, centre)

      sum = 0.
      DO i=1,nd+1
         x1  =>  simp % opp(i) % pnt % x
         DO j=1,nd
            sum = sum + (centre(j)-x1(j))**2
         ENDDO
      ENDDO
      radius = sum/float(nd+1)

   END SUBROUTINE simplex_center


   FUNCTION delaunay_measure (nd, poin, simp) RESULT (measure)

      ! +--------------------------------------------+
      ! |   EUCLID MEASURE = DISTANCE**2/RADIUS**2   |
      ! +--------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), TARGET, INTENT(INOUT) :: poin
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp
      REAL (KIND=8) :: measure

      INTEGER :: i, j
      REAL (KIND=8) :: eps, sum, det, radius, distance
      REAL (KIND=8), DIMENSION(:), POINTER :: x1, x2
      PARAMETER (eps = 1.0d-12)


      DO i=1,nd
	 x1  =>  simp % opp(i) % pnt % x
	 x2  =>  simp % opp(i+1) % pnt % x
         sum = 0.
         DO j=1,nd
            matrix(i,j) = x2(j)-x1(j)
            sum = sum + x2(j)**2-x1(j)**2
         ENDDO
         center(i) = 0.5*sum
      ENDDO

      CALL lufact (nd, pivot, matrix, det)

      IF ( ABS(det) <= eps ) THEN
         WRITE (*,'(a30,e12.5)') 'simplex_center warning, det = ',det
         IF ( ABS(det) <= 0.0 ) THEN
            WRITE (*,*) '(stop)'
            STOP
         ENDIF
      ENDIF

      CALL lusolv (nd, pivot, matrix, center)

      sum = 0.
      DO i=1,nd+1
         x1  =>  simp % opp(i) % pnt % x
         DO j=1,nd
            sum = sum + (center(j)-x1(j))**2
         ENDDO
      ENDDO
      radius = sum/float(nd+1)

      distance = 0.
      DO j=1,nd
         distance = distance + (poin % x(j) - center(j))**2
      ENDDO

      measure = distance/radius

   END FUNCTION delaunay_measure


   FUNCTION segment_r_length (nd, a, b) RESULT (leng2)

      ! +--------------------+
      ! |   RIEMANN LENGTH   |
      ! +--------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), INTENT(INOUT) :: a, b
      REAL (KIND=8) :: leng2

      INTEGER :: i, j
      REAL (KIND=8) :: r, s


      r = 0.
      DO i=1,nd
         vector(i) = a % x(i) - b % x(i)
      ENDDO

      DO i=1,nd
	 s = 0.
	 DO j=1,nd
	    s = s + (a % metric(i,j) + b % metric(i,j))*vector(j)
	 ENDDO
	 r = r + vector(i)*s
      ENDDO

      leng2 = 0.5*r

      IF ( leng2 <= 0.0 ) THEN
	 WRITE (*,*) 'segment_r_length error, lenght**2 = ',leng2
	 WRITE (*,*) '(stop)'
	 STOP
      ENDIF

   END FUNCTION segment_r_length


   SUBROUTINE simplex_r_center (nd, simp, metric, centre, radius)

      ! +---------------------------------------+
      ! |   RIEMANN SIMPLEX CENTER AND RADIUS   |
      ! +---------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp
      REAL (KIND=8), DIMENSION(:,:), INTENT(INOUT) :: metric
      REAL (KIND=8), DIMENSION(:), INTENT(INOUT) :: centre
      REAL (KIND=8), INTENT(INOUT) :: radius

      INTEGER :: i, j, k
      REAL (KIND=8), PARAMETER :: eps = 1.0d-12
      REAL (KIND=8) :: sum, sold, snew, det, distance
      REAL (KIND=8), DIMENSION(:), POINTER :: x


      snew = 0; x  =>  simp % opp(1) % pnt % x
      DO j=1,nd
	 sum = 0.
         DO k=1,nd
            sum = sum + metric(j,k)*x(k)
         ENDDO
	 snew = snew + x(j)*sum
      ENDDO

      DO i=1,nd
	 sold = snew; snew = 0.; x  =>  simp % opp(i+1) % pnt % x
	 DO j=1,nd
	    vector(j) = simp % opp(i+1) % pnt % x(j) - simp % opp(i) % pnt % x(j)
	 ENDDO
	 DO j=1,nd
	    sum = 0.
	    matrix(i,j) = 0.
            DO k=1,nd
	       sum = sum + metric(j,k)*x(k)
	       matrix(i,j) = matrix(i,j) + metric(j,k)*vector(k)
	    ENDDO
	    snew = snew + x(j)*sum
	 ENDDO
	 centre(i) = 0.5*(snew-sold)
      ENDDO

      CALL lufact (nd, pivot, matrix, det)

      IF ( ABS(det) <= eps ) THEN
         IF ( ABS(det) <= 0.0 ) THEN
            WRITE (*,'(a30,e12.5)') 'simplex_radius error, det = ',det
            WRITE (*,*) '(stop)'
            STOP
         ELSE
            WRITE (*,'(a30,e12.5)') 'simplex_radius warning, det = ',det
         ENDIF
      ENDIF

      CALL lusolv (nd, pivot, matrix, centre)

      radius = 0.
      DO i=1,nd+1
         DO j=1,nd
            vector(j) = simp % opp(i) % pnt % x(j) - centre(j)
         ENDDO
	 DO j=1,nd
	    sum = 0.
	    DO k=1,nd
	       sum = sum + metric(j,k)*vector(k)
	    ENDDO
	    radius = radius + vector(j)*sum
	 ENDDO
      ENDDO
      radius = radius/float(nd+1)

   END SUBROUTINE simplex_r_center


   FUNCTION delaunay_r_measure (nd, poin, simp, metric) RESULT (measure)

      ! +---------------------------------------------+
      ! |   RIEMANN MEASURE = DISTANCE**2/RADIUS**2   |
      ! +---------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), TARGET, INTENT(INOUT) :: poin
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp
      REAL (KIND=8), DIMENSION(:,:), INTENT(INOUT) :: metric
      REAL (KIND=8) :: measure

      INTEGER :: i, j, k
      REAL (KIND=8), PARAMETER :: eps = 1.0d-12
      REAL (KIND=8) :: sum, sold, snew, det, radius, distance
      REAL (KIND=8), DIMENSION(:), POINTER :: x


      snew = 0; x  =>  simp % opp(1) % pnt % x
      DO j=1,nd
	 sum = 0.
         DO k=1,nd
            sum = sum + metric(j,k)*x(k)
         ENDDO
	 snew = snew + x(j)*sum
      ENDDO

      DO i=1,nd
	 sold = snew; snew = 0.; x  =>  simp % opp(i+1) % pnt % x
	 DO j=1,nd
	    vector(j) = simp % opp(i+1) % pnt % x(j) - simp % opp(i) % pnt % x(j)
	 ENDDO
	 DO j=1,nd
	    sum = 0.
	    matrix(i,j) = 0.
            DO k=1,nd
	       sum = sum + metric(j,k)*x(k)
	       matrix(i,j) = matrix(i,j) + metric(j,k)*vector(k)
	    ENDDO
	    snew = snew + x(j)*sum
	 ENDDO
	 center(i) = 0.5*(snew-sold)
      ENDDO

      CALL lufact (nd, pivot, matrix, det)

      IF ( ABS(det) <= eps ) THEN
         IF ( ABS(det) <= 0.0 ) THEN
            WRITE (*,'(a30,e12.5)') 'delaunay_measure error, det = ',det
            WRITE (*,*) '(stop)'
            STOP
         ELSE
            WRITE (*,'(a30,e12.5)') 'delaunay_measure warning, det = ',det
         ENDIF
      ENDIF

      CALL lusolv (nd, pivot, matrix, center)

      radius = 0.
      DO i=1,nd+1
	 DO j=1,nd
            vector(j) = simp % opp(i) % pnt % x(j) - center(j)
	 ENDDO
	 DO j=1,nd
	    sum = 0.
	    DO k=1,nd
	       sum = sum + metric(j,k)*vector(k)
	    ENDDO
	    radius = radius + vector(j)*sum
	 ENDDO
      ENDDO
      radius = radius/float(nd+1)

      distance = 0.
      DO j=1,nd
	 vector(j) = poin % x(j) - center(j)
      ENDDO

      DO j=1,nd
	 sum = 0.
         DO k=1,nd
            sum = sum + metric(j,k)*vector(k)
         ENDDO
         distance = distance + vector(j)*sum
      ENDDO

      measure = distance/radius

   END FUNCTION delaunay_r_measure


   ! ### GENERAL ROUTINES (BUBBLE)


   SUBROUTINE build_bubble (nd, bbl_head, poin)

      ! +------------------------+
      ! |   BUBBLE AROUND POIN   |
      ! +------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: bbl_head
      TYPE (point), TARGET, INTENT(INOUT) :: poin

      INTEGER :: i, j
      TYPE (simplex), POINTER :: simp, neig


      ! Initialize the surrounding list

      simp  =>  poin % spx

      IF ( .NOT. ASSOCIATED (simp) ) THEN
	 WRITE (*,*) 'bubble error, unassociated pointer'
	 WRITE (*,*) '(stop)'
	 STOP
      ENDIF

      simp % status = - simp % status
      CALL movetop_simplex (bbl_head, simp)

      ! Link the surrounding vertices to the list

      simp  =>  bbl_head
      DO; simp  =>  simp % next; IF ( ASSOCIATED (simp, bbl_head) ) EXIT
         DO i=1,nd+1
            neig  =>  simp % opp(i) % spx
	    IF ( ASSOCIATED (neig) ) THEN
               IF ( neig % status > 0 ) THEN
                  DO j=1,nd+1
                     IF ( ASSOCIATED (neig % opp(j) % pnt, poin) ) THEN
                        neig % status = - neig % status
                        CALL movebot_simplex (bbl_head, neig)
                        EXIT
                     ENDIF
                  ENDDO
               ENDIF
	    ENDIF
         ENDDO
      ENDDO

      ! Unflag the surrounding list

      simp  =>  bbl_head
      DO; simp  =>  simp % next; IF ( ASSOCIATED (simp, bbl_head) ) EXIT
         simp % status = ABS(simp % status)
      ENDDO

   END SUBROUTINE build_bubble


   SUBROUTINE match_bubble_ndm1 (nd, s_ndm1, bbl_head, s_nd_int, s_nd_ext)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), INTENT(INOUT) :: s_ndm1
      TYPE (simplex), TARGET, INTENT(INOUT) :: bbl_head
      TYPE (simplex), POINTER :: s_nd_int, s_nd_ext

      LOGICAL :: first
      INTEGER :: i, j, k, kk
      TYPE (point), POINTER :: p
      TYPE (simplex), POINTER :: head, s


      first = .TRUE.; NULLIFY (s_nd_int, s_nd_ext)

      s  =>  bbl_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, bbl_head) ) EXIT

	 k = 0
	 DO i=1,nd
	    kk = k
	    p  =>  s_ndm1 % opp(i) % pnt % ppp
	    DO j=1,nd+1
	       IF ( ASSOCIATED (p, s % opp(j) % pnt) ) THEN
		  k = k+1
		  EXIT
	       ENDIF
	    ENDDO
	    IF (k == kk) EXIT
	 ENDDO

	 IF (k == nd) THEN
            IF (first) THEN
               s_nd_int  =>  s
	       first = .FALSE.
            ELSE
               s_nd_ext  =>  s
               EXIT
            ENDIF
	 ENDIF

      ENDDO

   END SUBROUTINE match_bubble_ndm1


   SUBROUTINE swap_ie_ndm1 (nd, s_ndm1, s_nd_int, s_nd_ext)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), POINTER :: s_ndm1, s_nd_int, s_nd_ext

      INTEGER :: i, j
      REAL (KIND=8) :: scal
      TYPE (simplex), POINTER :: s_nd_tmp


      CALL simplex_normal (nd, s_ndm1, vector)

      DO i=1,nd+1
	 IF ( ASSOCIATED (s_nd_int % opp(i) % spx, s_nd_ext) ) THEN
	    scal = 0.
	    DO j=1,nd
	       scal = scal + s_nd_int % fmat(i,j) * vector(j)
	    ENDDO
	    EXIT
	 ENDIF
      ENDDO

      IF ( scal > 0.0 ) THEN
         s_nd_tmp  =>  s_nd_int
         s_nd_int  =>  s_nd_ext
         s_nd_ext  =>  s_nd_tmp
      ENDIF

   END SUBROUTINE swap_ie_ndm1


   SUBROUTINE match_bubble_nd (nd, k_nd, s_nd, bbl_head, s_nd_int, s_nd_ext)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd, k_nd
      TYPE (simplex), INTENT(INOUT) :: s_nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: bbl_head
      TYPE (simplex), POINTER :: s_nd_int, s_nd_ext

      LOGICAL :: first
      INTEGER :: i, j, k, kk
      TYPE (point), POINTER :: p
      TYPE (simplex), POINTER :: s


      first = .TRUE.; NULLIFY (s_nd_int, s_nd_ext)

      s  =>  bbl_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, bbl_head) ) EXIT

	 k = 0
	 DO i=1,nd+1
	    IF (i /= k_nd) THEN
	       kk = k
	       p  =>  s_nd % opp(i) % pnt
	       DO j=1,nd+1
	          IF ( ASSOCIATED (p, s % opp(j) % pnt) ) THEN
		     k = k+1
		     EXIT
	          ENDIF
	       ENDDO
	       IF (k == kk) EXIT
	    ENDIF
	 ENDDO

	 IF (k == nd) THEN
            IF (first) THEN
               s_nd_int  =>  s
	       first = .FALSE.
            ELSE
               s_nd_ext  =>  s
               EXIT
            ENDIF
	 ENDIF

      ENDDO

   END SUBROUTINE match_bubble_nd


   SUBROUTINE swap_ie_nd (nd, k_nd, s_nd, s_nd_int, s_nd_ext)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd, k_nd
      TYPE (simplex), INTENT(INOUT) :: s_nd
      TYPE (simplex), POINTER :: s_nd_int, s_nd_ext

      INTEGER :: i, j
      TYPE (simplex), POINTER :: s_nd_tmp
      REAL (KIND=8) :: scal


      DO i=1,nd+1
	 IF ( ASSOCIATED (s_nd_int % opp(i) % spx, s_nd_ext) ) THEN
	    scal = 0.
	    DO j=1,nd
	       scal = scal + s_nd % fmat(k_nd,j) * s_nd_int % fmat(i,j)
	    ENDDO
	    EXIT
	 ENDIF
      ENDDO

      IF ( scal < 0.0 ) THEN
         s_nd_tmp  =>  s_nd_int
         s_nd_int  =>  s_nd_ext
         s_nd_ext  =>  s_nd_tmp
      ENDIF

   END SUBROUTINE swap_ie_nd


   !  ###  GENERAL ROTINES (SPLITTING)


   SUBROUTINE flag_shell (nd, int_head_ndm1, spx_head, stc_head_ndm1)

      !     +--------------------------------+
      !     |   FLAG THE INT/EXT SIMPLICES   |
      !     +--------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT)  :: int_head_ndm1, spx_head
      TYPE (simplex), TARGET, INTENT(INOUT) :: stc_head_ndm1

      TYPE (simplex), POINTER :: s_ndm1, t_ndm1, s_nd_int, s_nd_ext


      s_ndm1  =>  int_head_ndm1 % next

      DO; IF ( ASSOCIATED (s_ndm1, int_head_ndm1) ) EXIT
	 t_ndm1  =>  s_ndm1 % next

         CALL build_bubble      (nd, bbl_head, s_ndm1 % opp(1) % pnt % ppp)
         CALL match_bubble_ndm1 (nd, s_ndm1, bbl_head, s_nd_int, s_nd_ext)
	 CALL jointop_simplex   (spx_head, bbl_head)

	 IF ( ASSOCIATED (s_nd_int) .AND. ASSOCIATED (s_nd_ext) ) THEN
            CALL swap_ie_ndm1 (nd, s_ndm1, s_nd_int, s_nd_ext)

	    s_ndm1 % int  =>  s_nd_int
	    s_ndm1 % ext  =>  s_nd_ext

	    s_nd_int % status = int_status
	    s_nd_ext % status = ext_status
	 ELSE
	    CALL movetop_simplex (stc_head_ndm1, s_ndm1)
	 ENDIF

	 s_ndm1  =>  t_ndm1
      ENDDO

   END SUBROUTINE flag_shell


   SUBROUTINE split_ie_1d (nd, spx_head, int_head, ext_head)

      !     +---------------------------------------------+
      !     |   SPLIT INT/EXT SIMPLEX LISTS --- 1D CASE   |
      !     +---------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT)  :: spx_head
!EN TEST      TYPE (simplex), TARGET, INTENT(INOUT) :: int_head, ext_head
      TYPE (simplex), TARGET, INTENT(INOUT) :: int_head, ext_head

      INTEGER :: i
      TYPE (simplex), POINTER :: s, t, n


      IF ( nd /= 1 ) THEN
	 WRITE (*,*) 'split_1d error, unexpected nd value = ',nd
	 WRITE (*,*) '(stop)'
	 STOP
      ENDIF

      s  =>  spx_head % next
      DO; IF ( ASSOCIATED (s, spx_head) ) EXIT
	 t  =>  s % next
	 IF ( s % status > 0 ) THEN
	    s % status = int_status
	    CALL movetop_simplex (int_head, s)
	 ENDIF
	 s  =>  t
      ENDDO

      s  =>  int_head % next
      DO; IF ( ASSOCIATED (s, int_head) ) EXIT
	 t  =>  s % next
	 DO i=1,nd+1
	    n  =>  s % opp(i) % spx
	    IF ( n % status == 0 ) THEN
	       s % status = ext_status
	       CALL movetop_simplex (ext_head, s)
	    ENDIF
	 ENDDO
	 s  =>  t
      ENDDO

   END SUBROUTINE split_ie_1d


   SUBROUTINE split_ie (nd, spx_head, int_head, ext_head)

      !     +---------------------------------+
      !     |   SPLIT INT/EXT SIMPLEX LISTS   |
      !     +---------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT)  :: spx_head
!EN TEST      TYPE (simplex), TARGET, INTENT(INOUT) :: int_head, ext_head
      TYPE (simplex), TARGET, INTENT(INOUT) :: int_head, ext_head

      INTEGER :: i
      TYPE (simplex), POINTER :: s, t, n


      !  Link the shell simplices to the internal or external lists

      s  =>  spx_head % next
      DO; IF ( ASSOCIATED (s, spx_head) ) EXIT
	 t  =>  s % next
	 IF ( s % status == int_status ) THEN
	    CALL movetop_simplex (int_head, s)
	 ELSEIF ( s % status == ext_status ) THEN
	    CALL movetop_simplex (ext_head, s)
	 ENDIF
	 s  =>  t
      ENDDO

      !  Link the non-shell internal simplices without crossing boundaries

      s  =>  int_head % next
      DO; IF ( ASSOCIATED (s, int_head) ) EXIT
         t  =>  s % next
         DO i=1,nd+1
	    n  =>  s % opp(i) % spx

	    IF ( .NOT. ASSOCIATED (n) ) THEN
	       WRITE (*,*) 'fill_shell error, I am lost !!!'
	       WRITE (*,*) '(stop)'
	       STOP
	    ENDIF

            IF ( n % status == und_status ) THEN
               n % status = int_status
               CALL movebot_simplex  (int_head, n)
            ENDIF
         ENDDO
         s  =>  t
      ENDDO

      !  Link the remaining simplices as external

      s  =>  spx_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, spx_head) ) EXIT
	 IF ( s % status > 0 ) s % status = ext_status
      ENDDO

      IF ( .NOT. ASSOCIATED (spx_head % next, spx_head) ) THEN
         CALL jointop_simplex (ext_head, spx_head)
      ENDIF

   END SUBROUTINE split_ie


   SUBROUTINE merge_ie (nd, spx_head, int_head, ext_head)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT)  :: int_head, ext_head
!EN TEST      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head

      TYPE (simplex), POINTER :: s


      IF ( ASSOCIATED (spx_head % next, spx_head) ) THEN

         CALL jointop_simplex (spx_head, int_head)
         CALL jointop_simplex (spx_head, ext_head)

         s  =>  spx_head
         DO; s  =>  s % next; IF ( ASSOCIATED (s, spx_head) ) EXIT
	    IF ( s % status > 0 ) s % status = und_status
         ENDDO

      ENDIF

   END SUBROUTINE merge_ie


   !  ###  POINT INSERTION ROTINES


   SUBROUTINE set_delaunay (nd, xlb, xrt, axp_head, axs_head, spx_head)

      ! +--------------------------------+
      ! |   DEFINE A NEW TRIANGULATION   |
      ! +--------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      REAL (KIND=8), DIMENSION(:), INTENT(INOUT) :: xlb, xrt
      TYPE (point), TARGET, INTENT(INOUT) :: axp_head
      TYPE (simplex), TARGET, INTENT(INOUT) :: axs_head, spx_head

      INTEGER :: i, j, k
      REAL (KIND=8), PARAMETER :: parm1 = 0.25, parm2 = 2.*parm1+1.
      TYPE (simplex), POINTER :: simpl, sfrst, slast
      TYPE (point), POINTER :: ip, jp


      ! ### SIMPLEX LISTS ###

      DO i=1,nd+3
         simpl  =>  new_simplex ()
         CALL pushtop_simplex (axs_head, simpl)
      END DO


      ! ### BOX POINT LIST ###

      DO i=1,nd+1
         ip  =>  new_point ()
         CALL pushtop_point (axp_head, ip)
      END DO

      ip  =>  axp_head % next
      DO j=1,nd
         ip % x(j) = xlb(j)-parm1*ABS(xrt(j)-xlb(j))
      END DO

      DO i=1,nd
         ip  =>  ip % next
         DO j=1,nd
            ip % x(j) = xlb(j)-parm1*ABS(xrt(j)-xlb(j))
         END DO
         ip % x(i) = xlb(i)+(parm2*nd-parm1)*(xrt(i)-xlb(i))
      END DO


      ! ### POINT TOPOLOGY ###

      sfrst  =>  axs_head % next
      slast  =>  axs_head % prev

      ip  =>  axp_head; simpl  =>  sfrst
      DO i=1,nd+1
         ip  =>  ip % next; simpl  =>  simpl % next

         slast % opp(i) % pnt  =>  ip
         ip % spx  =>  slast

         jp  =>  axp_head
         DO j=1,nd+1
            jp  =>  jp % next
            simpl % opp(j) % pnt  =>  jp
         END DO

         NULLIFY (simpl % opp(i) % pnt)
         NULLIFY (sfrst % opp(i) % pnt)
      END DO


      ! ### CONNECTIVITY MATRICES ###

      simpl  =>  sfrst
      DO k=1,nd+1
         simpl  =>  simpl % next
	 DO i=1,nd+1
	    DO j=1,nd+1
	       simpl % cmat(i,j) = j
	    ENDDO
	 ENDDO
      END DO

      DO i=1,nd+1
         DO j=1,nd+1
            sfrst % cmat(i,j) = 0
            slast % cmat(i,j) = j
         ENDDO
      ENDDO


      ! ### NEIGHBOUR TOPOLOGY ###

      simpl  =>  sfrst

      DO i=1,nd+1
         simpl  =>  simpl % next

         DO j=1,nd+1
            simpl % opp(j) % spx  =>  sfrst
         END DO

         NULLIFY (sfrst % opp(i) % spx)

         simpl % opp(i) % spx  =>  slast
         slast % opp(i) % spx  =>  simpl
      END DO


      ! ### SIMPLEX GEOMETRY ###

      simpl  =>  sfrst
      simpl % status = frz_status

      DO i=1,nd+1
         simpl  =>  simpl % next
         simpl % status = frz_status
      END DO

      simpl  =>  simpl % next
      simpl % status = und_status

      CALL simplex_facets (nd, simpl)

      CALL movetop_simplex (spx_head, simpl)

   END SUBROUTINE set_delaunay


   SUBROUTINE insert_point (nd, spx_head, pnew)

      ! +--------------------------+
      ! |    INSERT A NEW POINT    |
      ! +--------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head
      TYPE (point), TARGET, INTENT(INOUT) :: pnew


      IF ( ASSOCIATED (pnew % spx) ) THEN
         WRITE (*,*) 'insert_point error, already connected new point'
         WRITE (*,*) '(stop)'
         STOP
      END IF

      CALL delete_first    (nd, spx_head, pnew)
      CALL delete_cavity   (nd, pnew)
      CALL update_topology (nd, pnew)
      CALL update_geometry (nd, new_head)

      CALL jointop_simplex (gar_head, del_head)
      CALL jointop_simplex (spx_head, new_head)

   END SUBROUTINE insert_point


   SUBROUTINE insert_far_point (nd, spx_head, pnew, far_point)

      ! +-----------------------------------------------+
      ! |    INSERT A NEW POINT, IF IT IS FAR ENOUGH    |
      ! +-----------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head
      TYPE (point), TARGET, INTENT(INOUT) :: pnew
      LOGICAL, INTENT(INOUT) :: far_point


      IF ( ASSOCIATED (pnew % spx) ) THEN
         WRITE (*,*) 'insert_far_point error, already connected new point'
         WRITE (*,*) '(stop)'
         STOP
      END IF

      CALL delete_first    (nd, spx_head, pnew)
      CALL delete_cavity   (nd, pnew)
      CALL check_cavity    (nd, pnew, far_point)

      IF (far_point) THEN
         CALL update_topology (nd, pnew)
         CALL update_geometry (nd, new_head)

         CALL jointop_simplex (gar_head, del_head)
         CALL jointop_simplex (spx_head, new_head)
      ELSE
         CALL jointop_simplex (spx_head, del_head)
      ENDIF

   END SUBROUTINE insert_far_point


   SUBROUTINE delete_first (nd, spx_head, pnew)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head
      TYPE (point), TARGET, INTENT(INOUT) :: pnew

      TYPE (simplex), POINTER :: simp


      IF ( ASSOCIATED (pnew % old) ) THEN
	 simp  =>  pnew % old % spx
         CALL walking_search (nd, pnew, simp)
      ELSE
         CALL trivial_search (nd, spx_head, pnew, simp)
      ENDIF

      CALL delete_basis (nd, pnew, simp)

   END SUBROUTINE delete_first


   SUBROUTINE delete_basis (nd, pnew, simp)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), TARGET, INTENT(INOUT) :: pnew
      TYPE (simplex), TARGET, INTENT(INOUT) :: simp

      INTEGER :: i, j, n
      TYPE (simplex), POINTER :: neig


      simp % status = - simp % status
      CALL movetop_simplex (del_head, simp)

      DO j=1,nd+1
	 neig  =>  simp % opp(j) % spx
	 IF ( neig % status > 0 ) THEN
            CALL eval_facets (nd, pnew, neig, vector)

	    n = 0
            DO i=1,nd+1
	       IF ( vector(i) >= neg_tol ) n = n+1
            ENDDO

	    IF ( n == nd+1 ) THEN
	       neig % status = - neig % status
	       CALL movetop_simplex (del_head, neig)
	       EXIT
	    ENDIF
	 ENDIF
      ENDDO

   END SUBROUTINE delete_basis


   SUBROUTINE delete_cavity (nd, pnew)

      ! +-----------------------+
      ! |    CAVITY DELETION    |
      ! +-----------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), TARGET, INTENT(INOUT) :: pnew

      LOGICAL :: delaunay_test
      INTEGER :: i
      TYPE (point), POINTER :: qnew
      TYPE (simplex), POINTER :: simp, neig
      REAL (KIND=8), PARAMETER :: eps = 1.0d-12, limit = 2.0 - eps
      REAL (KIND=8) :: pmeas, qmeas


      ! Status table:

      ! status  > 0 :: unvisited simplex
      ! status == 0 :: frozen simplex
      ! status  < 0 :: visited simplex

      ! Link the deleted simplices
      ! Set to "visited" the deleted and the adjacent simplices

      simp  =>  del_head
      DO; simp  =>  simp % next; IF ( ASSOCIATED (simp, del_head) ) EXIT
         DO i=1,nd+1
            neig  =>  simp % opp(i) % spx
            IF ( neig % status > 0 ) THEN
               neig % status = - neig % status

	       qnew  =>  neig % opp( simp % cmat(i,i) ) % pnt

               pmeas = delaunay_r_measure (nd, pnew, neig, pnew % metric)
               qmeas = delaunay_r_measure (nd, pnew, neig, qnew % metric)

               IF ( ( pmeas+qmeas ) < limit ) THEN
                  CALL movebot_simplex (del_head, neig)
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      ! Set to "unvisited" the deleted simplices

      simp  =>  del_head
      DO; simp  =>  simp % next; IF ( ASSOCIATED (simp, del_head) ) EXIT
	 simp % status = ABS(simp % status)
      ENDDO

   END SUBROUTINE delete_cavity


   SUBROUTINE check_cavity (nd, pnew, far_point)

      ! +------------------------------+
      ! |   CHECK THE DELETED CAVITY   |
      ! +------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), INTENT(INOUT) :: pnew
      LOGICAL, INTENT(INOUT) :: far_point

      LOGICAL :: undelete
      INTEGER :: i
      TYPE (simplex), POINTER :: s
      REAL (KIND=8), PARAMETER :: tol = 0.50 ! tol = 0.75
      REAL (KIND=8) :: pl2, leng2

      undelete = .FALSE.; pl2 = (pnew % leng * tol)**2

      s  =>  del_head
      DO; s  =>  s % next; IF ( ASSOCIATED(s, del_head) ) EXIT
	 DO i=1,nd+1
	    leng2 = segment_length (nd, s % opp(i) % pnt, pnew)
	    IF ( leng2 < pl2 ) THEN
	       undelete = .TRUE.
	       EXIT
	    ENDIF
	 ENDDO
	 IF ( undelete ) EXIT
      ENDDO

      IF ( undelete ) THEN

         s  =>  del_head
         DO; s  =>  s % next; IF ( ASSOCIATED (s, del_head) ) EXIT
	    DO i=1,nd+1
	       s % opp(i) % spx % status = ABS(s % opp(i) % spx % status)
	    ENDDO
         ENDDO
	 far_point = .FALSE.
      ELSE
	 far_point = .TRUE.
      ENDIF

   END SUBROUTINE check_cavity


   SUBROUTINE update_topology (nd, pnew)

      ! +------------------------------------------+
      ! |    UPDATE THE CAVITY TOPOLOGICAL DATA    |
      ! +------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (point), TARGET, INTENT(INOUT) :: pnew

      INTEGER :: mi, ni
      TYPE (simplex), POINTER :: del, adj, new


      ! Compute the new <---> adj data

      del  =>  del_head
      DO; del  =>  del % next; IF ( ASSOCIATED (del, del_head) ) EXIT
	 DO ni=1,nd+1
	    adj  =>  del % opp(ni) % spx
	    IF ( adj % status <= 0 ) THEN
	       mi = del % cmat(ni,ni)
               CALL adj_new
	    ENDIF
	 ENDDO
      ENDDO


      ! Compute the new <---> new data

      del  =>  del_head
      DO; del  =>  del % next; IF ( ASSOCIATED (del, del_head) ) EXIT
	 DO ni=1,nd+1
	    adj  =>  del % opp(ni) % spx
	    IF ( adj % status <= 0 ) THEN
	       new  =>  adj % opp(del % cmat(ni,ni)) % spx
	       DO mi=1,nd+1
		  IF (mi /= ni) CALL new_new
	       ENDDO
	    ENDIF
	 ENDDO
      ENDDO


      ! Compute the point ----> simplex
      ! Reset to "unvisited" the adjacent simplices

      new  =>  new_head
      DO; new  =>  new % next; IF ( ASSOCIATED (new, new_head) ) EXIT
         DO mi=1,nd+1
            new % opp(mi) % pnt % spx  =>  new
	    new % opp(mi) % spx % status = ABS(new % opp(mi) % spx % status)
         ENDDO
      ENDDO

   CONTAINS

      SUBROUTINE adj_new

	 IMPLICIT NONE

	 INTEGER :: i, ni


	 ! ###  NEW SIMPLEX  ###

         new  =>  gar_head % next

         IF ( ASSOCIATED (new, gar_head) ) THEN
	    new  =>  new_simplex ()
            CALL pushtop_simplex (new_head, new)
         ELSE
	    new % status = und_status
            CALL movetop_simplex (new_head, new)
	 ENDIF

	 ni = adj % cmat(mi,mi)

	 ! ###  NEW POINT POINTERS  ###

	 DO i=1,nd+1
	    new % opp(i) % pnt  =>  del % opp(i) % pnt
	 ENDDO
	 new % opp(ni) % pnt  =>  pnew

	 ! ###  NEW-ADJ SIMPLEX POINTERS

         adj % opp(mi) % spx  =>  new
         new % opp(ni) % spx  =>  adj

	 !   ###  NEW-ADJ CONNECTIVITY MATRIX  ###

	 DO i=1,nd+1
	    new % cmat(ni,i) = del % cmat(ni,i)
	 ENDDO

      END SUBROUTINE adj_new


      SUBROUTINE new_new

	 IMPLICIT NONE

	 TYPE (simplex), POINTER :: d, e
	 INTEGER :: i, j, m, n, x, y


	 j = 0; m = mi; n = ni

         DO i=1,nd+1
	    IF ((i /= mi) .AND. (i /= ni)) THEN
	       j = j+1
	       pivot(j) = i
	    ENDIF
	 ENDDO

	 d  =>  del

	 DO
	    e  =>  d % opp(m) % spx
	    IF ( e % status > 0 ) THEN
	       x = d % cmat(m,m)
	       y = d % cmat(m,n)

	       DO i=1,nd-1
		  pivot(i) = d % cmat(m, pivot(i))
	       ENDDO

	       m = y
	       n = x

	       d  =>  e
	    ELSE
	       EXIT
	    ENDIF
	 ENDDO

	 new % opp(mi) % spx  =>  d % opp(m) % spx % opp(d % cmat(m,m)) % spx

	 new % cmat(mi,mi) = n
	 new % cmat(mi,ni) = m

	 j = 0

         DO i=1,nd+1
	    IF ((i /= mi) .AND. (i /= ni)) THEN
	       j = j+1
	       new % cmat(mi,i) = pivot(j)
	    ENDIF
	 ENDDO

      END SUBROUTINE new_new


   END SUBROUTINE update_topology


   SUBROUTINE update_geometry (nd, head)

      ! +----------------------------------------+
      ! |    UPDATE THE HEAD GEOMETRICAL DATA    |
      ! +----------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: head
      TYPE (simplex), POINTER :: s

      s  =>  head

      DO; s  =>  s % next; IF ( ASSOCIATED (s, head) ) EXIT
         CALL simplex_facets (nd, s)
      ENDDO
         
   END SUBROUTINE update_geometry


   ! ### REMOVAL ROUTINES


   SUBROUTINE remove_point (nd, spx_head, p)

      !     +---------------------------+
      !     |    REMOVE AN OLD POINT    |
      !     +---------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: spx_head
      TYPE (point), TARGET, INTENT(INOUT) :: p


      CALL build_bubble    (nd, bbl_head, p)
      CALL bubble_triang   (nd, bbl_head, p, new_spx_head)
      CALL split_bubble    (nd, bbl_head, new_spx_head)
      CALL update_geometry (nd, new_int_head)

      CALL jointop_simplex (spx_head, new_int_head)
      CALL jointop_simplex (gar_head, new_ext_head)
      CALL jointop_simplex (gar_head, bbl_head)

      !   CALL del_delaunay    (new_spx_head)

   END SUBROUTINE remove_point


   SUBROUTINE bubble_triang (nd, bbl_head, poin, new_spx_head)

      ! +-------------------------------------------------------+
      ! |    NEW TRIANGULATION OF THE EXTERNAL BUBBLE POINTS    |
      ! +-------------------------------------------------------+

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: bbl_head
      TYPE (point), TARGET, INTENT(INOUT) :: poin
      TYPE (simplex), POINTER :: new_axs_head, new_spx_head

      INTEGER :: i
      TYPE (point), POINTER :: new_box_head, p
      TYPE (simplex), POINTER :: s
      REAL (KIND=8), DIMENSION(:), ALLOCATABLE :: xlb, xrt


      !  Bubble point box, set "visisted" bubble simplices, flag bubble points

      ALLOCATE ( xlb(nd), xrt(nd) ); xlb = +HUGE(xlb); xrt = -HUGE(xrt)

      s  =>  bbl_head; poin % visited = .TRUE.
      DO; s  =>  s % next; IF ( ASSOCIATED (s, bbl_head) ) EXIT
	 s % status = - s % status
	 DO i=1,nd+1
	    p  =>  s % opp(i) % pnt
	    IF ( .NOT. p % visited ) THEN
	       p % visited = .TRUE.
               xlb = MIN(xlb, p % x)
               xrt = MAX(xrt, p % x)
	    ENDIF
	 ENDDO
      ENDDO

      CALL set_delaunay (nd, xlb, xrt, new_box_head, new_axs_head, new_spx_head)

      DEALLOCATE ( xlb, xrt )


      !  Add the bubble points to the new triangulation

      s  =>  bbl_head; poin % visited = .FALSE.
      DO; s  =>  s % next; IF ( ASSOCIATED (s, bbl_head) ) EXIT
	 DO i=1,nd+1
	    p  =>  s % opp(i) % pnt
	    IF ( p % visited ) THEN
	       p % visited = .FALSE.
	       NULLIFY (p % old)
	       NULLIFY (p % spx)
	       CALL insert_point (nd, new_spx_head, p)
	    ENDIF
	 ENDDO
      ENDDO

   END SUBROUTINE bubble_triang


   SUBROUTINE split_bubble (nd, bbl_head, new_spx_head)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: nd
      TYPE (simplex), TARGET, INTENT(INOUT) :: bbl_head, new_spx_head

      INTEGER :: n, io
      TYPE (point), POINTER :: p
      TYPE (simplex), POINTER :: si_o, se_o, si_n, se_n


      si_o  =>  bbl_head
      DO; si_o  =>  si_o % next; IF ( ASSOCIATED (si_o, bbl_head) ) EXIT
	 DO io=1,nd+1
	    se_o  =>  si_o % opp(io) % spx
	    IF ( se_o % status > 0 ) THEN
	       IF (io == 1) THEN
	          p  =>  si_o % opp(2) % pnt
	       ELSE
	          p  =>  si_o % opp(1) % pnt
	       ENDIF

               CALL build_bubble    (nd, new_bbl_head, p)
               CALL match_bubble_nd (nd, io, si_o, new_bbl_head, si_n, se_n)
	       CALL jointop_simplex (new_spx_head, new_bbl_head)

	       IF ( ASSOCIATED (si_n) .AND. ASSOCIATED (se_n) ) THEN
                  CALL swap_ie_nd (nd, io, si_o, si_n, se_n)
		  si_n % status = int_status
		  se_n % status = ext_status
	          se_o % opp(si_o % cmat(io,io)) % spx  =>  si_n
	       ELSE
		  WRITE (*,*) 'split_bubble error, unmatched bubble facet'
		  WRITE (*,*) 'you may have removed too many points'
		  WRITE (*,*) '(stop)'
		  STOP
	       ENDIF
	    ENDIF
	 ENDDO
      ENDDO

      CALL split_ie (nd, new_spx_head, new_int_head, new_ext_head)

      si_o  =>  bbl_head
      DO; si_o  =>  si_o % next; IF ( ASSOCIATED (si_o, bbl_head) ) EXIT
	 DO io=1,nd+1
	    se_o  =>  si_o % opp(io) % spx
	    IF ( se_o % status > 0 ) CALL set_eo_in
	 ENDDO
      ENDDO

      si_n  =>  new_int_head
      DO; si_n  =>  si_n % next; IF ( ASSOCIATED (si_n, new_int_head) ) EXIT
	 si_n % status = und_status
	 DO n=1,nd+1
	    si_n % opp(n) % pnt % spx  =>  si_n
	 ENDDO
      ENDDO

   CONTAINS

      SUBROUTINE set_eo_in

	 IMPLICIT NONE

	 LOGICAL :: ok
	 INTEGER :: i, e, in, eo
	 TYPE (point), POINTER :: pi, pe


	 eo = si_o % cmat(io,io)

	 si_n  =>  se_o % opp(eo) % spx

	 DO i=1,nd+1
	    ok = .TRUE.; pi  =>  si_n % opp(i) % pnt

	    DO e=1,nd+1
	       IF (e /= eo) THEN
		  IF ( ASSOCIATED ( se_o % opp(e) % pnt, pi ) ) THEN
		     ok = .FALSE.
		     EXIT
		  ENDIF
	       ENDIF
	    ENDDO

	    IF (ok) THEN
	       in = i
	       EXIT
	    ENDIF
	 ENDDO

	 se_o % opp(eo) % spx  =>  si_n
	 si_n % opp(in) % spx  =>  se_o

	 se_o % cmat(eo,eo) = in
	 si_n % cmat(in,in) = eo

	 DO e=1,nd+1
	    pe  =>  se_o % opp(e) % pnt
	    DO i=1,nd+1
	       pi  =>  si_n % opp(i) % pnt
	       IF ( ASSOCIATED (pe, pi) ) THEN
		  se_o % cmat(eo,e) = i
		  si_n % cmat(in,i) = e
	       ENDIF
	    ENDDO
	 ENDDO

      END SUBROUTINE set_eo_in

   END SUBROUTINE split_bubble


END MODULE delaunay

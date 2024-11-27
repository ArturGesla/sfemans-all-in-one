!ATTENTION :: float change en float
!===================================
MODULE grid_2d

   USE refine_2d
   USE back_2d
   USE arrays

   IMPLICIT NONE

   TYPE (domain), PRIVATE :: grid, back

   PUBLIC ::  build_grid, refine_grid, save_grid, load_grid, plot_grid
   PRIVATE :: last_c_leng, boundary_grid, front_grid, set_metric,   &
	      add_points_1d, add_points_12d, g_plot

CONTAINS


   SUBROUTINE build_grid (idf, steps, dir, fil)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf, steps
      CHARACTER (LEN=64), INTENT(IN) :: dir, fil

      LOGICAL, PARAMETER :: ascii = .TRUE.
      INTEGER :: d_end, f_end, nadd



      d_end = last_c_leng (64, dir)
      f_end = last_c_leng (64, fil)

      WRITE (*,*) 'Loading topology ...'

      OPEN  (idf,file=dir(1:d_end)//'/topology.'//fil(1:f_end))
      CALL load_topology (idf, grid)
      CLOSE (idf)

      WRITE (*,*) 'Loading geometry ...'

      OPEN  (idf,file=dir(1:d_end)//'/geometry.'//fil(1:f_end))
      CALL load_curves (idf, ascii)
      CLOSE (idf)


      CALL delaunay_setup


      WRITE (*,*) 'Building grid ...'

      CALL boundary_grid (grid)

      IF ( steps > 0 ) THEN

         OPEN  (idf,file=dir(1:d_end)//'/topology.'//fil(1:f_end))
         CALL load_topology (idf, back)
         CLOSE (idf)

         OPEN  (idf,file=dir(1:d_end)//'/backgrid.'//fil(1:f_end))
	 CALL build_back    (idf, grid, back)
         CLOSE (idf)

         CALL front_grid    (grid, steps)
	 CALL set_metric    (grid)
         CALL build_arrays  (grid)

         CALL smooth
	 CALL update_geometry (nd_d, grid % int_head)

      ELSE

         ! OPEN  (idf,file=dir(1:d_end)//'/topology.'//fil(1:f_end))
         ! CALL load_topology (idf, back)
         ! CLOSE (idf)

         ! OPEN  (idf,file=dir(1:d_end)//'/backgrid.'//fil(1:f_end))
	 ! CALL build_back    (idf, grid, back)
         ! CLOSE (idf)

	 CALL set_metric    (grid)
         CALL build_arrays  (grid)

      ENDIF

      WRITE (*,*) 'Done.'

   END SUBROUTINE build_grid


   SUBROUTINE refine_grid

      IMPLICIT NONE

      INTEGER :: e, nb, np


      CALL delaunay_setup


      WRITE (*,*) 'Refining grid ...'

      CALL match_arrays

      DO e=1,grid % number_of_edges
         CALL update_geometry (nd_b, grid % edges(e) % int_head)
      ENDDO

      CALL update_geometry (nd_d, grid % int_head)

      CALL refine_boundary (grid, nb)
      CALL refine_domain   (grid, np)

      WRITE (*,'(2a5)') '   NB','   ND'
      WRITE (*,'(2i5)') nb,np

      CALL add_points_12d  (grid)
      !CALL crn_points
      !CALL add_crn_points
      CALL build_arrays    (grid)

      WRITE (*,*) 'Done.'


   CONTAINS


      SUBROUTINE crn_points

         IMPLICIT NONE

         INTEGER :: i, j, m
         TYPE (simplex), POINTER :: s, n


         m = 0; s  =>  grid % int_head

         DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % int_head) ) EXIT

	    j = 0

	    DO i=1,nd_d+1
	       n  =>  s % opp(i) % spx
	       IF ( n % status == ext_status ) j = j+1
	    ENDDO

	    IF ( j == nd_d ) THEN
	       DO i=1,nd_d+1
	          n  =>  s % opp(i) % spx
	          IF ( ( n % status == int_status ) ) THEN
	             s % status = stc_status
		     m = m+1
		     CALL halve_dom (grid, i, s)
		     EXIT
	          ENDIF
	       ENDDO
	    ELSEIF ( j == nd_d+1 ) THEN
	       WRITE (*,*) 'refine_corners error, unpossible element found'
	       WRITE (*,*) '(stop)'
	       STOP
	    ENDIF
         ENDDO

         s  =>  grid % int_head
         DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % int_head) ) EXIT
	    s % status = int_status
         ENDDO

      END SUBROUTINE crn_points


      SUBROUTINE add_crn_points

         IMPLICIT NONE

         INTEGER :: m
         TYPE (point), POINTER :: p, q


         m = 0; p  =>  grid % add_head % next

         DO; IF ( ASSOCIATED (p, grid % add_head) ) EXIT
	    q  =>  p % next
            m = m+1
            CALL insert_point (nd_d, grid % int_head, p)
	    CALL movetop_point (grid % pnt_head, p)
	    p  =>  q
         END DO
  
      END SUBROUTINE add_crn_points

   END SUBROUTINE refine_grid


   SUBROUTINE save_grid (idf, dir, fil)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf
      CHARACTER (LEN=64), INTENT(IN) :: dir, fil

      LOGICAL, PARAMETER :: ascii = .FALSE.
      INTEGER :: d_end, f_end


      d_end = last_c_leng (64, dir)
      f_end = last_c_leng (64, fil)

      WRITE (*,*) 'Saving grid ...'

      OPEN  (idf,file=dir(1:d_end)//'/restart.'//fil(1:f_end),form='unformatted')
      CALL g_save (idf, ascii, grid)
      CLOSE (idf)

      WRITE (*,*) 'Done.'

   END SUBROUTINE save_grid


   SUBROUTINE load_grid (idf, dir, fil)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf
      CHARACTER (LEN=64), INTENT(IN) :: dir, fil

      LOGICAL :: ascii
      INTEGER :: d_end, f_end


      d_end = last_c_leng (64, dir)
      f_end = last_c_leng (64, fil)

      WRITE (*,*) 'Loading topology ...'

      OPEN  (idf,file=dir(1:d_end)//'/topology.'//fil(1:f_end),form='formatted')
      CALL load_topology (idf, grid)
      CLOSE (idf)

      WRITE (*,*) 'Loading geometry ...'

      OPEN  (idf,file=dir(1:d_end)//'/geometry.'//fil(1:f_end),form='formatted')
      ascii = .TRUE.; CALL load_curves (idf, ascii)
      CLOSE (idf)

      WRITE (*,*) 'Loading grid ...'

      OPEN  (idf,file=dir(1:d_end)//'/restart.'//fil(1:f_end),form='unformatted')
      ascii = .FALSE.; CALL g_load (idf, ascii, grid)
      CLOSE (idf)

      CALL build_arrays (grid)

      WRITE (*,*) 'Done.'

   END SUBROUTINE load_grid


   SUBROUTINE plot_grid (idf, dir, fil)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf
      CHARACTER (LEN=64), INTENT(IN) :: dir, fil

      INTEGER :: d_end, f_end


      d_end = last_c_leng (64, dir)
      f_end = last_c_leng (64, fil)

      WRITE (*,*) 'Plotting grid ...'

      OPEN  (idf,file=dir(1:d_end)//'/gridplot.'//fil(1:f_end))
      CALL g_plot (idf, grid)
      CLOSE (idf)

      ! WRITE (*,*) 'Plotting back ...'

      ! OPEN  (idf,file=dir(1:d_end)//'/backplot.'//fil(1:f_end))
      ! CALL g_plot (idf, back)
      ! CLOSE (idf)

      WRITE (*,*) 'Done.'

   END SUBROUTINE plot_grid


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


   SUBROUTINE boundary_grid (grid)

      IMPLICIT NONE

!EN TEST      TYPE (domain), INTENT(OUT) :: grid
      TYPE (domain), INTENT(INOUT) :: grid


      CALL bou_points
      CALL set_grid
      CALL add_points_12d (grid)


   CONTAINS


      SUBROUTINE bou_points

         IMPLICIT NONE

         INTEGER :: i, j, k, n
         TYPE (point), POINTER :: h1, p1, p2
         REAL (KIND=8), DIMENSION(:), POINTER :: s, h


	 NULLIFY ( s, h )

	 !  1d points

         DO i=1,grid % number_of_edges
	    h1  =>  grid % edges(i) % add_head

	    CALL metric_1d (i, s, h)
	    CALL points_1d (h1, s, h)

	    p1  =>  h1; n = 0
	    DO; p1  =>  p1 % next; IF ( ASSOCIATED (p1, h1) ) EXIT
	       n = n+1
	       p1 % cnd = i
	       p1 % old  =>  p1 % prev
	    ENDDO

	    h1 % next % cnd = -i
	    h1 % prev % cnd = -i
	    NULLIFY (h1 % next % old)

	    WRITE (*,'(a9,i2,a11,i4)') ' Segment ',i,'; points = ',n
         ENDDO


	 !  2d points: vertex points

         DO i=1,grid % number_of_verts
	    p2  =>  new_point()
	    grid % verts(i) % pnt  =>  p2
	    CALL pushtop_point (grid % add_head, p2)
         ENDDO


	 !  2d points: edge points

         DO i=1,grid % number_of_edges
	    j = grid % edges(i) % index

	    h1  =>  grid % edges(i) % add_head
	    p1  =>  h1 % next
	    p2  =>  grid % verts(grid % edges(i) % begin_vert) % pnt

	    p1 % ppp  =>  p2
	    p2 % cnd = p1 % cnd
	    CALL evalc_0 (j, p1 % x, p2 % x)

	    h1  =>  h1 % prev
	    DO; p1  =>  p1 % next; IF ( ASSOCIATED (p1, h1) ) EXIT

	       p2  =>  new_point()
	       p1 % ppp  =>  p2
	       p2 % cnd = p1 % cnd

	       CALL evalc_0 (j, p1 % x, p2 % x)
	       CALL pushtop_point (grid % add_head, p2)

	    ENDDO

	    p2  =>  grid % verts(grid % edges(i) % end_vert) % pnt
	    p1 % ppp  =>  p2
	    p2 % cnd = p1 % cnd

	    CALL evalc_0 (j, p1 % x, p2 % x)
         ENDDO

      END SUBROUTINE bou_points


      SUBROUTINE metric_1d (c, s, h)

         IMPLICIT NONE

         INTEGER, INTENT(IN) :: c
         REAL (KIND=8), DIMENSION(:), POINTER :: s, h

         INTEGER :: ns, ms, i, j, j0, jm
         REAL (KIND=8) :: ss, s0, h0, si, hi, fs, fh, sh, dh, tj
         REAL (KIND=8), PARAMETER :: pi=3.14159265358979

         INTEGER,       DIMENSION(:), POINTER :: idata
         REAL (KIND=8), DIMENSION(:), POINTER :: sdata, hdata


	 CALL evalc_s (c, ns, s)

         IF ( ASSOCIATED ( h ) ) DEALLOCATE ( h )
	 ALLOCATE ( h(ns) )

	 idata  =>  grid % edges(c) % idata
	 sdata  =>  grid % edges(c) % sdata
	 hdata  =>  grid % edges(c) % hdata

         ms = SIZE(idata)


         !  Compute h(s) --- interpolated local spacing at curve knots

         j0 = 1; ss = s(ns); s0 = ss*sdata(1); h0 = hdata(1)
         
         h(j0) = h0

         DO i=2,ms

	    jm = j0; si = ss*sdata(i); hi = hdata(i)

	    DO j=j0+1,ns
	       IF ( s(j) >= si ) THEN
	          jm = j-1
	          EXIT
	       ENDIF
	    ENDDO

	    IF ( jm == j0 ) THEN
	       WRITE (*,*) 'build_h error for si = ',si
               WRITE (*,*) 'on edge ', c
	       WRITE (*,*) '(stop)'
	       STOP
	    ENDIF

	    SELECT CASE (idata(i))

	       CASE (1) ! Linear interpolation

	          fs = 1.0/(si-s0)
	          dh = hi-h0
	          DO j=j0+1,jm
	             tj   = fs*(s(j)-s0)
	             h(j) = h0+tj*dh
	          ENDDO

	       CASE (2) ! Geometrical interpolation

	          fs = 1.0/(si-s0)
	          fh = hi/h0
	          DO j=j0+1,jm
	             tj   = fs*(s(j)-s0)
	             h(j) = h0*(fh**tj)
	          ENDDO

	       CASE (3) ! Sinusoidal interpolation

	          fs = pi/(si-s0)
	          sh = 0.5*(hi+h0); dh = 0.5*(hi-h0)
	          DO j=j0+1,jm
	             tj   = fs*(s(j)-s0)
	             h(j) = sh-dh*cos(tj)
	          ENDDO

	    END SELECT

	    j0 = jm; s0 = si; h0 = hi

         ENDDO

         h(ns) = hdata(ms)


         !  Integral of 1/h(s) --- riemannian curvilinear abscissa at knots

         hi   = 1.0/h(1)
         h(1) = 0.0
         DO i=2,ns
	    h0   = hi
	    hi   = 1.0/h(i)
	    h(i) = h(i-1) + 0.5*(s(i)-s(i-1))*(hi+h0)
         ENDDO

      END SUBROUTINE metric_1d


      SUBROUTINE points_1d (head, s, h)

         IMPLICIT NONE

         TYPE (point), TARGET, INTENT(INOUT) :: head
         REAL (KIND=8), DIMENSION(:), POINTER :: s, h


         INTEGER :: ns, n, j0, i, j
         REAL (KIND=8) :: hh, ss, dh, hi, si
         TYPE (point), POINTER :: p


         !  Subdivide the curve into L segments of unit riemannian
         !  length, L being the riemannian length of the curve.

         ns = SIZE(s)
         hh = h(ns)
         n  = MAX(INT(hh),1); IF ( n*(n+1) < hh**2 ) n = n+1
         dh = hh/float(n)
         ss = 1.0/s(ns)

         j0 = 1; p  =>  new_point (); p % x(1) = 0.0

         CALL pushbot_point (head, p)

         DO i=2,n
            hi = dh*(i-1)
	    DO j=j0+1,ns
	       IF ( h(j) >= hi ) THEN
	          j0 = j-1
	          si = s(j-1) + (hi-h(j-1))*(s(j)-s(j-1))/(h(j)-h(j-1))

	          p  =>  new_point (); p % x(1) = si*ss

	          CALL pushbot_point (head, p)
	          EXIT
	       ENDIF
	    ENDDO
         ENDDO

         p  =>  new_point (); p % x(1) = 1.0

         CALL pushbot_point (head, p)

      END SUBROUTINE points_1d


      SUBROUTINE set_grid

         IMPLICIT NONE

         INTEGER :: i
         REAL (KIND=8), DIMENSION(:), ALLOCATABLE :: xlb, xrt
         TYPE (point), POINTER :: hap, p
         TYPE (simplex), POINTER :: has, hs


	 !  1d grids

         ALLOCATE ( xlb(nd_b) )
         ALLOCATE ( xrt(nd_b) )

         xlb = 0.
         xrt = 1.

         DO i=1,grid % number_of_edges
	    hap  =>  grid % edges(i) % axp_head
	    has  =>  grid % edges(i) % axs_head
	    hs   =>  grid % edges(i) % spx_head
            CALL set_delaunay (nd_b, xlb, xrt, hap, has, hs)
         ENDDO

         DEALLOCATE ( xlb )
         DEALLOCATE ( xrt )


	 !  2d grid

         ALLOCATE ( xlb(nd_d) )
         ALLOCATE ( xrt(nd_d) )

         xlb = +HUGE(xlb)
         xrt = -HUGE(xrt)

         p  =>  grid % add_head
         DO; p  =>  p % next; IF ( ASSOCIATED (p, grid % add_head) ) EXIT
            xlb = MIN(xlb, p % x); xrt = MAX(xrt, p % x)
         END DO

	 hap  =>  grid % axp_head
	 has  =>  grid % axs_head
	 hs   =>  grid % spx_head

         CALL set_delaunay (nd_d, xlb, xrt, hap, has, hs)

         DEALLOCATE ( xlb )
         DEALLOCATE ( xrt )

      END SUBROUTINE set_grid

      
   END SUBROUTINE boundary_grid


   SUBROUTINE front_grid (grid, steps)

      IMPLICIT NONE

!EN TEST      TYPE (domain), INTENT(OUT) :: grid
      TYPE (domain), INTENT(INOUT) :: grid
      INTEGER, INTENT(IN) :: steps

      INTEGER :: i, nfro, nadd

      ! by trive 27/2/97 -BEGIN-
      TYPE(simplex), POINTER :: s
      ! by trive 27/2/97 -END-

      ! by trive -begin    
      !  Update of referenced points: only internal elements are used for reference
      CALL adjust_spx
      ! by trive -end    

      CALL set_status (grid % ext_head, frz_status)
      CALL jointop_simplex (grid % spx_head, grid % int_head)
      CALL set_status (grid % spx_head, und_status)

      WRITE (*,*) 'Front begin:'
      WRITE (*,'(3(1x,a4))') 'STEP','NFRO','NADD'

      DO i=1,steps
         CALL uns_split
         CALL frn_points
         CALL add_frn_points

	 WRITE (*,'(3i5)') i, nfro, nadd
	 IF ( nadd .EQ. 0 ) EXIT
      ENDDO
      !===JLG Nov 19, 2019. Removed corner points
      CALL crn_points
      CALL add_crn_points
      !===JLG Nov 19, 2019
      CALL restore_lists

      ! by trive 27/2/97 -BEGIN-

      CALL merge_ie(nd_d, grid % spx_head, grid % int_head, grid % ext_head)

      DO i=1,grid % number_of_edges

         CALL flag_shell (nd_d, grid % edges(i) % int_head, grid % &
              & spx_head, grid % edges(i) % stc_head)

         s => grid % edges(i) % stc_head
         IF(.NOT. ASSOCIATED(s%next, s)) THEN
            WRITE(*,*) 'ohoh!!! TROUBLES in BOUNDARY', i
         ENDIF

      ENDDO

      CALL split_ie(nd_d, grid % spx_head, grid % int_head, grid % ext_head)

      ! by trive 27/2/97 -END-

      WRITE (*,*) 'Front end.'


   CONTAINS


     ! by trive 27/2/97 -BEGIN-
     SUBROUTINE adjust_spx

       INTEGER                 :: i
       TYPE (simplex), POINTER :: s1, b1, simp
       TYPE (point)  , POINTER :: p1
       
       s1 => grid%int_head
       b1 => back%int_head
       spx_loop: DO
          s1 => s1%next
          b1 => b1%next
          IF(ASSOCIATED(s1, grid%int_head)) EXIT spx_loop
          
          !  Adjusting pnt%spx using only internal elements
          
          DO i = 1, nd_d+1
             p1   => s1%opp(i)%pnt
             simp => p1%spx
             IF(ASSOCIATED(simp)) THEN
                IF(.NOT. ASSOCIATED(s1, simp)) THEN
                   p1%spx => s1
                ENDIF
             ELSE
                WRITE(*,*)'pnt%spx not associated? (adjust_spx)'
             ENDIF
             
             !  Adjusting pnt%ppp%spx using only internal elements (of backgrid)
             
             p1   => s1%opp(i)%pnt%ppp
             simp => p1%spx
             IF(ASSOCIATED(simp)) THEN
                IF(.NOT. ASSOCIATED(b1, simp)) THEN
                   p1%spx => b1
                ENDIF
             ELSE
                WRITE(*,*)'pnt%ppp%spx not associated? (adjust_spx)'
             ENDIF
         ENDDO
         
      ENDDO spx_loop
      
    END SUBROUTINE adjust_spx
    ! by trive 27/2/97 -END-
    

      SUBROUTINE set_status (head, value)

         IMPLICIT NONE

!EN TEST         TYPE (simplex), TARGET, INTENT(IN) :: head
         TYPE (simplex), TARGET :: head
	 INTEGER, INTENT(IN) :: value
         TYPE (simplex), POINTER :: s

	 s  =>  head
	 DO; s  =>  s % next; IF (ASSOCIATED(s,head)) EXIT
	    s % status = value
	 ENDDO

      END SUBROUTINE set_status


      SUBROUTINE uns_split

         IMPLICIT NONE

	 LOGICAL :: unsatisf
	 INTEGER :: i, j
	 REAL (KIND=8) :: dt2, dp2
         TYPE (simplex), POINTER :: s, t
         TYPE (point), POINTER :: pi, pj

	 REAL (KIND=8), PARAMETER :: tol = 1.50

	 s  =>  grid % spx_head % next
	 DO; IF ( ASSOCIATED (s, grid % spx_head) ) EXIT
	    t  =>  s % next

	    unsatisf = .FALSE.

	    DO i=1,nd_d+1
	       pi  =>  s % opp(i) % pnt
	       DO j=i+1,nd_d+1
	          pj  =>  s % opp(j) % pnt
		  dt2 = (tol*0.5*(pi % leng + pj % leng))**2
		  dp2 = segment_length (nd_d, pi, pj)
		  IF ( dp2 > dt2 ) THEN
		     unsatisf = .TRUE.
		     EXIT
		  ENDIF
	       ENDDO
	       IF ( unsatisf ) EXIT
	    ENDDO

	    IF ( .NOT. unsatisf ) THEN
	       s % status = int_status
	       CALL movetop_simplex (grid % int_head, s)
	    ENDIF

	    s  =>  t
	 ENDDO

      END SUBROUTINE uns_split


      SUBROUTINE frn_points

         IMPLICIT NONE

         INTEGER :: m, i, j, k
         TYPE (simplex), POINTER :: s
         TYPE (point), POINTER :: p
         REAL (KIND=8) :: fn, srad2, leng2, num, den, frad2, heig2, dist2, t
         REAL (KIND=8) :: sum, sz2
         REAL (KIND=8), DIMENSION(nd_d) :: center


         m = 0; fn = 1./float(nd_d); s  =>  grid % spx_head

         DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % spx_head) ) EXIT

	    CALL simplex_center (nd_d, s, center, srad2)

	    sum = 0.0
	    DO j=1,nd_d+1
	       sum = sum + s % opp(j) % pnt % leng
	    ENDDO

	    DO i=1,nd_d+1

	       IF ( s % opp(i) % spx % status /= und_status ) THEN

	          num = s % fmat(i,nd_d+1); den = 0.
	          DO j=1,nd_d
	             num = num + s % fmat(i,j) * center(j)
	             den = den + s % fmat(i,j) ** 2
	          ENDDO

	          IF ( num <= 0.0 ) CYCLE

	          leng2 = (fn*(sum - s % opp(i) % pnt % leng))**2

	          heig2 = num**2/den 
	          frad2 = srad2-heig2
	          dist2 = leng2-frad2

	          t = SQRT(heig2/den)-SQRT(dist2/den); t = MAX (0d0,t)

	          IF (t >= 0.0) THEN
	             m = m+1
		     k = 1+MOD(i,nd_d+1)
		     
		     p  =>  grid % gar_head % next

		     IF ( ASSOCIATED (p, grid % gar_head) ) THEN
		        p  =>  new_point()
	                CALL pushtop_point (grid % add_head, p)
		     ELSE
	                CALL movetop_point (grid % add_head, p)
		     ENDIF

	             DO j=1,nd_d
	                p % x(j) = center(j) - s % fmat(i,j) * t
	             ENDDO

	             p % old  =>  s % opp(k) % pnt

		     CALL eval_back (p)
	          ENDIF
	       ENDIF
	    ENDDO
         ENDDO

         nfro = m

      END SUBROUTINE frn_points


      SUBROUTINE add_frn_points

         IMPLICIT NONE

         LOGICAL :: far
         INTEGER :: m ! , i, nstc
         TYPE (point), POINTER :: p, q
         ! TYPE (simplex), POINTER :: hi, hs


         m = 0; p  =>  grid % add_head % next

         DO; IF ( ASSOCIATED (p, grid % add_head) ) EXIT
            q  =>  p % next
            CALL insert_far_point (nd_d, grid % spx_head, p, far)
            IF (far) THEN
	       m = m+1
	       CALL movetop_point (grid % pnt_head, p)
	    ELSE
	       CALL movetop_point (grid % gar_head, p)
	    ENDIF
	    p  =>  q
         END DO
  
         nadd = m

      END SUBROUTINE add_frn_points


      SUBROUTINE crn_points

         IMPLICIT NONE

         INTEGER :: i, j, m
         TYPE (simplex), POINTER :: s, n


         m = 0; s  =>  grid % int_head

         DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % int_head) ) EXIT

	    j = 0

	    DO i=1,nd_d+1
	       n  =>  s % opp(i) % spx
	       IF ( n % status == frz_status ) j = j+1
	    ENDDO

	    IF ( j == nd_d ) THEN
	       DO i=1,nd_d+1
	          n  =>  s % opp(i) % spx
	          IF ( ( n % status == int_status ) ) THEN
	             s % status = stc_status
		     m = m+1
		     CALL halve_dom (grid, i, s)
		     EXIT
	          ENDIF
	       ENDDO
	    ELSEIF ( j == nd_d+1 ) THEN
	       WRITE (*,*) 'refine_corners error, unpossible element found'
	       WRITE (*,*) '(stop)'
	       STOP
	    ENDIF
         ENDDO

         s  =>  grid % int_head
         DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % int_head) ) EXIT
	    s % status = int_status
         ENDDO

         nadd = m

      END SUBROUTINE crn_points


      SUBROUTINE add_crn_points

         IMPLICIT NONE

         INTEGER :: m
         TYPE (point), POINTER :: p, q


         m = 0; p  =>  grid % add_head % next

         DO; IF ( ASSOCIATED (p, grid % add_head) ) EXIT
	    q  =>  p % next
            m = m+1
            CALL insert_point (nd_d, grid % int_head, p)
	    CALL movetop_point (grid % pnt_head, p)
	    p  =>  q
         END DO
  
         nadd = m

      END SUBROUTINE add_crn_points


      SUBROUTINE restore_lists

         IMPLICIT NONE

         TYPE (simplex), POINTER :: s

	 s  =>  grid % spx_head
	 DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % spx_head) ) EXIT
	    s % status = int_status
	 ENDDO

	 s  =>  grid % ext_head
	 DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % ext_head) ) EXIT
	    s % status = ext_status
	 ENDDO

	 IF ( .NOT. ASSOCIATED (grid % spx_head % next, grid % spx_head) ) THEN
	    CALL jointop_simplex (grid % int_head, grid % spx_head)
	 ENDIF

      END SUBROUTINE restore_lists


   END SUBROUTINE front_grid


   SUBROUTINE set_metric (grid)

      IMPLICIT NONE

!EN TEST      TYPE (domain), INTENT(OUT) :: grid
      TYPE (domain), INTENT(INOUT) :: grid

      INTEGER :: i
      TYPE (point), POINTER :: p
      REAL (KIND=8) :: d


      p  =>  grid % pnt_head
      DO; p  =>  p % next; IF ( ASSOCIATED (p, grid % pnt_head) ) EXIT

	 IF ( p % leng > 0.0 ) THEN

	    p % metric = 0.0; d = 1.0/(p % leng)**2

	    DO i=1,nd_d
	       p % metric(i,i) = d
	    ENDDO

	 ENDIF

      ENDDO

   END SUBROUTINE set_metric


   SUBROUTINE add_points_1d (grid)

      IMPLICIT NONE

!EN TEST      TYPE (domain), INTENT(OUT) :: grid
      TYPE (domain), INTENT(INOUT) :: grid

      INTEGER :: i
      TYPE (point), POINTER :: p, q
      TYPE (simplex), POINTER :: hs, hi, he


      DO i=1,grid % number_of_edges

	 hs  =>  grid % edges(i) % spx_head
	 hi  =>  grid % edges(i) % int_head
	 he  =>  grid % edges(i) % ext_head

	 ! write (*,*) 'axs = ',numberof_simplex(grid % edges(i) % axs_head)
	 ! write (*,*) 'spx = ',numberof_simplex(hs)
	 ! write (*,*) 'int = ',numberof_simplex(hi)
	 ! write (*,*) 'ext = ',numberof_simplex(he)

         CALL merge_ie (nd_b, hs, hi, he)

!EN TEST
	 !write (*,*) 'axs = ',numberof_simplex(grid % edges(i) % axs_head)
	 ! write (*,*) 'spx = ',numberof_simplex(hs)
	 !write (*,*) 'int = ',numberof_simplex(hi)
	 !write (*,*) 'ext = ',numberof_simplex(he)

	 p  =>  grid % edges(i) % add_head % next
         DO; IF ( ASSOCIATED (p, grid % edges(i) % add_head) ) EXIT
	    q  =>  p % next;
            CALL insert_point  (nd_b, hs, p)
            CALL movetop_point (grid % edges(i) % pnt_head, p)
	    p  =>  q
         END DO

         CALL split_ie_1d  (nd_b, hs, hi, he)

	 ! write (*,*) 'axs = ',numberof_simplex(grid % edges(i) % axs_head)
	 ! write (*,*) 'spx = ',numberof_simplex(hs)
	 ! write (*,*) 'int = ',numberof_simplex(hi)
	 ! write (*,*) 'ext = ',numberof_simplex(he)

      ENDDO

   END SUBROUTINE add_points_1d


   SUBROUTINE add_points_12d (grid)

      IMPLICIT NONE

!EN TEST      TYPE (domain), INTENT(OUT) :: grid
      TYPE (domain), INTENT(INOUT) :: grid

      INTEGER :: i, nstc
      TYPE (point), POINTER :: p, q
      TYPE (simplex), POINTER :: hs, hi, he, hbi, hbs


      !  1d points

      CALL add_points_1d (grid)


      !  2d points

      hs  =>  grid % spx_head
      hi  =>  grid % int_head
      he  =>  grid % ext_head

      DO

	 ! write (*,*) 'axs = ',numberof_simplex(grid % axs_head)
	 ! write (*,*) 'spx = ',numberof_simplex(hs)
	 ! write (*,*) 'int = ',numberof_simplex(hi)
	 ! write (*,*) 'ext = ',numberof_simplex(he)

	 CALL merge_ie (nd_d, hs, hi, he)

	 ! write (*,*) 'axs = ',numberof_simplex(grid % axs_head)
	 ! write (*,*) 'spx = ',numberof_simplex(hs)
	 ! write (*,*) 'int = ',numberof_simplex(hi)
	 ! write (*,*) 'ext = ',numberof_simplex(he)

         p  =>  grid % add_head % next
         DO; IF ( ASSOCIATED (p, grid % add_head) ) EXIT
            q  =>  p % next
            CALL insert_point  (nd_d, grid % spx_head, p)
            CALL movetop_point (grid % pnt_head, p)
	    p  =>  q
         END DO

         DO i=1,grid % number_of_edges
	    hbi  =>  grid % edges(i) % int_head
	    hbs  =>  grid % edges(i) % stc_head
            CALL flag_shell (nd_d, hbi, hs, hbs)
         ENDDO

         CALL stitch_boundary (grid, nstc)

	 IF ( nstc .GT. 0 ) THEN
            WRITE (*,*) 'Stitch points   = ',nstc
	    CALL add_points_1d (grid)
	 ELSE
	    CALL split_ie (nd_d, hs, hi, he)
	    EXIT
	 ENDIF
      ENDDO

   END SUBROUTINE add_points_12d


   SUBROUTINE g_plot (idf, grid)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf
      TYPE (domain), INTENT(INOUT) :: grid

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

      INTEGER, PARAMETER :: dom_color = green, bou_color = yellow, marker = none

      LOGICAL :: first
      INTEGER :: i, j, k, ipl, e, n_lines
      TYPE (point), POINTER :: p
      TYPE (simplex), POINTER :: s_head, s
      REAL (KIND=8), DIMENSION(2) :: xpl


      WRITE (idf,*) '$ DATA = CURVE2D'
      WRITE (idf,*) '% equalscale = TRUE'
      WRITE (idf,*) '% boundary = TRUE'

      s_head  =>  grid % ext_head; s  =>  s_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, s_head) ) EXIT
	 s % status = - s % status
      ENDDO

      s_head  =>  grid % int_head; s  =>  s_head; n_lines = 0
      DO; s  =>  s % next; IF ( ASSOCIATED (s, s_head) ) EXIT
	 IF (s % status > 0) THEN
	    s % status = - s % status
	    DO i=1,nd_d+1
	       IF ( s % opp(i) % spx % status > 0 ) THEN
	          n_lines = n_lines+1; first = .TRUE.
                  WRITE (idf,*)
                  WRITE (idf,*) '% linecolor = ',dom_color,' markertype = ',marker
		  DO j=1,nd_d+1
		     IF (j .NE. i) THEN
			DO k=1,nd_d
			   xpl(k) = s % opp(j) % pnt % x(k)
			ENDDO
			ipl = s % opp(j) % pnt % index
                        WRITE (idf,'(2e15.7,i8)') xpl, ipl
		     ENDIF
		  ENDDO
	       ENDIF
	    ENDDO
	 ENDIF
      ENDDO

      s  =>  s_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, s_head) ) EXIT
	 s % status = ABS(s % status)
      ENDDO

      s_head  =>  grid % ext_head; s  =>  s_head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, s_head) ) EXIT
	 s % status = ABS(s % status)
      ENDDO

      DO e=1,grid % number_of_edges

         s  =>  grid % edges(e) % int_head
         DO; s  =>  s % next; IF ( ASSOCIATED (s, grid % edges(e) % int_head) ) EXIT

	    n_lines = n_lines+1

            WRITE (idf,*)
            WRITE (idf,*) '% linecolor = ',bou_color,' markertype = ',marker
	    DO k=1,nd_d
	       xpl(k) = s % opp(1) % pnt % ppp % x(k)
	    ENDDO
            ipl = s % opp(1) % pnt % ppp % index
            WRITE (idf,'(2e15.7,i8)') xpl, ipl
	    DO k=1,nd_d
	       xpl(k) = s % opp(2) % pnt % ppp % x(k)
	    ENDDO
            ipl = s % opp(2) % pnt % ppp % index
            WRITE (idf,'(2e15.7,i8)') xpl, ipl

         ENDDO

      ENDDO

   END SUBROUTINE g_plot


END MODULE grid_2d

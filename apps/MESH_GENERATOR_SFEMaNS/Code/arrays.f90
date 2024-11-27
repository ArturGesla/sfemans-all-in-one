MODULE arrays

   USE convert

   IMPLICIT NONE


   !  LAST GENERATED GRID

   TYPE (domain), POINTER, PUBLIC :: array

   INTEGER, PUBLIC :: np_d, ns_d, np_b, ns_b

   INTEGER, DIMENSION(:), ALLOCATABLE, PUBLIC :: iparent, idir, jjdir, iflux, neighs
   INTEGER, DIMENSION(:,:), ALLOCATABLE, PUBLIC :: jjs, neigh, jjs_b
   REAL (KIND=8), DIMENSION(:,:), ALLOCATABLE, PUBLIC :: rr_b
   REAL (KIND=8), DIMENSION(:,:,:), ALLOCATABLE, PUBLIC :: hess

   INTEGER, DIMENSION(:,:), POINTER, PUBLIC :: jj
   REAL (KIND=8), DIMENSION(:,:), POINTER, PUBLIC :: rr


   !  PREVIOUS GRID

   INTEGER, PUBLIC :: np_d_previous, ns_d_previous

   INTEGER, DIMENSION(:,:), POINTER, PUBLIC :: jj_previous
   REAL (KIND=8), DIMENSION(:,:), POINTER, PUBLIC :: rr_previous



   PUBLIC  :: build_arrays, match_arrays, save_arrays, smooth
   PRIVATE :: p_vect_domain, s_vect_domain, p_vect_boundary, s_vect_boundary


CONTAINS


   SUBROUTINE build_arrays (grid)

      IMPLICIT NONE

      TYPE (domain), TARGET, INTENT(INOUT) :: grid

      INTEGER :: e, n


      array  =>  grid

      np_d_previous = np_d; ns_d_previous = ns_d

      np_d = 0; ns_d = 0
      np_b = 0; ns_b = 0

      CALL set_p2i (np_d, grid % pnt_head)
      CALL set_s2i (ns_d, grid % int_head)

      CALL clr_s2i (grid % ext_head)

      DO e=1,grid % number_of_edges
         CALL set_p2i (np_b, grid % edges(e) % pnt_head)
      ENDDO

      DO e=1,grid % number_of_edges
         CALL set_s2i (ns_b, grid % edges(e) % int_head)
      ENDDO

      IF ( ALLOCATED ( iparent ) ) DEALLOCATE ( iparent )
      IF ( ALLOCATED ( idir ) )    DEALLOCATE ( idir )
      IF ( ALLOCATED ( jjdir ) )   DEALLOCATE ( jjdir )
      IF ( ALLOCATED ( iflux ) )   DEALLOCATE ( iflux )
      IF ( ALLOCATED ( jjs ) )     DEALLOCATE ( jjs )
      IF ( ALLOCATED ( neighs ) )  DEALLOCATE ( neighs )
      IF ( ALLOCATED ( neigh ) )   DEALLOCATE ( neigh )
      IF ( ALLOCATED ( hess ) )    DEALLOCATE ( hess )

      IF ( ALLOCATED ( jjs_b ) )   DEALLOCATE ( jjs_b )
      IF ( ALLOCATED ( rr_b ) )    DEALLOCATE ( rr_b )

!EN TEST      IF ( ASSOCIATED ( jj_previous ) ) DEALLOCATE ( jj_previous )
      IF ( ASSOCIATED ( jj_previous ) ) NULLIFY ( jj_previous )
!EN TEST      IF ( ASSOCIATED ( rr_previous ) ) DEALLOCATE ( rr_previous )
      IF ( ASSOCIATED ( rr_previous ) ) NULLIFY ( rr_previous )

      IF ( ASSOCIATED ( jj ) ) jj_previous  =>  jj
      IF ( ASSOCIATED ( rr ) ) rr_previous  =>  rr

      NULLIFY  ( jj )
      NULLIFY  ( rr )

      ALLOCATE ( iparent(np_d) )
      ALLOCATE ( idir(np_b) )
      ALLOCATE ( jjdir(np_b) )

      ALLOCATE ( iflux(ns_b) )
      ALLOCATE ( jjs(nd_b+1, ns_b) )
      ALLOCATE ( neighs(ns_b) )

      ALLOCATE ( jj(nd_d+1, ns_d) )
      ALLOCATE ( neigh(nd_d+1, ns_d) )

      ALLOCATE ( rr(nd_d, np_d) )
      ALLOCATE ( hess(nd_d, nd_d, np_d) )

      ALLOCATE ( jjs_b(nd_b+1, ns_b) )
      ALLOCATE ( rr_b(nd_b, np_b) )


      CALL p_vect_domain (grid % pnt_head)
      CALL s_vect_domain (grid % int_head)

      n = 0
      DO e=1,grid % number_of_edges
         CALL p_vect_boundary (grid % edges(e) % pnt_head, e, n)
      ENDDO

      n = 0
      DO e=1,grid % number_of_edges
         CALL s_vect_boundary (grid % edges(e) % int_head, e, n)
      ENDDO

   END SUBROUTINE build_arrays


   SUBROUTINE match_arrays

      IMPLICIT NONE

      INTEGER :: e, i, j, k, n
      TYPE (point), POINTER :: p


      IF ( .NOT. ASSOCIATED (array) ) THEN
	 WRITE (*,*) 'match_arrays error, array not associated'
	 WRITE (*,*) '(stop)'
	 STOP
      ENDIF

      n = 0
      CALL set_p2i (n, array % pnt_head)

      n = 0
      DO e=1,array % number_of_edges
         CALL set_p2i (n, array % edges(e) % pnt_head)
      ENDDO

      p  =>  array % pnt_head
      DO; p  =>  p % next; IF ( ASSOCIATED (p, array % pnt_head) ) EXIT
	 k = p % index
	 DO j=1,nd_d
	    DO i=1,nd_d
	       p % metric(i,j) = hess(i,j,k)
	    ENDDO
	    p % x(j) = rr(j,k)
	 ENDDO
      END DO

      DO e=1,array % number_of_edges
         p  =>  array % edges(e) % pnt_head
         DO; p  =>  p % next; IF ( ASSOCIATED (p, array % edges(e) % pnt_head) ) EXIT
	    k = p % index
	    DO j=1,nd_b
	       p % x(j) = rr_b(j,k)
	    ENDDO
         END DO
      ENDDO

   END SUBROUTINE match_arrays


   SUBROUTINE save_arrays (f)

      IMPLICIT NONE

      INTEGER, INTENT(INOUT) :: f

      INTEGER :: i

      WRITE (f,'(5(4x,a4))') 'ND_D','NP_D','NS_D','NP_B','NS_B'
      WRITE (f,'(5i8)') nd_d, np_d, ns_d, np_b, ns_b

      WRITE (f,'(5x,a3,8(2x,a5,i1))') 'IDX',('   JJ',i,i=1,nd_d+1),('NEIGH',i,i=1,nd_d+1)
      DO i=1,ns_d
	 WRITE (f,'(9i8)') i,jj(:,i),neigh(:,i)
      ENDDO

      WRITE (f,'(2(2x,a6),3(12x,a2,i1))' ) '   IDX','PARENT',('RR',i,i=1,nd_d)
      DO i=1,np_d
	 WRITE (f,'(2i8,3e15.7)') i,iparent(i),rr(:,i)
      ENDDO

      WRITE (f,'(2(3x,a5),2x,a6,8(2x,a5,i1))') '  IDX','IFLUX','NEIGHS',    &
					       ('  JJS',i,i=1,nd_b+1),('JJS_B',i,i=1,nd_b+1)
      DO i=1,ns_b
	 WRITE (f,'(9i8)') i,iflux(i),neighs(i),jjs(:,i),jjs_b(:,i)
      ENDDO

      WRITE (f,'(3(3x,a5),2(10x,a4,i1))' ) '  IDX',' IDIR','JJDIR',('RR_B',i,i=1,nd_b)
      DO i=1,np_b
	 WRITE (f,'(3i8,2e15.7)') i,idir(i),jjdir(i),rr_b(:,i)
      ENDDO

   END SUBROUTINE save_arrays


   SUBROUTINE smooth

      IMPLICIT NONE

      INTEGER :: i, j, k
      INTEGER, PARAMETER :: kmax = 10

      REAL (KIND=8) :: a, b, e
      REAL (KIND=8), PARAMETER :: tol = 1.d-6
      REAL (KIND=8), DIMENSION(:,:), ALLOCATABLE :: dr
      REAL (KIND=8), DIMENSION(:),   ALLOCATABLE :: rn, s

      TYPE (point), POINTER :: p


      IF ( .NOT. ASSOCIATED (array) ) THEN
	 WRITE (*,*) 'smooth error, array not associated'
	 WRITE (*,*) '(stop)'
	 STOP
      ENDIF

      ALLOCATE ( dr(nd_d, np_d) )
      ALLOCATE ( rn(np_d) )
      ALLOCATE ( s(nd_d) )

      rn = 0.

      DO i=1,ns_d
         DO j=1,nd_d+1
	    rn(jj(j,i)) = rn(jj(j,i))+2.
         ENDDO
      ENDDO

      DO i=1,np_d
         rn(i) = 1./rn(i)
      ENDDO

      DO k=1,kmax
	 dr = 0.

	 DO i=1,ns_d
	    s = 0.
	    DO j=1,nd_d+1
	       s(:) = s(:) + rr(:,jj(j,i))
	    ENDDO
	    DO j=1,nd_d+1
	       dr(:,jj(j,i)) = dr(:,jj(j,i)) + s(:) - rr(:,jj(j,i))
	    ENDDO
	 ENDDO

	 DO i=1,np_d
	    dr(:,i) = rn(i)*dr(:,i) - rr(:,i)
	 ENDDO

         DO i=1,np_b
            dr(:,jjdir(i)) = 0.
         ENDDO

	 DO i=1,np_d
	    rr(:,i) = rr(:,i) + dr(:,i)
	 ENDDO

	 e = 0.
	 DO i=1,np_d
	    DO j=1,nd_d
	       e = e + dr(j,i)**2
	    ENDDO
	 ENDDO

	 WRITE (*,'(i5,e12.5)') k, e

	 IF ( e .lt. tol ) EXIT
      ENDDO

      DEALLOCATE ( dr )
      DEALLOCATE ( rn )
      DEALLOCATE ( s )

      p  =>  array % pnt_head
      DO; p  =>  p % next; IF ( ASSOCIATED (p, array % pnt_head) ) EXIT
	 p % x = rr(:,p % index)
      ENDDO

   END SUBROUTINE smooth


   SUBROUTINE save_prev (f)

      IMPLICIT NONE

      INTEGER, INTENT(INOUT) :: f

      INTEGER :: i

      WRITE (f,'(5(4x,a4))') 'ND_D','NP_D','NS_D'
      WRITE (f,'(5i8)') nd_d,np_d_previous,ns_d_previous

      WRITE (f,'(5x,a3,8(2x,a5,i1))') 'IDX',('S2X_D',i,i=1,nd_d+1)
      DO i=1,ns_d_previous
	 WRITE (f,'(9i8)') i,jj_previous(:,i)
      ENDDO

      WRITE (f,'(5x,a3,3(12x,a2,i1))' ) 'IDX',('X',i,i=1,nd_d)
      DO i=1,np_d_previous
	 WRITE (f,'(i8,3e15.7)') i,rr_previous(:,i)
      ENDDO

   END SUBROUTINE save_prev


   SUBROUTINE p_vect_domain (head)

      IMPLICIT NONE

      TYPE (point), TARGET, INTENT(INOUT) :: head

      INTEGER :: k
      TYPE (point), POINTER :: p

      p  =>  head

      DO; p  =>  p % next; IF ( ASSOCIATED (p, head) ) EXIT
	 iparent(p % index) = p % parent
	 DO k=1,nd_d
	    rr(k,p % index) = p % x(k)
	 ENDDO
      END DO

   END SUBROUTINE p_vect_domain


   SUBROUTINE s_vect_domain (head)

      IMPLICIT NONE

      TYPE (simplex), TARGET, INTENT(INOUT) :: head
      TYPE (simplex), POINTER :: s
      INTEGER :: i


      s  =>  head
      DO; s  =>  s % next; IF ( ASSOCIATED (s, head) ) EXIT

	 DO i=1,nd_d+1
	    IF ( ASSOCIATED (s % opp(i) % pnt) ) THEN
	       jj(i,s % index) = s % opp(i) % pnt % index
	    ELSE
	       jj(i,s % index) = 0
            ENDIF
	 ENDDO

	 DO i=1,nd_d+1
	    IF ( ASSOCIATED (s % opp(i) % spx) ) THEN
	       neigh(i,s % index) = s % opp(i) % spx % index
	    ELSE
	       neigh(i,s % index) = 0
            ENDIF
	 ENDDO

      ENDDO

   END SUBROUTINE s_vect_domain


   SUBROUTINE p_vect_boundary (head, e, n)

      IMPLICIT NONE

      TYPE (point), TARGET, INTENT(INOUT) :: head
      INTEGER, INTENT(IN)    :: e
      INTEGER, INTENT(INOUT) :: n

      INTEGER :: i, k
      TYPE (point), POINTER :: p

      i = n; p  =>  head

      DO; p  =>  p % next; IF ( ASSOCIATED (p, head) ) EXIT
	 i = i+1
	 idir(i)   = p % cnd
	 jjdir(i)  = p % ppp % index
	 DO k=1,nd_b
            rr_b(k,i) = p % x(k)
	 ENDDO
      END DO

      n = i

   END SUBROUTINE p_vect_boundary


   SUBROUTINE s_vect_boundary (head, e, n)

      IMPLICIT NONE

      TYPE (simplex), TARGET, INTENT(INOUT) :: head
      INTEGER, INTENT(IN)    :: e
      INTEGER, INTENT(INOUT) :: n

      TYPE (simplex), POINTER :: s
      INTEGER :: i, j


      i = n; s  =>  head

      DO; s  =>  s % next; IF ( ASSOCIATED (s, head) ) EXIT

	 i = i+1
	 iflux(i) = e

	 IF ( ASSOCIATED(s % int) ) then
	    neighs(i) = s % int % index
	 ELSE
	    neighs(i) = 0
	 ENDIF

	 DO j=1,nd_b+1
	    IF ( ASSOCIATED (s % opp(j) % pnt) ) THEN
	       jjs_b(j,i) = s % opp(j) % pnt % index
	       jjs(j,i)   = s % opp(j) % pnt % ppp % index
	    ELSE
	       jjs_b(j,i) = 0
	       jjs(j,i)   = 0
            ENDIF
	 ENDDO

      ENDDO

      n = i

   END SUBROUTINE s_vect_boundary


END MODULE arrays

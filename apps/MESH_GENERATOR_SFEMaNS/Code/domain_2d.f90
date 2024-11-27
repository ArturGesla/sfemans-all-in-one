MODULE domain_2d


   !  ### 2D TOPOLOGICAL DEFINITIONS  ###



   USE curves
   USE delaunay

   IMPLICIT NONE


   TYPE vert
      TYPE (point), POINTER :: pnt
   END TYPE vert

   TYPE edge
      INTEGER :: index, ndata, begin_vert, end_vert
      INTEGER, DIMENSION(:), POINTER :: idata
      REAL (KIND=8), DIMENSION(:), POINTER :: sdata, hdata
      TYPE (point),   POINTER :: pnt_head, axp_head, add_head, rem_head, gar_head
      TYPE (simplex), POINTER :: spx_head, axs_head, int_head, ext_head, stc_head
   END TYPE edge

   TYPE domain
      INTEGER :: number_of_verts, number_of_edges
      TYPE (vert), DIMENSION(:), POINTER :: verts
      TYPE (edge), DIMENSION(:), POINTER :: edges
      TYPE (point),   POINTER :: pnt_head, axp_head, add_head, rem_head, gar_head
      TYPE (simplex), POINTER :: spx_head, axs_head, int_head, ext_head, stc_head
   END TYPE domain


   INTEGER, PARAMETER, PUBLIC :: nd_b = 1, nd_d = 2

   PUBLIC  :: load_topology


CONTAINS


   SUBROUTINE load_topology (idf, grid)

      !  READ THE 2D TOPOLOGY AND INTIALIZE THE VARIOUS LISTS

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: idf
!EN TEST      TYPE (domain), INTENT(OUT) :: grid
      TYPE (domain), INTENT(INOUT) :: grid

      INTEGER :: i, j, n

      READ (idf,*) ! NV, NE
      READ (idf,*) grid % number_of_verts, grid % number_of_edges

      IF ( .NOT. ASSOCIATED ( grid % verts ) ) THEN
	 ALLOCATE ( grid % verts(grid % number_of_verts) )
      ENDIF

      IF ( .NOT. ASSOCIATED ( grid % edges ) ) THEN
	 ALLOCATE ( grid % edges(grid % number_of_edges) )
      ENDIF


      DO i=1,grid % number_of_edges

         READ (idf,*) ! Space
         READ (idf,*) ! BEGIN
         READ (idf,*) ! INDEX, NDATA, BEGIN_VERT, END_VERT
         READ (idf,*) grid % edges(i) % index,         &
	              grid % edges(i) % ndata,         &
	              grid % edges(i) % begin_vert,    &
	              grid % edges(i) % end_vert

	 n = grid % edges(i) % ndata

	 IF ( n < 2 ) THEN
	    WRITE (*,*) 'load_topology error, valid ndata >= 2; I have ',n
	    WRITE (*,*) '(stop)'
	    STOP
	 ENDIF

	 ALLOCATE ( grid % edges(i) % idata(n) )
	 ALLOCATE ( grid % edges(i) % sdata(n) )
	 ALLOCATE ( grid % edges(i) % hdata(n) )

         READ (idf,*) ! IDATA, SDATA, HDATA
	 DO j=1,n
	    READ (idf,*) grid % edges(i) % idata(j),   &
			 grid % edges(i) % sdata(j),   &
			 grid % edges(i) % hdata(j)
	 ENDDO
         READ (idf,*) ! END

         grid % edges(i) % pnt_head  =>  newhead_point ()
         grid % edges(i) % axp_head  =>  newhead_point ()
         grid % edges(i) % add_head  =>  newhead_point ()
         grid % edges(i) % rem_head  =>  newhead_point ()
         grid % edges(i) % gar_head  =>  newhead_point ()

         grid % edges(i) % spx_head  =>  newhead_simplex ()
         grid % edges(i) % axs_head  =>  newhead_simplex ()
         grid % edges(i) % int_head  =>  newhead_simplex ()
         grid % edges(i) % ext_head  =>  newhead_simplex ()
         grid % edges(i) % stc_head  =>  newhead_simplex ()

      ENDDO

      grid % pnt_head  =>  newhead_point ()
      grid % axp_head  =>  newhead_point ()
      grid % add_head  =>  newhead_point ()
      grid % rem_head  =>  newhead_point ()
      grid % gar_head  =>  newhead_point ()

      grid % spx_head  =>  newhead_simplex ()
      grid % axs_head  =>  newhead_simplex ()
      grid % int_head  =>  newhead_simplex ()
      grid % ext_head  =>  newhead_simplex ()
      grid % stc_head  =>  newhead_simplex ()


   END SUBROUTINE load_topology


END MODULE domain_2d

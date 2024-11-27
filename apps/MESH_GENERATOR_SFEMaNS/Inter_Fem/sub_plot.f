MODULE sub_plot

CONTAINS

SUBROUTINE plot_vit_2d(jj, rr, uu)
!---FORMAT PLTMTV

   IMPLICIT NONE

   INTEGER,      DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(INOUT) :: uu  ! rigid translation

   INTEGER :: m, n1, n2, n3

   OPEN (UNIT = 20, FILE = 'vit_u', FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20,*) '% meshplot  = False'

   DO m = 1, SIZE(jj, 2)
!      n1 = jj(1, m)
!      n2 = jj(2, m)
!      n3 = jj(3, m)
!      WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
!      WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
!      WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
!      WRITE (20, *)

     n1 = jj(1, m)
     n2 = jj(6, m)
     n3 = jj(5, m)
     WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, *)


     n1 = jj(2, m)
     n2 = jj(4, m)
     n3 = jj(6, m)
     WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, *)

     n1 = jj(4, m)
     n2 = jj(3, m)
     n3 = jj(5, m)
     WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, *)

     n1 = jj(5, m)
     n2 = jj(6, m)
     n3 = jj(4, m)
     WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, *)
  ENDDO

END SUBROUTINE plot_vit_2d

!----------------------------------------------------------------

SUBROUTINE plot_arrow(jj, rr, vv)
!---FORMAT PLTMTV

   IMPLICIT NONE

   INTEGER,      DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr, vv

   INTEGER :: n
   REAL(KIND=8) :: z_d, w_d

   OPEN (UNIT = 20, FILE = 'velocity', FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, 9000) SIZE(rr,2), SIZE(jj,1), SIZE(jj,2)
   9000 FORMAT('#',3i10)
   WRITE (20, *) '$ DATA = VECTOR'
   WRITE (20, *) '% axisscale = FALSE'
   WRITE (20, *) '% vscale = 1.'

!  WRITE (20, *) '% contstyle = 2'
!  WRITE (20, *) '% meshplot  = True'

   SELECT CASE(SIZE(rr,1))

      CASE(2)
         z_d = 0
         w_d = 0
         DO n = 1, SIZE(rr, 2)
            WRITE (20,*) rr(1,n), rr(2,n), z_d, vv(1,n), vv(2,n), w_d
         ENDDO

      CASE(3)
         DO n = 1, SIZE(rr, 2)
            WRITE (20,*) rr(:,n), vv(:,n)
         ENDDO

   END SELECT

   close(20)

END SUBROUTINE plot_arrow

!------------------------------------------------------------------------------

SUBROUTINE plot_pressure_2d(jj, rr, uu)
!---FORMAT PLTMTV

   IMPLICIT NONE

   INTEGER, DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(INOUT) :: uu  ! rigid translation

   INTEGER :: na, nb, n, m, n1, n2, n3
   REAL(KIND=8) :: xa, xb, xm, r, p_midpoint

   OPEN (UNIT = 20, FILE = 'pressure_2D.plt', FORM = 'formatted', STATUS = 'unknown')

!  set pressure field so as to have a zero pressure at
!  the mid-point of the bottom unit side of the cavity

!  find the mid-point

   xa = 0;  xb = 1;  xm = 0.5
   na = 1;  nb = 1
   DO n = 1, SIZE(uu)

      IF (rr(2,n) < 1.0d-4) THEN

         r = rr(1,n)
         IF (r < xm  .AND.  r > xa) THEN;  xa = r;  na = n;  ENDIF
         IF (r > xm  .AND.  r < xb) THEN;  xb = r;  nb = n;  ENDIF

      ENDIF

   ENDDO

   p_midpoint = uu(na) + (uu(nb) - uu(na)) * (xm-xa)/(xb-xa)

!  reset pressure field

   uu = uu - p_midpoint

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 1'
   WRITE (20, *) '% contours = "( -.12  -.06   -.03 )"'
   WRITE (20, *) '% contours = "( -.015 -.0075 -.00375 )"'
   WRITE (20, *) '% contours = "( 0.0 )"'
   WRITE (20, *) '% contours = "(  .12   .06    .03 )"'
   WRITE (20, *) '% contours = "(  .015  .0075  .00375 )"'

   WRITE (20,*) '% meshplot  = False'

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
      WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
      WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
      WRITE (20, *)
   ENDDO
CLOSE(20)

END SUBROUTINE plot_pressure_2d

!------------------------------------------------------------------------------

SUBROUTINE plot_loc_rel_var(jj, rr, vo, vv)
!---FORMAT PLTMTV

!  plot local relative variation of speed


   IMPLICIT NONE

   INTEGER, DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr, vo, vv

   REAL(KIND=8), DIMENSION(SIZE(vv,2)) :: uu
   REAL(KIND=8) :: vo_n_mod
   INTEGER :: n, m, n1, n2, n3

   OPEN (UNIT = 20, FILE = 'speed_var', FORM = 'formatted', STATUS = 'unknown')


   DO n = 1, SIZE(vv,2)

      vo_n_mod = SQRT(SUM(vo(:,n)**2))

      IF (vo_n_mod < 1.0d-15)  THEN

         uu(n) = 0

      ELSE

         uu(n) = SQRT(SUM( (vo(:,n) - vv(:,n))**2 ))/vo_n_mod

      ENDIF

   ENDDO


   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20, *) '% meshplot  = True'

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
      WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
      WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
      WRITE (20, *)
   ENDDO

END SUBROUTINE plot_loc_rel_var

!------------------------------------------------------------------------------

SUBROUTINE plot_pressure(jj, rr, uu)
!---FORMAT PLTMTV

   IMPLICIT NONE

   INTEGER, DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(INOUT) :: uu  ! rigid translation

   INTEGER :: na, nb, n, m, n1, n2, n3
   REAL(KIND=8) :: xa, xb, xm, r, p_midpoint

   OPEN (UNIT = 20, FILE = 'pressure', FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20, *) '% cstep = 20'
   WRITE (20, *) '% meshplot  = False'

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (20, *) rr(1,n1), rr(2,n1), uu(n1), n1
      WRITE (20, *) rr(1,n2), rr(2,n2), uu(n2), n2
      WRITE (20, *) rr(1,n3), rr(2,n3), uu(n3), n3
      WRITE (20, *)
   ENDDO

END SUBROUTINE plot_pressure

SUBROUTINE plot_const_p1_label(jj, rr, uu, file_name )
!---FORMAT PLTMTV

   IMPLICIT NONE

   CHARACTER(*) :: file_name
   INTEGER, DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(IN) :: uu

   INTEGER :: na, nb, n, m, n1, n2, n3
   REAL(KIND=8) :: xa, xb, xm, r, p_midpoint

   OPEN (UNIT = 20, FILE = file_name, FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20, *) '% nsteps = 50'
   WRITE (20, *) '% meshplot  = false'
   WRITE (20, *)

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (20, 100) rr(1,n1), rr(2,n1), uu(m), n1
      WRITE (20, 100) rr(1,n2), rr(2,n2), uu(m), n2
      WRITE (20, 100) rr(1,n3), rr(2,n3), uu(m), n3
      WRITE (20, *)
   ENDDO
100 FORMAT(3(e11.5,3x),i5)
CLOSE(20)

END SUBROUTINE plot_const_p1_label

SUBROUTINE plot_pressure_p1_label(jj, rr, uu, file_name )
!---FORMAT PLTMTV

   IMPLICIT NONE

   CHARACTER(*) :: file_name
   INTEGER, DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(INOUT) :: uu  ! rigid translation

   INTEGER :: na, nb, n, m, n1, n2, n3
   REAL(KIND=8) :: xa, xb, xm, r, p_midpoint

   OPEN (UNIT = 20, FILE = file_name, FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20, *) '% nsteps = 50'
   WRITE (20, *) '% meshplot  = true'
   WRITE (20, *)

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (20, 100) rr(1,n1), rr(2,n1), uu(n1), n1
      WRITE (20, 100) rr(1,n2), rr(2,n2), uu(n2), n2
      WRITE (20, 100) rr(1,n3), rr(2,n3), uu(n3), n3
      WRITE (20, *)
   ENDDO
100 FORMAT(3(e11.5,3x),i5)
CLOSE(20)

END SUBROUTINE plot_pressure_p1_label

SUBROUTINE plot_pressure_label(jj, rr, uu, file_name )
!---FORMAT PLTMTV

   IMPLICIT NONE

   CHARACTER(*) :: file_name
   INTEGER, DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(INOUT) :: uu  ! rigid translation

   INTEGER :: na, nb, n, m, n1, n2, n3
   REAL(KIND=8) :: xa, xb, xm, r, p_midpoint

   OPEN (UNIT = 20, FILE = file_name, FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20, *) '% nsteps = 50'
   WRITE (20, *) '% meshplot  = False'
   WRITE (20, *)

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (20, 100) rr(1,n1), rr(2,n1), uu(n1), n1
      WRITE (20, 100) rr(1,n2), rr(2,n2), uu(n2), n2
      WRITE (20, 100) rr(1,n3), rr(2,n3), uu(n3), n3
      WRITE (20, *)
   ENDDO
100 FORMAT(3(e11.5,3x),i5)

CLOSE(20)

END SUBROUTINE plot_pressure_label

SUBROUTINE plot_arrow_label(jj, rr, vv, file_name)
!---FORMAT PLTMTV

   IMPLICIT NONE

   CHARACTER(*) :: file_name
   INTEGER,      DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr, vv

   INTEGER :: n
   REAL(KIND=8) :: z_d, w_d

   OPEN (UNIT = 20, FILE = file_name, FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, 9000) SIZE(rr,2), SIZE(jj,1), SIZE(jj,2)
   9000 FORMAT('#',3i10)
   WRITE (20, *) '$ DATA = VECTOR'
   WRITE (20, *) '% axisscale = FALSE'
   WRITE (20, *) '% vscale = 1.'

!  WRITE (20, *) '% contstyle = 2'
!  WRITE (20, *) '% meshplot  = True'

   SELECT CASE(SIZE(rr,1))

      CASE(2)
         z_d = 0
         w_d = 0
         DO n = 1, SIZE(rr, 2)
            WRITE (20,100) rr(1,n), rr(2,n), z_d, vv(1,n), vv(2,n), w_d
         ENDDO

      CASE(3)
         DO n = 1, SIZE(rr, 2)
            WRITE (20,100) rr(:,n), vv(:,n)
         ENDDO

   END SELECT

   close(20)
100 FORMAT(6(e11.5,3x))

END SUBROUTINE plot_arrow_label

SUBROUTINE plot_pressure_P2_label(jj, rr, uu, file_name)
!---FORMAT PLTMTV

   IMPLICIT NONE

   INTEGER,      DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(INOUT) :: uu  ! rigid translation
   CHARACTER(*) :: file_name

   INTEGER :: m, n1, n2, n3

   OPEN (UNIT = 20, FILE = file_name, FORM = 'formatted', STATUS = 'unknown')

   WRITE (20, *) '$ DATA = CONTCURVE'
   WRITE (20, *) '% contstyle = 2'
   WRITE (20, *) '% nsteps = 50'
   WRITE (20,*) '% meshplot  = false'

   DO m = 1, SIZE(jj, 2)

     n1 = jj(1, m)
     n2 = jj(6, m)
     n3 = jj(5, m)
     WRITE (20, 100) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, 100) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, 100) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, 100)


     n1 = jj(2, m)
     n2 = jj(4, m)
     n3 = jj(6, m)
     WRITE (20, 100) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, 100) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, 100) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, 100)

     n1 = jj(4, m)
     n2 = jj(3, m)
     n3 = jj(5, m)
     WRITE (20, 100) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, 100) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, 100) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, 100)

     n1 = jj(5, m)
     n2 = jj(6, m)
     n3 = jj(4, m)
     WRITE (20, 100) rr(1,n1), rr(2,n1), uu(n1), n1
     WRITE (20, 100) rr(1,n2), rr(2,n2), uu(n2), n2
     WRITE (20, 100) rr(1,n3), rr(2,n3), uu(n3), n3
     WRITE (20, 100)
  ENDDO
100 FORMAT(3(e11.5,3x),i5)

CLOSE(20)

END SUBROUTINE plot_pressure_p2_label

!----------------------------------------------------------------

SUBROUTINE plot_ENSIGHT_vecteur(u8, vit)
!--FORMAT ENSIGHT
    
   IMPLICIT NONE
    
   CHARACTER(LEN=*),             INTENT(IN) :: vit 
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: u8
   CHARACTER(LEN=80)                        :: sketuve 
   INTEGER                                  :: i, j

   OPEN(UNIT=33,FILE=vit,STATUS='unknown',FORM='UNFORMATTED')

      sketuve = 'rien'
      WRITE(33) sketuve

      WRITE(33) ((REAL(u8(i,j),KIND=4),i=1,SIZE(u8,1)),j=1,SIZE(u8,2))
    
   CLOSE(33)

END SUBROUTINE plot_ENSIGHT_vecteur

!----------------------------------------------------------------

SUBROUTINE plot_ENSIGHT_scalaire(p8, pres)
!--FORMAT ENSIGHT
    
   IMPLICIT NONE
    
   CHARACTER(LEN=*),             INTENT(IN) :: pres 
   REAL(KIND=8), DIMENSION(:),   INTENT(IN) :: p8
   CHARACTER(LEN=80)                        :: sketuve 
   INTEGER                                  :: i

   OPEN(UNIT=44,FILE=pres,STATUS='unknown',FORM='UNFORMATTED')

      sketuve = 'rien'
      WRITE(44) sketuve

      WRITE(44) (REAL(p8(i),KIND =4),i=1,SIZE(p8))
    
   CLOSE(44)

END SUBROUTINE plot_ENSIGHT_scalaire

SUBROUTINE  plot_vorticity (jj, rr, zz, i)

   IMPLICIT NONE

   INTEGER,      DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(IN) :: zz
   INTEGER,                      INTENT(IN) :: i

   INTEGER :: m, n1, n2, n3

!   OPEN (UNIT = 20, FILE = 'vorticity', FORM = 'formatted', STATUS = 'unknown')

   WRITE (i, *) '$ DATA = CONTCURVE'
   WRITE (i, *) '% contstyle = 1'
   WRITE (i, *) '% nsteps = 40'

!   WRITE (i, *) '% contours = "(-6.0 -5.0 -4.0 -3.0 -2.0 -1.0)"'
!   WRITE (i, *) '% contours = "(-0.5  0.0  0.5)"'
!   WRITE (i, *) '% contours = "( 1.0  2.0  3.0  4.0  5.0  6.0)"'

   WRITE (i, *) '% meshplot  = False'

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (i, *) rr(1,n1), rr(2,n1), zz(n1), n1
      WRITE (i, *) rr(1,n2), rr(2,n2), zz(n2), n2
      WRITE (i, *) rr(1,n3), rr(2,n3), zz(n3), n3
      WRITE (i, *)
   ENDDO

   CLOSE (UNIT = i)

END SUBROUTINE  plot_vorticity

SUBROUTINE  plot_stream (jj, rr, pp, i)

   IMPLICIT NONE

   INTEGER,      DIMENSION(:,:), INTENT(IN) :: jj
   REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: rr
   REAL(KIND=8), DIMENSION(:),   INTENT(IN) :: pp
   INTEGER,                      INTENT(IN) :: i

   INTEGER :: m, n1, n2, n3

!   OPEN (UNIT = 20, FILE = 'stream', FORM = 'formatted', STATUS = 'unknown')

   WRITE (i, *) '$ DATA = CONTCURVE'
   WRITE (i, *) '% contstyle = 1'
   WRITE (i, *) '% nsteps = 30'

!   WRITE (i, *) '% contours = "(-1.e-10  -1.e-7  -1.e-5  -1.e-4 )"'
!   WRITE (i, *) '% contours = "(-0.01  -0.03  -0.05  -0.07  -0.09 )"'
!   WRITE (i, *) '% contours = "(-0.100  -0.110  -0.115  -0.1175 )"'
!   WRITE (i, *) '% contours = "(+1.0e-8 +1.0e-7 +1.0e-6 +1.0e-5 )"'
!   WRITE (i, *) '% contours = "(+5.0e-5 +1.0e-4 +2.5e-4 +5.0e-4 )"'
!   WRITE (i, *) '% contours = "(+1.0e-3 +1.5e-3 +3.0e-3 )"'
    WRITE (i, *) '% meshplot  = False'

   DO m = 1, SIZE(jj, 2)
      n1 = jj(1, m)
      n2 = jj(2, m)
      n3 = jj(3, m)
      WRITE (i, *) rr(1,n1), rr(2,n1), pp(n1), n1
      WRITE (i, *) rr(1,n2), rr(2,n2), pp(n2), n2
      WRITE (i, *) rr(1,n3), rr(2,n3), pp(n3), n3
      WRITE (i, *)
   ENDDO

   CLOSE (UNIT = i)

END SUBROUTINE  plot_stream

END MODULE sub_plot

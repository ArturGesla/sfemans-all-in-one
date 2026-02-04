  MODULE test_vks_tm87
! CN 02/2024
! USE user_data
  IMPLICIT NONE

  REAL(KIND=8), PARAMETER   :: pi = 3.14159265358979323846d0
  REAL(KIND=8), PARAMETER   :: twopi=2*pi
  !===TM87
  !some offset to begin at the same vertical axis as the bottom propeller
  REAL(KIND=8),  PARAMETER:: top_propeller_angle_offset =  0.d0
  INTEGER :: nblades = 8 !number of blades
  REAL(KIND=8),  PARAMETER:: lw = 0.0125 !lw= is the half thickness of the blades
  REAL(KIND=8),  PARAMETER:: disk_bot=-1.02d0, top_of_disk_bot=-0.9d0, top_of_blade_bot=-0.7d0
  REAL(KIND=8),  PARAMETER:: disk_top= 1.02d0, bot_of_disk_top= 0.9d0, bot_of_blade_top= 0.7d0
  REAL(KIND=8),  PARAMETER:: hole_radius=0.1d0
  REAL(KIND=8),  PARAMETER:: disk_r = 0.925d0
  REAL(KIND=8),  PARAMETER:: two_rp = disk_r/SIN(twopi*72.d0/360.d0)
!!!!!!!$  REAL(KIND=8),  PARAMETER:: disk_radius=0.925d0, hole_radius=0.1d0 
!!$  REAL(KIND=8),  PARAMETER:: two_rp = disk_radius/SIN(twopi*72.d0/360.d0)
!!!!!!!$  REAL(KIND=8),  PARAMETER:: omega_Vol=pi*2.48 !pi*r^2*h
  !===End TM87

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !===TM73
!!$  !Parameters for discontinuous blades
!!$  !some offset to begin at the same vertical axis as the bottom propeller
!!$  REAL(KIND=8),  PARAMETER:: top_propeller_angle_offset =  0.d0  
!!$  INTEGER :: nblades = 8 !number of blades
!!$  REAL(KIND=8),  PARAMETER::  lw = 0.0125 !lw= is the half thickness of the blades
!!$  REAL(KIND=8),  PARAMETER::  disk_bot=-1.1d0, top_of_disk_bot=-0.9d0, top_of_blade_bot=-0.7d0
!!$  REAL(KIND=8),  PARAMETER::  disk_top= 1.1d0, bot_of_disk_top= 0.9d0, bot_of_blade_top= 0.7d0
!!$  REAL(KIND=8),  PARAMETER::  hole_radius=0.1d0 
!!$! REAL(KIND=8),  PARAMETER, PUBLIC:: disk_radius=0.75d0, hole_radius=0.1d0 ! DCQ
!!$! !For straight blades use two_rp=0.d0
!!$! REAL(KIND=8),  PARAMETER::  two_rp = disk_radius/SIN(twopi*24.d0/360.d0)
!!$
!!$  !Parameters for smooth_blades
!!$  REAL(KIND=8),  PARAMETER::  wjump_hole = 0.06*(1.0),    wjump= 0.04*(1.0)
!!$  REAL(KIND=8),  PARAMETER::  hole_r = hole_radius,       hole_rp=hole_r - wjump_hole
!!$! REAL(KIND=8),  PARAMETER::  disk_r = disk_radius, disk_rp= disk_r- wjump ! DCQ
!!$! CN 02/2024 in read_user_data.F90 + data
!!$  REAL(KIND=8),  PARAMETER::  disk_r = 0.75d0, disk_rp= disk_r- wjump
!!$! CN 02/2024 in read_user_data.F90 + data
!!$! REAL(KIND=8),  PARAMETER::  disk_r = user%disk_radius, disk_rp= disk_r- wjump
!!$  REAL(KIND=8),  PARAMETER::  two_rp = disk_r/SIN(twopi*24.d0/360.d0)
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  !Bottom Smooth_propeller
!!$  REAL(KIND=8),  PARAMETER::  cyl_bott = -1.0
!!$  REAL(KIND=8),  PARAMETER::  Bdisk_z =  cyl_bott  + 0.3d0,          bdisk_z_p= Bdisk_z  - wjump
!!$  REAL(KIND=8),  PARAMETER::  zbot =     cyl_bott  + 0.1d0,          zbot_p = zbot - wjump
!!$  REAL(KIND=8),  PARAMETER::  zbot_bar =     zbot  - 0.04d0,         zbot_bar_p = zbot_bar - wjump
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  !Top Smooth_propeller
!!$  REAL(KIND=8),  PARAMETER::  cyl_top = 1.0
!!$  REAL(KIND=8),  PARAMETER::  Tdisk_z = cyl_top - 0.3d0,           Tdisk_z_p= Tdisk_z  + wjump
!!$  REAL(KIND=8),  PARAMETER::  ztop =    cyl_top - 0.1d0,           ztop_p = ztop + wjump
!!$  REAL(KIND=8),  PARAMETER::  ztop_bar =  ztop  +  0.04d0,         ztop_bar_p = ztop_bar + wjump
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  !Parameters for  both smooth_blades
!!$  REAL(KIND=8),  PARAMETER::   alpha=200.d0*(1.0), alpha_th = 80.d0*(1.0)
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !===END TM73
  !Volume of the cylinder (all domain)
!!! REAL(KIND=8)            :: omega_Vol= pi*(1.0)*(2.0) or pi*2.48 !pi*r^2*h in read_user_data.F90 user%omega_Vol
!!! REAL(KIND=8), PUBLIC    :: omega_Vol= pi*(1.0)*(2.0) !=pi*2.48 !pi*r^2*h


  !Do we want both propellers? 
  LOGICAL,PARAMETER::  if_bottom_prop=.TRUE.
  LOGICAL,PARAMETER::  if_top_prop=.TRUE.
CONTAINS
  !===============================================================================
  !                       Boundary conditions for Navier-Stokes
  !===============================================================================
  FUNCTION mask_petit_cylindre(rr) RESULT(vv)
    USE user_data
    IMPLICIT NONE 
    REAL(KIND=8), DIMENSION(:,:),        INTENT(IN)   :: rr
    REAL(KIND=8), DIMENSION(SIZE(rr,2),2)             :: vv
    REAL(KIND=8)                                      :: r, z
    INTEGER :: n 

    vv=0.d0
    DO n = 1, SIZE(rr,2)
       r= rr(1,n)
       z= rr(2,n)
       IF ((z.GE.-0.1d0).AND.(z.LE.0.1d0).AND.(r.LE.0.1d0)) THEN
          vv(n,1) =  1.d0
       ENDIF
    END DO
    RETURN
  END FUNCTION mask_petit_cylindre

  FUNCTION sharp_penal_in_real_space(mesh,rr_gauss,angles,nb_angles,nb,ne,time) RESULT(vv)
    USE def_type_mesh
    IMPLICIT NONE
    TYPE(mesh_type)                            :: mesh
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN)   :: rr_gauss
    REAL(KIND=8), DIMENSION(:), INTENT(IN)     :: angles
    INTEGER,                    INTENT(IN)     :: nb_angles
    INTEGER,                    INTENT(IN)     :: nb, ne
    REAL(KIND=8),               INTENT(IN)     :: time
    REAL(KIND=8), DIMENSION(nb_angles,ne-nb+1) :: vv
    INTEGER                                    :: n, n_loc
    REAL(KIND=8)                               :: r,z

    DO n = nb, ne
       n_loc = n - nb + 1
       r = rr_gauss(1,n_loc)
       z = rr_gauss(2,n_loc)
       IF( if_bottom_prop .AND.  if_top_prop) THEN
          vv(:,n_loc) =   top_propeller(r,z,angles, nb_angles,time)&
               *bottom_propeller(r,z,angles, nb_angles,time)
       ELSE  IF (if_bottom_prop) THEN
          vv(:,n_loc) =   bottom_propeller(r,z,angles, nb_angles,time)
       ELSE
          vv(:,n_loc) =   top_propeller(r,z,angles, nb_angles,time)
       END IF
    END DO
    RETURN
  END FUNCTION sharp_penal_in_real_space

  !=== 1 - Characteristic Function of  top_propeller
  !===This coefficient is equal to 0 in top propeller and 1 elsewhere
  FUNCTION top_propeller(r,z,angles,nb_angles,time) RESULT(vv)
    USE user_data
    IMPLICIT NONE
    REAL(KIND=8)                               :: r, theta, z, time, a, r_theta
    REAL(KIND=8), DIMENSION(:)                 :: angles
    INTEGER                                    :: nb_angles
    REAL(KIND=8), DIMENSION(nb_angles)         :: vv
    INTEGER                                    :: na

    IF (z.LE.disk_top .AND. z.GE.bot_of_disk_top .AND. r.LE.user%disk_radius) THEN
       vv = 0.d0
       RETURN
    END IF

    IF (z.LE.bot_of_disk_top .AND. z.GE.bot_of_blade_top .AND. r.LE.user%disk_radius .AND. r.GE.hole_radius) THEN
       r_theta = ASIN(r/two_rp)
       DO na = 1, nb_angles   
          theta=   angles(na) + user%solid_vel(2)*time
          theta=   theta -   top_propeller_angle_offset 
          a = theta - r_theta - FLOOR(( theta - r_theta)/(twopi/nblades))*twopi/nblades &
               - twopi/(2*nblades)
          IF ((r*a+lw+lw*r).GE. 0.d0 .AND. (r*a-lw-lw*r) .LE. 0.d0) THEN
             vv(na) =0.d0
          ELSE
             vv(na) =1.d0
          END IF
       END DO
    ELSE
       vv = 1.d0
    END IF
    RETURN
  END FUNCTION top_propeller

  !=== 1 - Characteristic Function of  bottom_propeller
  !===This coefficient is equal to 0 in bot propeller and 1 elsewhere
  FUNCTION bottom_propeller(r,z,angles,nb_angles,time) RESULT(vv)
    USE user_data
    IMPLICIT NONE
    REAL(KIND=8)                               :: r,theta,z,time,a,r_theta 
    REAL(KIND=8), DIMENSION(:)                 :: angles
    INTEGER                                    :: nb_angles
    REAL(KIND=8), DIMENSION(nb_angles)         :: vv
    INTEGER                                    :: na

    IF (z.GE.disk_bot .AND. z.LE.top_of_disk_bot .AND. r.LE.user%disk_radius) THEN
       vv = 0.d0
       RETURN
    END IF

    IF (z.GE.top_of_disk_bot .AND. z.LE.top_of_blade_bot .AND. r.LE.user%disk_radius .AND. r.GE.hole_radius) THEN
       r_theta = ASIN(r/two_rp)
       DO na = 1, nb_angles   
          theta=   angles(na) - user%solid_vel(1)*time
          a = theta + r_theta - FLOOR(( theta +  r_theta)/(twopi/nblades))*twopi/nblades &
               - twopi/(2*nblades)
          IF ((r*a+lw+lw*r).GE. 0.d0 .AND. (r*a-lw-lw*r).LE.0.d0) THEN
             vv(na) = 0.d0
          ELSE
             vv(na) = 1.d0
          END IF
       END DO
    ELSE
       vv = 1.d0
    END IF
    RETURN
  END FUNCTION bottom_propeller

  !===============================================================================
  !                       Boundary conditions for Maxwell
  !===============================================================================

  FUNCTION Hexact_init(TYPE, rr, m, mu_H_field, t) RESULT(vv) 
    IMPLICIT NONE
    INTEGER     ,                        INTENT(IN)   :: TYPE
    REAL(KIND=8), DIMENSION(:,:),        INTENT(IN)   :: rr
    INTEGER     ,                        INTENT(IN)   :: m  !mode 
    REAL(KIND=8),                        INTENT(IN)   :: t 
    REAL(KIND=8), DIMENSION(:),          INTENT(IN)   :: mu_H_field
    REAL(KIND=8), DIMENSION(SIZE(rr,2))               :: vv

    vv = 0.d0
    !===Constant vertical field Hz
    IF (m == 0) THEN
       IF (TYPE == 5) THEN
          vv = 1.d-6
       ENDIF
    ENDIF
    !===Constant horizontal field Hx
    IF (m == 1) THEN
       IF (TYPE == 1) THEN
          vv = 1.d-6
       ELSEIF (TYPE == 4) THEN
          vv = -1.d-6
       ENDIF
    ENDIF
    RETURN
  END FUNCTION Hexact_init

!!$ ! TM73 smooth
!!$  FUNCTION mu_bar_func(r,z) RESULT(vv)
!!$     USE def_type_mesh
!!$     USE input_data
!!$     USE user_data
!!$     USE my_util
!!$   
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               ::  r,z,vv
!!$    REAL(KIND=8)                               :: psi
!!$ 
!!$ 
!!$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !mu bar for Blades , a disks with no hole
!!$ 
!!$     if( if_bottom_prop .AND.  if_top_prop) then
!!$           vv=(top_mu_bar_func(r,z) + bottom_mu_bar_func(r,z))*(user%mu_disk-1.0) + 1.0
!!$        else  if (if_bottom_prop) then
!!$           vv= bottom_mu_bar_func(r,z)*(user%mu_disk-1.0) + 1.0
!!$        else
!!$           vv= top_mu_bar_func(r,z)*(user%mu_disk-1.0) + 1.0
!!$        end if
!!$    RETURN
!!$    
!!$  END FUNCTION mu_bar_func
!!$ 
!!$ 
!!$ 
!!$  FUNCTION bottom_mu_bar_func(r,z) RESULT(vv)
!!$     USE def_type_mesh
!!$     USE input_data
!!$     USE my_util
!!$   
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               ::  r,z,vv
!!$    REAL(KIND=8)                               :: r2,r3,z0,z1
!!$    REAL(KIND=8)                               :: psi
!!$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !mu bar for Blades , a disks with no hole
!!$    r2=disk_rp
!!$    r3=disk_r
!!$ 
!!$    z0=zbot_bar_p
!!$    z1=zbot_bar
!!$    
!!$    psi=0.d0
!!$    IF ( z .GE.  z1  .AND. r .GE. r3 )  THEN
!!$          psi = 0.d0 ;          
!!$ 
!!$    ELSE IF (r.LE.r2) THEN
!!$ 
!!$         IF(z.LE.z0) THEN
!!$            psi=1.0;
!!$         ELSE IF (z.GE.z1) THEN
!!$            psi=0.0;
!!$         ELSE
!!$            psi=smooth_jump_down(z,z0,z1);
!!$ 
!!$         END IF
!!$ 
!!$    ELSE IF (r.GE.r2 .AND. r.LE.r3) THEN
!!$ 
!!$         IF(z.GE.z1) THEN
!!$            psi=0.0;
!!$         ELSE IF(z.LE.z0) THEN
!!$            psi=smooth_jump_down(r,r2,r3);
!!$         ELSE
!!$            psi=smooth_jump_down(r,r2,r3)*smooth_jump_down(z,z0,z1);
!!$         END IF
!!$ 
!!$    END IF
!!$ 
!!$    vv = psi
!!$ 
!!$    RETURN
!!$    
!!$  END FUNCTION bottom_mu_bar_func
!!$ 
!!$  FUNCTION top_mu_bar_func(r,z) RESULT(vv)
!!$     USE def_type_mesh
!!$     USE input_data
!!$     USE my_util
!!$   
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               ::  r,z,vv,psi
!!$    REAL(KIND=8)                               :: r2,r3,z2,z3
!!$    
!!$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !mu bar for Blades , a disks with no hole
!!$     r2=disk_rp
!!$     r3=disk_r
!!$ 
!!$     z2=ztop_bar_p
!!$     z3=ztop_bar
!!$ 
!!$ 
!!$     psi=0.d0
!!$     IF ( z .LE.  z3 .AND. r .GE. r3)  THEN
!!$        psi = 0.d0 ;             
!!$ 
!!$     ELSE IF(r.LE.r2) THEN
!!$        IF(z.GE.z2) THEN
!!$           psi=1.0;
!!$        ELSE IF (z.LE.z3) THEN
!!$           psi=0.0;
!!$        ELSE
!!$           psi=smooth_jump_up(z,z3,z2);
!!$        END IF
!!$        
!!$     ELSE IF (r.GE.r2 .AND. r.LE.r3) THEN
!!$        IF(z.LE.z3) THEN
!!$           psi=0.0;
!!$        ELSE IF(z.GE.z2) THEN
!!$           psi=smooth_jump_down(r,r2,r3);
!!$        ELSE
!!$           psi=smooth_jump_down(r,r2,r3)*smooth_jump_up(z,z3,z2);
!!$        END IF
!!$        
!!$     END IF
!!$ 
!!$     vv=psi
!!$ 
!!$ 
!!$    RETURN
!!$    
!!$  END FUNCTION top_mu_bar_func
!!$ 
!!$ 
!!$  FUNCTION grad_mu_bar_func(r,z) RESULT(vv)
!!$     USE def_type_mesh
!!$     USE input_data
!!$     USE user_data
!!$     USE my_util
!!$   
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               ::  r,z
!!$    REAL(KIND=8),DIMENSION(2)                  ::  vv
!!$ 
!!$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !mu bar for Blades , a disks with no hole
!!$ 
!!$     if( if_bottom_prop .AND.  if_top_prop) then
!!$           vv=( grad_top_mu_bar_func(r,z) + grad_bottom_mu_bar_func(r,z) )*(user%mu_disk-1.0) 
!!$        else  if (if_bottom_prop) then
!!$           vv= grad_bottom_mu_bar_func(r,z)*(user%mu_disk-1.0) 
!!$        else
!!$           vv= grad_top_mu_bar_func(r,z)*(user%mu_disk-1.0) 
!!$        end if
!!$ 
!!$    RETURN
!!$    
!!$  END FUNCTION grad_mu_bar_func
!!$ 
!!$ 
!!$  FUNCTION grad_bottom_mu_bar_func(r,z) RESULT(vv)
!!$     USE def_type_mesh
!!$     USE input_data
!!$     USE my_util
!!$   
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               ::  r,z
!!$    REAL(KIND=8),DIMENSION(2)                  ::  vv
!!$ 
!!$    REAL(KIND=8)                               :: r2,r3,z0,z1
!!$    REAL(KIND=8)                               :: DFr,DFz
!!$ 
!!$ 
!!$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !mu bar for Blades , a disks with no hole
!!$    r2=disk_rp
!!$    r3=disk_r
!!$ 
!!$    z0=zbot_bar_p
!!$    z1=zbot_bar
!!$    
!!$    DFr=0.d0
!!$    DFz=0.d0
!!$ 
!!$    IF ( z .GE.  z1  .AND. r .GE. r3 )  THEN
!!$           DFr=0.d0
!!$           DFz=0.d0
!!$         
!!$    ELSE IF (r.LE.r2) THEN
!!$ 
!!$         IF(z.LE.z0) THEN
!!$             DFr=0.d0
!!$             DFz=0.d0
!!$         ELSE IF (z.GE.z1) THEN
!!$            DFr=0.d0
!!$            DFz=0.d0
!!$ 
!!$         ELSE
!!$            DFr=0.d0
!!$            DFz=Dsmooth_jump_down(z,z0,z1);
!!$ 
!!$         END IF
!!$ 
!!$    ELSE IF (r.GE.r2 .AND. r.LE.r3) THEN
!!$ 
!!$         IF(z.GE.z1) THEN
!!$            DFr=0.d0
!!$            DFz=0.d0
!!$         ELSE IF(z.LE.z0) THEN
!!$            DFr=Dsmooth_jump_down(r,r2,r3);
!!$            DFz=0.d0
!!$         ELSE
!!$            DFr=Dsmooth_jump_down(r,r2,r3)*smooth_jump_down(z,z0,z1);
!!$            DFz=smooth_jump_down(r,r2,r3)*Dsmooth_jump_down(z,z0,z1);
!!$         END IF
!!$ 
!!$    END IF
!!$ 
!!$    vv(1)=DFr
!!$    vv(2)=DFz
!!$       
!!$    RETURN
!!$    
!!$  END FUNCTION grad_bottom_mu_bar_func
!!$ 
!!$ 
!!$ 
!!$  FUNCTION grad_top_mu_bar_func(r,z) RESULT(vv)
!!$     USE def_type_mesh
!!$     USE input_data
!!$     USE my_util
!!$   
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               :: r,z
!!$    REAL(KIND=8),DIMENSION(2)                  :: vv
!!$    REAL(KIND=8)                               :: r2,r3,z2,z3
!!$    REAL(KIND=8)                               :: DFr,DFz
!!$ 
!!$   
!!$ 
!!$ 
!!$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !mu bar for Blades , a disks with no hole
!!$     r2=disk_rp
!!$     r3=disk_r
!!$ 
!!$     z2=ztop_bar_p
!!$     z3=ztop_bar
!!$ 
!!$ 
!!$     DFr=0.d0
!!$     DFz=0.d0
!!$     IF ( z .LE.  z3 .AND. r .GE. r3)  THEN
!!$           DFr=0.d0
!!$           DFz=0.d0
!!$ 
!!$     ELSE IF(r.LE.r2) THEN
!!$        IF(z.GE.z2) THEN
!!$           DFr=0.d0
!!$           DFz=0.d0
!!$        ELSE IF (z.LE.z3) THEN
!!$           DFr=0.d0
!!$           DFz=0.d0         
!!$        ELSE
!!$           DFr=0.d0
!!$           DFz=Dsmooth_jump_up(z,z3,z2);
!!$        END IF
!!$        
!!$     ELSE IF (r.GE.r2 .AND. r.LE.r3) THEN
!!$        IF(z.LE.z3) THEN
!!$           DFr=0.d0
!!$           DFz=0.d0         
!!$        ELSE IF(z.GE.z2) THEN
!!$           DFr=Dsmooth_jump_down(r,r2,r3);
!!$           DFz=0.d0         
!!$        ELSE
!!$           DFr=Dsmooth_jump_down(r,r2,r3)*smooth_jump_up(z,z3,z2);
!!$           DFz=smooth_jump_down(r,r2,r3)*Dsmooth_jump_up(z,z3,z2);
!!$        END IF
!!$        
!!$     END IF
!!$ 
!!$ 
!!$     vv(1)=DFr
!!$     vv(2)=DFz
!!$   
!!$    RETURN
!!$    
!!$  END FUNCTION grad_top_mu_bar_func
!!$
!!$
!!$  !This is 1 in the fluid, and  0 in the solid
!!$  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$  FUNCTION smooth_penal_in_real_space(mesh,rr_gauss,angles,nb_angles,nb,ne,time) RESULT(vv)
!!$    USE def_type_mesh
!!$    USE input_data
!!$    USE user_data
!!$    USE my_util
!!$    IMPLICIT NONE
!!$    TYPE(mesh_type)                            :: mesh
!!$    REAL(KIND=8), DIMENSION(:,:)               :: rr_gauss
!!$    REAL(KIND=8), DIMENSION(:)                 :: angles
!!$    INTEGER                                    :: nb_angles
!!$    INTEGER                                    :: nb, ne
!!$    REAL(KIND=8), DIMENSION(nb_angles,ne-nb+1) :: vv
!!$    INTEGER, DIMENSION(mesh%np)                :: id
!!$    INTEGER                                    :: n, n_loc
!!$    REAL(KIND=8)                               :: r,z,time
!!$
!!$
!!$    DO n = nb, ne
!!$       n_loc = n - nb + 1
!!$
!!$       r = rr_gauss(1,n_loc)
!!$       z = rr_gauss(2,n_loc)
!!$
!!$
!!$       ! DCQ   14/08/2014
!!$       if( if_bottom_prop .AND.  if_top_prop) then
!!$          vv(:,n_loc) =   smooth_top_propeller(r,z,angles, nb_angles,time)&
!!$               *smooth_bottom_propeller(r,z,angles, nb_angles,time)
!!$       else  if (if_bottom_prop) then
!!$          vv(:,n_loc) =   smooth_bottom_propeller(r,z,angles, nb_angles,time)
!!$       else
!!$          vv(:,n_loc) =   smooth_top_propeller(r,z,angles, nb_angles,time)
!!$       end if
!!$                         
!!$       END DO
!!$    RETURN
!!$  END FUNCTION smooth_penal_in_real_space
!!$
!!$
!!$
!!$  ! DCQ   14/08/2014
!!$  ! 1 - Characteristic Function of  top_propeller
!!$  FUNCTION smooth_top_propeller(r,z,angles,nb_angles,time) RESULT(vv)
!!$    USE def_type_mesh
!!$    USE input_data
!!$    USE user_data
!!$    USE my_util
!!$    IMPLICIT NONE
!!$
!!$    REAL(KIND=8)                               :: r,theta,z,time
!!$    REAL(KIND=8), DIMENSION(:)                 :: angles
!!$    INTEGER                                    :: nb_angles
!!$    REAL(KIND=8), DIMENSION(nb_angles)         :: vv
!!$
!!$ 
!!$    INTEGER                                    :: na
!!$    REAL(KIND=8)                               :: F, g, a,alphaz,alphar
!!$    REAL(KIND=8)                               :: r0,r1,r2,r3,z0,z1,z2,z3
!!$    REAL(KIND=8)                               :: curve_1,curve_2,psi, tanhp,tanhm, tanhd, r_theta
!!$
!!$
!!$    !Get characteristic function of the supporting disk-cylinder
!!$    psi=smooth_top_supporting_disk(r,z)
!!$
!!$    !If we are outside of the supporting disk (respect to r)
!!$    IF(ABS(psi) .LE. 1.d-8) THEN
!!$         DO na = 1, nb_angles   
!!$            vv(na) = 1-psi
!!$         END DO
!!$         RETURN
!!$    END IF
!!$    
!!$    !Do Blades stuff     
!!$    r0=hole_rp
!!$    r1=hole_r
!!$    r2=disk_rp
!!$    r3=disk_r
!!$      
!!$    z0=ztop_p - wjump 
!!$    z1=ztop   - wjump
!!$    z2=Tdisk_z_p
!!$    z3=Tdisk_z
!!$
!!$     
!!$
!!$    ! Parabolic jump
!!$    IF ( z .LE. z1 )  THEN
!!$       alphaz = 1.d0;          
!!$    ELSE IF(z .LE. z0 .AND. z .GE. z1) THEN
!!$       alphaz= smooth_jump_down(z,z1,z0);
!!$    ELSE IF(z .GE. z0) THEN
!!$       alphaz=0.0;
!!$    END IF
!!$    
!!$    If ( r .LE. r0 )  THEN 
!!$       alphar = 0.d0;          
!!$    ELSE IF(r .GE. r0 .AND. r .LE. r1) THEN
!!$       alphar= smooth_jump_up(r,r0,r1);
!!$    ELSE
!!$       alphar=1.0;
!!$    END IF
!!$    alphaz= alpha_th*alphaz*alphar
!!$
!!$
!!$    r_theta = ASIN(r/two_rp)
!!$    
!!$       DO na = 1, nb_angles   
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$          !DCQ go backwards and do the test 
!!$          !These blades  rotate the other way (-lbd), (+user%solid_vel*time) and they begin at the same angle,
!!$          
!!$          theta=   angles(na) + user%solid_vel(2)*time
!!$          theta=   theta -   top_propeller_angle_offset !some offset to begin at the same vertical axis as the bottom propeller
!!$          
!!$          !a = theta + (-lbd*r) - FLOOR(( theta + (-lbd)*r)/(twopi/nblades))*twopi/nblades &
!!$          !     - twopi/(2*nblades)
!!$
!!$          !JL-CN 01/2015
!!$          a = theta - r_theta - FLOOR(( theta - r_theta)/(twopi/nblades))*twopi/nblades &
!!$            - twopi/(2*nblades)
!!$          
!!$          tanhp = tanh(alphaz*(r*a+lw+lw*r))
!!$          tanhm = tanh(alphaz*(r*a-lw-lw*r))
!!$          tanhd = tanh(alphaz*(lw+lw*r))
!!$          g=(1+tanhp)*(1-tanhm)/(1+tanhd)**2 
!!$          vv(na) = 1-g*psi 
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$
!!$
!!$       END DO
!!$       
!!$       RETURN
!!$  END FUNCTION smooth_top_propeller
!!$
!!$
!!$  ! DCQ   14/08/2014
!!$  ! 1 - Characteristic Function of  bottom_propeller
!!$  FUNCTION smooth_bottom_propeller(r,z,angles,nb_angles,time) RESULT(vv)
!!$    USE def_type_mesh
!!$    USE input_data
!!$    USE user_data
!!$    USE my_util
!!$    IMPLICIT NONE
!!$
!!$    REAL(KIND=8)                               :: r,theta,z,time
!!$    REAL(KIND=8), DIMENSION(:)                 :: angles
!!$    INTEGER                                    :: nb_angles
!!$    REAL(KIND=8), DIMENSION(nb_angles)         :: vv
!!$
!!$
!!$    INTEGER                                    :: na
!!$    REAL(KIND=8)                               :: F, g, a,alphaz,alphar
!!$    REAL(KIND=8)                               :: r0,r1,r2,r3,z0,z1,z2,z3
!!$    REAL(KIND=8)                               :: curve_1,curve_2,psi, tanhp,tanhm, tanhd, r_theta 
!!$
!!$
!!$
!!$    !Supporting disk stuff
!!$    !Get characteristic function of the supporting disk-cylinder
!!$    psi=smooth_bottom_supporting_disk(r,z)
!!$
!!$    !If we are outside of the supporting disk (respect to r)
!!$    IF(ABS(psi) .LE. 1.d-8) THEN
!!$         DO na = 1, nb_angles   
!!$            vv(na) = 1-psi
!!$         END DO
!!$         RETURN
!!$    END IF
!!$    
!!$
!!$     
!!$    ! Do blades stuff     
!!$    !! Blades with no hole in the disk
!!$    r0=hole_rp
!!$    r1=hole_r
!!$    r2=disk_rp
!!$    r3=disk_r
!!$
!!$    z0=zbot_p + wjump 
!!$    z1=zbot   + wjump
!!$    z2=Bdisk_z_p
!!$    z3=Bdisk_z
!!$
!!$
!!$
!!$    ! Parabolic jump
!!$    IF ( z .LE. z0 )  THEN
!!$       alphaz = 0.d0;          
!!$    ELSE IF(z .GE. z0 .AND. z .LE. z1) THEN
!!$       alphaz= smooth_jump_up(z,z0,z1);
!!$    ELSE IF(z .GE. z1) THEN
!!$       alphaz=1.0;
!!$    END IF
!!$
!!$
!!$    If ( r .LE. r0 )  THEN 
!!$       alphar = 0.d0;          
!!$    ELSE IF(r .GE. r0 .AND. r .LE. r1) THEN
!!$       alphar= smooth_jump_up(r,r0,r1);
!!$    ELSE
!!$       alphar=1.0;
!!$    END IF
!!$    alphaz= alpha_th*alphaz*alphar
!!$
!!$
!!$
!!$    r_theta = ASIN(r/two_rp)
!!$
!!$    DO na = 1, nb_angles   
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$       !DCQ go backwards and do the test 
!!$       theta=   angles(na) - user%solid_vel(1)*time
!!$       !a = theta + lbd*r - FLOOR(( theta +  lbd*r)/(twopi/nblades))*twopi/nblades &
!!$       !     - twopi/(2*nblades)
!!$
!!$       !JL-CN 01/2015
!!$       a = theta + r_theta - FLOOR(( theta +  r_theta)/(twopi/nblades))*twopi/nblades &
!!$            - twopi/(2*nblades)
!!$
!!$       tanhp = tanh(alphaz*(r*a+lw+lw*r))
!!$       tanhm = tanh(alphaz*(r*a-lw-lw*r))
!!$       tanhd = tanh(alphaz*(lw+lw*r))
!!$       g=(1+tanhp)*(1-tanhm)/(1+tanhd)**2 
!!$       vv(na) = 1-g*psi 
!!$
!!$
!!$
!!$    END DO
!!$
!!$    RETURN
!!$  END FUNCTION smooth_bottom_propeller
!!$
!!$
!!$!Characteristic Function of  top_supporting_disk
!!$  FUNCTION smooth_top_supporting_disk(r,z) RESULT(vv)
!!$    USE def_type_mesh
!!$    USE input_data
!!$    USE user_data
!!$    USE my_util
!!$    IMPLICIT NONE
!!$
!!$    REAL(KIND=8)                               :: r,z
!!$    REAL(KIND=8)                                :: vv
!!$    REAL(KIND=8)                               :: F, g, a,alphaz,alphar
!!$    REAL(KIND=8)                               :: r0,r1,r2,r3,z0,z1,z2,z3
!!$    REAL(KIND=8)                               :: curve_1,curve_2,psi
!!$
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$    !! Blades with no hole in the disk
!!$    !Supporting disk stuff
!!$    r0=hole_rp
!!$    r1=hole_r
!!$    r2=disk_rp
!!$    r3=disk_r
!!$
!!$
!!$    z0=ztop_p - wjump 
!!$    z1=ztop   - wjump
!!$    z2=Tdisk_z_p
!!$    z3=Tdisk_z
!!$
!!$
!!$    psi=0.d0
!!$    IF ( z .LE.  z3 .AND. r .GE. r3)  THEN
!!$       psi = 0.d0 ;             
!!$
!!$    ELSE IF (r.LE.r0) THEN
!!$       IF(z.GE.z0) THEN
!!$          psi=1.0;
!!$       ELSE IF (z.LE.z1) THEN
!!$          psi=0.0;
!!$       ELSE
!!$          psi=smooth_jump_up(z,z1,z0);
!!$       END IF
!!$
!!$    ELSE IF(r.GE.r0 .AND. r.LE.r1) THEN
!!$
!!$       curve_2= smooth_jump_up(r,r0,r1)*(z3-z1)+z1;
!!$       curve_1= smooth_jump_up(r,r0,r1)*(z2-z0)+z0;
!!$
!!$       IF(z.LE.curve_2) THEN 
!!$          psi=0.0;
!!$       ELSE IF(z.GE.curve_1) THEN
!!$          psi=1.0;
!!$       ELSE
!!$          psi = 1 - smooth_jump_up(z ,curve_1,curve_2);
!!$       END IF
!!$
!!$    ELSE IF(r.GE.r1 .AND. r.LE.r2) THEN
!!$       IF(z.GE.z2) THEN
!!$          psi=1.0;
!!$       ELSE IF (z.LE.z3) THEN
!!$          psi=0.0;
!!$       ELSE
!!$          psi=smooth_jump_up(z,z3,z2);
!!$       END IF
!!$
!!$    ELSE IF (r.GE.r2 .AND. r.LE.r3) THEN
!!$       IF(z.LE.z3) THEN
!!$          psi=0.0;
!!$       ELSE IF(z.GE.z2) THEN
!!$          psi=smooth_jump_down(r,r2,r3);
!!$       ELSE
!!$          psi=smooth_jump_down(r,r2,r3)*smooth_jump_up(z,z3,z2);
!!$       END IF
!!$
!!$    END IF
!!$
!!$    vv=psi
!!$
!!$  END FUNCTION smooth_top_supporting_disk
!!$
!!$!Characteristic Function of  bot_supporting_disk
!!$  FUNCTION smooth_bottom_supporting_disk(r,z) RESULT(vv)
!!$  USE def_type_mesh
!!$  USE input_data
!!$  USE user_data
!!$  USE my_util
!!$  IMPLICIT NONE
!!$
!!$  REAL(KIND=8)                               :: r,z
!!$  REAL(KIND=8)                                :: vv
!!$  REAL(KIND=8)                               :: F, g, a,alphaz,alphar
!!$  REAL(KIND=8)                               :: r0,r1,r2,r3,z0,z1,z2,z3
!!$  REAL(KIND=8)                               :: curve_1,curve_2,psi
!!$
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!$       !! Blades with no hole in the disk
!!$       !Supporting disk stuff
!!$
!!$  r0=hole_rp
!!$  r1=hole_r
!!$  r2=disk_rp
!!$  r3=disk_r
!!$
!!$  z0=zbot_p + wjump 
!!$  z1=zbot   + wjump
!!$  z2=Bdisk_z_p
!!$  z3=Bdisk_z
!!$
!!$  psi=0.d0
!!$  IF ( z .GE.  z3 .AND. r .GE. r3)  THEN
!!$     psi = 0.d0 ;             
!!$
!!$  ELSE IF (r.LE.r0) THEN
!!$     IF(z.LE.z0) THEN
!!$        psi=1.0;
!!$     ELSE IF (z.GE.z1) THEN
!!$        psi=0.0;
!!$     ELSE
!!$        psi=smooth_jump_up(z,z1,z0);
!!$     END IF
!!$
!!$  ELSE IF(r.GE.r0 .AND. r.LE.r1) THEN
!!$
!!$     curve_2= smooth_jump_up(r,r0,r1)*(z3-z1)+z1;
!!$     curve_1= smooth_jump_up(r,r0,r1)*(z2-z0)+z0;
!!$
!!$     IF(z.GE.curve_2) THEN 
!!$        psi=0.0;
!!$     ELSE IF(z.LE.curve_1) THEN
!!$        psi=1.0;
!!$     ELSE
!!$        psi = 1 - smooth_jump_up(z ,curve_1,curve_2);
!!$     END IF
!!$
!!$  ELSE IF(r.GE.r1 .AND. r.LE.r2) THEN
!!$     IF(z.LE.z2) THEN
!!$        psi=1.0;
!!$     ELSE IF (z.GE.z3) THEN
!!$        psi=0.0;
!!$     ELSE
!!$        psi=smooth_jump_up(z,z3,z2);
!!$     END IF
!!$
!!$  ELSE IF (r.GE.r2 .AND. r.LE.r3) THEN
!!$     IF(z.GE.z3) THEN
!!$        psi=0.0;
!!$     ELSE IF(z.LE.z2) THEN
!!$        psi=smooth_jump_down(r,r2,r3);
!!$     ELSE
!!$        psi=smooth_jump_down(r,r2,r3)*smooth_jump_up(z,z3,z2);
!!$     END IF
!!$
!!$  END IF
!!$
!!$  vv=psi
!!$
!!$  END FUNCTION smooth_bottom_supporting_disk
!!$
!!$
!!$
!!$  !A cubic profile, which is 1 at x0 and 0 at x1
!!$  FUNCTION smooth_jump_down(x,x0,x1) RESULT(vv)
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               :: x,x0,x1
!!$    REAL(KIND=8)                               :: vv
!!$    REAL(KIND=8)                               :: a
!!$    REAL(KIND=8)                               :: a0,a1,a2,a3
!!$
!!$    !Cubic
!!$    a0 = x1**2*(3*x0-x1)/(x0-x1)**3; 
!!$    a1 = -6.0*x0*x1/(x0-x1)**3; 
!!$    a2 = (3.0*(x0+x1))/(x0-x1)**3;
!!$    a3 = -2.0/(x0-x1)**3;
!!$    vv = a0+a1*x+a2*x*x + a3*x*x*x
!!$    RETURN
!!$  END FUNCTION smooth_jump_down
!!$
!!$  !A cubic profile, which is 0 at x0 and 1 at x1
!!$  FUNCTION smooth_jump_up(x,x0,x1) RESULT(vv)
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               :: x,x0,x1
!!$    REAL(KIND=8)                               :: vv
!!$    vv = 1.d0 - smooth_jump_down( x,x0,x1 );
!!$    RETURN
!!$  END FUNCTION smooth_jump_up
!!$
!!$
!!$ !derivative with respect to x
!!$  FUNCTION Dsmooth_jump_down(x,x0,x1) RESULT(vv)
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               :: x,x0,x1
!!$    REAL(KIND=8)                               :: vv
!!$    REAL(KIND=8)                               :: a
!!$    REAL(KIND=8)                               :: a0,a1,a2,a3
!!$
!!$    !Cubic Factorized
!!$    a0 = x1**2*(3*x0-x1)/(x0-x1)**3; 
!!$    a1 = -6.0*x0*x1/(x0-x1)**3; 
!!$    a2 = (3.0*(x0+x1))/(x0-x1)**3;
!!$    a3 = -2.0/(x0-x1)**3;
!!$
!!$
!!$    vv = a1+2.d0*a2*x + 3.d0*a3*x*x 
!!$
!!$    RETURN
!!$  END FUNCTION Dsmooth_jump_down
!!$
!!$
!!$ !derivative with respect to x
!!$  FUNCTION Dsmooth_jump_up(x,x0,x1) RESULT(vv)
!!$    IMPLICIT NONE
!!$    REAL(KIND=8)                               :: x,x0,x1
!!$    REAL(KIND=8)                               :: vv
!!$    vv =  - Dsmooth_jump_down( x,x0,x1 );
!!$
!!$    RETURN
!!$  END FUNCTION Dsmooth_jump_up
!!$ ! End TM73 smooth

END MODULE test_vks_tm87

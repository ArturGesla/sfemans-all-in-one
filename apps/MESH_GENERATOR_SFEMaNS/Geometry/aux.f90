MODULE aux_functions

PUBLIC :: choun, natun, claun

CONTAINS
   SUBROUTINE choun (l,n,x,s)

!     Chord parametrization for spline univariate interpolation

!        input
!     l       block DIMENSION
!     n       knot number
!     x       knot coordinates

!        output
!     s       chord abscissa

      IMPLICIT NONE

      INTEGER :: l,n
      REAL (KIND=8) :: x,s
      DIMENSION x(l,n),s(n)

      INTEGER :: i,k
      REAL (KIND=8) :: t


      s(1) = 0.
      DO i=2,n
         t = 0.
         DO k=1,l
            t = t + (x(k,i)-x(k,i-1))**2
         ENDDO
         s(i) = s(i-1) + sqrt(t)
      ENDDO

   END SUBROUTINE choun

   SUBROUTINE natun (l,n,s,f,dummy1,dummyn,a,b,c,r)

!     System definitions for spline univariate interpolation
!     natural end conditions

!        input
!     l       block DIMENSIONs
!     n       knot number
!     s       knot parametrization
!     f       knot function values
!     dummy1  dummy variable
!     dummy1  dummy variable

!        output
!     a,b,c   coefficients of the matrix
!     r       right hand side

      IMPLICIT NONE

      INTEGER :: l,n
      REAL (KIND=8) :: s,f,dummy1,dummyn,a,b,c,r
      DIMENSION s(n),f(l,n),dummy1(l),dummyn(l),a(n),b(n),c(n),r(l,n)

      INTEGER :: k


      b(1) = 2.
      c(1) = 1.
      a(n) = 1.
      b(n) = 2.

      DO k=1,l
         r(k,1) = 3.*(f(k,2)-f(k,1  ))/(s(2)-s(1  ))
         r(k,n) = 3.*(f(k,n)-f(k,n-1))/(s(n)-s(n-1))
      ENDDO

   END SUBROUTINE natun


   SUBROUTINE claun (l,n,s,f,rs1,rsn,a,b,c,r)

!     System definitions for spline univariate interpolation
!     clamped end conditions

!        input
!     l       block DIMENSIONs
!     n       knot number
!     s       knot parametrization
!     f       knot function values
!     rs1     prescribed condition (s-slope) at node 1
!     rsn     prescribed condition (s-slope) at node n

!        output
!     a,b,c   coefficients of the matrix
!     r       right hand side

      IMPLICIT NONE

      INTEGER :: l,n
      REAL (KIND=8) :: s,f,rs1,rsn,a,b,c,r
      DIMENSION s(n),f(l,n),rs1(l),rsn(l),a(n),b(n),c(n),r(l,n)

      INTEGER :: k


      b(1) = 1.
      c(1) = 0.
      a(n) = 0.
      b(n) = 1.

      DO k=1,l
         r(k,1) = rs1(k)
         r(k,n) = rsn(k)
      ENDDO

   END SUBROUTINE claun


END MODULE aux_functions

      program rigira
c=======================================================================
      implicit none
      character*20 name
      integer i,n
      double precision x(1024),y(1024)
c-----------------------------------------------------------------------
c
      write (*,*) 'file input = ?'
      read  (*,1) name
      open  (1,file=name,form='formatted',status='old')
      read  (1,*) n
      do i=1,n
         read  (1,*) x(i),y(i)
      enddo
      close (1)
c
      write (*,*) 'file output = ?'
      read  (*,1) name
      open  (1,file=name,form='formatted')
      write (1,2) n
      do i=n,1,-1
         write (1,3) x(i),y(i)
      enddo
      close (1)
c
      stop
    1 format (a20)
    2 format (i10)
    3 format (2f10.5)
      end

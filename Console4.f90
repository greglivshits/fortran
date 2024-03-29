!  Console4.f90 
!
!  FUNCTIONS:
!  Console4 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Console4
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Console4

    implicit Real(8) (A-H,O-Z)
    integer(4),parameter:: n=5
    Real(8) A(n,n), B(n), X2(n)
    
    Open(5,file='test.xyz')
    Do i=1,n
        Read (5,*) (A(i,j),j=1,n)
    Enddo
    Do i=1,n
        Read (5,*) B(i)
    Enddo
    close(5)
    Call Gauss(n,A,B,X2)
    Open(6,file='otvet.xyz')
    Do i=1,n 
      Write (6,'(<n+1>f8.3)') (A(i,j),j=1,n), B(i)
    Enddo
    Write (6,*)
    Do i=1,n 
      Write (6,*) X2(i)
    Enddo
 !   Write (6,*)
 !   X2=matmul(A,X2)
 !   Do i=1,n 
 !       Write (6,*) X2(i)
 !   Enddo
 !   Write (6,*)
 !   X2=X2-B
 !   Do i=1,n 
 !     Write (6,*) X2(i)
 !   Enddo
    end program Console4


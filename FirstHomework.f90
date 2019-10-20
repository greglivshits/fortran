!  Console1.f90 
!
!  FUNCTIONS:
!  Console1 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Console1
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program Console1
    Use Elements, Only:AMS

    implicit Real(8) (A-H, O-Z)
    Character(255) Comment
    Integer(4), parameter::MaxAt=100
    Integer(4) NA(MaxAt)
    Real(8) C(3,MaxAt), S(MaxAt), cm(3), x(MaxAt), TI(3,3)
    ! Variables

    ! Body of Console1
    ! Openning and reading
    Open (5, File  ='h2o.xyz')
        Read (5,*)NumAt
        Read (5,*)Comment
        Totmass=0
        Do i=1,NumAt
            Read(5,*) NA(i), C(1:3, i)
            S(i) = AMS (NA(i))
            Totmass = Totmass+S(i)
        Enddo
    Close (5)
    ! MassCenter coordinates
        Do i=1,3
            x(i)=0
            Do j=1,NumAt
                x(i)=x(i)+S(j)*C(i,j)
            Enddo
            cm(i) = x(i)/Totmass
        Enddo
    ! Moment of inertia
        TI(1:3, 1:3)=0
        Do i=1,NumAt
            TI(1,1)=TI(1,1)+S(i)*((C(2,i)-cm(2))**2+(C(3,i)-cm(3))**2)
            TI(1,2)=TI(1,2)-S(i)*(C(1,i)-cm(1))*(C(2,i)-cm(2))
            TI(1,3)=TI(1,3)-S(i)*(C(1,i)-cm(1))*(C(3,i)-cm(3))
            TI(2,1)=TI(2,1)-S(i)*(C(1,i)-cm(1))*(C(2,i)-cm(2))
            TI(2,2)=TI(2,2)+S(i)*((C(1,i)-cm(1))**2+(C(3,i)-cm(3))**2)
            TI(2,3)=TI(2,3)-S(i)*(C(2,i)-cm(2))*(C(3,i)-cm(3))
            TI(3,1)=TI(3,1)-S(i)*(C(3,i)-cm(3))*(C(1,i)-cm(1))
            TI(3,2)=TI(3,2)-S(i)*(C(3,i)-cm(3))*(C(2,i)-cm(2))
            TI(3,3)=TI(3,3)+S(i)*((C(1,i)-cm(1))**2+(C(2,i)-cm(2))**2)
        Enddo
    ! Writing
    Open (6, File ='h2o.out')
        Write(6,'(/32x,''*** Program ReadXYZ ***''/)')
        Write (6,*)NumAt
        Write (6,*)Comment
        Do i=1,NumAt
            Write(6,'(4f15.5)') S(i), c(1, i), c(2, i), c(3, i)
        Enddo
        Write(6,'("MassCenter")')
        Write(6,'(/14x,3f15.5)') cm(1:3)
        Write(6,'("Moment of Inertia")')
        Write(6,'(/14x,3f15.5)') TI(1:3, 1)
        Write(6,'(/14x,3f15.5)') TI(1:3, 2)
        Write(6,'(/14x,3f15.5)') TI(1:3, 3)
    end program Console1


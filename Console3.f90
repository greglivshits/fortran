!  Console3.f90 
!
!  FUNCTIONS:
!  Console3 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Console3
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
 Program RandomMoleculs
    
    Use Elements
    
    Implicit Real(8) (A-H,O-Z)
    
    Integer(4),parameter::MaxAt=5,MaxSubStr=10,NumMol=20   
    Character(255) Comment,Str,SubStr(MaxSubStr)
    Character(10) Aname
    Integer(4) NA(MaxAt)
    Real(8) C(3,MaxAt,NumMol),CI(3,MaxAt),CM(3),U(3,3), X(3) !T(3,3),PMI(3),Paxes(3,3), 
    
! Reading
    Open(5,File='h2o.inp')
    Call ReadNXYZ(5,MaxAt,Numat,NA,CI,'*Geo')
    Close(5)
! Coordinates about mass center 
    TotMass=0.d0
    CM=0.d0
    Do i=1,Numat
        Call SetAName(NA(i),i,Aname)
        Write(6,'(a10,3f15.5)')Aname,C(1:3,i,1)
        TotMass=TotMass+AMS(NA(i))
        CM=CM+AMS(NA(i))*CI(1:3,i)
    Enddo
    CM=CM/TotMass
    Write(6,'(/'' TotMass = '',f15.5)')TotMass
    
    Write(6,'(/'' Mass center: '')')
    Write(6,'(3f15.6)')CM
    
    Do i=1,Numat
        CI(1:3,i)=CI(1:3,i)-CM
    Enddo
    Open(6,File='h2o.out')
Do j=1,NumMol
! Random Euler Angles
    Call RANDOM_NUMBER(X)
    X=360.d0*X
! Rotation of molecule
    irad=0
    Call RotMat2(X(1),X(2),X(3),irad,U)
    Do i=1,Numat
        C(1:3,i,j)=matmul(U,CI(1:3,i))
    Enddo
! Random coordinates
    Call RANDOM_NUMBER(X)
    X=10.d0*X
    Do i=1,NumAt
        C(1:3,i,j)=C(1:3,i,j)+X
    Enddo
! Coordinates check
!        Do i=1,j-1
!            Do k=1,NumAt
!                Do n=1,NumAt
!                    q=dsqrt(sum(C(1:3,k,j)-C(1:3,n,i)))
!                    if (q<1.d0) Then
!                        Exit
!                        Exit
!                        Exit
!                    endif
!                Enddo
!            Enddo
!        Enddo
Enddo
Call PrintNXYZ(6,MaxAt,Numat,NA,C,'*RotGeo')
!    Do i=1,MaxAt*NumMol
!        If (C(1,1,i)/=0 .and. C(2,1,i)/=0 .and. C(3,1,i)/=0) then 
!            CI(1:3,1,i)=C(1:3,1,i)
!        Endif
!    Enddo
    end program RandomMoleculs
    
!***********************************************************
Subroutine ReadNXYZ(iu,MaxAt,Numat,NA,C,SearchStr)

Implicit Real(8) (A-H,O-Z)

Integer(4),parameter:: MaxSubStr=10, NumMol=20
Character(*) SearchStr
Character(255) Str,SubStr(MaxSubStr),UpperCase
Integer(4) NA(MaxAt)
Real(8) C(3,MaxAt)

    Do While(.not.EOF(iu))
        Read(iu,'(a255)')Str
        If (Len_Trim(Str)==0) Cycle
        If (INDEX(UpperCase(Str),Trim(UpperCase(SearchStr)))==1) Then
            Numat=0
            Do While(.not.EOF(iu))
                Read(iu,'(a255)')Str
                If (INDEX(Str,'!')==1) Cycle
                If (Len_Trim(Str)==0) Exit
                Numat=Numat+1
                Call SubString(Str,MaxSubStr,nSubStr,SubStr)
                Call ParseAname(SubStr(1),NA(Numat))
                Read(SubStr(2),'(f255.10)')C(1,Numat)
                Read(SubStr(3),'(f255.10)')C(2,Numat)
                Read(SubStr(4),'(f255.10)')C(3,Numat)
            Enddo
            Exit
        Endif
    Enddo
   
    End
    !**************************************************
Subroutine PrintNXYZ(iu,MaxAt,Numat,NA,C,Str)

Implicit Real(8) (A-H,O-Z)
Integer(4),parameter:: NumMol=20
Character(*) Str
Real(8) C(3,MaxAt,NumMol)
Character(10) Aname
Integer(4) NA(MaxAt)

ll=Len_Trim(Str)
   
    Do j=1,NumMol        
    Do i=1,Numat
        Call SetAName(NA(i),i,Aname)
        Write(iu,'(a10,3f15.6)')Aname,C(1:3,i,j)
    Enddo
    Enddo
    
    End

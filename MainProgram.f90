!  Console2.f90 
!
!  FUNCTIONS:
!  Console2 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: Console2
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    Program ReadXYZ
    
    Use Elements, Only: AMS
    
    Implicit Real(8) (A-H,O-Z)
    
    Integer(4),parameter::MaxAt=100,MaxSubStr=10   
    Character(255) Comment,Str,SubStr(MaxSubStr)
    Character(10) Aname
    Integer(4) NA(MaxAt)
    Real(8) C(3,MaxAt),CM(3),T(3,3),PMI(3),Paxes(3,3)
    
! Reading
    Open(5,File='c2h6.inp')
    Call ReadNXYZ(5,MaxAt,Numat,NA,C,'*Geo')
    Close(5)

    
    Open(6,File='c2h6.out')
    Write(6,'(/32x,''*** Program ReadXYZ ***''/)')
    Write(6,'(''*Geo'')')
    TotMass=0.d0
    CM=0.d0
    Do i=1,Numat
        Call SetAName(NA(i),i,Aname)
        Write(6,'(a10,3f15.5)')Aname,C(1:3,i)
        TotMass=TotMass+AMS(NA(i))
        CM=CM+AMS(NA(i))*C(1:3,i)
    Enddo
    CM=CM/TotMass
    Write(6,'(/'' TotMass = '',f15.5)')TotMass
    
    Write(6,'(/'' Mass center: '')')
    Write(6,'(3f15.6)')CM
    
    Do i=1,Numat
        C(1:3,i)=C(1:3,i)-CM
    Enddo
   
    Call PrintNXYZ(6,MaxAt,Numat,NA,C,'Reduced molecular coordinates')
! Calculation of tensor of inertia
    T=0.d0
    Do i=1,Numat
        amsi=AMS(NA(i))
        x=C(1,i)
        y=C(2,i)
        z=C(3,i)
        T(1,1)=T(1,1)+amsi*(y*y+z*z)
        T(2,2)=T(2,2)+amsi*(x*x+z*z)
        T(3,3)=T(3,3)+amsi*(x*x+y*y)
        T(1,2)=T(1,2)-amsi*x*y
        T(1,3)=T(1,3)-amsi*z*x
        T(2,3)=T(2,3)-amsi*y*z
    Enddo
    T(2,1)=T(1,2)
    T(3,1)=T(1,3)
    T(3,2)=T(2,3)
    
! Print T
Write(6,'(3f10.5)')T

Call JacobiSorted(T,3,3,PMI,Paxes,nrot,1)

Write(6,'(/3f12.7/)')PMI
Do i=1,3
    Write(6,'(3f12.4)')Paxes(i,1:3)
Enddo

Do i=1,Numat
    C(1:3,i)=matmul(Transpose(Paxes),C(1:3,i))
Enddo
Call PrintNXYZ(6,MaxAt,Numat,NA,C,'Molecular coordinates reduced to principal axes')
Call TypeOfInertia(6,NumAt,PMI,1.d-6)
Call Ortogonality(6,Paxes,1.d-6)
Call matrixcheck(6,T,PMI,1.d-6)
End
!***********************************************************
Subroutine ReadNXYZ(iu,MaxAt,Numat,NA,C,SearchStr)

Implicit Real(8) (A-H,O-Z)

Integer(4),parameter:: MaxSubStr=10
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
                Read(SubStr(4),'(f255.10)')C(3,NUmat)
            Enddo
            Exit
        Endif
    Enddo
   
    End
    !**************************************************
Subroutine PrintNXYZ(iu,MaxAt,Numat,NA,C,Str)

Implicit Real(8) (A-H,O-Z)

Character(*) Str
Real(8) C(3,MaxAt)
Character(10) Aname
Integer(4) NA(MaxAt)

ll=Len_Trim(Str)
   
    Write(iu,'(/<ll>a1)')(Str(j:j),j=1,ll)
    Write(iu,'(''*Geo'')')
    Do i=1,Numat
        Call SetAName(NA(i),i,Aname)
        Write(iu,'(a10,3f15.6)')Aname,C(1:3,i)
    Enddo
    Write(iu,*)
    
    End
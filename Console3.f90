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
    Real(8) C(3,MaxAt,NumMol),CI(3,MaxAt),CJ(3,MaxAt),CM(3),U(3,3), X(3)

    Call Place(5,CJ)
    C(1:3,1:NumAt,1)=CJ(1:3,1:NumAt)
    k=1.d0
Do while (k<=20)
    Open(5,File='h2o.inp')
    Call Place(5,CJ)
    m=0.d0
    Do j=1,NumAt
        Do i=1,k
            Do l=1,NumAt
                r=dsqrt(sum(CJ(1:3,j)**2-C(1:3,l,i)**2))
                If (r<=1.d0) m=m+1
            Enddo
        Enddo
    Enddo
    If (m==0) Then
        k=k+1
        C(1:3,1:NumAt,k) = CJ(1:3,1:NumAt)
    Endif
Enddo

Open(6,File='h2o.out')
Call PrintNXYZ(6,MaxAt,Numat,NA,C,'*RotGeo')
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
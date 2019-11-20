Subroutine Place(iu,CJ)

Use Elements
       
Implicit Real(8) (A-H,O-Z)

Integer(4),parameter::MaxAt=5,MaxSubStr=10,NumMol=20   
Character(255) Comment,Str,SubStr(MaxSubStr)
Character(10) Aname
Integer(4) NA(MaxAt)
Real(8) CI(3,MaxAt),CJ(3,MaxAt),CM(3),U(3,3), X(3)
! Reading
    Open(iu,File='h2o.inp')
    Call ReadNXYZ(iu,MaxAt,Numat,NA,CI,'*Geo')
    Close(iu)
! Coordinates about mass center 
    TotMass=0.d0
    CM=0.d0
    Do i=1,Numat
        Call SetAName(NA(i),i,Aname)
 !       Write(6,'(a10,3f15.5)')Aname,C(1:3,i)
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
    

! Random Euler Angles
    Call RANDOM_NUMBER(X)
    X=360.d0*X
! Rotation of molecule
    irad=0
    Call RotMat2(X(1),X(2),X(3),irad,U)
    Do i=1,Numat
        CJ(1:3,i)=matmul(U,CI(1:3,i))
    Enddo
! Random coordinates
    Call RANDOM_NUMBER(X)
    X=10.d0*X
    Do i=1,NumAt
        CJ(1:3,i)=CJ(1:3,i)+X
    Enddo
End
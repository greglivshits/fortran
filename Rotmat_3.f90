Subroutine RotMat3(alp,C,m,n,NA,NumAt,Nrot,iAtoms,iu)
Implicit Real(8) (A-H,O-Z)
Integer(4),parameter::MaxAt=100
Real(8) R(3), U(3,3), C(3,MaxAt), CM(3)
Integer(4) NA(MaxAt), iAtoms(MaxAt)
Real(8),parameter::deg=180.d0/3.141592653589793d0

iAtoms(m)=0
iAtoms(n)=0

a=alp
Do i=1,3
    R(i)=C(i,n)-C(i,m)
Enddo
Rnorm = 1.d0/dsqrt(r(1)**2+r(2)**2+r(3)**2)
R=R*Rnorm

!If (irad==1) a=alp*deg
ca=DCOSD(a/2.d0)
sa=DSIND(a/2.d0)

    w=ca
    x=R(1)*ca
    y=R(2)*ca
    z=R(3)*ca
    qnorm=1.d0/dsqrt(w**2+x**2+y**2+z**2)
    w=w*qnorm
    x=x*qnorm
    y=y*qnorm
    z=z*qnorm

    U(1,1)=1.d0-2.d0*(y**2+z**2)
    U(1,2)=2.d0*(x*y-w*z)
    U(1,3)=2.d0*(x*z+w*y)
    U(2,1)=2.d0*(x*y+w*z)
    U(2,2)=1.d0-2.d0*(x**2+z**2)
    U(2,3)=2.d0*(z*y-w*x)
    U(3,1)=2.d0*(x*z-w*y)
    U(3,2)=2.d0*(z*y+w*x)
    U(3,3)=1.d0-2.d0*(x**2+y**2)
Do k=1,nrot
    Do j=1,NumAt
         If (iAtoms(j)==1) C(1:3,j)=matmul(U,C(1:3,j))
!        Do i=1,3
!          C(i,j)=U(i,1)*C(1,j)+U(i,2)*C(2,j)+U(i,3)*C(3,j)
!        Enddo
    Enddo
    Call PrintNXYZ(iu,MaxAt,Numat,NA,C,'*RotGeo')
Enddo
End
Subroutine RotMat1(iXYZ,alp,irad,U)

Implicit Real(8) (A-H,O-Z)

Real(8) U(3,3)
Real(8),parameter::deg=180.d0/3.141592653589793d0

a=alp
If (irad==1) a=alp*deg

ca=DCOSD(a)
sa=DSIND(a)
U=0.d0

If (iXYZ==1) Then
    U(1,1)=1.d0
    U(2,2)=ca
    U(3,3)=ca
    U(2,3)=-sa
    U(3,2)=sa
ElseIF(iXYZ==2) Then
    U(1,1)=ca
    U(2,2)=1.d0
    U(3,3)=ca
    U(1,3)=sa
    U(3,1)=-sa
ElseIF(iXYZ==3) Then
    U(1,1)=ca
    U(2,2)=ca
    U(3,3)=1.d0
    U(1,2)=-sa
    U(2,1)=sa
Endif    
    
End
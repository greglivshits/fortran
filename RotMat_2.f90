Subroutine RotMat2(alp,bet,gamma,irad,U)

Implicit Real(8) (A-H,O-Z)

Real(8) U(3,3), U1(3,3), U2(3,3), U3(3,3)
Real(8),parameter::deg=180.d0/3.141592653589793d0

a=alp
b=bet
g=gamma
If (irad==1) a=alp*deg

ca=DCOSD(a)
sa=DSIND(a)
cb=DCOSD(b)
sb=DSIND(b)
cg=DCOSD(g)
sg=DSIND(g)
U1=0.d0
U2=0.d0
U3=0.d0

    U1(1,1)=ca
    U1(2,2)=ca
    U1(3,3)=1.d0
    U1(1,2)=-sa
    U1(2,1)=sa
    
    U2(1,1)=1.d0
    U2(2,2)=cb
    U2(3,3)=cb
    U2(2,3)=-sb
    U2(3,2)=sb

    U3(1,1)=cg
    U3(2,2)=cg
    U3(3,3)=1.d0
    U3(1,2)=-sg
    U3(2,1)=sg
    
U=matmul(matmul(U1,U2),U3) 
    
End
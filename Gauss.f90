Subroutine Gauss(n,A,B,X1)
implicit Real(8) (A-H,O-Z)
Real(8) A(n,n), B(n), X1(n), X2(n), IPiv(n), R(n)
Do i=1,n
    IPiv(i)=i
Enddo
Do i=1,n
    m=0.d0
    l=i
    Do k=i,n
        If (abs(A(k,i))>m) Then
        m = abs(A(k,i))
        l=k
        Endif
    Enddo
    If (l>i) Then
        r(1:n)=a(i,1:n)
        a(i,1:n)=a(l,1:n)
        a(l,1:n)=r(1:n)
        c=b(i)
        b(i)=b(l)
        b(l)=c
        q=IPiv(i)
        IPiv(i)=IPiv(l)
        IPiv(l)=q
    Endif
    Do j=i+1,n
        Do ma=i,n
            A(j,ma)=A(j,ma)-(A(j,i)/A(i,i))*A(i,ma)
        Enddo
        B(j)=B(j)-(A(j,i)/A(i,i))*B(j)
    Enddo
Enddo
if (a(n,n)==0) then
    write(6,*)'0 решений'
    stop
endif
if (b(n)==0) then
    write(6,*)'Ѕесконечное количество решений'
    stop
endif
X1(n)=B(n)/A(n,n)
Do i=n-1,1,-1
    alp=0.d0
    Do j=i+1,n
        alp=alp+A(i,j)*X1(j)
    Enddo
    X1(i)=(B(i)-alp)/A(i,i)
Enddo
!Do i=1,n
!    X1(IPiv(i))=X1(i)
!Enddo
End
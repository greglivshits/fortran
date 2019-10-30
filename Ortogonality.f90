SubRoutine Ortogonality(iu,a,eps)
 Real(8) a(3,3)
 Real(8) eps,b,c,d,x,y,z
If (DABS(a(1,1)*a(2,1)+a(1,2)*a(2,2)+a(1,3)*a(2,3) - 0.d0)<eps .and. DABS(a(2,1)*a(3,1)+a(2,2)*a(3,2)+a(2,3)*a(3,3) - 0.d0)<eps .and. DABS(a(1,1)*a(3,1)+a(1,2)*a(3,2)+a(1,3)*a(3,3) - 0.d0)<eps) then
        write(iu,'("ortogonal")')
    Else 
        write(iu,'("not ortogonal")')
    EndIf
 If (DABS(a(1,1)*a(1,1)+a(1,2)*a(1,2)+a(1,3)*a(1,3) - 1.d0)<eps .and. DABS(a(2,1)*a(2,1)+a(2,2)*a(2,2)+a(2,3)*a(2,3) - 1.d0)<eps .and. DABS(a(3,1)*a(3,1)+a(3,2)*a(3,2)+a(3,3)*a(3,3) - 1.d0)<eps) then
        write(iu,'("normalized")') 
    Else 
        write(iu,'("not normalized")')
    EndIf
    x = a(1,1)*a(2,1)+a(1,2)*a(2,2)+a(1,3)*a(2,3)
    y = a(2,1)*a(3,1)+a(2,2)*a(3,2)+a(2,3)*a(3,3)
    z = a(1,1)*a(3,1)+a(1,2)*a(3,2)+a(1,3)*a(3,3) 
    b = a(1,1)*a(1,1)+a(1,2)*a(1,2)+a(1,3)*a(1,3)
    c = a(2,1)*a(2,1)+a(2,2)*a(2,2)+a(2,3)*a(2,3) 
    d = a(3,1)*a(3,1)+a(3,2)*a(3,2)+a(3,3)*a(3,3)
    Write (iu,*) x,y,z
    Write (iu,*) b,c,d
End

SubRoutine Ortogonality(iu,a)
 Real(8) a(3,3)
 Real(8) b,c,d,x,y,z
If (a(1,1)*a(2,1)+a(1,2)*a(2,2)+a(1,3)*a(2,3) == 0 .and. a(2,1)*a(3,1)+a(2,2)*a(3,2)+a(2,3)*a(3,3) == 0 .and. a(1,1)*a(3,1)+a(1,2)*a(3,2)+a(1,3)*a(3,3) == 0) then
        write(iu,'("ortogonal")')
    Else 
        write(iu,'("not ortogonal")')
    EndIf
 If (a(1,1)*a(1,1)+a(1,2)*a(1,2)+a(1,3)*a(1,3) == 1 .and. a(2,1)*a(2,1)+a(2,2)*a(2,2)+a(2,3)*a(2,3) == 1 .and. a(3,1)*a(3,1)+a(3,2)*a(3,2)+a(3,3)*a(3,3) == 1) then
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
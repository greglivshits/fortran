SubRoutine Ortogonality(iu,a)
    Real(8) a(3,3)
    Logical Ort
    If (a(1,1)*a(2,1)+a(1,2)*a(2,2)+a(1,3)*a(2,3) == 0 .and. a(2,1)*a(3,1)+a(2,2)*a(3,2)+a(2,3)*a(3,3) == 0 .and. a(1,1)*a(3,1)+a(1,2)*a(3,2)+a(1,3)*a(3,3) == 0) then
        Ort = .TRUE.
    EndIf
    x = a(1,1)*a(2,1)+a(1,2)*a(2,2)+a(1,3)*a(2,3)
    y = a(2,1)*a(3,1)+a(2,2)*a(3,2)+a(2,3)*a(3,3)
    z = a(1,1)*a(3,1)+a(1,2)*a(3,2)+a(1,3)*a(3,3) 
    Write (iu,'(3f15.5)') x,y,z
    Write (iu,*) Ort
End
Subroutine TypeOfInertia(iu,NumAt,PMI)
    Integer(4) NumAt,kRotType
    Real(8) PMI(3)
    !Calculating type of Inertia
If (Numat == 1) then 
    kRotType = 6
    Elseif (PMI(1) == PMI(2) .and. PMI(1) == PMI(3)) then
    kRotType = 1
    Elseif (PMI(1) == PMI(2) .and. PMI(1) > PMI(3) .or. PMI(1) == PMI(3) .and. PMI(1) > PMI(2) .or. PMI(2) == PMI(3) .and. PMI(2) > PMI(1)) then 
    kRotType = 2
    Elseif (PMI(1) == PMI(2) .and. PMI(1) < PMI(3) .or. PMI(1) == PMI(3) .and. PMI(1) < PMI(2) .or. PMI(2) == PMI(3) .and. PMI(2) < PMI(1)) then 
    kRotType = 3
    Elseif (PMI(1) /= PMI(2) .and. PMI(1) /= PMI(3)) then 
    kRotType = 4
    Elseif (PMI(1) == PMI(2) .and. PMI(1) /= 0 .and. PMI(3) == 0 .or. PMI(1) == PMI(3) .and. PMI(1) /= 0 .and. PMI(2) == 0 .or. PMI(2) == PMI(3) .and. PMI(2) /= 0 .and. PMI(1) == 0) then 
    kRotType = 5
Endif
!Writing type of inertia
TOI: Select Case(kRotType)
    Case(1)
    write(iu,'("Spherical top")')
    Case(2)
    write(iu,'("Prolate symmetric top")')
    Case(3)
    write(iu,'("Oblate symmetric top")')
    Case(4)
    write(iu,'("Asymmetric top")')
    Case(5)
    write(iu,'("Linear top")')
    Case(6)
    write(iu,'("Atom")')
    Case default
    write(iu,'("Error")')
    End Select TOI
End
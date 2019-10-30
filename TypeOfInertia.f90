Subroutine TypeOfInertia(iu,NumAt,PMI,eps)
    Integer(4) NumAt,kRotType
    Real(8) PMI(3)
    Real(8) eps
    !Calculating type of Inertia
If (Numat == 1) then 
    kRotType = 6
    Elseif ((DABS(PMI(1) - PMI(2))<eps) .and. (DABS(PMI(1) - PMI(3))<eps)) then
    kRotType = 1
    Elseif ((DABS(PMI(1) - PMI(2))<eps) .and. PMI(1) > PMI(3) .or. (DABS(PMI(1) - PMI(3))<eps) .and. PMI(1) > PMI(2) .or. (DABS(PMI(2) - PMI(3))<eps) .and. PMI(2) > PMI(1)) then 
    kRotType = 2
    Elseif ((DABS(PMI(1) - PMI(2))<eps) .and. PMI(1) < PMI(3) .or. (DABS(PMI(1) - PMI(3))<eps) .and. PMI(1) < PMI(2) .or. (DABS(PMI(2) - PMI(3))<eps6) .and. PMI(2) < PMI(1)) then 
    kRotType = 3
    Elseif ((DABS(PMI(1) - PMI(2))>eps) .and. (DABS(PMI(1) - PMI(3))>eps)) then 
    kRotType = 4
    Elseif ((DABS(PMI(1) - PMI(2))<eps) .and. PMI(1) /= 0 .and. (DABS(PMI(3) - 0.d0)<eps) .or. (DABS(PMI(1) - PMI(3))<eps) .and. PMI(1) /= 0 .and. (DABS(PMI(2) - 0.d0)<eps) .or. (DABS(PMI(2) - PMI(3))<eps) .and. PMI(2) /= 0 .and. (DABS(PMI(1) - 0.d0)<eps)) then 
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

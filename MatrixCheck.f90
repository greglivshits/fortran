subroutine matrixcheck(iu,a,PMI,epsil)
    Real(8) a(3,3), b(3,3), v(3,3), d(3,3), eps(3,3), r(3,3), t(3,3), PMI(3)
    Real(8) epsil
    Call jacobi(a,3,3,d,v,nrot)
    Do i=1,3
        Do j=1,3
            eps(i,j)=epsil
        enddo
    enddo   
    d(2,2) = d(2,1)
    d(3,3) = d(3,1)
    d(2,1) = 0
    d(3,1) = 0
    Do i=1,3 
        write(iu,'(3f12.5)') d(i,1:3)
    Enddo
    r=matmul(matmul(Transpose(v),a),v)-d
    t=matmul(matmul(v,d),Transpose(v))-a
    If (r(1,1) < eps(1,1) .and. r(1,2) < eps(1,2) .and. r(1,3) < eps(1,3) .and. r(2,1) < eps(2,1) .and. r(2,2) < eps(2,2) .and. r(2,3) < eps(2,3) .and. r(3,1) < eps(3,1) .and. r(3,2) < eps(3,2) .and. r(3,3) < eps(3,3) .and. t(1,1) < eps(1,1) .and. t(1,2) < eps(1,2) .and. t(1,3) < eps(1,3) .and. t(2,1) < eps(2,1) .and. t(2,2) < eps(2,2) .and. t(2,3) < eps(2,3) .and. t(3,1) < eps(3,1) .and. t(3,2) < eps(3,2) .and. t(3,3) < eps(3,3)) then 
        write(iu,'("the matrix is symmetric")')
    Else 
    write(iu,'("the matrix is not symmetric")')
    Endif
    Do i=1,3
        write(iu,'(3f12.5)') r(i,1:3)
    Enddo
    write(iu,'(/)')
    Do i=1,3
        write(iu,'(3f12.5)') t(i,1:3)
    Enddo
end
    
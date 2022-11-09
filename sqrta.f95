subroutine fatoracao(a,b,c)
    implicit none
    real(16):: a, b
    integer(8):: c
    c = 0
    b = a
    do while(b>=2)
        b = b/2
        c = c+1
    enddo
end subroutine

subroutine squareroot2(n,sq2)
    implicit none
    real(16):: sq2
    integer(8):: i, n, check
    sq2 = 1.d0
    do i=1, n
       sq2 = (0.5)*sq2*(3.0-2.0*sq2*sq2)
    enddo
    sq2 = sq2*2
end subroutine

subroutine squareroota(a,n,sqa)
    implicit none
    integer(8)::i, n, c, j
    real(16):: a, b, sq2, sqa, sqb
    call fatoracao(a,b,c)
    
    sqb = 1.d0 
    do i=1, n
       sqb = (0.5)*sqb*(3.0-b*sqb*sqb)
    enddo
    sqb = sqb*b
    j = 2
    if(MOD(c, j).NE.0) then
        call squareroot2(n,sq2)
        sqa = sq2*sqb*j**((c-1.0)/2.0)
    else
        sqa = sqb*2.0**(c/2.0)
    endif
end subroutine


program sqrta
    implicit none
    real(16):: a, b, sqa, sq2
    integer(8):: c, n, i
    write(*,*)"Insira a raiz que deseja calcular"
	read(*,*)a
    n = 9
    call squareroota(a,n,sqa)
    write(*,*)sqa
	write(*,*)"Feche o prompt de comando"
	read(*,*)a
end program
!!fatora o numero inserido em um número do tipo b*2^c, onde 0<b<2 e c é int
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

!!calcula a  raiz quadrada de 2 usando o método de newton-raphson
!!será útil para calcular a raiz de 2^c
subroutine squareroot2(n,sq2)
    implicit none
    real(16):: sq2
    integer(8):: i, n, check
    sq2 = 1.d0	!!valor inicial
    do i=1, n
       sq2 = (0.5)*sq2*(3.0-2.0*sq2*sq2)	!!método de newton-raphson modificado
    enddo
    sq2 = sq2*2	!!resultado da raiz de 2
end subroutine

!!unica subrotina chamada diretamente pelo programa
!!chama as outras subrotinas
!!utiliza o número b*2^c, calcula a raiz de b e multiplica o c vezes a raiz de 2
subroutine squareroota(a,n,sqa)
    implicit none
    integer(8)::i, n, c, j
    real(16):: a, b, sq2, sqa, sqb
    call fatoracao(a,b,c) !!fatora o número de input em b*2^c
    
    sqb = 1.d0 
    do i=1, n
       sqb = (0.5)*sqb*(3.0-b*sqb*sqb) !!metodo de newton-raphson
    enddo
    sqb = sqb*b	!!calcula a raiz da parte b
    j = 2
    if(MOD(c, j).NE.0) then	!!caso c!=0, ou seja, a possui valores fatorados em 2
        call squareroot2(n,sq2)	!!recupera o valor de raiz de 2, unicamente para fins de daticos, a mesma poderia ser armazenada diretamente no codigo fonte
        sqa = sq2*sqb*j**((c-1.0)/2.0) !!multiplica os valores encontrados anteriormente
    else	!!caso c=0, não há fatores de 2 no número a
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
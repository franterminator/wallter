Program CN
    real*8, dimension(:,:),allocatable:: matriz, banda
    real*8, dimension(:),allocatable:: b
    integer:: i

    ! numero de puntos
    write(*,*) "Introduzca el número de puntos:"
    write(*,'(5x,a)') "n"
    n = 6
    allocate(matriz(n,n),b(n))

    do i=1,n
        do j=1,n
            matriz(i,j) = 0
        end do
    end do

    ! matriz de prueba
    matriz(1,1) = 7
    matriz(1,2) = 2
    matriz(1,3) = 0
    matriz(1,4) = 3
    matriz(1,5) = 0
    matriz(1,6) = 0

    matriz(2,1) = 2
    matriz(2,2) = 7
    matriz(2,3) = 2
    matriz(2,4) = 0
    matriz(2,5) = 3
    matriz(2,6) = 0

    matriz(3,1) = 0
    matriz(3,2) = 2
    matriz(3,3) = 7
    matriz(3,4) = 2
    matriz(3,5) = 0
    matriz(3,6) = 3

    matriz(4,1) = 3
    matriz(4,2) = 0
    matriz(4,3) = 2
    matriz(4,4) = 7
    matriz(4,5) = 2
    matriz(4,6) = 0

    matriz(5,1) = 0
    matriz(5,2) = 3
    matriz(5,3) = 0
    matriz(5,4) = 2
    matriz(5,5) = 7
    matriz(5,6) = 2

    matriz(6,1) = 0
    matriz(6,2) = 0
    matriz(6,3) = 3
    matriz(6,4) = 0
    matriz(6,5) = 2
    matriz(6,6) = 7

    b(1) = 1
    b(2) = 2
    b(3) = 3
    b(4) = 4
    b(5) = 5
    b(6) = 6

    call printMatrix(matriz,n)
    call linea
    do i=1,n
        write(*,*) b(i)
    end do
    call linea

    call fCholesky(matriz,n)

    call linea
    call printMatrix(matriz,n)
    call linea

    call linearSystem(matriz,b,n)

    call linea
    do i=1,n
        write(*,*) b(i)
    end do

    ! para que no se cierre el programa derepente
    write(*,*) "Gracias por usar el programa..."
    read(*,*)
End Program CN

subroutine printMatrix(matriz,n)
    real*8,dimension(n,n),intent(in):: matriz
    integer:: i,j

    do i=1,n
        write(*,'(*(f0.4,5x))') (matriz(i,j),j=1,n)
    end do
end subroutine

subroutine linea()
    write(*,*) "--------------------------------------------------"
end subroutine

subroutine fCholesky(matriz, n)
    real*8,dimension(n,n),intent(inout):: matriz
    real*8, dimension(n,n):: l,d
    integer:: k,i,j,n,m
    real*8:: suma

    ! factorizacion de cholesky
    l(1,1) = 1
    d(1,1) = matriz(1,1)
    do k=1,n-1
        do i=1,k
            suma = 0
            do j=1,i-1
                suma = suma + l(i,j)*l(k+1,j)
            end do
            l(k+1,i) = (matriz(k+1,i) - suma)
        end do
        do i=1,k
            l(k+1,i) = l(k+1,i)/d(i,i)
        end do
        l(k+1,k+1) = 1

        suma = 0
        do j=1,k
            suma = suma + l(k+1,j)*d(j,j)*l(k+1,j)
        end do
        d(k+1,k+1) = matriz(k+1,k+1) - suma
    end do

    do i=1,n
        do j=1,n
            if (i<j) matriz(i,j) = l(j,i)
            if (i==j) matriz(i,j) = d(j,j)
            if (i>j) matriz(i,j) = l(i,j)
        end do
    end do

    call linea
    call printMatrix(l,n)
    call linea
    call printMatrix(d,n)
end subroutine

subroutine linearSystem (A,b,n)
    real*8,dimension(n,n),intent(inout):: A
    real*8,dimension(n),intent(inout):: b
    integer:: i,j
    real*8:: suma = 0

    do i=2,n
        suma = 0
        do j=1,i-1
            suma = suma + A(i,j)*b(j)
        end do
        b(i) = b(i) - suma
    end do

    do i=1,n
        b(i) = b(i) / A(i,i)
    end do

    do i=n-1,1,-1
        suma = 0
        do j=i+1,n
            suma = suma + a(j,i)*b(j)
        end do
        b(i) = b(i) - suma
    end do
end subroutine

Program CN
    real*8, dimension(:,:),allocatable:: matriz


    ! numero de puntos
    write(*,*) "Introduzca el número de puntos:"
    write(*,'(5x,a)') "n"
    n = 6
    allocate(matriz(n,n))

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

    matriz(2,1) = 7
    matriz(2,2) = 2
    matriz(2,3) = 0
    matriz(2,4) = 3
    matriz(2,5) = 0
    matriz(2,6) = 0

    matriz(3,1) = 7
    matriz(3,2) = 2
    matriz(3,3) = 0
    matriz(3,4) = 3
    matriz(3,5) = 0
    matriz(3,6) = 0

    matriz(4,1) = 7
    matriz(4,2) = 2
    matriz(4,3) = 0
    matriz(4,4) = 3
    matriz(4,5) = 0
    matriz(4,6) = 0

    matriz(5,1) = 7
    matriz(5,2) = 2
    matriz(5,3) = 0
    matriz(5,4) = 3
    matriz(5,5) = 0
    matriz(5,6) = 0

    matriz(6,1) = 7
    matriz(6,2) = 2
    matriz(6,3) = 0
    matriz(6,4) = 3
    matriz(6,5) = 0
    matriz(6,6) = 0

    call printMatrix(matriz,n)
    call linea
    call fCholesky(matriz,n)
    call linea
    call printMatrix(matriz,n)

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
    integer:: k,i,j,n,m
    real*8:: suma

    ! factorizacion de cholesky
    do k=1,n-1
        do i=1,k
            suma = 0
            do j=1,i-1
                suma = suma + matriz(j,i-j+1)*matriz(j,k+2-j)
            end do
            matriz(i,k+2-i) = (matriz(i,k+2-i) - suma)
        end do
        do i=1,k
            matriz(i,k+2-i) = matriz(i,k+2-i)/matriz(i,1)
        end do

        suma = 0
        do j=1,k
            suma = suma + matriz(j,k+2-j)*matriz(j,1)*matriz(j,k+2-j)
        end do
        matriz(k+1,1) = matriz(k+1,1) - suma
    end do

end subroutine

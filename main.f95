Program CN
    real*8, dimension(:,:),allocatable:: matriz
    real*8, dimension(:),allocatable:: f
    integer:: i,n,m
    real*8:: A, B, C

    ! numero de puntos
    call bienvenido()
    call datos(A,B,C,n,m)
    allocate(matriz(n*m,n*m),f(n*m))
    call constructMatrix(matriz,A,B,C,n,m)

    f(1) = 1
    f(2) = 2
    f(3) = 3
    f(4) = 4
    f(5) = 5
    f(6) = 6

    call printMatrix(matriz,n,m)
    call linea
    do i=1,n*m
        write(*,*) f(i)
    end do
    call linea

    call fCholesky(matriz,n,m)

    call linea
    call printMatrix(matriz,n,m)
    call linea
    call linearSystem(matriz,f,n,m)
    call linea
    do i=1,n*m
        write(*,*) f(i)
    end do
    call linea

    ! para que no se cierre el programa derepente
    write(*,*) "Gracias por usar el programa..."
    read(*,*)
End Program CN

subroutine linea()
    write(*,*) "--------------------------------------------------"
end subroutine

subroutine printMatrix(matriz,n,m)
    real*8,dimension(n*m,n*m),intent(in):: matriz
    integer:: i,j

    do i=1,n*m
        write(*,'(*(f0.4,5x))') (matriz(i,j),j=1,n*m)
    end do
end subroutine

subroutine bienvenido()
    character(len=300):: line
    integer:: iostat = 1

    ! abrir archivo de bienvenida e imprimir mensaje
    write(*,*) ".______    __          ___      .__   __.  __  ___      _______. __    __   _______  __       __      "
    write(*,*) "|   _  \  |  |        /   \     |  \ |  | |  |/  /     /       ||  |  |  | |   ____||  |     |  |     "
    write(*,*) "|  |_)  | |  |       /  ^  \    |   \|  | |  '  /     |   (----`|  |__|  | |  |__   |  |     |  |     "
    write(*,*) "|   ___/  |  |      /  /_\  \   |  . `  | |    <       \   \    |   __   | |   __|  |  |     |  |     "
    write(*,*) "|  |      |  `----./  _____  \  |  |\   | |  .  \  .----)   |   |  |  |  | |  |____ |  `----.|  `----."
    write(*,*) "| _|      |_______/__/     \__\ |__| \__| |__|\__\ |_______/    |__|  |__| |_______||_______||_______|"

end subroutine

subroutine datos(A,B,C,n,m)
    real*8,intent(out):: A,B,C
    integer*4,intent(out):: n,m
    real*8:: largo, ancho, deltaX, deltaY

    write(*,*) "Bienvenido -------> "
    write(*,'(20X,A)') "Pulse enter para continuar"
    read(*,*)

    ! Datos tecnicos de la placa
    write(*,*) "*************************"
    write(*,*) "*   DATOS DE LA PLACA   *"
    write(*,*) "*************************"
    write(*,*) "-> ancho de la placa"
    write(*,'(A,$)') "(metros) "
    read(*,*) ancho
    write(*,*) "-> puntos para discretizar el largo"
    write(*,'(A,$)') "(metros) "
    read(*,*) largo

    ! datos para la discretizacion del modelo
    write(*,*) "*************************"
    write(*,*) "*   NUMERO DE PUNTOS    *"
    write(*,*) "*************************"
    write(*,*) "-> puntos para discretizar el ancho"
    write(*,'(A,$)') "(integer) "
    read(*,*) n
    write(*,*) "-> puntos para discretizar el largo"
    write(*,'(A,$)') "(integer) "
    read(*,*) m

    ! calculo de los coef A, B y C
    deltaX = ancho / (n + 1)
    deltaY = largo / (m + 1)
    B = 1 / (deltaX**2)
    C = 1 / (deltaY**2)
    A = -2 * (B + C)
    write(*,'(A,3(f0.2,X),A)') "[A,B,C] -> [ ",A,B,C,"]"

    ! cambiamos n para adecuar la nueva dimension de la matriz
end subroutine

subroutine constructMatrix(matriz,A,B,C,n,m)
    real*8,dimension(n*m,n*m),intent(inout):: matriz
    real*8,intent(in):: A,B,C
    integer,intent(in):: n,m
    integer:: i,j

    do i=1,n*m
        do j=1,n*m
            matriz(i,j) = 0
        end do
    end do


    do i=1,n*m
        matriz(i,1) = A
        if (mod(i,n) == 0 .AND. i > 1) then
            matriz(i,2) = 0
        else
            matriz(i,2) = B
        end if
        if(i+n <= n*m) matriz(i,n+1) = C
    end do
end subroutine

subroutine fCholesky(matriz, n, m)
    real*8,dimension(n*m,n*m),intent(inout):: matriz
    integer:: k,i,j
    real*8:: suma

    ! factorizacion de cholesky
    do k=1,n*m-1
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

subroutine linearSystem (matriz, f, n, m)
    real*8,dimension(n*m,n*m),intent(inout):: matriz
    real*8,dimension(n*m),intent(inout):: f
    integer,intent(in):: n, m
    integer:: i,j
    real*8:: suma = 0

    do i=2,n*m
        suma = 0
        do j=1,i-1
            suma = suma + matriz(j,i-j+1)*f(j)
        end do
        f(i) = f(i) - suma
    end do

    do i=1,n*m
        f(i) = f(i) / matriz(i,1)
    end do

    do i=n*m-1,1,-1
        suma = 0
        do j=i+1,n*m
            suma = suma + matriz(i,j-i+1)*f(j)
        end do
        f(i) = f(i) - suma
    end do
end subroutine

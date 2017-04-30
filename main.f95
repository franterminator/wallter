!< author: francisco rivera alvarez
!  Programa para el calculo de flechas de una placa apoyada
!  en sus cantos con carga hidr�ulica hasta la mitad de su largo
Program CN

    !< Interface para poder fijar las dimensiones de la matriz en la
    ! subroutine constructor
    interface
        subroutine constructor(matriz,f,n,m)
            real*8,dimension(:,:),allocatable,intent(inout):: matriz
            real*8,dimension(:),allocatable,intent(inout):: f
            integer,intent(out):: n,m
        end subroutine
    end interface

    real*8, dimension(:,:),allocatable:: matriz !! sistema de ecuaciones
    real*8, dimension(:),allocatable:: f        !! vector terminos independientes
    integer:: n, m                              !! numero de puntos para discretizar largo(n) y ancho(m)
    real*8:: ancho, largo, espesor              !! dimensiones de la placa
    logical:: asserts = .FALSE.

    ! numero de puntos
    call commandLine(asserts)
    call bienvenido()
    call constructor(matriz,f,n,m)

    ! muestra la matriz
    if (asserts) then
        call printMatrix(matriz, 'Matriz')
        call printVector(f, 'Vector')

        ! factorizacion y resultados
        call fCholesky(matriz,n,m)
        call printMatrix(matriz ,'Matriz (factorizada)')

        ! resolucion del sistema
        call linearSystem(matriz,f,n,m)
        call linearSystem(matriz,f,n,m)
        call printVector(f, 'Vector solucion')
    end if

    call fCholesky(matriz,n,m)
    call linearSystem(matriz,f,n,m)
    call linearSystem(matriz,f,n,m)
    call printVector(f, 'Vector solucion')

    ! para que no se cierre el programa derepente
    write(*,*) "Gracias por usar el programa..."
    read(*,*)

    contains
    subroutine header(label)
        character(len=*),intent(in):: label
        character(len=30):: formato
        character(len=2):: iString
        integer:: i

        ! 30 is the line length / 2
        i = 30 - len(label)/2
        write(iString,'(i2)') i
        formato = '('//iString//'X,A)'
        call linea()
        write(*,formato) label
        call linea()
    end subroutine

    subroutine printMatrix(matriz,label)
        real*8,dimension(:,:),intent(in):: matriz
        character(len=*),intent(in):: label
        integer:: i,j

        call header(label)
        do i=1,ubound(matriz,1)
            write(*,'(*(f0.4,5x))') (matriz(i,j),j=1,ubound(matriz,2))
        end do
        call linea()
    end subroutine

    subroutine printVector(f,label)
        real*8,dimension(:),intent(in):: f
        character(len=*),intent(in):: label
        integer:: i

        call header(label)
        do i=1,ubound(f,1)
            write(*,*) f(i)
        end do
        call linea()
    end subroutine
End Program CN

subroutine linea()
    write(*,*) "____________________________________________________________"
end subroutine

subroutine commandLine(asserts)
    logical,intent(out):: asserts
    integer:: i
    character(len=30):: cmd

    i = iargc()
    if(i>0) then
        call getarg(i,cmd)
        if(cmd == '-h') then
            write(*,*) 'You wanna help, bitch'
            call exit(0)
        end if
        if(cmd == '-a') then
            write(*,*) 'Oh,fuck you active the asserts'
            asserts = .TRUE.
        end if
    end if
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

    write(*,*) "Bienvenido -------> "
    write(*,'(20X,A)') "Pulse enter para continuar"
    read(*,*)
end subroutine

subroutine datosPlaca(ancho,largo,espesor,coefMat,n,m)
    integer*4,intent(out):: n,m
    real*8,intent(out):: largo, ancho, espesor, coefMat
    real*8:: Young, poisson

    ! Datos tecnicos de la placa
    write(*,*) "*************************"
    write(*,*) "*   DATOS DE LA PLACA   *"
    write(*,*) "*************************"
    write(*,*) "-> ancho de la placa"
    write(*,'(A,$)') "(metros) "
    read(*,*) ancho
    write(*,*) "-> largo de la placa"
    write(*,'(A,$)') "(metros) "
    read(*,*) largo
    write(*,*) "-> espesor de la placa"
    write(*,'(A,$)') "(metros) "
    read(*,*) espesor

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

    ! propiedades del material
    write(*,*) "*************************"
    write(*,*) "*   DATOS DEL MATERIAL  *"
    write(*,*) "*************************"
    write(*,*) "-> Modulo de Young"
    write(*,'(A,$)') "(MPa) "
    read(*,*) Young
    write(*,*) "-> Coef. de poisson"
    write(*,'(A,$)') "(_real_) "
    read(*,*) poisson

    coefMat = Young*espesor**3
    coefMat = coefMat/(12*(1-poisson**2))
end subroutine

subroutine constructor(matriz,f,n,m)
    real*8,dimension(:,:),allocatable,intent(inout):: matriz
    real*8,dimension(:),allocatable,intent(inout):: f
    integer,intent(out):: n,m

    real*8:: ancho,largo,espesor,A,B,C,deltaX,deltaY
    real*8:: coefMat,presion
    integer:: i,j

    call datosPlaca(ancho,largo,espesor,coefMat,n,m)

    ! calculo de los coef A, B y C
    deltaX = ancho / (n + 1)
    deltaY = largo / (m + 1)
    B = 1 / (deltaX**2)
    C = 1 / (deltaY**2)
    A = -2 * (B + C)
    write(*,'(A,3(f0.2,X),A)') "[A,B,C] -> [ ",A,B,C,"]"

    !matriz y vector a cero
    allocate(matriz(n*m,n*m),f(n*m))
    do i=1,n*m
        f(i) = 0
        do j=1,n*m
            matriz(i,j) = 0
        end do
    end do

    ! construccion matriz
    do i=1,n*m
        matriz(i,1) = A
        if (mod(i,n) == 0 .AND. i > 1) then
            matriz(i,2) = 0
        else
            matriz(i,2) = B
        end if
        if(i+n <= n*m) matriz(i,n+1) = C
    end do

    do i=1,n*m
        ! peso especifico agua = 10000 N / m2 -> 0.01 N / mm2 -> 0.01 MPa
        presion = 0.01*(largo/2-(i+1)/m*deltaY)/coefMat
        if (presion < 0) then
            f(i) = 0
        else
            f(i) = presion
        end if
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

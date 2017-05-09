!< author: francisco rivera alvarez
!  Programa para el calculo de flechas de una placa apoyada
!  en sus cantos con carga hidraulica hasta la mitad de su largo
Program WALLTER

    real*8, dimension(:,:),allocatable:: matriz !! sistema de ecuaciones
    real*8, dimension(:),allocatable:: f        !! vector terminos independientes
    real*8,dimension(:,:),allocatable:: navier  !! vector terminos analiticos

    real*8, dimension(:,:),allocatable:: fMatriz!! solucion en forma matricial
    integer,dimension(2):: forma                !! forma de la matriz solucion
    integer,dimension(2):: orden = (/2,1/)      !! orden de los numeros al cambiar vector a matriz solucion

    character(len=50):: resultsFile = './result/resultados.html'    !! directorio donde se escribiran los resultados
    character(len=50):: configFile = ''                             !! directorio donde se encuentran los config file
    logical:: asserts = .FALSE.                 !! si se activa se imprimen los datos de la factorizacion
    logical:: exists                            !! comprueba la existencia de los directorios de resultados

    real*8:: ancho,largo,espesor                !! dimensiones de la placa
    real*8:: rigidez                            !! rigidez a flexion de la placa
    integer:: n, m                              !! numero de puntos para discretizar largo(n) y ancho(m)

    COMMON resultsFile,configFile               !! variables globales

    ! comprueba si existe el directorio de resultados
    inquire(file=resultsFile,exist=exists)
    if(.NOT.exists) resultsFile = 'result.html' ! si no existe crea el archivo al lado del programa

    ! inicio del programa
    call commandLine(asserts)                   ! opciones de ejecucion
    call bienvenido()                           ! mensaje de bienvenida (header)
    call datos(ancho,largo,espesor,rigidez,n,m) ! datos para poder funcionar el programa

    allocate(matriz(n*m,n*m))                   ! fija la matriz de resultados numericos
    call constructMatriz(ancho,largo,matriz,n,m)! construye la matriz para los resultados numericos
    allocate(f(n*m))                            ! fija el vector de resultados numericos
    call constructVector(largo,rigidez,f,n,m)   ! construye el vector para los resultados numericos

    ! muestra la matriz y factorizacion
    if (asserts) then
        call printMatrix(matriz, 'Matriz')  ! imprime en pantalla la matriz
        call printVector(f, 'Vector')       ! imprime en pantallas el vector de terminos independientes

        ! factorizacion y resultados
        call fCholesky(matriz,n,m)          ! factorizada la matriz por cholesky
        call printMatrix(matriz ,'Matriz (factorizada)')

        ! resolucion del sistema
        call linearSystem(matriz,f,n,m)
        call linearSystem(matriz,f,n,m)

    ! muestra solo la solucion
    else

        ! factoriza y resuelve
        call fCholesky(matriz,n,m)
        call linearSystem(matriz,f,n,m)
        call linearSystem(matriz,f,n,m)
    end if

    ! cambio de vector a matriz solucion
    allocate(fMatriz(m,n))
    forma(1) = n
    forma(2) = m
    fMatriz = reshape(f,forma,order=orden)
    call printMatrix(fMatriz, 'Solucion numerica')
    call resNumericos(ancho,largo,fMatriz,n,m)

    ! calcula los resultados analiticos por Navier
    allocate(navier(m,n))
    call analiticaNavier(navier,ancho,largo,rigidez,n,m)
    call printMatrix(navier, 'Solucion analitica')
    call resAnaliticos(ancho,largo,navier,n,m)

    ! para que no se cierre el programa derepente
    write(*,*) "Gracias por usar el programa..."
    read(*,*)

    contains
    !< Imprime una linea, un texto centrado y una línea.
    ! Creando como un titulo.
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

    !< Imprime una matriz mas un texto como titulo.
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

    !< Imprime un vector mas un texto como titulo
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
End Program WALLTER

!< Imprime una línea de 60 caracteres.
!  Se usa para separar la informacion que se imprime en la pantalla
subroutine linea()
    write(*,*) "____________________________________________________________"
end subroutine

!< Analiza los argumentos cuando se ejecuto el programa.
!  -h -> help: imprime las opciones disponibles.
!  -a -> asserts: activa la impresion de los datos de la factorizacion.
subroutine commandLine(asserts)
    logical,intent(out):: asserts   !! cuando es verdadera se activa la impresion de los datos de factorizacion

    integer:: i                     !! numero de argumentos
    character(len=30):: cmd         !! el argumento que se analiza

    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile


    i = iargc()
    if(i>0) then
        do i=1,iargc()
            call getarg(i,cmd)
            if(cmd == '-h') then
                write(*,*) 'Las opciones disponibles son:'
                write(*,*) '-h para ver esta ayuda'
                write(*,*) '-a para activar la impresion de los datos de factorizacion'
                write(*,*) '-c [FILENAME] para usar un programa de configuracion con los datos'
                call exit(0)
            end if
            if(cmd == '-a') then
                write(*,*) 'Se ha activado la impresion de los datos de factorizacion.'
                asserts = .TRUE.
            end if
            if(cmd == '-c') then
                write(*,*) 'Se usara para los datos el siguiente config file: '
                call getarg(i+1,configFile)
                write(*,*) configFile
                exit
            end if
        end do

    end if
end subroutine

!< Imprime un mensaje de bienvenida
subroutine bienvenido()

    write(*,*) "                                  ,--,      ,--,           ,----,                     "
    write(*,*) "                               ,---.'|   ,---.'|         ,/   .`|                     "
    write(*,*) "           .---.   ,---,       |   | :   |   | :       ,`   .'  :   ,---,.,-.----.    "
    write(*,*) "          /. ./|  '  .' \      :   : |   :   : |     ;    ;     / ,'  .' |\    /  \   "
    write(*,*) "      .--'.  ' ; /  ;    '.    |   ' :   |   ' :   .'___,/    ,',---.'   |;   :    \  "
    write(*,*) "     /__./ \ : |:  :       \   ;   ; '   ;   ; '   |    :     | |   |   .'|   | .\ :  "
    write(*,*) " .--'.  '   \' .:  |   /\   \  '   | |__ '   | |__ ;    |.';  ; :   :  |-,.   : |: |  "
    write(*,*) "/___/ \ |    ' '|  :  ' ;.   : |   | :.'||   | :.'|`----'  |  | :   |  ;/||   |  \ :  "
    write(*,*) ";   \  \;      :|  |  ;/  \   \'   :    ;'   :    ;    '   :  ; |   :   .'|   : .  /  "
    write(*,*) " \   ;  `      |'  :  | \  \ ,'|   |  ./ |   |  ./     |   |  ' |   |  |-,;   | |  \  "
    write(*,*) "  .   \    .\  ;|  |  '  '--'  ;   : ;   ;   : ;       '   :  | '   :  ;/||   | ;\  \ "
    write(*,*) "   \   \   ' \ ||  :  :        |   ,/    |   ,/        ;   |.'  |   |    \:   ' | \.' "
    write(*,*) "    :   '  |--' |  | ,'        '---'     '---'         '---'    |   :   .':   : :-'   "
    write(*,*) "     \   \ ;    `--''                                           |   | ,'  |   |.'     "
    write(*,*) "      '---'                                                     `----'    `---'       "


    write(*,*) "Bienvenido -------> "
    write(*,'(20X,A)') "Pulse enter para continuar"
    read(*,*)
end subroutine

!< Solicita los datos necesarios para el calculo de la flecha.
subroutine datos(ancho,largo,espesor,rigidez,n,m)
    integer*4,intent(out):: n,m                 !! numero de puntos para discretizar la placa
    real*8,intent(out):: largo, ancho, espesor  !! dimensiones de la placa
    real*8,intent(out):: rigidez                !! rigidez a flexion -> D = E*t^3/(12(1-v^2))

    real*8:: Young                              !! Modulo de Young
    real*8:: poisson                            !! Coeficiente de Poisson

    logical:: exists = .FALSE.                  !! existe el archivo de configuracion?
    character(len=50):: label                   !! etiquetas del archivo de configuracion

    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    ! comprueba que se haya definido un archivo de configuracion y de que existe
    if(configFile /= '') then
        inquire(file=configFile,exist=exists)
        if(.NOT.exists) then
            write(*,*) 'No se ha encontrado el archivo de configuracion.'
            write(*,*)
        end if
    end if

    ! si existe se abre y se lee la informacion
    if(exists) then
        open(unit=24,file=configFile,status='old',action='read')    ! se abre
        ! se lee la informacion, quitando las etiquetas
        ! importante el orden
        read(24,*) label
        read(24,*) label,ancho
        read(24,*) label,largo
        read(24,*) label,espesor
        read(24,*) label
        read(24,*) label,n
        read(24,*) label,m
        read(24,*) label
        read(24,*) label,Young
        read(24,*) label,poisson

        ! se imprime los datos obtenidos por si el usuario los quiere cambiar
        write(*,*) "Datos config file ::"
        write(*,'(5X,A,f0.3,A,f0.3,A,f0.3)') "Datos placa ->",ancho," x ",largo," x ",espesor
        write(*,'(5X,A,i2,A,i2,A)') "Datos discretizacion [n,m] -> [",n,",",m,"]"
        write(*,'(5X,A,f0.1,A,f0.3)') "Datos material -> Young:: ",Young,"; poisson:: ",poisson

    else
        write(*,*) 'Se procede a pedir los datos al usuario.'
        write(*,*) 'Si se equivoca en algun numero podra cambiarlo al final de programa'

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
    end if

    ! MPa (N/mm3) -> 1000 KPa (kN/m2)
    rigidez = Young*1000*espesor**3
    rigidez = rigidez/(12*(1-poisson**2))

    ! EXPORT HTML
    call createHTML("resultados")
    call exportMaterial(Young,poisson)
    call exportPlaca(largo,ancho,espesor)
end subroutine

!< Calculo los terminos de la matriz de resultados numericos y
!  los coloca en su sitio. La matriz se almacena en banda.
subroutine constructMatriz(ancho,largo,matriz,n,m)
    real*8,dimension(n*m,n*m),intent(inout):: matriz
    real*8,intent(in):: ancho,largo
    integer,intent(in):: n,m

    real*8:: A,B,C,deltaX,deltaY
    integer:: i,j

    ! calculo de los coef A, B y C
    deltaX = ancho / (n + 1)
    deltaY = largo / (m + 1)
    B = 1 / (deltaX**2)
    C = 1 / (deltaY**2)
    A = -2 * (B + C)
    write(*,'(A,3(f0.2,X),A)') "[A,B,C] -> [ ",A,B,C,"]"

    !matriz y vector a cero
    do i=1,n*m
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
end subroutine

!< Construye el vector de resultados numericos
subroutine constructVector(largo,rigidez,vector,n,m)
    real*8,dimension(n*m),intent(inout):: vector
    real*8,intent(in):: largo,rigidez
    integer,intent(in):: n,m

    real*8:: deltaY,presion

    ! vector a cero
    do i=1,n*m
        vector(i) = 0
    end do

    deltaY = largo / (m + 1)

    ! construccion vector
    j = 1
    do i=1,n*m
        ! peso especifico agua = 10000 N / m3 -> 10 kN / m3
        presion = 10*(largo/2-j*deltaY)/rigidez

        !solo hasta la mitad
        if (presion < 0) then
            vector(i) = 0
        else
            vector(i) = presion
        end if

        ! las filas tienen la misma presion
        if(mod(i,m) == 0) j = j + 1
    end do
end subroutine

!< Factorización de Cholesky -> A = L * D * Transpose[L]
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

!< Resolucion del sistema de ecuaciones para una matriz en banda factorizada
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

!< Resolucion del sistema mediante el metodo de Navier
subroutine analiticaNavier(w,ancho,largo,rigidez,n,m)
    real*8,intent(in):: ancho,largo
    real*8,intent(in):: rigidez
    real*8,dimension(m,n),intent(out):: w
    integer,intent(in):: n, m

    integer:: i, j, r, s
    integer:: k, u
    real*8:: deltaX, deltaY, X, Y
    real*8:: p_ku, w_ku
    real*8:: pi = acos(-1.0d0)

    logical:: exists = .FALSE.                  !! existe el archivo de configuracion?
    character(len=50):: label                   !! etiquetas del archivo de configuracion

    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    ! comprueba que se haya definido un archivo de configuracion y de que existe
    if(configFile /= '') then
        inquire(file=configFile,exist=exists)
        if(.NOT.exists) then
            write(*,*) 'No se ha encontrado el archivo de configuracion.'
            write(*,*)
        end if
    end if

    if(exists) then
        open(unit=24,file=configFile,status='old',action='read')
        read(24,*) label
        read(24,*) label,r
        read(24,*) label,s
        close(24)

        write(*,*) "Los datos de ",configFile," son:"
        write(*,'(A,I4,I4,A)') "Número de iteraciones para el calculo analitico -> [r,s] = [",r,s,"]"
    else
        ! datos para el bucle de calculo de flecha
         ! Datos tecnicos de la placa
        write(*,*) "*************************"
        write(*,*) "* NUMERO DE ITERACIONES *"
        write(*,*) "*************************"
        write(*,*) "-> Numero de iteraciones para el calculo analitico: "
        write(*,'(A,$)') 'n (interger): '
        read(*,*) r
        write(*,'(A,$)') 'm (interger): '
        read(*,*) s
    end if



    !calculamos deltaX y deltaY
    deltaX = ancho / (n + 1)
    deltaY = largo / (m + 1)

    ! ceros en w
    do i=1,n
        do j=1,m
            w(j,i) = 0
        end do
    end do
    !bucle para cada x e y
    do i=1,n
        X = i*deltaX
        do j=1,m
            Y = j*deltaY
            !bucle para el calculo de la flecha
            do k=1,r
                do u=1,s
                    p_ku = 4 * largo * 10 * sin(k*pi/2)**2 / (k * u**2 * pi**3)
                    p_ku = p_ku * (u * pi - 2 * sin(u*pi/2))

                    w_ku =(k**2 / ancho**2 + u**2 / largo**2)
                    w_ku = w_ku**2
                    w_ku = p_ku / (pi**4 * rigidez * w_ku)

                    w(j,i) = w(j,i) + w_ku * sin(k*pi*X/ancho) * sin(u*pi*Y/largo)
                end do
            end do
        end do
    end do

end subroutine


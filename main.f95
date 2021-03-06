!< author: francisco rivera alvarez
!  Programa para el calculo de flechas de una placa apoyada
!  en sus cantos con carga hidraulica hasta la mitad de su largo
Program WALLTER

    real*8, dimension(:,:),allocatable:: matriz !! sistema de ecuaciones
    real*8, dimension(:),allocatable:: f        !! vector terminos independientes
    real*8,dimension(:,:),allocatable:: navier  !! vector terminos analiticos

    real*8, dimension(:,:),allocatable:: fMatriz!! solucion en forma matricial

    character(len=50):: resultsFile = './result/'    !! directorio donde se escribiran los resultados
    character(len=50):: configFile = ''                             !! directorio donde se encuentran los config file
    logical:: asserts = .FALSE.                 !! si se activa se imprimen los datos de la factorizacion
    logical:: exists                            !! comprueba la existencia de los directorios de resultados

    real*8:: ancho,largo,espesor                !! dimensiones de la placa
    real*8:: rigidez                            !! rigidez a flexion de la placa
    integer:: n, m                              !! numero de puntos para discretizar largo(n) y ancho(m)

    COMMON resultsFile,configFile               !! variables globales

    ! comprueba si existe el directorio de resultados
    inquire(file=resultsFile,exist=exists)
    if(.NOT.exists) resultsFile = 'resultados.html' ! si no existe crea el archivo al lado del programa
    if(exists) resultsFile = TRIM(resultsFile) // TRIM('resultados.html')

    ! inicio del programa
    call commandLine(asserts)                   ! opciones de ejecucion
    call bienvenido()                           ! mensaje de bienvenida (header)
    call datos(ancho,largo,espesor,rigidez,n,m) ! datos para poder funcionar el programa

    allocate(matriz(n*m,n+1))                   ! fija la matriz de resultados numericos
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
    allocate(fMatriz(n,m))
    call vectorToMatrix(f,fMatriz,n,m)
    call printMatrix(fMatriz, 'Solucion numerica')
    call resNumericos(ancho,largo,fMatriz,n,m)

    ! calcula los resultados analiticos por Navier
    allocate(navier(n,m))
    call analiticaNavier(navier,ancho,largo,rigidez,n,m)
    call printMatrix(navier, 'Solucion analitica')
    call resAnaliticos(ancho,largo,navier,n,m)

    ! para que no se cierre el programa derepente
    write(*,*) "Gracias por usar el programa..."
    read(*,*)

    contains
    !< Imprime una linea, un texto centrado y una l�nea.
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

!< Imprime una l�nea de 60 caracteres.
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
    ! ------------- Variables ----------------- !
    integer*4,intent(out):: n,m                 !! numero de puntos para discretizar la placa
    real*8,intent(out):: largo, ancho, espesor  !! dimensiones de la placa
    real*8,intent(out):: rigidez                !! rigidez a flexion -> D = E*t^3/(12(1-v^2))

    real*8:: Young                              !! Modulo de Young
    real*8:: poisson                            !! Coeficiente de Poisson

    logical:: exists = .FALSE.                  !! existe el archivo de configuracion?
    character(len=50):: label                   !! etiquetas del archivo de configuracion
    real*8:: numero = 0

    integer*4,dimension(7):: modif = 0
    integer:: i
    integer:: sel=1
    character(len=7),dimension(7):: etiquetas = &
    (/"ancho  ","largo  ","espesor","n      ","m      ","Young  ","poisson"/)

    integer*4:: ios=0
    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    ! ************** Codigo ****************** !

    ! comprueba que se haya definido un archivo de configuracion y de que existe
    if(configFile /= '') then
        inquire(file=configFile,exist=exists)
        if(.NOT.exists) then
            write(*,*) 'No se ha encontrado el archivo de configuracion.'
        end if
    end if

    ! si existe se abre y se lee la informacion
    if(exists) then
        open(unit=24,file=configFile,status='old',action='read')    ! se abre
        ! se lee la informacion, quitando las etiquetas
        ! importante poner las etiquetas como en los archivos ejemplo

        do while(ios>=0)
            read(24,*,iostat=ios) label, numero

            ! check if there were problems reading the file
            if(ios < 0) then
                continue
            end if

            ! check label reference
            if(label=='ancho::') then
                ancho = numero
                modif(1) = 1
            end if
            if(label=='largo::') then
                largo = numero
                modif(2) = 1
            end if
            if(label=='espesor::') then
                espesor = numero
                modif(3) = 1
            end if
            if(label=='n::') then
                n = numero
                modif(4) = 1
            end if
            if(label=='m::') then
                m = numero
                modif(5) = 1
            end if
            if(label=='young::') then
                Young = numero
                modif(6) = 1
            end if
            if(label=='poisson::') then
                poisson = numero
                modif(7) = 1
            end if
        end do
        close(24)

        ! se imprime los datos obtenidos por si el usuario los quiere cambiar
        write(*,*) "Datos config file ::"
        write(*,'(5X,A,f0.3,A,f0.3,A,f0.3)') "Datos placa ->",ancho," x ",largo," x ",espesor
        write(*,'(5X,A,i2,A,i2,A)') "Datos discretizacion [n,m] -> [",n,",",m,"]"
        write(*,'(5X,A,f0.1,A,f0.3)') "Datos material -> Young:: ",Young,"; poisson:: ",poisson

    endif

    call linea()
    write(*,*) 'Se procede a pedir los datos al usuario que falten.'
    write(*,*) 'Si se equivoca en algun numero podra cambiarlo al final de programa.'

    do while(sel==1)
        ! Datos tecnicos de la placa
        if(modif(1)==0 .OR. modif(2)==0 .OR. modif(3)==0) then
            write(*,*) "*************************"
            write(*,*) "*   DATOS DE LA PLACA   *"
            write(*,*) "*************************"
            if(modif(1)==0) then
                write(*,*) "-> ancho de la placa"
                write(*,'(A,$)') "(metros) "
                read(*,*) ancho; modif(1) = 1
            end if
            if (modif(2)==0) then
                write(*,*) "-> largo de la placa"
                write(*,'(A,$)') "(metros) "
                read(*,*) largo; modif(2) = 1
            end if
            if (modif(3)==0) then
                write(*,*) "-> espesor de la placa"
                write(*,'(A,$)') "(metros) "
                read(*,*) espesor; modif(3) = 1
            end if
        end if

        ! datos para la discretizacion del modelo
        if(modif(4)==0 .OR. modif(5)==0) then
            write(*,*) "*************************"
            write(*,*) "*   NUMERO DE PUNTOS    *"
            write(*,*) "*************************"
            if(modif(4)==0) then
                write(*,*) "-> puntos para discretizar el largo"
                write(*,'(A,$)') "(integer) "
                read(*,*) n; modif(4) = 1
            endif
            if(modif(5)==0) then
                write(*,*) "-> puntos para discretizar el ancho"
                write(*,'(A,$)') "(integer) "
                read(*,*) m; modif(5) = 1
            endif
        endif

        ! propiedades del material
        if(modif(6)==0 .OR. modif(7)==0) then
            write(*,*) "*************************"
            write(*,*) "*   DATOS DEL MATERIAL  *"
            write(*,*) "*************************"
            if(modif(6)==0) then
                write(*,*) "-> Modulo de Young"
                write(*,'(A,$)') "(MPa) "
                read(*,*) Young; modif(6) = 1
            endif
            if(modif(7)==0) then
                write(*,*) "-> Coef. de poisson"
                write(*,'(A,$)') "(_real_) "
                read(*,*) poisson; modif(7) = 1
            endif
        endif

        ! cambiar valores si se han introducido valores a mano
        ! si hay un archivo se entiende que el usuario no tiene
        ! la necesidad de cambiar ningun dato.
        if(.NOT.exists) then
            call linea()
            write(*,*) "Desea cambiar algun valor:"
            write(*,*) "1) Si"
            write(*,*) "2) No"
            read(*,*) sel
            if(sel==1) then
                write(*,"(5X,A)") "Que valor desea cambiar?"
                do i = 1,7
                    write(*,"(10X,I2,A,A)") i,") ",etiquetas(i)
                end do
                read(*,*) i
                modif(i) = 0
            end if
        else
            sel = 20
        end if
    end do

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
    ! ------------- Variables ----------------- !
    real*8,dimension(n*m,n+1),intent(inout):: matriz
    real*8,intent(in):: ancho,largo
    integer,intent(in):: n,m

    real*8:: A,B,C,deltaX,deltaY
    integer:: i

    ! calculo de los coef A, B y C
    deltaX = ancho / (m + 1)
    deltaY = largo / (n + 1)
    B = 1 / (deltaX**2)
    C = 1 / (deltaY**2)
    A = -2 * (B + C)
    write(*,'(A,3(f0.2,X),A)') "[A,B,C] -> [ ",A,B,C,"]"

    !matriz y vector a cero
    matriz(:,:) = 0


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
    ! ------------- Variables ----------------- !
    real*8,dimension(n*m),intent(inout):: vector
    real*8,intent(in):: largo,rigidez
    integer,intent(in):: n,m
    integer:: i,j,t

    real*8:: deltaY,presion

    ! vector a cero
    vector(:) = 0

    deltaY = largo / (n + 1)

    ! construccion vector
    t = 1
    do j=1,m
        do i=1,n
            ! peso especifico agua = 10000 N / m3 -> 10 kN / m3
            presion = 10*(largo/2-i*deltaY)/rigidez

            !solo hasta la mitad
            if (presion > 0 .AND. mod(t-1,n)==0) then
                vector(t) = presion
            end if
            t = t + 1
        end do
    end do
end subroutine

!< Factorizaci�n de Cholesky -> A = L * D * Transpose[L]
subroutine fCholesky(matriz, n, m)
    ! ------------- Variables ----------------- !
    real*8,dimension(n*m,n+1),intent(inout):: matriz
    integer:: k,i,j
    real*8:: suma

    ! factorizacion de cholesky
    do k=1,n*m-1
        do i=k+1-n+1,k
            if(i<1) cycle
            suma = 0
            do j=MAX(i-n,k+1-n),i-1
                if(j<1) cycle
                suma = suma + matriz(j,i-j+1)*matriz(j,k+2-j)
            end do
            matriz(i,k+2-i) = (matriz(i,k+2-i) - suma)
        end do
        do i=k+1-n,k
            if(i<1) cycle
            matriz(i,k+2-i) = matriz(i,k+2-i)/matriz(i,1)
        end do
        suma = 0
        do j=k+1-n,k
            if(j<1) cycle
            suma = suma + matriz(j,k+2-j)*matriz(j,1)*matriz(j,k+2-j)
        end do
        matriz(k+1,1) = matriz(k+1,1) - suma
    end do
end subroutine

!< Resolucion del sistema de ecuaciones para una matriz en banda factorizada
subroutine linearSystem (matriz, f, n, m)
    ! ------------- Variables ----------------- !
    real*8,dimension(n*m,n*m),intent(inout):: matriz
    real*8,dimension(n*m),intent(inout):: f
    integer,intent(in):: n, m
    integer:: i,j
    real*8:: suma = 0

    do i=2,n*m
        suma = 0
        do j=i-n,i-1
            if(j<1) cycle
            suma = suma + matriz(j,i-j+1)*f(j)
        end do
        f(i) = f(i) - suma
    end do

    do i=1,n*m
        f(i) = f(i) / matriz(i,1)
    end do

    do i=n*m,2,-1
        do j=i-n,i-1
            if(j<1) cycle
            f(j) = f(j) - matriz(j,i-j+1)*f(i)
        end do
    end do
end subroutine

!< Resolucion del sistema mediante el metodo de Navier
subroutine analiticaNavier(w,ancho,largo,rigidez,n,m)
    ! ------------- Variables ----------------- !
    real*8,intent(in):: ancho,largo
    real*8,intent(in):: rigidez
    real*8,dimension(n,m),intent(out):: w
    integer,intent(in):: n, m

    integer:: i, j, r, s
    integer:: k, u
    real*8:: deltaX, deltaY, X, Y
    real*8:: p_ku, w_ku
    real*8:: pi = acos(-1.0d0)

    integer,dimension(2):: modif = 0
    integer:: sel=1
    integer:: h

    logical:: exists = .FALSE.                  !! existe el archivo de configuracion?
    character(len=50):: label                   !! etiquetas del archivo de configuracion
    real*8:: numero

    integer:: ios
    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    ! ************** Codigo ****************** !

    ! comprueba que se haya definido un archivo de configuracion y de que existe
    if(configFile /= '') then
        inquire(file=configFile,exist=exists)
        if(.NOT.exists) then
            write(*,*) 'No se ha encontrado el archivo de configuracion.'
            write(*,*)
        end if
    end if

    if(exists) then
        open(unit=24,file=configFile,status='old',action='read')    ! se abre
        do while(ios>=0)
            read(24,*,iostat=ios) label, numero

            ! check if there were problems reading the file
            if(ios < 0) then
                write(*,*) "Fin de archivo"
                continue
            end if

            ! check label reference
            if(label=='r::') then
                r = numero
                modif(1) = 1
            end if
            if(label=='s::') then
                s = numero
                modif(2) = 1
            end if
        end do
        close(24)

        write(*,*) "Los datos de ",configFile," son:"
        write(*,'(A,I4,I4,A)') "Numero de iteraciones para el calculo analitico -> [r,s] = [",r,s,"]"
    end if

    do while(sel==1)
        ! datos para el bucle de calculo de flecha
        if (modif(1) == 0 .OR. modif(2) == 0) then
            write(*,*) "*************************"
            write(*,*) "* NUMERO DE ITERACIONES *"
            write(*,*) "*************************"
            write(*,*) "-> Numero de iteraciones para el calculo analitico: "
            if(modif(1) == 0) then
                write(*,'(A,$)') 'r (interger): '
                read(*,*) r; modif(1) = 1
            end if
            if(modif(2) == 0) then
                write(*,'(A,$)') 's (interger): '
                read(*,*) s; modif(2) = 1
            end if
        end if

        ! cambiar valores si se han introducido valores a mano
        ! si hay un archivo se entiende que el usuario no tiene
        ! la necesidad de cambiar ningun dato.
        if(.NOT.exists) then
            call linea()
            write(*,*) "Desea cambiar algun valor:"
            write(*,*) "1) Si"
            write(*,*) "2) No"
            read(*,*) sel
            if(sel==1) then
                write(*,"(5X,A)") "Que valor desea cambiar?"
                write(*,"(10X,A)") "1) r"
                write(*,"(10X,A)") "2) s"
                read(*,*) h
                modif(h) = 0
            end if
        else
            sel = 20
        end if
    end do




    !calculamos deltaX y deltaY
    deltaX = ancho / (m + 1)
    deltaY = largo / (n + 1)

    ! ceros en w
    w(:,:) = 0

    !bucle para cada x e y
    do j=1,m
        X = j*deltaX
        do i=1,n
            Y = i*deltaY

            !bucle para el calculo de la flecha
            do k=1,r
                do u=1,s
                    p_ku = 4 * largo * 10 * sin(k*pi/2)**2 / (k * u**2 * pi**3)
                    p_ku = p_ku * (u * pi - 2 * sin(u*pi/2))

                    w_ku =(k**2 / ancho**2 + u**2 / largo**2)
                    w_ku = w_ku**2
                    w_ku = p_ku / (pi**4 * rigidez * w_ku)

                    w(i,j) = w(i,j) + w_ku * sin(k*pi*X/ancho) * sin(u*pi*Y/largo)
                end do
            end do
        end do
    end do

end subroutine

!< Convierte el vector solución en matriz
subroutine vectorToMatrix(vector,matrix,n,m)
    real*8,dimension(n*m),intent(in):: vector
    real*8,dimension(n,m),intent(out):: matrix
    integer:: i,j,t
    ! llena la matriz con ceros
    matrix(:,:)=0

    ! coloca los elementos del vector en la correcta posicion en la matriz
    t = 1           ! posicion en el vector
    do j=1,m
        do i=1,n
            matrix(i,j) = vector(t)
            t = t + 1
        end do
    end do
end subroutine

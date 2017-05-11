@echo off

ECHO.
ECHO ...............................................
ECHO Selecciona el modo de ejeccucion
ECHO ...............................................
ECHO.
ECHO 1 - Ejecuccion por defecto
ECHO 2 - Imprimir resultados factorizacion
ECHO 3 - Usar achivo de configuracion
ECHO 4 - EXIT
ECHO.

:MENU
SET /P M=Escribe 1, 2, 3 o 4 y pulsa ENTER: 
IF %M%==1 GOTO NORMAL
IF %M%==2 GOTO FACTORIZATION
IF %M%==3 GOTO CONFIGURATION
IF %M%==4 GOTO EOF

:NORMAL
start "Wallter" "bin/Release/wallter.exe"
GOTO MENU

:FACTORIZATION
start "Wallter" "bin/Release/wallter.exe" -a
GOTO MENU

:CONFIGURATION
setlocal EnableDelayedExpansion
SET n=1
FOR /r %%i IN (config/*.txt) DO (
ECHO !n! - %%~nxi
SET vector[!n!]=%%~nxi 
SET /A n=n+1
)
set /p sel=numero archivo: 
start "Wallter" "bin/Release/wallter.exe" -c "./config/!vector[%sel%]!"
GOTO MENU

:EOF
PAUSE
#!/bin/bash
i = 0
PS3='Por facor escoga el modo de ejecuccion: '
options=("Por defecto" "Imprimir resultados factorizacion" "Escoger archivo de configuracion" "Salir")
select opt in "${options[@]}"
do
    case $opt in
        "Por defecto")
            echo "Ha escogido escoger el programa con las opciones por defecto."
			./bin/Release/wallter.exe 
            ;;
        "Imprimir resultados factorizacion")
            echo "Ha escogido imprimir los resultados de la factorizacion."
			./bin/Release/wallter.exe -a
            ;;
        "Escoger archivo de configuracion")
            echo "Escoga el archivo de configuracion:"
			echo "./config/* :"
			for file in ./config/*; do
				((i+=1))
				names[i]="./config/${file##*/}"
				echo "$i) ${names[i]}"
			done
			read sel
			./bin/Release/wallter.exe -c ${names[$sel]}
            ;;
        "Salir")
            break
            ;;
        *) echo invalid option;;
    esac
done

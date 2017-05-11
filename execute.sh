#!/bin/bash
i = 0
PS3='Por facor escoga el modo de ejecuccion: '
options=("Por defecto" "Imprimir resultados factorizacion" "Escoger archivo de configuracion" "Salir")
select opt in "${options[@]}"
do
    case $opt in
        "Por defecto")
            echo "Ha escogido escoger el programa con las opciones por defecto."
			./wallter.exe 
            ;;
        "Imprimir resultados factorizacion")
            echo "Ha escogido imprimir los resultados de la factorizacion."
			./wallter.exe -a
            ;;
        "Escoger archivo de configuracion")
            echo "Escoga el archivo de configuracion de la carpeta config que desee:"
			for file in ./config/*.txt; do
				((i+=1))
				names[i]="./config/${file##*/}"
				echo "$i) ${names[i]}"
			done
			read sel
			./wallter.exe -c ${names[$sel]}
            ;;
        "Salir")
            break
            ;;
        *) echo invalid option;;
    esac
done

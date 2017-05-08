subroutine createHTML(name)
    character(len=*),intent(in):: name

    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile


    open(unit=12,file=resultsFile)
    write(12,'(A,$)') '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"'
    write(12,*) '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'
    write(12,*) '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'
    write(12,*) '<head>'
	write(12,'(5X,3A)') '<title>',name,'</title>'
	write(12,'(5X,A)') '<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />'
	write(12,'(5X,A)') '<meta name="description" content="" />'
	write(12,'(5X,A)') '<meta name="keywords" content="" />'
	write(12,'(5X,A)') '<meta name="robots" content="index,follow" />'
	write(12,'(5X,A)') '<link rel="stylesheet" type="text/css" href="./css/style.css" />'
	write(12,'(5X,A)') '<script src="https://ajax.aspnetcdn.com/ajax/jQuery/jquery-3.2.1.min.js">'
	write(12,'(5X,A)') '</script>'
	write(12,'(5X,A)') '<script src="https://cdn.jsdelivr.net/clipboard.js/1.6.0/clipboard.min.js">'
	write(12,'(5X,A)') '</script>'
    write(12,*) '</head>'
    write(12,*) '<body>'
    write(12,'(5X,A)') '<div class="row">'
    write(12,'(10X,A)') '<div class="header">'
    write(12,'(15X,A)') '<h1> Resultados del programa </h1>'
    write(12,'(15X,A)') '<button id="bNumerico">Resultados Numericos</button>'
	write(12,'(15X,A)') '<button id="bAnalitico">Resultados Analiticos</button>'
    write(12,'(10X,A)') '</div>'
	write(12,'(5X,A)') '</div>'
	close(12)
end subroutine

subroutine exportMaterial(Young,poisson)
    real*8,intent(in):: Young,poisson
    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    open(unit=12,file=resultsFile,status='old',position="append")
    write(12,'(5X,A)') '<div class="row">'
    write(12,'(10X,A)') '<div class="box-item">'
    write(12,'(15X,A)') '<header> Material </header>'
    write(12,'(15X,A)') '<p> El material empleado es : 	</p>'
    write(12,'(15X,A)') '<table>'
    write(12,'(20X,A,F0.3,A)') '<tr><td>E = ',Young,' MPa</td></tr>'
    write(12,'(20X,A,F4.3,A)') '<tr><td>v = ',poisson,'</td></tr>'
    write(12,'(15X,A)') '</table>'
    close(12)
end subroutine

subroutine exportPlaca(largo,ancho,espesor)
    real*8,intent(in):: largo,ancho,espesor
    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    open(unit=12,file=resultsFile,status='old',position="append")
    write(12,'(15X,A)') '<header> Placa </header>'
    write(12,'(15X,A)') '<p>Las medidas de la placa son:</p>'
    write(12,'(15X,A)') '<table>'
    write(12,'(20X,A,F6.3,A)') '<tr><td>Largo = ',largo,' m</td></tr>'
    write(12,'(20X,A,F6.3,A)') '<tr><td>Ancho = ',ancho,' m</td></tr>'
    write(12,'(20X,A,F4.3,A)') '<tr><td>Espesor = ',espesor,' m</td></tr>'
    write(12,'(15X,A)') '</table>'
    write(12,'(10X,A)') '</div>'
    close(12)
end subroutine

subroutine resAnaliticos(navier,n,m)
    real*8,dimension(n,m),intent(in):: navier
    integer,intent(in):: n,m
    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    open(unit=12,file=resultsFile,status='old',position="append")
    write(12,'(10X,A)') '<div class="box-item" id="resultados">'
    write(12,'(15X,A)') '<header id="azul"> Resultados </header>'
    write(12,'(15X,A)') '<table id="tAnalitico">'
    write(12,'(20X,A)') '<tr class="tr-h">'
    write(12,'(25X,A,I5,A)') '<td rowspan="',m+2,'"> Y </th>'
    write(12,'(20X,A)') '</tr>'

    do i=1,n
        write(12,'(20X,A)') '<tr>'
        write(12,'(25X,A,I5,A)') '<td class="td-h">',i*2,'</td>'
        write(12,'(25X,*(A,f0.4,A))') ('<td>',navier(i,j),'</td>',j=1,m)
        write(12,'(20X,A)') '</tr>'
    end do

    write(12,'(20X,A)') '<tr class="tr-h">'
    write(12,'(25X,A)') '<td>/</td>'
    write(12,'(25X,*(A,I5,A))') ('<td>',j*2,'</td>',j=1,m)
    write(12,'(20X,A)') '</tr>'
    write(12,'(20X,A)') '<tr class="tr-h">'
    write(12,'(25X,A,I4,A)') '<td>0</td><td colspan="',n+1,'">X</td>'
    write(12,'(20X,A)') '</tr>'
    write(12,'(15X,A)')'</table>'
    close(12)
end subroutine

subroutine resNumericos (fMatriz,n,m)
    real*8,dimension(n,m),intent(in):: fMatriz
    integer,intent(in):: n,m
    character(len=50):: resultsFile,configFile
    COMMON resultsFile,configFile

    open(unit=12,file=resultsFile,status='old',position="append")
    write(12,'(15X,A)') '<table id="tNumerico">'
    write(12,'(20X,A)') '<tr class="tr-h">'
    write(12,'(25X,A,I5,A)') '<td rowspan="',m+2,'"> Y </th>'
    write(12,'(20X,A)') '</tr>'

    do i=1,n
        write(12,'(20X,A)') '<tr>'
        write(12,'(25X,A,I5,A)') '<td class="td-h">',i*2,'</td>'
        write(12,'(25X,*(A,f0.4,A))') ('<td>',fMatriz(i,j),'</td>',j=1,m)
        write(12,'(20X,A)') '</tr>'
    end do

    write(12,'(20X,A)') '<tr class="tr-h">'
    write(12,'(25X,A)') '<td>/</td>'
    write(12,'(25X,*(A,I5,A))') ('<td>',j*2,'</td>',j=1,m)
    write(12,'(20X,A)') '</tr>'
    write(12,'(20X,A)') '<tr class="tr-h">'
    write(12,'(25X,A,I4,A)') '<td>0</td><td colspan="',n+1,'">X</td>'
    write(12,'(20X,A)') '</tr>'
    write(12,'(15X,A)')'</table>'

    write(12,'(10X,A)') '<button id="copyButton" data-clipboard-action="copy" data-clipboard-target=".active">'
    write(12,'(15X,A)') 'Copia la tabla</button>'
	write(12,'(10X,A)') '</div>'
	write(12,'(5X,A)') '</div>'
	write(12,'(5X,A)') '<script src="js/buttons.js"></script>'
    write(12,*) '</body>'
    write(12,*) '</html>'
    close(12)
end subroutine



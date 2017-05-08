/* clipboard.js
	Realiza la accion designada con el atributo
	data-clipboard-action="copy"
	cuando el boton se active y el objetivo marcado
	con el atributo 
	data-clipboard-target="someSelector"
	
	Usaremos la clase active como selector del boton.
	Añadiremos esa clase a la tabla visible en el momento
	y el boton copiara solo esa tabla.
*/
var clipboard = new Clipboard("#copyButton");
	
var analitico = $("#tAnalitico");				// tabla de resultados analiticos
var numerico = $("#tNumerico");					// tabla de resultados numericos

$(document).ready(function () {
	// esconde la tabla de resultados analiticos
	analitico.hide();
	
	// muestra la tabla de resultados numericos
	numerico.show();
	numerico.addClass("active");	// añade la clase active para el boton copiar
});

$("#bNumerico").click(function () {
	// esconde y no deja copiar la tabla de resultados analiticos
	analitico.hide();
	if(analitico.hasClass("active")) analitico.removeClass("active");
	
	// muestra la tabla de resultados numericos
	numerico.show();
	if(!numerico.hasClass("active")) numerico.addClass("active");
});
		
$("#bAnalitico").click(function () {
	// esconde y no deja copiar la tabla de resultados analiticos
	numerico.hide();
	if(numerico.hasClass("active")) numerico.removeClass("active");
	
	// muestra la tabla de resultados numericos
	analitico.show();
	if(!analitico.hasClass("active")) analitico.addClass("active");
});


$( document ).ready(      
  	function getFilename(){
  		$(document).on("shiny:value", function(e) {
  			if (e.name == "filename") {  
        		return document.getElementById('filename').value;
        	}
        });	
	}
);
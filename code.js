/* program the keyboard "Enter" key*/

document
    .addEventListener("keydown",function(event){
        if(event.key === "Enter"){
            event.preventDefault();
            document.getElementById("search").click();
        }
    })
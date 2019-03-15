var calledOn;


// var acquireVsCodeApi = "sdasdasdas";
function acquireVsCodeApi (){
    let obj = {};
    obj.postMessage = function(message){

        calledOn = message;
        
    }
    return obj;
}


// export function acquireVsCodeApi(){
//     console.log("doing nothing")
// }
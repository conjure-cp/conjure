declare var saveAs: any;
declare var d3: any;

export default class Img {
    public static  writeImage() {

        function getSVGString(svgNode: any) {
            svgNode.setAttribute('xlink', 'http://www.w3.org/1999/xlink');
            var cssStyleText = getCSSStyles(svgNode);
            appendCSS(cssStyleText, svgNode);
            var serializer = new XMLSerializer();
            var svgString = serializer.serializeToString(svgNode);
            svgString = svgString.replace(/(\w+)?:?xlink=/g, 'xmlns:xlink='); 
            svgString = svgString.replace(/NS\d+:href/g, 'xlink:href'); 
            return svgString;
            function getCSSStyles(parentElement: any) {
                var selectorTextArr = [];
                
                selectorTextArr.push('#' + parentElement.id);
                for (var c = 0; c < parentElement.classList.length; c++)
                    if (!contains('.' + parentElement.classList[c], selectorTextArr))
                        selectorTextArr.push('.' + parentElement.classList[c]);
               
                var nodes = parentElement.getElementsByTagName("*");
                for (var i = 0; i < nodes.length; i++) {
                    var id = nodes[i].id;
                    if (!contains('#' + id, selectorTextArr))
                        selectorTextArr.push('#' + id);
                    var classes = nodes[i].classList;
                    for (var c = 0; c < classes.length; c++)
                        if (!contains('.' + classes[c], selectorTextArr))
                            selectorTextArr.push('.' + classes[c]);
                }
            s
                var extractedCSSText = "";
                for (var i = 0; i < document.styleSheets.length; i++) {
                    var s = document.styleSheets[i];
                    if (!(s instanceof CSSStyleSheet)) {
                        continue;
                    }
                    try {
                        if (!s.cssRules) continue;
                    } catch (e) {
                        if (e.name !== 'SecurityError') throw e; 
                        continue;
                    }
                    var cssRules = s.cssRules;
                    for (var r = 0; r < cssRules.length; r++) {

                        console.log(cssRules[r]);

                        extractedCSSText += cssRules[r].cssText;
                    }
                }

                return extractedCSSText;
                function contains(str: string, arr: any) {
                    return arr.indexOf(str) === -1 ? false : true;
                }
            }
            function appendCSS(cssText: string, element: any) {
                var styleElement = document.createElement("style");
                styleElement.setAttribute("type", "text/css");
                styleElement.innerHTML = cssText;
                var refNode = element.hasChildNodes() ? element.children[0] : null;
                element.insertBefore(styleElement, refNode);
            }
        }

        function svgString2Image(svgString: string, width: number, height: number, format: string, callback: any) {
            var format = format ? format : 'png';
            var imgsrc = 'data:image/svg+xml;base64,' + btoa(unescape(encodeURIComponent(svgString))); 
            var canvas = document.createElement("canvas");
            var context = canvas.getContext("2d");
            canvas.width = width;
            canvas.height = height;
            var image = new Image();
            image.onload = function () {
                context.clearRect(0, 0, width, height);
                context.drawImage(image, 0, 0, width, height);
                canvas.toBlob(function (blob:any) {
                    var filesize = Math.round(blob.length / 1024) + ' KB';
                    if (callback) callback(blob, filesize);
                });

            };
            image.src = imgsrc;
        }

        var svgString = getSVGString(d3.select("#theTree").node());

        if ($("#wantSVG").is(":checked")) {

            let blob = new Blob([svgString], { type: "text/plain;charset=utf-8" });
            saveAs(blob, "tree.svg");
        }
        else {
            svgString2Image(svgString, 4 * 1920, 4 * 1080, 'png',
                function save(dataBlob: any) {
                    saveAs(dataBlob, 'tree.png'); 
                })
        }
    }
}

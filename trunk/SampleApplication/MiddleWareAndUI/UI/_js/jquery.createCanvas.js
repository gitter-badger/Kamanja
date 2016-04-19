/**

   Copyright 2015 ligaDATA

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
/**
 * This method gets the SVG of the chart and uses canvg to draw it on a HTML5 canvas.
 * The canvas is then streamed to the browser window as a download.
 */
(function(H) {

    H.Chart.prototype.createCanvas = function(divId) {
        var svg = this.getSVG(),
            width = parseInt(svg.match(/width="([0-9]+)"/)[1]),
            height = parseInt(svg.match(/height="([0-9]+)"/)[1]),
            canvas = document.createElement('canvas');

        canvas.setAttribute('width', width);
        canvas.setAttribute('height', height);

        if (canvas.getContext && canvas.getContext('2d')) {

            canvg(canvas, svg);

            var image = canvas.toDataURL("image/png")
                .replace("image/png", "image/octet-stream");

            // Save locally
            window.location.href = image;
        } else {
            alert("Your browser doesn't support this feature, please use a modern browser");
        }

    };
}(Highcharts));
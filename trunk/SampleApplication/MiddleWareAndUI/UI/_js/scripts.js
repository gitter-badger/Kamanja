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
function formatNumber(num) {

    return num.toString().replace(/(\d)(?=(\d\d\d)+(?!\d))/g, "$1,");
}


/*** no. formatting ***/

function numberAbbreviations(number, decPlaces) {

    number = parseInt(number);

    // 2 decimal places => 100, 3 => 1000, etc
    decPlaces = Math.pow(10, decPlaces);

    // Enumerate number abbreviations
    var abbrev = ["k", "m", "b", "t"];

    // Go through the array backwards, so we do the largest first
    for (var i = abbrev.length - 1; i >= 0; i--) {

        // Convert array index to "1000", "1000000", etc
        var size = Math.pow(10, (i + 1) * 3);

        // If the number is bigger or equal do the abbreviation
        if (size <= number) {
            // Here, we multiply by decPlaces, round, and then divide by decPlaces.
            // This gives us nice rounding to a particular decimal place.
            number = Math.round(number * decPlaces / size) / decPlaces;

            // Handle special case where we round up to the next abbreviation
            if ((number == 1000) && (i < abbrev.length - 1)) {
                number = 1;
                i++;
            }

            // Add the letter for the abbreviation
            number += abbrev[i];

            // We are done... stop
            break;
        }
    }

    return number;
}

function getRandomArbitrary(min, max) {

    return parseInt(Math.random() * (max - min) + min);
}

function prepareDrawChart(id) {

    switch (id) {
    case 'eb1':

        drawChart('Emergency Borrowing 1', '693 Queued', 'eb1', '#FFD700');
        break;
    case 'eb2':

        drawChart('Emergency Borrowing 2', '693 Queued', 'eb2', '#66CD00');
        break;
    case 'nod1':

        drawChart('Near Overdraft 1', '693 Queued', 'nod1', '#FF8C00');
        break;
    case 'od1':

        drawChart('Overdraft 1', '693 Queued', 'od1', '#FF0000');
        break;
    case 'od2':

        drawChart('Overdraft 2', '693 Queued', 'od2', '#9400D3');
        break;
    case 'od3':

        drawChart('Overdraft 3', '693 Queued', 'od2', '#8B8878');
        break;
    case 'lb':

        drawChart('Low Balance', '693 Queued', 'lb', '#3A5FCD');
        break;
//    case 'f':
//
//        drawChart('Fraud - 100 Mile Multiple Transactions', '693 Queued', 'lb', '#FFFF00');
//        break;
    default:
    }
}

//charts

function drawChart(title, subtitle, code, color) {

    $('#chart-container').highcharts({
        chart: {
            type: 'line',
            color: "red"
        },
        title: {
            text: title
        },
        subtitle: {
            text: subtitle
        },
        xAxis: {
            categories: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        yAxis: {
            title: {
                text: null
            }
        },
        plotOptions: {
            line: {
                dataLabels: {
                    enabled: true
                },
                enableMouseTracking: true
            }
        },
        series: [
            {
                name: code.toUpperCase(),
                data: [800, 1000, 1100, 1300, 900, 1010, 1200, 600, 300, 1010, 800, 900, 800, 1000, 1100, 1300, 900, 1010, 1200, 600, 300, 1010, 800, 900, 800, 1000, 1100, 1300, 900, 1010],
                color: color,
            }
        ],
//        tickInterval: 30 * 24 * 3600 * 1000
    });
}

$(function() {

    var a = [
        { y: 0, color: '#FFD700' },
        { y: 0, color: '#66CD00' },
        { y: 0, color: '#FF8C00' },
        { y: 0, color: '#FF0000' },
        { y: 0, color: '#9400D3' },
        { y: 0, color: '#8B8878' },
        { y: 0, color: '#3A5FCD' },
//        { y: 0, color: '#FFFF00' }
    ];
    var b = [
        { y: 2200000, color: '#6495ED' },
        { y: 3000000, color: '#6495ED' },
        { y: 2700000, color: '#6495ED' },
        { y: 2500000, color: '#6495ED' },
        { y: 3400000, color: '#6495ED' },
        { y: 2700000, color: '#6495ED' },
        { y: 2600000, color: '#6495ED' },
//        { y: 2800000, color: '#6495ED' }
    ];;

    $('#upper-container').highcharts({
        chart: {
            type: 'column'
        },
        title: {
            text: null, //'Historic World Population by Region'

        },
        subtitle: {
            text: null, //'Source: Wikipedia.org'

        },
        xAxis: {
            categories: ['Emergency Borrowing 2', 'Emergency Borrowing 1', 'Near Overdraft', 'Overdraft 3', 'Overdraft 2', 'Overdraft 1', 'Low Balance', 'Fraud'],
            title: {
                text: null
            }
        },
        yAxis: {
            min: 0,
            title: {
                text: null, //'Population (millions)',
                align: 'high'
            },
            labels: {
                overflow: 'justify'
            }
        },
        tooltip: {
            valueSuffix: ' alert(s)'
        },
        plotOptions: {
            bar: {
                dataLabels: {
                    enabled: true
                }
            }
        },
        legend: {
            enabled: false,
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'top',
            x: -40,
            y: 100,
            floating: true,
            borderWidth: 1,
//            backgroundColor: (Highcharts.theme && Highcharts.theme.legendBackgroundColor || '#FFFFFF'),
            shadow: true
        },
        credits: {
            enabled: false
        },
        series: [
            {
                //                    name: 'Year 1800',
                data: a
            }
//            , {
//                name: null,
//                data: b,
//            }
        ]
    });
});


function toDate(dStr, format) {
    var now = new Date();
    if (format == "h:m") {
        now.setHours(dStr.substr(0, dStr.indexOf(":")));
        now.setMinutes(dStr.substr(dStr.indexOf(":") + 1));
        now.setSeconds(0);
        return now;
    } else
        return "Invalid Format";
}
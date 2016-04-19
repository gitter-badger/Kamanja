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


$(function() {

//    $('.slider-content').on('click', function() {
//        
//        prepareDrawChart($(this).attr('data-id'));
//
//        $("html, body").animate({ scrollTop: $(window).height() }, 600);
//    });

    $('.home').on('click', function() {

        $(window.location).attr('href', './landing.html');
    });

    $('.processing').on('click', function() {

        $(window.location).attr('href', './processing.html');
    });

});
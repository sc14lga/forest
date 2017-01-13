/* JavaScript for the Forest application */
jsonContents = [];


var pengine;
var querynum = 0;
function addQuery(){
    var query = $("#query").val();

    if (query) {
        querynum ++;
        var num = querynum;
        $("#accordion").append( '<div class="panel panel-default"><div class="panel-heading" role="tab" id="heading'+ num + 
                    '"><h4 class="panel-title"><a role="button" data-toggle="collapse" data-parent="#accordion" href="#collapse'+num+
                    '" aria-expanded="true" aria-controls="collapse'+num+'" id="df'+num+'-query">'+ query + 
                //    '<div class="col-sm-2">'+
                //                '<div class="input-group colorpicker-component" id="df'+num+'-colour">'+
                //                  '<input type="text" value="#00AABB" class="form-control" />'+
                //                  '<span class="input-group-addon"><i></i></span>'+
                //                '</div>'+
                //              '</div>'+
                    '</a></h4><div class="btn-group show-hide" data-toggle="buttons">'+
                              '<label class="btn btn-primary active" id="df'+num+'-show">'+
                                '<input type="radio" name="options" id="show" autocomplete="off" checked> Show</label><label class="btn btn-primary">'+
                                '<input type="radio" name="options" id="hide" autocomplete="off"> Hide</label> </div></div>'+
                          '<div id="collapse'+num+'" class="panel-collapse collapse in" role="tabpanel" aria-labelledby="heading'+num+'"><div class="panel-body">'+

                              '<form onsubmit="return false" class="form-horizontal">'+
                                '<div class="form-group">'+
                                  '<label for="df'+num+'-colour" class="col-sm-4 control-label">Colour</label>'+
                                  '<div class="col-sm-8">'+
                                    '<div class="input-group colorpicker-component" id="df'+num+'-colour">'+
                                     '<input type="text" value="#00AABB" class="form-control" />'+
                                      '<span class="input-group-addon"><i></i></span>'+
                                    '</div>'+
                                  '</div>'+
                                '</div>'+
                              '</form>   '+      
                            '</div>'+
                          '</div>'+
                        '</div>');
        $('.colorpicker-component').colorpicker(); 
    }
}





function add() {
    var query = $("#query").val();
    if (query) {
        pengine = new Pengine({
            application: 'forest',
            ask: query,
            onsuccess: function() {
                writeln(JSON.stringify(this.data));

                if (this.more) {
                    disableButtons(true, false, false, false);
                } else {
                    writeln("No more solutions");
                    disableButtons(false, true, true, true);                        
                }
            },
            onfailure: function() {
                writeln("Failure");
                disableButtons(false, true, true, true);
            },
            onstop: function() {
                writeln("Stopped");
                disableButtons(false, true, true, true);
            },
            onabort: function() {
                writeln("Aborted");
                disableButtons(false, true, true, true);
            },
            onerror: function() {
                writeln("Error: " + this.data);
                disableButtons(false, true, true, true);
            }
        });
    }
}

function next() {
    pengine.next();
}

function stop() {
    pengine.stop();
}

function abort() {
    pengine.abort();
}


function update(op) {
    var pred = op + $("input[name=sex]:checked").val(),
        X = $("#X").val().toLowerCase() || '_',
        Y = $("#Y").val().toLowerCase() || '_',
        command = pred + '(' + X + ',' + Y + ')';
    pengine = new Pengine({
        application: 'forest',
        ask: command,
        onsuccess: function() {
            writeln(command);
            $("#X,#Y").val("");
        },
        onerror: function() {
            writeln("Error: " + this.data);
        }
    });
}


function writeln(string) {
    $('#output').append(string + "<br />");
}

function disableButtons(ask, next, stop, abort) {
    $("#add-btn").prop("disabled", ask);
    $("#next-btn").prop("disabled", next);
    $("#stop-btn").prop("disabled", stop);
    $("#abort-btn").prop("disabled", abort);
}


$(document).ready(function() {
    $("#sample-queries").on("change", function() {
        $("#query").val($("#sample-queries option:selected").text());
    });
    $("#add-btn").on("click", addQuery);
    $("#next-btn").on("click", next);
    $("#stop-btn").on("click", stop);
    $("#abort-btn").on("click", abort);
    $("#assert-btn").on("click", function() {
        update('assert_');
    });
    $("#retract-btn").on("click", function() {
        update('retract_');
    });
    $("#clear-btn").on("click", function() {
        $('#output').html('');
    });
});


// This example creates a simple polygon representing the Bermuda Triangle.

jsonContents = [];


function readXMLfile(file_Location) {  
  if (window.XMLHttpRequest) {// code for IE7+, Firefox, Chrome, Opera, Safari
    xmlhttp = new XMLHttpRequest();
  }
  else {// code for IE6, IE5
    xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
  }

  xmlhttp.open("GET", file_Location, false);
  xmlhttp.send();
  xmlDoc = xmlhttp.responseXML;
  //document.getElementById(your_div_id).value = xmlDoc.getElementsByTagName(The_tag_in_xml_you_want_to_display)[0].childNodes[0].nodeValue;
  return xmlDoc;
}


function readJsonFile(){
  $.ajax({
    url: "data/reduced.json",
    success: function (data) {
        jsonContents = JSON.parse(data);

        $.each( jsonContents, function( index, value ) {
          if(value.many_trees == "TRUE" || value.tree_count >= 10)
            printBox(getCEpolygonFromJSON2(value));
        });

    }
  });
}





function customPrologPrintBoxes(color, query){
    query = "findall("+query+", (color("+query+")), L).";

    if (query) {
        pengine = new Pengine({
            application: 'forest',
            ask: query,
            onsuccess: function() {
                writeln(JSON.stringify(this.data));

                if (this.more) {
                    disableButtons(true, false, false, false);
                } else {
                    writeln("No more solutions");
                    disableButtons(false, true, true, true);                        
                }

                for(var i=0; i<this.data[0].L.length; i++){
                    printBox(getCEpolygonFromProlog2JSON(this.data[0].L[i]),color);
                }

                /*do{
                    printBox(getCEpolygonFromProlog2JSON(this.data));
                } while (this.more);*/
            },
            onfailure: function() {
                writeln("Failure");
                disableButtons(false, true, true, true);
            },
            onstop: function() {
                writeln("Stopped");
                disableButtons(false, true, true, true);
            },
            onabort: function() {
                writeln("Aborted");
                disableButtons(false, true, true, true);
            },
            onerror: function() {
                writeln("Error: " + this.data);
                disableButtons(false, true, true, true);
            }
        });
    }

}


function getCEpolygonFromJSON(json){
  var x = json.location.x;
  var y = json.location.y;

  return {LatLng:{lat:parseFloat(y),lng:parseFloat(x)}, data:json};
}

function getCEpolygonFromJSON2(json){
  var x = json.location_x;
  var y = json.location_y;

  return {LatLng:{lat:parseFloat(y),lng:parseFloat(x)}, data:json};
}

function getCEpolygonFromProlog2JSON(json){
  var LonN = json[2];
  var LonD = json[3];
  var LatN = json[4];
  var LatD = json[5];

  return {LatLng:{lat:parseFloat(LatN+"."+LatD),lng:parseFloat(LonN+"."+LonD)}, data:json};
}



/*
function retrieveXMLtag(xmlDoc, tag_name){
  return xmlDoc.getElementsByTagName(tag_name)[0].childNodes[0].nodeValue;
}

function addPropertyFromXML(object, property_name, xmlDoc){
  object.properties[property_name]=retrieveXMLtag(xmlDoc,property_name);
  return object;
}

function readXMLfilesCE(){
  var data_points=[];
  for (var i = 5; i<19; i++){
    var xmlDoc = readXMLfile("data/"+i+".xml");
    data_points.push(getCEpolygon(xmlDoc));
  }
  return data_points;
}
*/
function readXMLfilesCEtoJSON(xmlDoc){

  for (var i = 5; i<19; i++){
    $.get("data/"+i+".xml", 
    function(xml, data_points){ 
      var json = $.xml2json(xml); 
      if (json.country.value == "India" && json.land_use_category.code == "forest"){
        jsonContents[json.id.value]={many_trees : json.many_trees, tree_count: json.tree_count};
      }
      printBox(getCEpolygonFromJSON(json));
    }); 
    
  }
}

function JsonByCountry (country){
  var url = 'data:text/json;charset=utf8,' + encodeURIComponent(jsonContents);
  window.open(url, '_blank');
  window.focus();
}

/*

  <many_trees>
    <value>false</value>
  </many_trees>
  <tree_count>
    <value>1</value>
  </tree_count>
*/


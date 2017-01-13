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

function contactProlog(){
  $.getJSON( "api", function( data ) {
  var items = [];
  $.each( data, function( key, val ) {
    items.push( "<li id='" + key + "'>" + val + "</li>" );
  });
 
  $( "<ul/>", {
    "class": "my-new-list",
    html: items.join( "" )
  }).appendTo( "body" );
});
}


function customPrintBoxes(color, trees){
  $.each( jsonContents, function( index, value ) {
          if(value.many_trees == "TRUE" || value.tree_count >= trees)
            printBox(getCEpolygonFromJSON2(value), color);
        });
}



/*
function getCEpolygon(xmlDoc){
  var x = xmlDoc.getElementsByTagName("x")[0].childNodes[0].nodeValue;
  var y = xmlDoc.getElementsByTagName("y")[0].childNodes[0].nodeValue;
  var land_use_category = xmlDoc.getElementsByTagName("land_use_category")[0].childNodes[0].nodeValue;

  return {LatLng:{lat:parseFloat(y),lng:parseFloat(x)}, data:0};
}*/

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
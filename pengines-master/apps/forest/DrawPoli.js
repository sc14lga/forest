// This example creates a simple polygon representing the Bermuda Triangle.
map = null;
polygons = [];
style = [
  {
    "featureType": "poi",
    "elementType": "geometry",
    "stylers": [
      { "visibility": "simplified" },
      { "saturation": -87 },
      { "lightness": 80 }
    ]
  },{
    "featureType": "landscape",
    "stylers": [
      { "saturation": -100 },
      { "lightness": 77 }
    ]
  },{
    "featureType": "administrative.province",
    "stylers": [
      { "visibility": "off" }
    ]
  },{
    "featureType": "administrative.locality",
    "stylers": [
      { "visibility": "off" }
    ]
  },{
    "featureType": "road",
    "stylers": [
      { "visibility": "off" }
    ]
  }
]

function initMap() {
  map = new google.maps.Map(document.getElementById('map'), {
    zoom: 7,
    center: {lat:33.5990977,  lng: 66.85906129},
    mapTypeId: google.maps.MapTypeId.TERRAIN
  });

  map.setOptions({styles: style});

  readJsonFile();
}
/*
function printBoxes(){
  data_points = readXMLfilesCE();
  for (i = 0; i < data_points.length; i++) {
    var bb = getBoundingBox ( data_points[i].LatLng, 30);
    var box = new google.maps.Polygon({
      paths: bb,
      strokeWeight: 2,
      strokeOpacity: 0,
      fillColor: '#FF0000',
      fillOpacity: 0.35
    });
    box.setMap(map);
  }
}*/

function printBox(datapoint, color){
  
    if (!color)
      color = '#00aabb';

    var bb = getBoundingBox ( datapoint.LatLng, 15000);
    var box = new google.maps.Polygon({
      paths: bb,
      strokeWeight: 2,
      strokeOpacity: 0,
      fillColor: color,
      fillOpacity: 0.35
    });
    box.setMap(map);
    polygons.push(box);
}

function clearMap(){
  $.each(polygons, function(index, value){
    value.setMap(null);
  });
  polygons = [];
}


function simplePoint(point){
  return {lat:point.lat(), lng: point.lng()};
}

function getBoundingBox(pointLL, distance){
  var point = new google.maps.LatLng(pointLL.lat, pointLL.lng);
  var spherical = google.maps.geometry.spherical; 

  var center_top = spherical.computeOffset(point, distance/2, 0);
  var top_left = spherical.computeOffset(center_top, distance/2, 270);
  var top_right = spherical.computeOffset(center_top, distance/2, 90);
  var bottom_left = spherical.computeOffset(top_left, distance, 180);
  var bottom_right = spherical.computeOffset(top_right, distance, 180);
  return [top_left,top_right,bottom_right,bottom_left,top_left];
}
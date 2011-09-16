var overlays = []
var map;

function toBounds(r) {
  var swl = r.b; var swg = r.l; var nel = r.t; var neg = r.r;
  return new google.maps.LatLngBounds(new google.maps.LatLng(swl, swg), new google.maps.LatLng(nel, neg))
}

function renderMap(rects, center, zoom, opacity) {
  // Clear existing overlays
  for (var i = 0; i < overlays.length; i++) overlays[i].setMap(null)
  overlays.length = 0

  for (var i = 0; i < rects.length; i++) {
    var r = rects[i]
    var rectBounds = toBounds(r)
    var rect = new google.maps.Rectangle();

    var opts = {
      strokeColor: "#000000",
      strokeOpacity: 1.0,
      strokeWeight: 0,
      fillColor: "#000000",
      fillOpacity: opacity,
      map: map,
      zIndex: 10,
      bounds: rectBounds
    }

    rect.setOptions(opts)
    overlays.push(rect)
  }
  map.setZoom(zoom)
  map.panTo(new google.maps.LatLng(center[0], center[1]))
}

function setupMap() {
  var mapOptions = {
    zoom: 10,
    center: new google.maps.LatLng(40, -74),
    mapTypeId: google.maps.MapTypeId.ROADMAP
  };
  map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);
}

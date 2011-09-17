var overlays = []
var map;

// Sticky Options
var stickyDefaults = {
  strokeColor: "#FF0000",
  strokeOpacity: 1.0,
  strokeWeight: 0,
  fillColor: "#000000",
  fillOpacity: 1.0,
  zIndex: 10,
}


function toBounds(r) {
  var swl = r.b; var swg = r.l; var nel = r.t; var neg = r.r;
  return new google.maps.LatLngBounds(new google.maps.LatLng(swl, swg), new google.maps.LatLng(nel, neg))
}

function defaultOptions(map, rectBounds) {
  return {
      strokeColor: stickyDefaults.strokeColor,
      strokeOpacity: stickyDefaults.strokeOpacity,
      strokeWeight: stickyDefaults.strokeWeight,
      fillColor: stickyDefaults.fillColor,
      fillOpacity: stickyDefaults.fillOpacity,
      map: map,
      zIndex: stickyDefaults.zIndex,
      bounds: rectBounds
  }
}

function updateOpacity(opacity) {
  stickyDefaults.fillOpacity = opacity
  updateRects()
}

function showBorders(show) {
  if (show) {
    stickyDefaults.strokeWeight = 2
  } else {
    stickyDefaults.strokeWeight = 0
  }
  updateRects()
}

function updateRects() {
  for (var i = 0; i < overlays.length; i++) {
    var bounds = overlays[i].getBounds()
    var opts = defaultOptions(map, bounds)
    overlays[i].setOptions(opts)
  }
}

function renderMap(rects, center, zoom, opacity) {
  // Clear existing overlays
  for (var i = 0; i < overlays.length; i++)
    overlays[i].setMap(null)
  overlays.length = 0

  for (var i = 0; i < rects.length; i++) {
    var r = rects[i]
    var rectBounds = toBounds(r)
    var rect = new google.maps.Rectangle();

    var opts = defaultOptions(map, rectBounds)

    rect.setOptions(opts)
    overlays.push(rect)
  }
  if (center) {
    map.setZoom(zoom)
    map.panTo(new google.maps.LatLng(center[0], center[1]))
  }
}

function setupMap() {
  var mapOptions = {
    zoom: 10,
    center: new google.maps.LatLng(40, -74),
    mapTypeId: google.maps.MapTypeId.ROADMAP
  };
  map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);
}

var overlays = []
var recs = []
var currPos;
var searchPos;
var map;

var infowindow = new google.maps.InfoWindow({disableAutoPan: false})
/*
google.maps.event.addListener(marker, 'click', function() {
  infowindow.open(map, marker);
});
*/


// Sticky Options
var stickyDefaults = {
  strokeColor: "#FF0000",
  strokeOpacity: 1.0,
  strokeWeight: 0,
  fillColor: "#000000",
  fillOpacity: 1.0,
  zIndex: 0
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

function renderMap(rects, inRecs, pos, center, zoom, opacity, redrawOverlays) {

  if (redrawOverlays) {
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
      google.maps.event.addListener(rect, 'click', function() {
        infowindow.close()
      });

      overlays.push(rect)
    }
  }

  // Clear existing recommendations
  for (var i = 0; i < recs.length; i++)
    recs[i].setMap(null)
  recs.length = 0

  for (var i = 0; i < inRecs.length; i++) {
    var r = inRecs[i]
    var generateMarker = function(r) {
      var marker = new google.maps.Marker({
        position: new google.maps.LatLng(r.lat, r.lng),
        map: map,
        icon: r.catIcon
      })
      var eventFn = function() {
        var content = document.createElement("div")
        var addLine = function(header, body, parent) {
          if (body) {
            var subdiv = document.createElement("div")
            parent.appendChild(subdiv)
            subdiv.appendChild(document.createTextNode(header + ": " + body))
          }
        }
        addLine("Name", r.name, content)
        addLine("Type", r.catName, content)
        addLine("Address", r.address, content)
        infowindow.setContent(content)
        infowindow.open(map, marker);
      }
      google.maps.event.addListener(marker, 'click', eventFn);
      recs.push(marker)
    }
    generateMarker(inRecs[i])
  }

  if (pos) updateSearchPosition(pos[0], pos[1], false)

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
  google.maps.event.addListener(map, 'click', function() {
    infowindow.close()
  });
  google.maps.event.addListener(map, 'dragstart', function() {
    infowindow.close()
  });
}

function updateSearchPosition(lat, lng, forTouch) {
  if (!searchPos) {
    searchPos = new google.maps.Marker({
      position: new google.maps.LatLng(lat, lng),
      map: map,
      clickable: !forTouch,
      draggable: !forTouch
    })
    if (!forTouch) {
      google.maps.event.addListener(searchPos, 'dragstart', function() {
        infowindow.close()
      });
      google.maps.event.addListener(searchPos, 'click', function() {
        infowindow.close()
      });
      google.maps.event.addListener(searchPos, 'dragend', function() {
        var p = searchPos.getPosition()
        $('#searchLatLng').val(p.lat() + ',' + p.lng()).blur()
      });
    }
  }
  else searchPos.setPosition(new google.maps.LatLng(lat, lng))
}

function updateCurrentPosition(lat, lng) {
  if (!currPos) {
    currPos = new google.maps.Marker({
      position: new google.maps.LatLng(lat, lng),
      icon: "/images/currentPosition.gif",
      map: map,
      clickable: false,
      draggable: false
    })
  }
  else currPos.setPosition(new google.maps.LatLng(lat, lng))
}

function setupTouchMap() {
  var mapOptions = {
    zoom: 10,
    center: new google.maps.LatLng(40, -74),
    mapTypeId: google.maps.MapTypeId.ROADMAP,
    zoomControl: false, panControl: false, rotateControl: false,
    streetViewControl: false, scaleControl: false,
    scrollwheel: false, overviewMapControl: false,
    mapTypeControl: false
  };
  map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);

  google.maps.event.addListener(map, 'click', function(event) {
    var lat = event.latLng.lat()
    var lng = event.latLng.lng()
    updateSearchPosition(lat, lng, true)
    $('#searchLatLng').val(lat + ',' + lng).blur()
  });
}

var g4c = function() {

  // Context Variables
  var currPos;
  var searchPos;
  var map;

  var isIphone = navigator.userAgent.toLowerCase().indexOf('iphone') >= 0
  var isAndroid = navigator.userAgent.toLowerCase().indexOf('android') >= 0
  var isTouch = isIphone || isAndroid

  // Map Overlays
  var overlays = []
  var recs = []
  var infowindow = new google.maps.InfoWindow({disableAutoPan: false})

  // Info Window State
  var windowopen = false
  function closeInfoWindow() {infowindow.close(); windowopen=false}
  function openInfoWindow(map, marker) {infowindow.open(map, marker); windowopen=true}

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
    setOverlayOpacity(opacity)
    redrawOverlays()
  }

  function showBorders(show) {
    if (show) {
      stickyDefaults.strokeWeight = 2
    } else {
      stickyDefaults.strokeWeight = 0
    }
    redrawOverlays()
  }

  function redrawOverlays() {
    for (var i = 0; i < overlays.length; i++) {
      var bounds = overlays[i].getBounds()
      var opts = defaultOptions(map, bounds)
      overlays[i].setOptions(opts)
    }
  }

  function renderRecommendations(inRecs) {
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
          var id = r.id

          var webLink = 'https://foursquare.com/venue/'+id;
          var iphoneLink = 'foursquare://venue/'+id; // TODO: make this be something we can check into?
          var link = (isIphone ? iphoneLink : webLink)
          var target = (!isIphone ? 'target="_blank"' : '')

          var html = '<div style="text-align:left">' +
            '<div><a href="' + link + '" ' + target + '>' + r.name + '</a></div>' +
            (r.catName ? '<div>' + r.catName + '</div>' : '') +
            (r.address ? '<div>' + r.address + '</div>' : '') +
          '</div>'
          infowindow.setContent(html)
          openInfoWindow(map, marker)
        }
        google.maps.event.addListener(marker, 'click', eventFn);
        recs.push(marker)
      }
      generateMarker(inRecs[i])
    }
  }

  function resetCenterAndZoom(lat, lng, zoom) {
    map.setZoom(zoom)
    map.panTo(new google.maps.LatLng(lat, lng))
  }

  function setOverlayOpacity(opacity) { stickyDefaults.fillOpacity = opacity }

  function renderMap(rects) {
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
      google.maps.event.addListener(rect, 'click', function(event) {
        if (isTouch && !windowopen) {
          var lat = event.latLng.lat()
          var lng = event.latLng.lng()
          updateSearchPosition(lat, lng)
          $('#searchLatLng').val(lat + ',' + lng).blur()
        }
        closeInfoWindow()
      });
      if (!isTouch) {
        google.maps.event.addListener(rect, 'dblclick', function(event) {
          var lat = event.latLng.lat()
          var lng = event.latLng.lng()
          updateSearchPosition(lat, lng)
          $('#searchLatLng').val(lat + ',' + lng).blur()
        })
      }
      overlays.push(rect)
    }
  }

  function setupMap() {
    var mapOptions = {
      zoom: 10,
      center: new google.maps.LatLng(40, -74),
      mapTypeId: google.maps.MapTypeId.ROADMAP,
      disableDoubleClickZoom: true
    };
    map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);
    google.maps.event.addListener(map, 'click', function() {
      closeInfoWindow()
    });
    google.maps.event.addListener(map, 'dragstart', function() {
      closeInfoWindow()
    });
    google.maps.event.addListener(map, 'dblclick', function(event) {
      var lat = event.latLng.lat()
      var lng = event.latLng.lng()
      updateSearchPosition(lat, lng)
      $('#searchLatLng').val(lat + ',' + lng).blur()
    })
  }

  function updateSearchPosition(lat, lng) {
    if (!searchPos) {
      searchPos = new google.maps.Marker({
        position: new google.maps.LatLng(lat, lng),
        map: map,
        icon:"/images/target.gif",
        clickable: !isTouch,
        draggable: !isTouch
      })
      if (!isTouch) {
        google.maps.event.addListener(searchPos, 'dragstart', function() {
          closeInfoWindow()
        });
        google.maps.event.addListener(searchPos, 'click', function() {
          closeInfoWindow()
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
    isTouch = true // Force touch mode
    var mapOptions = {
      zoom: 10,
      center: new google.maps.LatLng(40, -74),
      mapTypeId: google.maps.MapTypeId.ROADMAP,
      disableDoubleClickZoom: true,
      zoomControl: false, panControl: false, rotateControl: false,
      streetViewControl: false, scaleControl: false,
      scrollwheel: false, overviewMapControl: false,
      mapTypeControl: false
    };
    map = new google.maps.Map(document.getElementById("map_canvas"), mapOptions);

    google.maps.event.addListener(map, 'click', function(event) {
      if (!windowopen) {
        var lat = event.latLng.lat()
        var lng = event.latLng.lng()
        updateSearchPosition(lat, lng)
        $('#searchLatLng').val(lat + ',' + lng).blur()
      }
      closeInfoWindow()
    });
  }

  return {
    setupMap: setupMap,
    setupTouchMap: setupTouchMap,

    updateCurrentPosition: updateCurrentPosition,
    updateSearchPosition: updateSearchPosition,

    renderMap: renderMap,
    redrawOverlays: redrawOverlays,
    renderRecommendations: renderRecommendations,
    setOverlayOpacity: setOverlayOpacity,
    resetCenterAndZoom: resetCenterAndZoom,

    updateOpacity: updateOpacity,
    showBorders: showBorders
  }
}()


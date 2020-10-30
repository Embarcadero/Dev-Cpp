/*

 This example defines an image map type using the Gall-Peters
 projection.
 https://en.wikipedia.org/wiki/Gall%E2%80%93Peters_projection

*/


//#region functions

function initMap() {
  // Create a map. Use the Gall-Peters map type.
  var map = new google.maps.Map(document.getElementById('map'), {
    zoom: 0,
    center: {lat: 0, lng: 0},
    mapTypeControl: false
  });

  initGallPeters();
  map.mapTypes.set('gallPeters', gallPetersMapType);
  map.setMapTypeId('gallPeters');

  // Show the lat and lng under the mouse cursor.
  var coordsDiv = document.getElementById('coords');
  map.controls[google.maps.ControlPosition.TOP_CENTER].push(coordsDiv);
  map.addListener('mousemove', function(event) {
    coordsDiv.textContent =
        'lat: ' + Math.round(event.latLng.lat()) + ', ' +
        'lng: ' + Math.round(event.latLng.lng());
  });

  // Add some markers to the map.
  map.data.setStyle(function(feature) {
    return {
      title: feature.getProperty('name'),
      optimized: false
    };
  });
  map.data.addGeoJson(cities);
}

var gallPetersMapType;
function initGallPeters() {
  var GALL_PETERS_RANGE_X = 800;
  var GALL_PETERS_RANGE_Y = 512;

  // Fetch Gall-Peters tiles stored locally on our server.
  gallPetersMapType = new google.maps.ImageMapType({
    getTileUrl: function(coord, zoom) {
      var scale = 1 << zoom;

      // Wrap tiles horizontally.
      var x = ((coord.x % scale) + scale) % scale;

      // Don't wrap tiles vertically.
      var y = coord.y;
      if (y < 0 || y >= scale) return null;

      return 'https://developers.google.com/maps/documentation/' +
             'javascript/examples/full/images/gall-peters_' + zoom +
             '_' + x + '_' + y + '.png';
    },
    tileSize: new google.maps.Size(GALL_PETERS_RANGE_X, GALL_PETERS_RANGE_Y),
    minZoom: 0,
    maxZoom: 1,
    name: 'Gall-Peters'
  });

  // Describe the Gall-Peters projection used by these tiles.
  gallPetersMapType.projection = {
    fromLatLngToPoint: function(latLng) {
      var latRadians = latLng.lat() * Math.PI / 180;
      return new google.maps.Point(
          GALL_PETERS_RANGE_X * (0.5 + latLng.lng() / 360),
          GALL_PETERS_RANGE_Y * (0.5 - 0.5 * Math.sin(latRadians)));
    },
    fromPointToLatLng: function(point, noWrap) {
      var x = point.x / GALL_PETERS_RANGE_X;
      var y = Math.max(0, Math.min(1, point.y / GALL_PETERS_RANGE_Y));

      return new google.maps.LatLng(
          Math.asin(1 - 2 * y) * 180 / Math.PI,
          -180 + 360 * x,
          noWrap);
    }
  };
}
//#endregion functions

//#region data

// GeoJSON, describing the locations and names of some cities.
var cities = {
  type: 'FeatureCollection',
  features: [{
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [-87.650, 41.850]},
    properties: {name: 'Chicago'}
  },
  {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [-149.900, 61.218]},
    properties: {name: 'Anchorage'}
  }, {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [-99.127, 19.427]},
    properties: {name: 'Mexico City'}
  },
  {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [-0.126, 51.500]},
    properties: {name: 'London'}
  },
  {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [28.045, -26.201]},
    properties: {name: 'Johannesburg'}
  },
  {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [15.322, -4.325]},
    properties: {name: 'Kinshasa'}
  },
  {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [151.207, -33.867]},
    properties: {name: 'Sydney'}
  },
  {
    type: 'Feature',
    geometry: {type: 'Point', coordinates: [0, 0]},
    properties: {name: '0°N 0°E'}
  }]
};

//#endregion data



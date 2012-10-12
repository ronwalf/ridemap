var timeThreshold = 0.6;
var cellColor = "#FF0000";
var selectControl;

var grid = null
var rider = null;

function init(datadir){
    map = new OpenLayers.Map('map');

    var wmsLayer = new OpenLayers.Layer.WMS( "OpenLayers WMS",
        "http://vmap0.tiles.osgeo.org/wms/vmap0?", {layers: 'basic'});
    var omLayer = new OpenLayers.Layer.OSM();
    //map.addLayers([omLayer, wmsLayer]);
    grid = new Grid(map, datadir);
    rider = new Rider(map, grid, datadir);
    map.addLayers([omLayer, grid.layer, rider.layer, rider.markers]);
    //gridLayer.setIsBaseLayer(true); map.addLayers([gridLayer]);
    map.addControl(new OpenLayers.Control.LayerSwitcher());
    map.addControl(new OpenLayers.Control.MousePosition());
    //map.setCenter(new OpenLayers.LonLat(0, 0), 3);
    
}


function Grid(map, datadir) {
    this.map = map
    this.data = null;
    this.desc = null;

    this.layer = new OpenLayers.Layer.Vector("Grid Layer", 
      { //projection: new OpenLayers.Projection("EPSG:4326"),
        renderers: ["Canvas", "SVG", "VML"]
      });
    //map.addLayer(this.layer);
    this.layer.events.on({
        'featureselected': onFeatureSelect,
        'featureunselected': onFeatureUnselect
    });
    
    var report = function(e) {
        OpenLayers.Console.log(e.type, e.feature.id);
    };
            
    var highlightCtrl = new OpenLayers.Control.SelectFeature(this.layer, {
        hover: true,
        highlightOnly: true,
        renderIntent: "temporary",
        eventListeners: {
            beforefeaturehighlighted: report,
            featurehighlighted: report,
            featureunhighlighted: report
        },
        selectStyle : {
            fillOpacity: 0.5,
            stroke : true,
            strokeWidth : 0.5
        }
    });
    map.addControl(highlightCtrl);
    highlightCtrl.activate();
    
    selectControl = new OpenLayers.Control.SelectFeature(this.layer,
        {clickout: true}
    );
    map.addControl(selectControl);
    selectControl.activate();
    
    var grid = this;
    var xobj = new XMLHttpRequest();
    xobj.overrideMimeType("application/json");
    xobj.open('GET', datadir + '/grid.json', true);
    xobj.onreadystatechange = function () {
      if (xobj.readyState == 4) {
        var jsonText = xobj.responseText;
        grid.update(JSON.parse(jsonText));
      }
    };
    xobj.send();
}


Grid.prototype.hexItoP = function(p) {
  var s = this.desc[1];
  var h = this.desc[2];
  return [p[0] * s, h * (p[1] - 0.5 * Math.abs(p[0] % 2))];
}


Grid.prototype.cellFeature = function(cell) {
    var pos = this.hexItoP(cell['pos']);
    var x = pos[0];
    var y = pos[1];
    var r = this.desc[0];
    var s = this.desc[1];
    var h = this.desc[2];

    var points = new Array(
      new OpenLayers.Geometry.Point(x,y),
      new OpenLayers.Geometry.Point(x + r ,y),
      new OpenLayers.Geometry.Point(x+s,y+(h/2.0)),
      new OpenLayers.Geometry.Point(x+r,y+h),
      new OpenLayers.Geometry.Point(x,y+h),
      new OpenLayers.Geometry.Point(x+r-s,y+(h/2.0)));
    var hexPoly = new OpenLayers.Geometry.Polygon([new OpenLayers.Geometry.LinearRing(points)]);
    hexPoly.transform(this.projection, map.baseLayer.projection);

    var cellOp = 0.1+ 0.9*(Math.log(1+cell["time"])/Math.log(1+this.maxTime));
    var hexFeature = new OpenLayers.Feature.Vector(hexPoly, cell, 
        {stroke:true,
        strokeColor: cellColor,
        strokeOpacity: cellOp, 
        strokeWidth: cellOp*2,
        //strokeWidth: 3,
        fillColor:cellColor, 
        fillOpacity:cellOp});
    return hexFeature;
}


Grid.prototype.update = function (data) {
    this.data = data;
    this.projection = new OpenLayers.Projection(data["projection"]);
    this.desc = data['desc'];
    this.maxTime = data['maxTime'];
    this.totalTime = data['totalTime'];
    this.rides = data['rides'];

    this.layer.removeAllFeatures();

  
    var p = this.hexItoP(data['cells'][0]["pos"]);

    var maxTime = data['maxTime'];
    var totalTime = data['totalTime'];
    var currentTotal = 0;

    var minx = p[0], maxx = p[0], miny = p[1], maxy = p[1];
    var features = [];
    for(var i in data['cells']) {

        var cell = data['cells'][i];
        p = this.hexItoP(cell["pos"]);
        var x = p[0];
        var y = p[1];
        if(currentTotal < timeThreshold * totalTime) {
            currentTotal += cell["time"];
            if(x > maxx) {maxx = x;}
            if(x < minx) {minx = x;}
            if(y < miny) {miny = y;}
            if(y > maxy) {maxy = y;}
        }
                
        features[features.length] = this.cellFeature(cell);
    }
    this.layer.addFeatures(features);
    
    var bbox = new OpenLayers.Bounds(minx, miny, maxx, maxy); // transform(map.displayProjection, map.projection);
    bbox.transform(this.projection, map.baseLayer.projection);
    var zoom = map.getZoomForExtent(bbox) - 1;
    map.setCenter(bbox.getCenterLonLat(), zoom);

    //  Update total time
    updateTime(totalTime);
}

function updateTime(totalSecs) {
    var secs = totalSecs % 60;
    var totalMinutes = Math.floor(totalSecs/60);
    var minutes = totalMinutes % 60;
    var totalHours = Math.floor(totalMinutes/60);
    updateNode("totalTime", "" + totalHours + " hours and " + minutes + " minutes");
}

function updateNode(id, text) {
    var node = document.getElementById(id);
    while (node.hasChildNodes()) { 
        node.removeChild(node.firstChild); 
    }
    node.appendChild(document.createTextNode(text));
}

// Needed only for interaction, not for the display.
function onPopupClose(evt) {
    // 'this' is the popup.
    var feature = this.feature;
    if (feature.layer) { // The feature is not destroyed
        selectControl.unselect(feature);
    } else { // After "moveend" or "refresh" events on POIs layer all 
             //     features have been destroyed by the Strategy.BBOX
        this.destroy();
    }
}
function onFeatureSelect(evt) {
    feature = evt.feature;
    popup = new OpenLayers.Popup.FramedCloud("featurePopup",
                             feature.geometry.getBounds().getCenterLonLat(),
                             new OpenLayers.Size(100,100),
                             "<h2>"+feature.geometry.getBounds().getCenterLonLat().clone().transform(map.baseLayer.projection, "EPSG:4326") + "</h2>" +
                             "<table>" +
                             "<tr><td>Total seconds:</td><td>" + Math.round(feature.attributes["time"]) + "</td></tr>" +
                             "<tr><td>Total visits:</td><td>" + feature.attributes["count"] + "</td></tr>" +
                             "</table>",
                             null, true, onPopupClose);
    feature.popup = popup;
    popup.feature = feature;
    map.addPopup(popup, true);
}

function onFeatureUnselect(evt) {
    feature = evt.feature;
    if (feature.popup) {
        popup.feature = null;
        map.removePopup(feature.popup);
        feature.popup.destroy();
        feature.popup = null;
    }
}


function Rider(map, grid, datadir) {
    this.map = map;
    this.grid = grid;
    this.datadir = datadir;
    this.maxrides = 30;
    this.speed = 100;
    this.rides = [];    
    this.time = new Date();


    this.layer = new OpenLayers.Layer.Vector("Ride Layer", 
      { //projection: new OpenLayers.Projection("EPSG:4326"),
        renderers: ["Canvas", "SVG", "VML"]
      });

    this.markers = new OpenLayers.Layer.Markers( "Ride Markers" );
    var size = new OpenLayers.Size(20,20);
    var offset = new OpenLayers.Pixel(-(size.w/2), -(size.h/2));
    this.icon = new OpenLayers.Icon('http://upload.wikimedia.org/wikipedia/commons/4/42/MUTCD_R4-11.svg', size, offset);
    //this.icon = new OpenLayers.Icon('http://upload.wikimedia.org/wikipedia/commons/thumb/4/42/MUTCD_R4-11.svg/200px-MUTCD_R4-11.svg.png', size, offset);

    //map.addLayers([this.layer, this.markers]);
    //map.setLayerIndex(this.markers, 0);
    
    setInterval(OpenLayers.Animation.requestFrame, 100, function() { rider.step(); });
}

Rider.prototype.step = function() {
    
    var nextTime = new Date();
    var step = nextTime.getTime() - this.time.getTime();
    this.time = nextTime;

    for (var i = 0; i < this.maxrides; i++) {
        if (this.rides[i] == null || this.rides[i].step(step)) {
            this.requestRide(i);
        }
    }
}

Rider.prototype.requestRide = function(rideIndex) {
    if (this.grid.rides > 0 && !this.isRequesting) {
        var rider = this;
        rider.isRequesting = true;
        var ride = Math.floor(this.grid.rides * Math.random());
        var ridePath = this.datadir + '/ride' + ride + '.json';
        var xobj = new XMLHttpRequest();
        xobj.overrideMimeType("application/json");
        xobj.open('GET', ridePath, true);
        xobj.onreadystatechange = function () {
            if (xobj.readyState == 4) {
                rider.isRequesting = false;
                var jsonText = xobj.responseText;
                rider.startRide(rideIndex, ridePath, JSON.parse(jsonText));
            }
        };
        xobj.send();
    }
}

Rider.prototype.startRide = function(rideIndex, ridePath, rideData) {
    if (rideData['ride'] == null) {
        badRide = [ridePath, rideData];
        return;
    }
    this.rides[rideIndex] = new Ride(this, rideIndex, rideData['ride']);

}

function Ride(rider, index, points) {
    this.rider = rider;
    this.index = index
    this.points = points;
    this.countdown = 3*1000;
    this.current = 0;
    this.time = 0;

    this.point = null;

    var size = new OpenLayers.Size
    this.marker = new OpenLayers.Marker(new OpenLayers.LonLat(-76.87728303647957,38.99905256164584).transform(rider.grid.projection,rider.map.projection),rider.icon.clone());
    rider.markers.addMarker(this.marker);
    //this.marker = new OpenLayers.Marker(new OpenLayers.Pixel(0,0),rider.icon.clone());
    //this.marker.display(true);
}




Ride.prototype.step = function (millis) {
    this.time += millis * this.rider.speed;
    var nextPoint = this.getPoint();

    if (nextPoint == null) {
        this.countdown -= millis;
        if (this.countdown <= 0) {
            /*
            this.rider.layer.removeFeatures(this.features);
            for (var feature in this.features) {
                this.features[feature].destroy();
            }
            */
            this.rider.markers.removeMarker(this.marker);
            return true;
        }
        this.marker.inflate(this.countdown/(3*1000));
    } else {
        /*
        if (this.point != null) {
            this.rider.layer.removeFeatures([this.point]);
        }
        */
        this.marker.display(true);
        this.marker.moveTo(this.rider.map.getPixelFromLonLat(nextPoint));
        //this.marker.draw();
        this.rider.markers.redraw();
    }
    return false;
}


Ride.prototype.getPoint = function() {
    if (this.current + 1 >= this.points.length) {
        return null;
    }
    var lastPoint = this.points[this.current];
    var nextPoint = this.points[this.current + 1];
    var totalTime = (nextPoint[2] - lastPoint[2])*1000;

    if (this.time > totalTime) {
        this.current++;
        this.time -= totalTime;
        return this.getPoint();
    }

    var x = (lastPoint[0]*this.time + nextPoint[0]*(totalTime - this.time))/totalTime;
    var y = (lastPoint[1]*this.time + nextPoint[1]*(totalTime - this.time))/totalTime;
    var point = new OpenLayers.LonLat(x,y);
    point.transform(this.rider.grid.projection, this.rider.map);
    return point;
}



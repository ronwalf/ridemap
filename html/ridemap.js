/*
 * ridemap.js -- Copyright 2012 by Ron Alford
 * Distributed under the BSD-3 license
 * (see LICENSE file for details)
 */
var timeThreshold = 0.6;
// var cellColor = "#FF0000";

var grid = null
var rider = null;

function init(datadir){
    map = new OpenLayers.Map('map');

    //var wmsLayer = new OpenLayers.Layer.WMS( "OpenLayers WMS",
    //    "http://vmap0.tiles.osgeo.org/wms/vmap0?", {layers: 'basic'});
    var omLayer = new OpenLayers.Layer.OSM();
    var blankLayer = new OpenLayers.Layer.Vector("Blank Map", { isBaseLayer : true, projection: omLayer.projection });
    //map.addLayers([omLayer, wmsLayer]);
    map.addLayers([omLayer, blankLayer]);
    
    
    var xobj = new XMLHttpRequest();
    if (xobj.overrideMimeType) { xobj.overrideMimeType("application/json"); }
    xobj.open('GET', datadir + '/grid.json', true);
    xobj.onreadystatechange = function () {
      if (xobj.readyState == 4) {
        var jsonText = xobj.responseText;
        grid = new Grid(map, datadir, JSON.parse(jsonText));
        rider = new Rider(map, grid, datadir);
        map.addLayers([grid.layer, rider.routeLayer, rider.layer]);
      }
    };
    xobj.send();

    map.addControl(new OpenLayers.Control.LayerSwitcher());
}


function Grid(map, datadir, data) {
    this.map = map

    this.data = data;
    this.projection = new OpenLayers.Projection(data["projection"]);
    this.layer = new OpenLayers.Layer.Vector("Grid Layer", 
        { projection: this.projection });
    this.desc = data['desc'];
    this.maxTime = data['maxTime'];
    this.totalTime = data['totalTime'];
    this.rides = data['rides'];
    this.maxSkip = data['maxSkip'];
    this.tags = data['tags']['tags']
    this.defaultTag = data['tags']['default'];

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
    this.setLegend();
}

Grid.prototype.setLegend = function () {
    if (this.legend != null) {
        document.getElementById("legend").removeChild(this.legend);
    } 
    this.legend = document.createElement("table");
    var title = document.createTextNode("Legend");
    var titleth= document.createElement("th");
    titleth.appendChild(title);
    var titletr = document.createElement("tr");
    titletr.appendChild(titleth);
    titletr.appendChild(document.createElement("th"));
    this.legend.appendChild(titletr);

    var alltags = this.tags.concat([this.defaultTag]);
    for (var i in alltags) {
        var tr = document.createElement("tr");
        var img = document.createElement("img");
        var td = document.createElement("td");
        td.setAttribute("style", "padding:10px; background-color:" + alltags[i]['color']);
        img.setAttribute("width", 40);
        img.setAttribute("height", 40);
        img.setAttribute("src", alltags[i]["img"]);
        td.appendChild(img);
        tr.appendChild(td);
        
        td = document.createElement("td");
        td.appendChild(document.createTextNode(alltags[i]["name"]));
        tr.appendChild(td);

        this.legend.appendChild(tr);
    }
    document.getElementById("legend").appendChild(this.legend);
}

Grid.prototype.hexItoP = function(p) {
  var s = this.desc[1];
  var h = this.desc[2];
  return [p[0] * s, h * (p[1] - 0.5 * Math.abs(p[0] % 2))];
}

Grid.prototype.tagData = function(tagname) {
    var tag = this.defaultTag;
    for (var i in this.tags) {
        if (tagname == this.tags[i]['name']) {
            tag = this.tags[i];
            break;
        }
    }
    return tag;
}

Grid.prototype.cellFeature = function(cell) {
    var pos = this.hexItoP(cell['pos']);
    var x = pos[0];
    var y = pos[1];
    var r = this.desc[0];
    var s = this.desc[1];
    var h = this.desc[2];

    points = new Array(
      new OpenLayers.Geometry.Point(x,y),
      new OpenLayers.Geometry.Point(x + r ,y),
      new OpenLayers.Geometry.Point(x+s,y+(h/2.0)),
      new OpenLayers.Geometry.Point(x+r,y+h),
      new OpenLayers.Geometry.Point(x,y+h),
      new OpenLayers.Geometry.Point(x+r-s,y+(h/2.0)));
    var hexPoly = new OpenLayers.Geometry.Polygon([new OpenLayers.Geometry.LinearRing(points)]);
    hexPoly.transform(this.projection, map.baseLayer.projection);

    var cellOp = 0.25 + 0.75*(Math.log(1+cell["time"])/Math.log(1+this.maxTime));
    var hexFeature = new OpenLayers.Feature.Vector(hexPoly, cell, 
        {stroke:true,
        //strokeColor: cellColor,
        strokeColor : cell["color"],
        strokeOpacity: Math.max(0.25, cellOp/2),
        strokeWidth: Math.max(1,cellOp*3),
        //strokeWidth: 3,
        //fillColor:cellColor, 
        fillColor: cell["color"],
        fillOpacity:cellOp,
        graphicZIndex: 20});
    return hexFeature;
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

function Rider(map, grid, datadir) {
    this.map = map;
    this.grid = grid;
    this.datadir = datadir;
    this.maxrides = 20;
    this.speed = 100;
    this.rides = [];    
    this.time = new Date();

    var defaultStyle = OpenLayers.Util.applyDefaults({
        graphicWidth: 20,
        graphicHeight: 20,
        externalGraphic: '${img}',
        graphicZIndex: 1,
        //externalGraphic: "http://upload.wikimedia.org/wikipedia/commons/thumb/4/42/MUTCD_R4-11.svg/200px-MUTCD_R4-11.svg.png",
        fill: false,
        fillOpacity: 1,
        stroke: false,
        pointRadius: 0,
        }, OpenLayers.Feature.Vector.style["default"]);
    var selectedStyle = OpenLayers.Util.applyDefaults({
        graphicWidth: 30,
        graphicHeight: 30
    }, defaultStyle);
    
    var styleMap = new OpenLayers.StyleMap({"default": new OpenLayers.Style(defaultStyle), "select": new OpenLayers.Style(selectedStyle)}); 

    this.layer = new OpenLayers.Layer.Vector("Ride Layer", 
      { projection: grid.projection,
        styleMap : styleMap
      });
    
    this.layer.events.on({
        'featureselected': this.onFeatureSelect,
        'featureunselected': this.onFeatureUnselect
    });
    
    this.selectControl = new OpenLayers.Control.SelectFeature(this.layer,
        {clickout: false}
    );
    map.addControl(this.selectControl);
    this.selectControl.activate();
    
    this.routeLayer = new OpenLayers.Layer.Vector("Route Layer", 
      { projection: grid.projection,
        styleMap : styleMap
      });
    

    this.selectRide();
    var thisrider = this;
    this.animation = OpenLayers.Animation.start(function() { thisrider.step(); });
    window.addEventListener("hashchange", function(evt) { thisrider.selectRide(); }, true);
}


Rider.prototype.onFeatureSelect = function(evt) {
    var ride = evt.feature.data
    ride.select();
    window.location.hash = '#' + ride.num;
    //feature = evt.feature;
}

Rider.prototype.onFeatureUnselect = function(evt) {
    var ride = evt.feature.data;
    ride.unselect();
    window.location.hash = '#';
}


Rider.prototype.step = function() {
    
    var nextTime = new Date();
    var step = nextTime.getTime() - this.time.getTime();
    this.time = nextTime;
    
    for (var i = 0; i < this.maxrides; i++) {
        if (this.rides[i] == null || this.rides[i].step(step)) {
            if (this.requestRide(i)) {
                break;
            }
        }
    }
}

Rider.prototype.requestRide = function(rideIndex, ride) {
    if (this.grid.rides > 0 && !this.isRequesting) {
        var rider = this;
        rider.isRequesting = true;
        if (ride == null || ride < 0 || ride >= this.grid.rides) { 
            ride = Math.floor(this.grid.rides * Math.random()); 
        }
        var ridePath = this.datadir + '/ride' + ride + '.json';
        var xobj = new XMLHttpRequest();
        if (xobj.overrideMimeType) { xobj.overrideMimeType("application/json"); }
        xobj.open('GET', ridePath, true);
        xobj.onreadystatechange = function () {
            if (xobj.readyState == 4) {
                rider.isRequesting = false;
                var jsonText = xobj.responseText;
                rider.startRide(rideIndex, ride, JSON.parse(jsonText));
            }
        };
        xobj.send();
        return true;
    }
    return false;
}

Rider.prototype.selectRide = function() {
    
    // Parse hash for ride and check to see if its already selected
    var selected = window.location.hash.slice(1);
    if (selected == null || selected == "") {
        return;
    } else {
        selected = Number(selected);
    }
    if (isNaN(selected) || selected < 0 && selected >= this.grid.rides) {
        return;
    }
    for (var i in this.rides) {
        if (this.rides[i] != null && this.rides[i].num == selected) {
            if (!this.rides[i].selected) {
                //this.selectControl.unselectAll();
                this.selectControl.select(this.rides[i].feature);
            }
            return;
        }
    }

    // Unselect all and reset hash
    this.selectControl.unselectAll();
    window.location.hash = "#" + selected;

    // search for empty spot ride (if not, pick a random one)
    var empty = Math.floor(this.maxrides * Math.random());
    for (var i = 0; i < this.maxrides; i++) {
        if (!this.rides[i]) {
            empty = i;
            continue;
        }
    }

    if (this.rides[empty]) {
        this.rides[empty].destroy();
        this.rides[empty] = null;
    }
    this.requestRide(empty, selected);
}

Rider.prototype.startRide = function(rideIndex, rideNum, rideData) {
    if (rideData['rides'] == null || rideData['rides'].length == 0 || rideData['rides'][0]['ride'].length == 0) {
        badRide = [rideNum, rideData];
        return;
    }
    this.rides[rideIndex] = new Ride(this, rideIndex, rideNum, rideData['rides']);

    for (var i in this.rides) {
        if (this.rides[i].selected && this.rides[i].num == rideNum) {
            return;
        }
    }
    if (window.location.hash == '#' + rideNum) {
        this.selectControl.select(this.rides[rideIndex].feature);
    }   
}

function Ride(rider, index, rideNum, rideData) {
    this.rider = rider;
    this.index = index;
    this.num = rideNum;
    this.rides = rideData;
    
    this.feature = null;
    this.setLeg(0);

    this.rideFeatures = [];

    this.point = this.getPoint();
    this.selected = false;

    var size = new OpenLayers.Size;
    
    if (this.point == null || this.point.lat == 0) {
        badRide = this;
        return;
    }

}

Ride.prototype.destroy = function() {
    if (this.selected) {
        this.rider.selectControl.unselect(this.feature);
    }
    this.rider.layer.removeFeatures([this.feature]);
    this.rider.routeLayer.removeFeatures(this.rideFeatures);
    this.feature.destroy();
    this.rider = null;
}


Ride.prototype.setLeg = function (leg) {
    var reselect = this.selected;
    this.leg = leg;
    this.current = 0;
    this.time = 0;
    if (reselect) {
        this.rider.selectControl.unselectAll();
    }
    this.img = this.rider.grid.tagData(this.rides[leg]['tag'])['img'];
    if (this.feature != null) {
        this.rider.layer.removeFeatures([this.feature]);
        this.feature.destroy();
    }
    this.point = this.getPoint();
    this.feature = new OpenLayers.Feature.Vector(new OpenLayers.Geometry.Point(this.point.lon, this.point.lat), this);
    this.rider.layer.addFeatures([this.feature]);
    if (reselect) {
        this.rider.selectControl.select(this.feature);
    }
}



Ride.prototype.select = function() {
    selectedRide = this;
    this.selected = true;
    if (this.rideFeatures != null) {
        this.rider.routeLayer.removeFeatures(this.rideFeatures);
    }
    this.rideFeatures = [];
    for (var i in this.rides) {
        var line = new OpenLayers.Geometry.LineString();
        var lines = [];
        var ride = this.rides[i]["ride"];
        var tag = this.rides[i]["tag"];
        var time = 0;
        for (var j in ride) {
            var tpoint = ride[j];
            if (tpoint[2] - time > this.rider.grid.maxSkip) {
                if (line.getVertices().length > 2) {
                    lines[lines.length] = line;
                }
                line = new OpenLayers.Geometry.LineString();
            }
            time = tpoint[2];

            line.addPoint(new OpenLayers.Geometry.Point(tpoint[0], tpoint[1]));
        }
        if (line.getVertices().length > 2) {
            lines[lines.length] = line;
        }

        if (lines.length == 0) {
            return null;
        }
        
        var multiline = new OpenLayers.Geometry.MultiLineString(lines);
        multiline.transform(this.rider.grid.projection, this.rider.map.baseLayer.projection);
        this.rideFeatures[this.rideFeatures.length] = new OpenLayers.Feature.Vector(multiline, this, {
                stroke: true,
                strokeColor: this.rider.grid.tagData(tag)['color'],
                strokeWidth: 4,
                strokeOpacity: 0.8,
                graphicZIndex: 15});
        this.rideFeatures[this.rideFeatures.length] = new OpenLayers.Feature.Vector(multiline.clone(), this, {
                stroke: true,
                strokeColor: "#000000",
                strokeWidth: 1.5,
                strokeOpacity: 1,
                graphicZIndex: 10});
    }
    this.rider.routeLayer.addFeatures(this.rideFeatures);

}

Ride.prototype.unselect = function() {
    this.selected = false;
    if (this.rideFeatures != null) {
        this.rider.routeLayer.removeFeatures(this.rideFeatures);
        this.rideFeatures = null;
    }
}

Ride.prototype.step = function (millis) {
    this.time += millis * this.rider.speed;
    var nextPoint = this.getPoint();

    if (nextPoint == null) {
        if (this.time > 1000 * this.rider.grid.maxSkip) {
            endTime = this.time;
            //this.rider.markers.removeMarker(this.marker);
            if (this.leg + 1 < this.rides.length) {
                this.setLeg(this.leg + 1);
            } else if (!this.selected) { 
                this.rider.layer.removeFeatures([this.feature]);
                this.feature.destroy();
                return true;
            } else {
               this.setLeg(0);
            }
        }
    } else {
        this.feature.move(nextPoint);
    }
    return false;
}


Ride.prototype.getPoint = function() {

    var point = null;
    var ride = this.rides[this.leg]["ride"];
    var tag = this.rides[this.leg]["tag"]
    
    while (point == null && this.current + 1 < ride.length) {
        var lastPoint = ride[this.current];
        var nextPoint = ride[this.current + 1];
        var duration = nextPoint[2] - lastPoint[2];
        var maxMillis = 1000 * Math.min(this.rider.grid.maxSkip, duration);
        if (this.time > maxMillis) {
            this.time -= maxMillis;
            this.current++;
            continue;
        }

        var x, y;
        if (this.rider.grid.maxSkip < duration) {
            x = lastPoint[0];
            y = lastPoint[1];
        } else {
            x = (lastPoint[0]*(maxMillis - this.time) + nextPoint[0]*this.time)/maxMillis;
            y = (lastPoint[1]*(maxMillis - this.time) + nextPoint[1]*this.time)/maxMillis;
        }
        var point = new OpenLayers.LonLat(x,y).transform(this.rider.grid.projection, this.rider.map.baseLayer.projection);
    }
    return point;
}

var map, gridLayer;
var gripCells;

var fromTransform = new OpenLayers.Projection("EPSG:4326");
var toTransform = new OpenLayers.Projection("EPSG:900913");
var timeThreshold = 0.6;
var cellColor = "#FF0000";
var selectControl;

function init(){
    map = new OpenLayers.Map('map');

    //var wmsLayer = new OpenLayers.Layer.WMS( "OpenLayers WMS",
    //    "http://vmap0.tiles.osgeo.org/wms/vmap0?", {layers: 'basic'});
    var omLayer = new OpenLayers.Layer.OSM();
    gridLayer = new OpenLayers.Layer.Vector("Grid Layer", 
      { projection: new OpenLayers.Projection("EPSG:4326")
      , renderers: ["Canvas", "SVG", "VML"]
      });
    gridLayer.events.on({
        'featureselected': onFeatureSelect,
        'featureunselected': onFeatureUnselect
    });
    map.addLayers([omLayer, gridLayer]);
    //map.addControl(new OpenLayers.Control.LayerSwitcher());
    //map.addControl(new OpenLayers.Control.MousePosition());
    //map.setCenter(new OpenLayers.LonLat(0, 0), 3);
    
    var report = function(e) {
        OpenLayers.Console.log(e.type, e.feature.id);
    };
            
    var highlightCtrl = new OpenLayers.Control.SelectFeature(gridLayer, {
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
    
    selectControl = new OpenLayers.Control.SelectFeature(gridLayer,
        {clickout: true}
    );
    map.addControl(selectControl);
    selectControl.activate();
    

    var xobj = new XMLHttpRequest();
    xobj.overrideMimeType("application/json");
    xobj.open('GET', 'data.json', true);
    xobj.onreadystatechange = function () {
      if (xobj.readyState == 4) {
        var jsonText = xobj.responseText;
        newGrid(JSON.parse(jsonText));
      }
    };
    xobj.send();
}

function hexItoP(d, p) {
  var s = d[1];
  var h = d[2];
  return [p[0] * s, h * (p[1] - 0.5 * Math.abs(p[0] % 2))];
}

function newGrid(data) {
  gridCells = {};
  gridLayer.removeAllFeatures();
  var desc = data['desc']
  var r = desc[0];
  var s = desc[1];
  var h = desc[2];
  var p = hexItoP(desc, data['cells'][0]["pos"]);

  var maxTime = data['maxTime'];
  var opScale = function(x) {
    return 0.2+ 0.8*(Math.log(1+x["time"])/Math.log(1+maxTime));
  };
  var totalTime = data['totalTime'];
  var currentTotal = 0;

  var minx = p[0], maxx = p[0], miny = p[1], maxy = p[1];
  var features = [];
  for(var i in data['cells']) {
    var cell = data['cells'][i];
    gridCells[cell[0]] = cell;
    p = hexItoP(desc, cell["pos"]);
    var x = p[0];
    var y = p[1];
    if(currentTotal < timeThreshold * totalTime) {
        currentTotal += cell["time"];
        if(x > maxx) {maxx = x;}
        if(x < minx) {minx = x;}
        if(y < miny) {miny = y;}
        if(y > maxy) {maxy = y;}
    }
                
    var points = new Array(
      new OpenLayers.Geometry.Point(x,y),
      new OpenLayers.Geometry.Point(x + r ,y),
      new OpenLayers.Geometry.Point(x+s,y+(h/2.0)),
      new OpenLayers.Geometry.Point(x+r,y+h),
      new OpenLayers.Geometry.Point(x,y+h),
      new OpenLayers.Geometry.Point(x+r-s,y+(h/2.0)));
    var hexPoly = new OpenLayers.Geometry.Polygon([new OpenLayers.Geometry.LinearRing(points)]);
    //hexPoly.transform(new OpenLayers.Projection("EPSG:900913"), new OpenLayers.Projection("EPSG:4326"))
    hexPoly.transform(fromTransform, toTransform);
    var hexFeature = new OpenLayers.Feature.Vector(hexPoly, cell, 
        {stroke:true,
        strokeColor: cellColor,
        strokeOpacity: opScale(cell),
        //strokeWidth: opScale(cell)*2,
        strokeWidth: 1,
        fillColor:cellColor, 
        fillOpacity:opScale(cell)});
    //var hexFeature = new OpenLayers.Feature.Vector(hexPoly, {size:cell[1]}, {stroke:false, fillColor:"grey", fillOpacity:opScale(cell)});
    features[features.length] = hexFeature;
  }
  //alert([currentTotal, totalTime]);
  gridLayer.addFeatures(features);
  var bbox = new OpenLayers.Bounds(minx, miny, maxx, maxy); // transform(map.displayProjection, map.projection);
  //bbox.transform(new OpenLayers.Projection("EPSG:900913"), new OpenLayers.Projection("EPSG:4326"));
  bbox.transform(fromTransform, toTransform);
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
    /*
    var hours = totalHours % 24;
    var totalDays = Math.floor(totalHours/24);
    var days = totalDays % 7;
    var totalWeeks = Math.floor(totalDays/7);

    var durStr = [];
    if (totalWeeks > 0) { durStr[durStr.length] = "" + totalWeeks + " weeks"; }
    if (days > 0) { durStr[durStr.length] = "" + days + " days"; }
    if (hours > 0) { durStr[durStr.length] = "" + hours + " hours"; }
    if (minutes > 0) { durStr[durStr.length] = "" + minutes + " minutes"; }
    if (secs > 0) { durStr[durStr.length] = "" + secs + " seconds"; }
    updateNode("totalTime", durStr.join(", "));
    */
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
                             "<h2>"+feature.geometry.getBounds().getCenterLonLat()+ "</h2>" +
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

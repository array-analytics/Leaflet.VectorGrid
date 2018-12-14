'use strict';

function __$strToBlobUri(str, mime, isBinary) {try {return window.URL.createObjectURL(new Blob([Uint8Array.from(str.split('').map(function(c) {return c.charCodeAt(0)}))], {type: mime}));} catch (e) {return "data:" + mime + (isBinary ? ";base64," : ",") + str;}}
L.SVG.Tile = L.SVG.extend({

	initialize: function (tileCoord, tileSize, options) {
		L.SVG.prototype.initialize.call(this, options);
		this._tileCoord = tileCoord;
		this._size = tileSize;

		this._initContainer();
		this._container.setAttribute('width', this._size.x);
		this._container.setAttribute('height', this._size.y);
		this._container.setAttribute('viewBox', [0, 0, this._size.x, this._size.y].join(' '));

		this._layers = {};
	},

	getCoord: function() {
		return this._tileCoord;
	},

	getContainer: function() {
		return this._container;
	},

	onAdd: L.Util.falseFn,

	addTo: function(map) {
		this._map = map;
		if (this.options.interactive) {
			for (var i in this._layers) {
				var layer = this._layers[i];
				// By default, Leaflet tiles do not have pointer events.
				layer._path.style.pointerEvents = 'auto';
				this._map._targets[L.stamp(layer._path)] = layer;
			}
		}
	},

	removeFrom: function (map) {
		if (this.options.interactive) {
			for (var i in this._layers) {
				var layer = this._layers[i];
				delete this._map._targets[L.stamp(layer._path)];
			}
		}
		delete this._map;
	},

	_initContainer: function() {
		L.SVG.prototype._initContainer.call(this);
		var rect =  L.SVG.create('rect');
	},

	/// TODO: Modify _initPath to include an extra parameter, a group name
	/// to order symbolizers by z-index

	_addPath: function (layer) {
		this._rootGroup.appendChild(layer._path);
		this._layers[L.stamp(layer)] = layer;
	},

	_updateIcon: function (layer) {
		var path = layer._path = L.SVG.create('image'),
		    icon = layer.options.icon,
		    options = icon.options,
		    size = L.point(options.iconSize),
		    anchor = options.iconAnchor ||
		        	 size && size.divideBy(2, true),
		    p = layer._point.subtract(anchor);
		path.setAttribute('x', p.x);
		path.setAttribute('y', p.y);
		path.setAttribute('width', size.x + 'px');
		path.setAttribute('height', size.y + 'px');
		path.setAttribute('href', options.iconUrl);
	}
});


L.svg.tile = function(tileCoord, tileSize, opts){
	return new L.SVG.Tile(tileCoord, tileSize, opts);
};

// 🍂class Symbolizer
// 🍂inherits Class
// The abstract Symbolizer class is mostly equivalent in concept to a `L.Path` - it's an interface for
// polylines, polygons and circles. But instead of representing leaflet Layers,
// it represents things that have to be drawn inside a vector tile.

// A vector tile *data layer* might have zero, one, or more *symbolizer definitions*
// A vector tile *feature* might have zero, one, or more *symbolizers*.
// The actual symbolizers applied will depend on filters and the symbolizer functions.

var Symbolizer = L.Class.extend({
	// 🍂method initialize(feature: GeoJSON, pxPerExtent: Number)
	// Initializes a new Line Symbolizer given a GeoJSON feature and the
	// pixel-to-coordinate-units ratio. Internal use only.

	// 🍂method render(renderer, style)
	// Renders this symbolizer in the given tiled renderer, with the given
	// `L.Path` options.  Internal use only.
	render: function(renderer, style) {
		this._renderer = renderer;
		this.options = style;
		renderer._initPath(this);
		renderer._updateStyle(this);
	},

	// 🍂method render(renderer, style)
	// Updates the `L.Path` options used to style this symbolizer, and re-renders it.
	// Internal use only.
	updateStyle: function(renderer, style) {
		this.options = style;
		renderer._updateStyle(this);
	},

	_getPixelBounds: function() {
		var parts = this._parts;
		var bounds = L.bounds([]);
		for (var i = 0; i < parts.length; i++) {
			var part = parts[i];
			for (var j = 0; j < part.length; j++) {
				bounds.extend(part[j]);
			}
		}

		var w = this._clickTolerance(),
		    p = new L.Point(w, w);

		bounds.min._subtract(p);
		bounds.max._add(p);

		return bounds;
	},
	_clickTolerance: L.Path.prototype._clickTolerance,
});

// Contains mixins which are common to the Line Symbolizer and the Fill Symbolizer.

var PolyBase = {
	_makeFeatureParts: function(feat, pxPerExtent) {
		var rings = feat.geometry;
		var coord;

		this._parts = [];
		for (var i = 0; i < rings.length; i++) {
			var ring = rings[i];
			var part = [];
			for (var j = 0; j < ring.length; j++) {
				coord = ring[j];
				// Protobuf vector tiles return {x: , y:}
				// Geojson-vt returns [,]
				part.push(L.point(coord).scaleBy(pxPerExtent));
			}
			this._parts.push(part);
		}
	},

	makeInteractive: function() {
		this._pxBounds = this._getPixelBounds();
	}
};

// 🍂class PointSymbolizer
// 🍂inherits CircleMarker
// A symbolizer for points.

var PointSymbolizer = L.CircleMarker.extend({
	includes: Symbolizer.prototype,

	statics: {
		iconCache: {}
	},

	initialize: function(feature, pxPerExtent) {
		this.properties = feature.properties;
		this._makeFeatureParts(feature, pxPerExtent);
	},

	render: function(renderer, style) {
		Symbolizer.prototype.render.call(this, renderer, style);
		this._radius = style.radius || L.CircleMarker.prototype.options.radius;
		this._updatePath();
	},

	_makeFeatureParts: function(feat, pxPerExtent) {
		var coord = feat.geometry[0];
		if (typeof coord[0] === 'object' && 'x' in coord[0]) {
			// Protobuf vector tiles return [{x: , y:}]
			this._point = L.point(coord[0]).scaleBy(pxPerExtent);
			this._empty = L.Util.falseFn;
		} else {
			// Geojson-vt returns [,]
			this._point = L.point(coord).scaleBy(pxPerExtent);
			this._empty = L.Util.falseFn;
		}
	},

	makeInteractive: function() {
		this._updateBounds();
	},

	updateStyle: function(renderer, style) {
		this._radius = style.radius || this._radius;
		this._updateBounds();
		return Symbolizer.prototype.updateStyle.call(this, renderer, style);
	},

	_updateBounds: function() {
		var icon = this.options.icon;
		if (icon) {
			var size = L.point(icon.options.iconSize),
			    anchor = icon.options.iconAnchor ||
			             size && size.divideBy(2, true),
			    p = this._point.subtract(anchor);
			this._pxBounds = new L.Bounds(p, p.add(icon.options.iconSize));
		} else {
			L.CircleMarker.prototype._updateBounds.call(this);
		}
	},

	_updatePath: function() {
		if (this.options.icon) {
			this._renderer._updateIcon(this);
		} else {
			L.CircleMarker.prototype._updatePath.call(this);
		}
	},

	_getImage: function () {
		if (this.options.icon) {
			var url = this.options.icon.options.iconUrl,
			    img = PointSymbolizer.iconCache[url];
			if (!img) {
				var icon = this.options.icon;
				img = PointSymbolizer.iconCache[url] = icon.createIcon();
			}
			return img;
		} else {
			return null;
		}

	},

	_containsPoint: function(p) {
		var icon = this.options.icon;
		if (icon) {
			return this._pxBounds.contains(p);
		} else {
			return L.CircleMarker.prototype._containsPoint.call(this, p);
		}
	}
});

// 🍂class LineSymbolizer
// 🍂inherits Polyline
// A symbolizer for lines. Can be applied to line and polygon features.

var LineSymbolizer = L.Polyline.extend({
	includes: [Symbolizer.prototype, PolyBase],

	initialize: function(feature, pxPerExtent) {
		this.properties = feature.properties;
		this._makeFeatureParts(feature, pxPerExtent);
	},

	render: function(renderer, style) {
		style.fill = false;
		Symbolizer.prototype.render.call(this, renderer, style);
		this._updatePath();
	},

	updateStyle: function(renderer, style) {
		style.fill = false;
		Symbolizer.prototype.updateStyle.call(this, renderer, style);
	},
});

// 🍂class FillSymbolizer
// 🍂inherits Polyline
// A symbolizer for filled areas. Applies only to polygon features.

var FillSymbolizer = L.Polygon.extend({
	includes: [Symbolizer.prototype, PolyBase],

	initialize: function(feature, pxPerExtent) {
		this.properties = feature.properties;
		this._makeFeatureParts(feature, pxPerExtent);
	},

	render: function(renderer, style) {
		Symbolizer.prototype.render.call(this, renderer, style);
		this._updatePath();
	}
});

/* 🍂class VectorGrid
 * 🍂inherits GridLayer
 *
 * A `VectorGrid` is a generic, abstract class for displaying tiled vector data.
 * it provides facilities for symbolizing and rendering the data in the vector
 * tiles, but lacks the functionality to fetch the vector tiles from wherever
 * they are.
 *
 * Extends Leaflet's `L.GridLayer`.
 */

L.VectorGrid = L.GridLayer.extend({

	options: {
		// 🍂option rendererFactory = L.svg.tile
		// A factory method which will be used to instantiate the per-tile renderers.
		rendererFactory: L.svg.tile,

		// 🍂option vectorTileLayerStyles: Object = {}
		// A data structure holding initial symbolizer definitions for the vector features.
		vectorTileLayerStyles: {},

		// 🍂option interactive: Boolean = false
		// Whether this `VectorGrid` fires `Interactive Layer` events.
		interactive: false,

		// 🍂option getFeatureId: Function = undefined
		// A function that, given a vector feature, returns an unique identifier for it, e.g.
		// `function(feat) { return feat.properties.uniqueIdField; }`.
		// Must be defined for `setFeatureStyle` to work.
	},

	initialize: function(options) {
		L.setOptions(this, options);
		L.GridLayer.prototype.initialize.apply(this, arguments);
		if (this.options.getFeatureId) {
			this._vectorTiles = {};
			this._overriddenStyles = {};
			this.on('tileunload', function(e) {
				var key = this._tileCoordsToKey(e.coords),
				    tile = this._vectorTiles[key];

				if (tile && this._map) {
					tile.removeFrom(this._map);
				}
				delete this._vectorTiles[key];
			}, this);
		}
		this._dataLayerNames = {};
	},

	createTile: function(coords, done) {

		var storeFeatures = this.options.getFeatureId;

		var tileSize = this.getTileSize();
		var renderer = this.options.rendererFactory(coords, tileSize, this.options);

		var tileBounds = this._tileCoordsToBounds(coords);	

		var vectorTilePromise = this._getVectorTilePromise(coords, tileBounds);

		if (storeFeatures) {
			this._vectorTiles[this._tileCoordsToKey(coords)] = renderer;
			renderer._features = {};
		}

		vectorTilePromise.then( function renderTile(vectorTile) {

			if (vectorTile.layers && vectorTile.layers.length !== 0) {

				for (var layerName in vectorTile.layers) {
					this._dataLayerNames[layerName] = true;
					var layer = vectorTile.layers[layerName];
	
					var pxPerExtent = this.getTileSize().divideBy(layer.extent);
	
					var layerStyle = this.options.vectorTileLayerStyles[ layerName ] ||
					L.Path.prototype.options;
	
					for (var i = 0; i < layer.features.length; i++) {
						var feat = layer.features[i];
						var id;
	
						var styleOptions = layerStyle;
						if (storeFeatures) {
							id = this.options.getFeatureId(feat);
							var styleOverride = this._overriddenStyles[id];
							if (styleOverride) {
								if (styleOverride[layerName]) {
									styleOptions = styleOverride[layerName];
								} else {
									styleOptions = styleOverride;
								}
							}
						}
	
						if (styleOptions instanceof Function) {
							styleOptions = styleOptions(feat.properties, coords.z);
						}
	
						if (!(styleOptions instanceof Array)) {
							styleOptions = [styleOptions];
						}
	
						if (!styleOptions.length) {
							continue;
						}
	
						var featureLayer = this._createLayer(feat, pxPerExtent);
	
						for (var j = 0; j < styleOptions.length; j++) {
							var style = L.extend({}, L.Path.prototype.options, styleOptions[j]);
							featureLayer.render(renderer, style);
							renderer._addPath(featureLayer);
						}
	
						if (this.options.interactive) {
							featureLayer.makeInteractive();
						}
	
						if (storeFeatures) {
							renderer._features[id] = {
								layerName: layerName,
								feature: featureLayer
							};
						}
					}
	
				}
	
			}
		
			if (this._map != null) {
				renderer.addTo(this._map);
			}
	
			L.Util.requestAnimFrame(done.bind(coords, null, null));

		}.bind(this));

		return renderer.getContainer();
	},

	// 🍂method setFeatureStyle(id: Number, layerStyle: L.Path Options): this
	// Given the unique ID for a vector features (as per the `getFeatureId` option),
	// re-symbolizes that feature across all tiles it appears in.
	setFeatureStyle: function(id, layerStyle) {
		this._overriddenStyles[id] = layerStyle;

		for (var tileKey in this._vectorTiles) {
			var tile = this._vectorTiles[tileKey];
			var features = tile._features;
			var data = features[id];
			if (data) {
				var feat = data.feature;

				var styleOptions = layerStyle;
				if (layerStyle[data.layerName]) {
					styleOptions = layerStyle[data.layerName];
				}

				this._updateStyles(feat, tile, styleOptions);
			}
		}
		return this;
	},

	// 🍂method setFeatureStyle(id: Number): this
	// Reverts the effects of a previous `setFeatureStyle` call.
	resetFeatureStyle: function(id) {
		delete this._overriddenStyles[id];

		for (var tileKey in this._vectorTiles) {
			var tile = this._vectorTiles[tileKey];
			var features = tile._features;
			var data = features[id];
			if (data) {
				var feat = data.feature;
				var styleOptions = this.options.vectorTileLayerStyles[ data.layerName ] ||
				L.Path.prototype.options;
				this._updateStyles(feat, tile, styleOptions);
			}
		}
		return this;
	},

	// 🍂method getDataLayerNames(): Array
	// Returns an array of strings, with all the known names of data layers in
	// the vector tiles displayed. Useful for introspection.
	getDataLayerNames: function() {
		return Object.keys(this._dataLayerNames);
	},

	_updateStyles: function(feat, renderer, styleOptions) {
		styleOptions = (styleOptions instanceof Function) ?
			styleOptions(feat.properties, renderer.getCoord().z) :
			styleOptions;

		if (!(styleOptions instanceof Array)) {
			styleOptions = [styleOptions];
		}

		for (var j = 0; j < styleOptions.length; j++) {
			var style = L.extend({}, L.Path.prototype.options, styleOptions[j]);
			feat.updateStyle(renderer, style);
		}
	},

	_createLayer: function(feat, pxPerExtent, layerStyle) {
		var layer;
		switch (feat.type) {
		case 1:
			layer = new PointSymbolizer(feat, pxPerExtent);
			break;
		case 2:
			layer = new LineSymbolizer(feat, pxPerExtent);
			break;
		case 3:
			layer = new FillSymbolizer(feat, pxPerExtent);
			break;
		}

		if (this.options.interactive) {
			layer.addEventParent(this);
		}

		return layer;
	},
});

/*
 * 🍂section Extension methods
 *
 * Classes inheriting from `VectorGrid` **must** define the `_getVectorTilePromise` private method.
 *
 * 🍂method getVectorTilePromise(coords: Object): Promise
 * Given a `coords` object in the form of `{x: Number, y: Number, z: Number}`,
 * this function must return a `Promise` for a vector tile.
 *
 */
L.vectorGrid = function (options) {
	return new L.VectorGrid(options);
};

var read = function (buffer, offset, isLE, mLen, nBytes) {
  var e, m;
  var eLen = (nBytes * 8) - mLen - 1;
  var eMax = (1 << eLen) - 1;
  var eBias = eMax >> 1;
  var nBits = -7;
  var i = isLE ? (nBytes - 1) : 0;
  var d = isLE ? -1 : 1;
  var s = buffer[offset + i];

  i += d;

  e = s & ((1 << (-nBits)) - 1);
  s >>= (-nBits);
  nBits += eLen;
  for (; nBits > 0; e = (e * 256) + buffer[offset + i], i += d, nBits -= 8) {}

  m = e & ((1 << (-nBits)) - 1);
  e >>= (-nBits);
  nBits += mLen;
  for (; nBits > 0; m = (m * 256) + buffer[offset + i], i += d, nBits -= 8) {}

  if (e === 0) {
    e = 1 - eBias;
  } else if (e === eMax) {
    return m ? NaN : ((s ? -1 : 1) * Infinity)
  } else {
    m = m + Math.pow(2, mLen);
    e = e - eBias;
  }
  return (s ? -1 : 1) * m * Math.pow(2, e - mLen)
};

var write = function (buffer, value, offset, isLE, mLen, nBytes) {
  var e, m, c;
  var eLen = (nBytes * 8) - mLen - 1;
  var eMax = (1 << eLen) - 1;
  var eBias = eMax >> 1;
  var rt = (mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0);
  var i = isLE ? 0 : (nBytes - 1);
  var d = isLE ? 1 : -1;
  var s = value < 0 || (value === 0 && 1 / value < 0) ? 1 : 0;

  value = Math.abs(value);

  if (isNaN(value) || value === Infinity) {
    m = isNaN(value) ? 1 : 0;
    e = eMax;
  } else {
    e = Math.floor(Math.log(value) / Math.LN2);
    if (value * (c = Math.pow(2, -e)) < 1) {
      e--;
      c *= 2;
    }
    if (e + eBias >= 1) {
      value += rt / c;
    } else {
      value += rt * Math.pow(2, 1 - eBias);
    }
    if (value * c >= 2) {
      e++;
      c /= 2;
    }

    if (e + eBias >= eMax) {
      m = 0;
      e = eMax;
    } else if (e + eBias >= 1) {
      m = ((value * c) - 1) * Math.pow(2, mLen);
      e = e + eBias;
    } else {
      m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen);
      e = 0;
    }
  }

  for (; mLen >= 8; buffer[offset + i] = m & 0xff, i += d, m /= 256, mLen -= 8) {}

  e = (e << mLen) | m;
  eLen += mLen;
  for (; eLen > 0; buffer[offset + i] = e & 0xff, i += d, e /= 256, eLen -= 8) {}

  buffer[offset + i - d] |= s * 128;
};

var index$1 = {
	read: read,
	write: write
};

var index = Pbf;

var ieee754 = index$1;

function Pbf(buf) {
    this.buf = ArrayBuffer.isView && ArrayBuffer.isView(buf) ? buf : new Uint8Array(buf || 0);
    this.pos = 0;
    this.type = 0;
    this.length = this.buf.length;
}

Pbf.Varint  = 0; // varint: int32, int64, uint32, uint64, sint32, sint64, bool, enum
Pbf.Fixed64 = 1; // 64-bit: double, fixed64, sfixed64
Pbf.Bytes   = 2; // length-delimited: string, bytes, embedded messages, packed repeated fields
Pbf.Fixed32 = 5; // 32-bit: float, fixed32, sfixed32

var SHIFT_LEFT_32 = (1 << 16) * (1 << 16);
var SHIFT_RIGHT_32 = 1 / SHIFT_LEFT_32;

Pbf.prototype = {

    destroy: function() {
        this.buf = null;
    },

    // === READING =================================================================

    readFields: function(readField, result, end) {
        var this$1 = this;

        end = end || this.length;

        while (this.pos < end) {
            var val = this$1.readVarint(),
                tag = val >> 3,
                startPos = this$1.pos;

            this$1.type = val & 0x7;
            readField(tag, result, this$1);

            if (this$1.pos === startPos) { this$1.skip(val); }
        }
        return result;
    },

    readMessage: function(readField, result) {
        return this.readFields(readField, result, this.readVarint() + this.pos);
    },

    readFixed32: function() {
        var val = readUInt32(this.buf, this.pos);
        this.pos += 4;
        return val;
    },

    readSFixed32: function() {
        var val = readInt32(this.buf, this.pos);
        this.pos += 4;
        return val;
    },

    // 64-bit int handling is based on github.com/dpw/node-buffer-more-ints (MIT-licensed)

    readFixed64: function() {
        var val = readUInt32(this.buf, this.pos) + readUInt32(this.buf, this.pos + 4) * SHIFT_LEFT_32;
        this.pos += 8;
        return val;
    },

    readSFixed64: function() {
        var val = readUInt32(this.buf, this.pos) + readInt32(this.buf, this.pos + 4) * SHIFT_LEFT_32;
        this.pos += 8;
        return val;
    },

    readFloat: function() {
        var val = ieee754.read(this.buf, this.pos, true, 23, 4);
        this.pos += 4;
        return val;
    },

    readDouble: function() {
        var val = ieee754.read(this.buf, this.pos, true, 52, 8);
        this.pos += 8;
        return val;
    },

    readVarint: function(isSigned) {
        var buf = this.buf,
            val, b;

        b = buf[this.pos++]; val  =  b & 0x7f;        if (b < 0x80) { return val; }
        b = buf[this.pos++]; val |= (b & 0x7f) << 7;  if (b < 0x80) { return val; }
        b = buf[this.pos++]; val |= (b & 0x7f) << 14; if (b < 0x80) { return val; }
        b = buf[this.pos++]; val |= (b & 0x7f) << 21; if (b < 0x80) { return val; }
        b = buf[this.pos];   val |= (b & 0x0f) << 28;

        return readVarintRemainder(val, isSigned, this);
    },

    readVarint64: function() { // for compatibility with v2.0.1
        return this.readVarint(true);
    },

    readSVarint: function() {
        var num = this.readVarint();
        return num % 2 === 1 ? (num + 1) / -2 : num / 2; // zigzag encoding
    },

    readBoolean: function() {
        return Boolean(this.readVarint());
    },

    readString: function() {
        var end = this.readVarint() + this.pos,
            str = readUtf8(this.buf, this.pos, end);
        this.pos = end;
        return str;
    },

    readBytes: function() {
        var end = this.readVarint() + this.pos,
            buffer = this.buf.subarray(this.pos, end);
        this.pos = end;
        return buffer;
    },

    // verbose for performance reasons; doesn't affect gzipped size

    readPackedVarint: function(arr, isSigned) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readVarint(isSigned)); }
        return arr;
    },
    readPackedSVarint: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readSVarint()); }
        return arr;
    },
    readPackedBoolean: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readBoolean()); }
        return arr;
    },
    readPackedFloat: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readFloat()); }
        return arr;
    },
    readPackedDouble: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readDouble()); }
        return arr;
    },
    readPackedFixed32: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readFixed32()); }
        return arr;
    },
    readPackedSFixed32: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readSFixed32()); }
        return arr;
    },
    readPackedFixed64: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readFixed64()); }
        return arr;
    },
    readPackedSFixed64: function(arr) {
        var this$1 = this;

        var end = readPackedEnd(this);
        arr = arr || [];
        while (this.pos < end) { arr.push(this$1.readSFixed64()); }
        return arr;
    },

    skip: function(val) {
        var type = val & 0x7;
        if (type === Pbf.Varint) { while (this.buf[this.pos++] > 0x7f) {} }
        else if (type === Pbf.Bytes) { this.pos = this.readVarint() + this.pos; }
        else if (type === Pbf.Fixed32) { this.pos += 4; }
        else if (type === Pbf.Fixed64) { this.pos += 8; }
        else { throw new Error('Unimplemented type: ' + type); }
    },

    // === WRITING =================================================================

    writeTag: function(tag, type) {
        this.writeVarint((tag << 3) | type);
    },

    realloc: function(min) {
        var length = this.length || 16;

        while (length < this.pos + min) { length *= 2; }

        if (length !== this.length) {
            var buf = new Uint8Array(length);
            buf.set(this.buf);
            this.buf = buf;
            this.length = length;
        }
    },

    finish: function() {
        this.length = this.pos;
        this.pos = 0;
        return this.buf.subarray(0, this.length);
    },

    writeFixed32: function(val) {
        this.realloc(4);
        writeInt32(this.buf, val, this.pos);
        this.pos += 4;
    },

    writeSFixed32: function(val) {
        this.realloc(4);
        writeInt32(this.buf, val, this.pos);
        this.pos += 4;
    },

    writeFixed64: function(val) {
        this.realloc(8);
        writeInt32(this.buf, val & -1, this.pos);
        writeInt32(this.buf, Math.floor(val * SHIFT_RIGHT_32), this.pos + 4);
        this.pos += 8;
    },

    writeSFixed64: function(val) {
        this.realloc(8);
        writeInt32(this.buf, val & -1, this.pos);
        writeInt32(this.buf, Math.floor(val * SHIFT_RIGHT_32), this.pos + 4);
        this.pos += 8;
    },

    writeVarint: function(val) {
        val = +val || 0;

        if (val > 0xfffffff || val < 0) {
            writeBigVarint(val, this);
            return;
        }

        this.realloc(4);

        this.buf[this.pos++] =           val & 0x7f  | (val > 0x7f ? 0x80 : 0); if (val <= 0x7f) { return; }
        this.buf[this.pos++] = ((val >>>= 7) & 0x7f) | (val > 0x7f ? 0x80 : 0); if (val <= 0x7f) { return; }
        this.buf[this.pos++] = ((val >>>= 7) & 0x7f) | (val > 0x7f ? 0x80 : 0); if (val <= 0x7f) { return; }
        this.buf[this.pos++] =   (val >>> 7) & 0x7f;
    },

    writeSVarint: function(val) {
        this.writeVarint(val < 0 ? -val * 2 - 1 : val * 2);
    },

    writeBoolean: function(val) {
        this.writeVarint(Boolean(val));
    },

    writeString: function(str) {
        str = String(str);
        this.realloc(str.length * 4);

        this.pos++; // reserve 1 byte for short string length

        var startPos = this.pos;
        // write the string directly to the buffer and see how much was written
        this.pos = writeUtf8(this.buf, str, this.pos);
        var len = this.pos - startPos;

        if (len >= 0x80) { makeRoomForExtraLength(startPos, len, this); }

        // finally, write the message length in the reserved place and restore the position
        this.pos = startPos - 1;
        this.writeVarint(len);
        this.pos += len;
    },

    writeFloat: function(val) {
        this.realloc(4);
        ieee754.write(this.buf, val, this.pos, true, 23, 4);
        this.pos += 4;
    },

    writeDouble: function(val) {
        this.realloc(8);
        ieee754.write(this.buf, val, this.pos, true, 52, 8);
        this.pos += 8;
    },

    writeBytes: function(buffer) {
        var this$1 = this;

        var len = buffer.length;
        this.writeVarint(len);
        this.realloc(len);
        for (var i = 0; i < len; i++) { this$1.buf[this$1.pos++] = buffer[i]; }
    },

    writeRawMessage: function(fn, obj) {
        this.pos++; // reserve 1 byte for short message length

        // write the message directly to the buffer and see how much was written
        var startPos = this.pos;
        fn(obj, this);
        var len = this.pos - startPos;

        if (len >= 0x80) { makeRoomForExtraLength(startPos, len, this); }

        // finally, write the message length in the reserved place and restore the position
        this.pos = startPos - 1;
        this.writeVarint(len);
        this.pos += len;
    },

    writeMessage: function(tag, fn, obj) {
        this.writeTag(tag, Pbf.Bytes);
        this.writeRawMessage(fn, obj);
    },

    writePackedVarint:   function(tag, arr) { this.writeMessage(tag, writePackedVarint, arr);   },
    writePackedSVarint:  function(tag, arr) { this.writeMessage(tag, writePackedSVarint, arr);  },
    writePackedBoolean:  function(tag, arr) { this.writeMessage(tag, writePackedBoolean, arr);  },
    writePackedFloat:    function(tag, arr) { this.writeMessage(tag, writePackedFloat, arr);    },
    writePackedDouble:   function(tag, arr) { this.writeMessage(tag, writePackedDouble, arr);   },
    writePackedFixed32:  function(tag, arr) { this.writeMessage(tag, writePackedFixed32, arr);  },
    writePackedSFixed32: function(tag, arr) { this.writeMessage(tag, writePackedSFixed32, arr); },
    writePackedFixed64:  function(tag, arr) { this.writeMessage(tag, writePackedFixed64, arr);  },
    writePackedSFixed64: function(tag, arr) { this.writeMessage(tag, writePackedSFixed64, arr); },

    writeBytesField: function(tag, buffer) {
        this.writeTag(tag, Pbf.Bytes);
        this.writeBytes(buffer);
    },
    writeFixed32Field: function(tag, val) {
        this.writeTag(tag, Pbf.Fixed32);
        this.writeFixed32(val);
    },
    writeSFixed32Field: function(tag, val) {
        this.writeTag(tag, Pbf.Fixed32);
        this.writeSFixed32(val);
    },
    writeFixed64Field: function(tag, val) {
        this.writeTag(tag, Pbf.Fixed64);
        this.writeFixed64(val);
    },
    writeSFixed64Field: function(tag, val) {
        this.writeTag(tag, Pbf.Fixed64);
        this.writeSFixed64(val);
    },
    writeVarintField: function(tag, val) {
        this.writeTag(tag, Pbf.Varint);
        this.writeVarint(val);
    },
    writeSVarintField: function(tag, val) {
        this.writeTag(tag, Pbf.Varint);
        this.writeSVarint(val);
    },
    writeStringField: function(tag, str) {
        this.writeTag(tag, Pbf.Bytes);
        this.writeString(str);
    },
    writeFloatField: function(tag, val) {
        this.writeTag(tag, Pbf.Fixed32);
        this.writeFloat(val);
    },
    writeDoubleField: function(tag, val) {
        this.writeTag(tag, Pbf.Fixed64);
        this.writeDouble(val);
    },
    writeBooleanField: function(tag, val) {
        this.writeVarintField(tag, Boolean(val));
    }
};

function readVarintRemainder(l, s, p) {
    var buf = p.buf,
        h, b;

    b = buf[p.pos++]; h  = (b & 0x70) >> 4;  if (b < 0x80) { return toNum(l, h, s); }
    b = buf[p.pos++]; h |= (b & 0x7f) << 3;  if (b < 0x80) { return toNum(l, h, s); }
    b = buf[p.pos++]; h |= (b & 0x7f) << 10; if (b < 0x80) { return toNum(l, h, s); }
    b = buf[p.pos++]; h |= (b & 0x7f) << 17; if (b < 0x80) { return toNum(l, h, s); }
    b = buf[p.pos++]; h |= (b & 0x7f) << 24; if (b < 0x80) { return toNum(l, h, s); }
    b = buf[p.pos++]; h |= (b & 0x01) << 31; if (b < 0x80) { return toNum(l, h, s); }

    throw new Error('Expected varint not more than 10 bytes');
}

function readPackedEnd(pbf) {
    return pbf.type === Pbf.Bytes ?
        pbf.readVarint() + pbf.pos : pbf.pos + 1;
}

function toNum(low, high, isSigned) {
    if (isSigned) {
        return high * 0x100000000 + (low >>> 0);
    }

    return ((high >>> 0) * 0x100000000) + (low >>> 0);
}

function writeBigVarint(val, pbf) {
    var low, high;

    if (val >= 0) {
        low  = (val % 0x100000000) | 0;
        high = (val / 0x100000000) | 0;
    } else {
        low  = ~(-val % 0x100000000);
        high = ~(-val / 0x100000000);

        if (low ^ 0xffffffff) {
            low = (low + 1) | 0;
        } else {
            low = 0;
            high = (high + 1) | 0;
        }
    }

    if (val >= 0x10000000000000000 || val < -0x10000000000000000) {
        throw new Error('Given varint doesn\'t fit into 10 bytes');
    }

    pbf.realloc(10);

    writeBigVarintLow(low, high, pbf);
    writeBigVarintHigh(high, pbf);
}

function writeBigVarintLow(low, high, pbf) {
    pbf.buf[pbf.pos++] = low & 0x7f | 0x80; low >>>= 7;
    pbf.buf[pbf.pos++] = low & 0x7f | 0x80; low >>>= 7;
    pbf.buf[pbf.pos++] = low & 0x7f | 0x80; low >>>= 7;
    pbf.buf[pbf.pos++] = low & 0x7f | 0x80; low >>>= 7;
    pbf.buf[pbf.pos]   = low & 0x7f;
}

function writeBigVarintHigh(high, pbf) {
    var lsb = (high & 0x07) << 4;

    pbf.buf[pbf.pos++] |= lsb         | ((high >>>= 3) ? 0x80 : 0); if (!high) { return; }
    pbf.buf[pbf.pos++]  = high & 0x7f | ((high >>>= 7) ? 0x80 : 0); if (!high) { return; }
    pbf.buf[pbf.pos++]  = high & 0x7f | ((high >>>= 7) ? 0x80 : 0); if (!high) { return; }
    pbf.buf[pbf.pos++]  = high & 0x7f | ((high >>>= 7) ? 0x80 : 0); if (!high) { return; }
    pbf.buf[pbf.pos++]  = high & 0x7f | ((high >>>= 7) ? 0x80 : 0); if (!high) { return; }
    pbf.buf[pbf.pos++]  = high & 0x7f;
}

function makeRoomForExtraLength(startPos, len, pbf) {
    var extraLen =
        len <= 0x3fff ? 1 :
        len <= 0x1fffff ? 2 :
        len <= 0xfffffff ? 3 : Math.ceil(Math.log(len) / (Math.LN2 * 7));

    // if 1 byte isn't enough for encoding message length, shift the data to the right
    pbf.realloc(extraLen);
    for (var i = pbf.pos - 1; i >= startPos; i--) { pbf.buf[i + extraLen] = pbf.buf[i]; }
}

function writePackedVarint(arr, pbf)   { for (var i = 0; i < arr.length; i++) { pbf.writeVarint(arr[i]); }   }
function writePackedSVarint(arr, pbf)  { for (var i = 0; i < arr.length; i++) { pbf.writeSVarint(arr[i]); }  }
function writePackedFloat(arr, pbf)    { for (var i = 0; i < arr.length; i++) { pbf.writeFloat(arr[i]); }    }
function writePackedDouble(arr, pbf)   { for (var i = 0; i < arr.length; i++) { pbf.writeDouble(arr[i]); }   }
function writePackedBoolean(arr, pbf)  { for (var i = 0; i < arr.length; i++) { pbf.writeBoolean(arr[i]); }  }
function writePackedFixed32(arr, pbf)  { for (var i = 0; i < arr.length; i++) { pbf.writeFixed32(arr[i]); }  }
function writePackedSFixed32(arr, pbf) { for (var i = 0; i < arr.length; i++) { pbf.writeSFixed32(arr[i]); } }
function writePackedFixed64(arr, pbf)  { for (var i = 0; i < arr.length; i++) { pbf.writeFixed64(arr[i]); }  }
function writePackedSFixed64(arr, pbf) { for (var i = 0; i < arr.length; i++) { pbf.writeSFixed64(arr[i]); } }

// Buffer code below from https://github.com/feross/buffer, MIT-licensed

function readUInt32(buf, pos) {
    return ((buf[pos]) |
        (buf[pos + 1] << 8) |
        (buf[pos + 2] << 16)) +
        (buf[pos + 3] * 0x1000000);
}

function writeInt32(buf, val, pos) {
    buf[pos] = val;
    buf[pos + 1] = (val >>> 8);
    buf[pos + 2] = (val >>> 16);
    buf[pos + 3] = (val >>> 24);
}

function readInt32(buf, pos) {
    return ((buf[pos]) |
        (buf[pos + 1] << 8) |
        (buf[pos + 2] << 16)) +
        (buf[pos + 3] << 24);
}

function readUtf8(buf, pos, end) {
    var str = '';
    var i = pos;

    while (i < end) {
        var b0 = buf[i];
        var c = null; // codepoint
        var bytesPerSequence =
            b0 > 0xEF ? 4 :
            b0 > 0xDF ? 3 :
            b0 > 0xBF ? 2 : 1;

        if (i + bytesPerSequence > end) { break; }

        var b1, b2, b3;

        if (bytesPerSequence === 1) {
            if (b0 < 0x80) {
                c = b0;
            }
        } else if (bytesPerSequence === 2) {
            b1 = buf[i + 1];
            if ((b1 & 0xC0) === 0x80) {
                c = (b0 & 0x1F) << 0x6 | (b1 & 0x3F);
                if (c <= 0x7F) {
                    c = null;
                }
            }
        } else if (bytesPerSequence === 3) {
            b1 = buf[i + 1];
            b2 = buf[i + 2];
            if ((b1 & 0xC0) === 0x80 && (b2 & 0xC0) === 0x80) {
                c = (b0 & 0xF) << 0xC | (b1 & 0x3F) << 0x6 | (b2 & 0x3F);
                if (c <= 0x7FF || (c >= 0xD800 && c <= 0xDFFF)) {
                    c = null;
                }
            }
        } else if (bytesPerSequence === 4) {
            b1 = buf[i + 1];
            b2 = buf[i + 2];
            b3 = buf[i + 3];
            if ((b1 & 0xC0) === 0x80 && (b2 & 0xC0) === 0x80 && (b3 & 0xC0) === 0x80) {
                c = (b0 & 0xF) << 0x12 | (b1 & 0x3F) << 0xC | (b2 & 0x3F) << 0x6 | (b3 & 0x3F);
                if (c <= 0xFFFF || c >= 0x110000) {
                    c = null;
                }
            }
        }

        if (c === null) {
            c = 0xFFFD;
            bytesPerSequence = 1;

        } else if (c > 0xFFFF) {
            c -= 0x10000;
            str += String.fromCharCode(c >>> 10 & 0x3FF | 0xD800);
            c = 0xDC00 | c & 0x3FF;
        }

        str += String.fromCharCode(c);
        i += bytesPerSequence;
    }

    return str;
}

function writeUtf8(buf, str, pos) {
    for (var i = 0, c, lead; i < str.length; i++) {
        c = str.charCodeAt(i); // code point

        if (c > 0xD7FF && c < 0xE000) {
            if (lead) {
                if (c < 0xDC00) {
                    buf[pos++] = 0xEF;
                    buf[pos++] = 0xBF;
                    buf[pos++] = 0xBD;
                    lead = c;
                    continue;
                } else {
                    c = lead - 0xD800 << 10 | c - 0xDC00 | 0x10000;
                    lead = null;
                }
            } else {
                if (c > 0xDBFF || (i + 1 === str.length)) {
                    buf[pos++] = 0xEF;
                    buf[pos++] = 0xBF;
                    buf[pos++] = 0xBD;
                } else {
                    lead = c;
                }
                continue;
            }
        } else if (lead) {
            buf[pos++] = 0xEF;
            buf[pos++] = 0xBF;
            buf[pos++] = 0xBD;
            lead = null;
        }

        if (c < 0x80) {
            buf[pos++] = c;
        } else {
            if (c < 0x800) {
                buf[pos++] = c >> 0x6 | 0xC0;
            } else {
                if (c < 0x10000) {
                    buf[pos++] = c >> 0xC | 0xE0;
                } else {
                    buf[pos++] = c >> 0x12 | 0xF0;
                    buf[pos++] = c >> 0xC & 0x3F | 0x80;
                }
                buf[pos++] = c >> 0x6 & 0x3F | 0x80;
            }
            buf[pos++] = c & 0x3F | 0x80;
        }
    }
    return pos;
}

var index$5 = Point$1;

/**
 * A standalone point geometry with useful accessor, comparison, and
 * modification methods.
 *
 * @class Point
 * @param {Number} x the x-coordinate. this could be longitude or screen
 * pixels, or any other sort of unit.
 * @param {Number} y the y-coordinate. this could be latitude or screen
 * pixels, or any other sort of unit.
 * @example
 * var point = new Point(-77, 38);
 */
function Point$1(x, y) {
    this.x = x;
    this.y = y;
}

Point$1.prototype = {

    /**
     * Clone this point, returning a new point that can be modified
     * without affecting the old one.
     * @return {Point} the clone
     */
    clone: function() { return new Point$1(this.x, this.y); },

    /**
     * Add this point's x & y coordinates to another point,
     * yielding a new point.
     * @param {Point} p the other point
     * @return {Point} output point
     */
    add:     function(p) { return this.clone()._add(p); },

    /**
     * Subtract this point's x & y coordinates to from point,
     * yielding a new point.
     * @param {Point} p the other point
     * @return {Point} output point
     */
    sub:     function(p) { return this.clone()._sub(p); },

    /**
     * Multiply this point's x & y coordinates by point,
     * yielding a new point.
     * @param {Point} p the other point
     * @return {Point} output point
     */
    multByPoint:    function(p) { return this.clone()._multByPoint(p); },

    /**
     * Divide this point's x & y coordinates by point,
     * yielding a new point.
     * @param {Point} p the other point
     * @return {Point} output point
     */
    divByPoint:     function(p) { return this.clone()._divByPoint(p); },

    /**
     * Multiply this point's x & y coordinates by a factor,
     * yielding a new point.
     * @param {Point} k factor
     * @return {Point} output point
     */
    mult:    function(k) { return this.clone()._mult(k); },

    /**
     * Divide this point's x & y coordinates by a factor,
     * yielding a new point.
     * @param {Point} k factor
     * @return {Point} output point
     */
    div:     function(k) { return this.clone()._div(k); },

    /**
     * Rotate this point around the 0, 0 origin by an angle a,
     * given in radians
     * @param {Number} a angle to rotate around, in radians
     * @return {Point} output point
     */
    rotate:  function(a) { return this.clone()._rotate(a); },

    /**
     * Rotate this point around p point by an angle a,
     * given in radians
     * @param {Number} a angle to rotate around, in radians
     * @param {Point} p Point to rotate around
     * @return {Point} output point
     */
    rotateAround:  function(a,p) { return this.clone()._rotateAround(a,p); },

    /**
     * Multiply this point by a 4x1 transformation matrix
     * @param {Array<Number>} m transformation matrix
     * @return {Point} output point
     */
    matMult: function(m) { return this.clone()._matMult(m); },

    /**
     * Calculate this point but as a unit vector from 0, 0, meaning
     * that the distance from the resulting point to the 0, 0
     * coordinate will be equal to 1 and the angle from the resulting
     * point to the 0, 0 coordinate will be the same as before.
     * @return {Point} unit vector point
     */
    unit:    function() { return this.clone()._unit(); },

    /**
     * Compute a perpendicular point, where the new y coordinate
     * is the old x coordinate and the new x coordinate is the old y
     * coordinate multiplied by -1
     * @return {Point} perpendicular point
     */
    perp:    function() { return this.clone()._perp(); },

    /**
     * Return a version of this point with the x & y coordinates
     * rounded to integers.
     * @return {Point} rounded point
     */
    round:   function() { return this.clone()._round(); },

    /**
     * Return the magitude of this point: this is the Euclidean
     * distance from the 0, 0 coordinate to this point's x and y
     * coordinates.
     * @return {Number} magnitude
     */
    mag: function() {
        return Math.sqrt(this.x * this.x + this.y * this.y);
    },

    /**
     * Judge whether this point is equal to another point, returning
     * true or false.
     * @param {Point} other the other point
     * @return {boolean} whether the points are equal
     */
    equals: function(other) {
        return this.x === other.x &&
               this.y === other.y;
    },

    /**
     * Calculate the distance from this point to another point
     * @param {Point} p the other point
     * @return {Number} distance
     */
    dist: function(p) {
        return Math.sqrt(this.distSqr(p));
    },

    /**
     * Calculate the distance from this point to another point,
     * without the square root step. Useful if you're comparing
     * relative distances.
     * @param {Point} p the other point
     * @return {Number} distance
     */
    distSqr: function(p) {
        var dx = p.x - this.x,
            dy = p.y - this.y;
        return dx * dx + dy * dy;
    },

    /**
     * Get the angle from the 0, 0 coordinate to this point, in radians
     * coordinates.
     * @return {Number} angle
     */
    angle: function() {
        return Math.atan2(this.y, this.x);
    },

    /**
     * Get the angle from this point to another point, in radians
     * @param {Point} b the other point
     * @return {Number} angle
     */
    angleTo: function(b) {
        return Math.atan2(this.y - b.y, this.x - b.x);
    },

    /**
     * Get the angle between this point and another point, in radians
     * @param {Point} b the other point
     * @return {Number} angle
     */
    angleWith: function(b) {
        return this.angleWithSep(b.x, b.y);
    },

    /*
     * Find the angle of the two vectors, solving the formula for
     * the cross product a x b = |a||b|sin(θ) for θ.
     * @param {Number} x the x-coordinate
     * @param {Number} y the y-coordinate
     * @return {Number} the angle in radians
     */
    angleWithSep: function(x, y) {
        return Math.atan2(
            this.x * y - this.y * x,
            this.x * x + this.y * y);
    },

    _matMult: function(m) {
        var x = m[0] * this.x + m[1] * this.y,
            y = m[2] * this.x + m[3] * this.y;
        this.x = x;
        this.y = y;
        return this;
    },

    _add: function(p) {
        this.x += p.x;
        this.y += p.y;
        return this;
    },

    _sub: function(p) {
        this.x -= p.x;
        this.y -= p.y;
        return this;
    },

    _mult: function(k) {
        this.x *= k;
        this.y *= k;
        return this;
    },

    _div: function(k) {
        this.x /= k;
        this.y /= k;
        return this;
    },

    _multByPoint: function(p) {
        this.x *= p.x;
        this.y *= p.y;
        return this;
    },

    _divByPoint: function(p) {
        this.x /= p.x;
        this.y /= p.y;
        return this;
    },

    _unit: function() {
        this._div(this.mag());
        return this;
    },

    _perp: function() {
        var y = this.y;
        this.y = this.x;
        this.x = -y;
        return this;
    },

    _rotate: function(angle) {
        var cos = Math.cos(angle),
            sin = Math.sin(angle),
            x = cos * this.x - sin * this.y,
            y = sin * this.x + cos * this.y;
        this.x = x;
        this.y = y;
        return this;
    },

    _rotateAround: function(angle, p) {
        var cos = Math.cos(angle),
            sin = Math.sin(angle),
            x = p.x + cos * (this.x - p.x) - sin * (this.y - p.y),
            y = p.y + sin * (this.x - p.x) + cos * (this.y - p.y);
        this.x = x;
        this.y = y;
        return this;
    },

    _round: function() {
        this.x = Math.round(this.x);
        this.y = Math.round(this.y);
        return this;
    }
};

/**
 * Construct a point from an array if necessary, otherwise if the input
 * is already a Point, or an unknown type, return it unchanged
 * @param {Array<Number>|Point|*} a any kind of input value
 * @return {Point} constructed point, or passed-through value.
 * @example
 * // this
 * var point = Point.convert([0, 1]);
 * // is equivalent to
 * var point = new Point(0, 1);
 */
Point$1.convert = function (a) {
    if (a instanceof Point$1) {
        return a;
    }
    if (Array.isArray(a)) {
        return new Point$1(a[0], a[1]);
    }
    return a;
};

var Point = index$5;

var vectortilefeature = VectorTileFeature$2;

function VectorTileFeature$2(pbf, end, extent, keys, values) {
    // Public
    this.properties = {};
    this.extent = extent;
    this.type = 0;

    // Private
    this._pbf = pbf;
    this._geometry = -1;
    this._keys = keys;
    this._values = values;

    pbf.readFields(readFeature, this, end);
}

function readFeature(tag, feature, pbf) {
    if (tag == 1) { feature.id = pbf.readVarint(); }
    else if (tag == 2) { readTag(pbf, feature); }
    else if (tag == 3) { feature.type = pbf.readVarint(); }
    else if (tag == 4) { feature._geometry = pbf.pos; }
}

function readTag(pbf, feature) {
    var end = pbf.readVarint() + pbf.pos;

    while (pbf.pos < end) {
        var key = feature._keys[pbf.readVarint()],
            value = feature._values[pbf.readVarint()];
        feature.properties[key] = value;
    }
}

VectorTileFeature$2.types = ['Unknown', 'Point', 'LineString', 'Polygon'];

VectorTileFeature$2.prototype.loadGeometry = function() {
    var pbf = this._pbf;
    pbf.pos = this._geometry;

    var end = pbf.readVarint() + pbf.pos,
        cmd = 1,
        length = 0,
        x = 0,
        y = 0,
        lines = [],
        line;

    while (pbf.pos < end) {
        if (length <= 0) {
            var cmdLen = pbf.readVarint();
            cmd = cmdLen & 0x7;
            length = cmdLen >> 3;
        }

        length--;

        if (cmd === 1 || cmd === 2) {
            x += pbf.readSVarint();
            y += pbf.readSVarint();

            if (cmd === 1) { // moveTo
                if (line) { lines.push(line); }
                line = [];
            }

            line.push(new Point(x, y));

        } else if (cmd === 7) {

            // Workaround for https://github.com/mapbox/mapnik-vector-tile/issues/90
            if (line) {
                line.push(line[0].clone()); // closePolygon
            }

        } else {
            throw new Error('unknown command ' + cmd);
        }
    }

    if (line) { lines.push(line); }

    return lines;
};

VectorTileFeature$2.prototype.bbox = function() {
    var pbf = this._pbf;
    pbf.pos = this._geometry;

    var end = pbf.readVarint() + pbf.pos,
        cmd = 1,
        length = 0,
        x = 0,
        y = 0,
        x1 = Infinity,
        x2 = -Infinity,
        y1 = Infinity,
        y2 = -Infinity;

    while (pbf.pos < end) {
        if (length <= 0) {
            var cmdLen = pbf.readVarint();
            cmd = cmdLen & 0x7;
            length = cmdLen >> 3;
        }

        length--;

        if (cmd === 1 || cmd === 2) {
            x += pbf.readSVarint();
            y += pbf.readSVarint();
            if (x < x1) { x1 = x; }
            if (x > x2) { x2 = x; }
            if (y < y1) { y1 = y; }
            if (y > y2) { y2 = y; }

        } else if (cmd !== 7) {
            throw new Error('unknown command ' + cmd);
        }
    }

    return [x1, y1, x2, y2];
};

VectorTileFeature$2.prototype.toGeoJSON = function(x, y, z) {
    var size = this.extent * Math.pow(2, z),
        x0 = this.extent * x,
        y0 = this.extent * y,
        coords = this.loadGeometry(),
        type = VectorTileFeature$2.types[this.type],
        i, j;

    function project(line) {
        for (var j = 0; j < line.length; j++) {
            var p = line[j], y2 = 180 - (p.y + y0) * 360 / size;
            line[j] = [
                (p.x + x0) * 360 / size - 180,
                360 / Math.PI * Math.atan(Math.exp(y2 * Math.PI / 180)) - 90
            ];
        }
    }

    switch (this.type) {
    case 1:
        var points = [];
        for (i = 0; i < coords.length; i++) {
            points[i] = coords[i][0];
        }
        coords = points;
        project(coords);
        break;

    case 2:
        for (i = 0; i < coords.length; i++) {
            project(coords[i]);
        }
        break;

    case 3:
        coords = classifyRings(coords);
        for (i = 0; i < coords.length; i++) {
            for (j = 0; j < coords[i].length; j++) {
                project(coords[i][j]);
            }
        }
        break;
    }

    if (coords.length === 1) {
        coords = coords[0];
    } else {
        type = 'Multi' + type;
    }

    var result = {
        type: "Feature",
        geometry: {
            type: type,
            coordinates: coords
        },
        properties: this.properties
    };

    if ('id' in this) {
        result.id = this.id;
    }

    return result;
};

// classifies an array of rings into polygons with outer rings and holes

function classifyRings(rings) {
    var len = rings.length;

    if (len <= 1) { return [rings]; }

    var polygons = [],
        polygon,
        ccw;

    for (var i = 0; i < len; i++) {
        var area = signedArea(rings[i]);
        if (area === 0) { continue; }

        if (ccw === undefined) { ccw = area < 0; }

        if (ccw === area < 0) {
            if (polygon) { polygons.push(polygon); }
            polygon = [rings[i]];

        } else {
            polygon.push(rings[i]);
        }
    }
    if (polygon) { polygons.push(polygon); }

    return polygons;
}

function signedArea(ring) {
    var sum = 0;
    for (var i = 0, len = ring.length, j = len - 1, p1, p2; i < len; j = i++) {
        p1 = ring[i];
        p2 = ring[j];
        sum += (p2.x - p1.x) * (p1.y + p2.y);
    }
    return sum;
}

var VectorTileFeature$1 = vectortilefeature;

var vectortilelayer = VectorTileLayer$2;

function VectorTileLayer$2(pbf, end) {
    // Public
    this.version = 1;
    this.name = null;
    this.extent = 4096;
    this.length = 0;

    // Private
    this._pbf = pbf;
    this._keys = [];
    this._values = [];
    this._features = [];

    pbf.readFields(readLayer, this, end);

    this.length = this._features.length;
}

function readLayer(tag, layer, pbf) {
    if (tag === 15) { layer.version = pbf.readVarint(); }
    else if (tag === 1) { layer.name = pbf.readString(); }
    else if (tag === 5) { layer.extent = pbf.readVarint(); }
    else if (tag === 2) { layer._features.push(pbf.pos); }
    else if (tag === 3) { layer._keys.push(pbf.readString()); }
    else if (tag === 4) { layer._values.push(readValueMessage(pbf)); }
}

function readValueMessage(pbf) {
    var value = null,
        end = pbf.readVarint() + pbf.pos;

    while (pbf.pos < end) {
        var tag = pbf.readVarint() >> 3;

        value = tag === 1 ? pbf.readString() :
            tag === 2 ? pbf.readFloat() :
            tag === 3 ? pbf.readDouble() :
            tag === 4 ? pbf.readVarint64() :
            tag === 5 ? pbf.readVarint() :
            tag === 6 ? pbf.readSVarint() :
            tag === 7 ? pbf.readBoolean() : null;
    }

    return value;
}

// return feature `i` from this layer as a `VectorTileFeature`
VectorTileLayer$2.prototype.feature = function(i) {
    if (i < 0 || i >= this._features.length) { throw new Error('feature index out of bounds'); }

    this._pbf.pos = this._features[i];

    var end = this._pbf.readVarint() + this._pbf.pos;
    return new VectorTileFeature$1(this._pbf, end, this.extent, this._keys, this._values);
};

var VectorTileLayer$1 = vectortilelayer;

var vectortile = VectorTile$1;

function VectorTile$1(pbf, end) {
    this.layers = pbf.readFields(readTile, {}, end);
}

function readTile(tag, layers, pbf) {
    if (tag === 3) {
        var layer = new VectorTileLayer$1(pbf, pbf.readVarint() + pbf.pos);
        if (layer.length) { layers[layer.name] = layer; }
    }
}

var VectorTile = vectortile;

/*
 * 🍂class VectorGrid.Protobuf
 * 🍂extends VectorGrid
 *
 * A `VectorGrid` for vector tiles fetched from the internet.
 * Tiles are supposed to be protobufs (AKA "protobuffer" or "Protocol Buffers"),
 * containing data which complies with the
 * [MapBox Vector Tile Specification](https://github.com/mapbox/vector-tile-spec/tree/master/2.1).
 *
 * This is the format used by:
 * - Mapbox Vector Tiles
 * - Mapzen Vector Tiles
 * - ESRI Vector Tiles
 * - [OpenMapTiles hosted Vector Tiles](https://openmaptiles.com/hosting/)
 *
 * 🍂example
 *
 * You must initialize a `VectorGrid.Protobuf` with a URL template, just like in
 * `L.TileLayer`s. The difference is that the template must point to vector tiles
 * (usually `.pbf` or `.mvt`) instead of raster (`.png` or `.jpg`) tiles, and that
 * you should define the styling for all the features.
 *
 * <br><br>
 *
 * For OpenMapTiles, with a key from [https://openmaptiles.org/docs/host/use-cdn/](https://openmaptiles.org/docs/host/use-cdn/),
 * initialization looks like this:
 *
 * ```
 * L.vectorGrid.protobuf("https://free-{s}.tilehosting.com/data/v3/{z}/{x}/{y}.pbf.pict?key={key}", {
 * 	vectorTileLayerStyles: { ... },
 * 	subdomains: "0123",
 * 	key: 'abcdefghi01234567890',
 * 	maxNativeZoom: 14
 * }).addTo(map);
 * ```
 *
 * And for Mapbox vector tiles, it looks like this:
 *
 * ```
 * L.vectorGrid.protobuf("https://{s}.tiles.mapbox.com/v4/mapbox.mapbox-streets-v6/{z}/{x}/{y}.vector.pbf?access_token={token}", {
 * 	vectorTileLayerStyles: { ... },
 * 	subdomains: "abcd",
 * 	token: "pk.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRTS.TUVWXTZ0123456789abcde"
 * }).addTo(map);
 * ```
 */
L.VectorGrid.Protobuf = L.VectorGrid.extend({

	options: {
		// 🍂section
		// As with `L.TileLayer`, the URL template might contain a reference to
		// any option (see the example above and note the `{key}` or `token` in the URL
		// template, and the corresponding option).
		//
		// 🍂option subdomains: String = 'abc'
		// Akin to the `subdomains` option for `L.TileLayer`.
		subdomains: 'abc',	// Like L.TileLayer
		//
		// 🍂option fetchOptions: Object = {}
		// options passed to `fetch`, e.g. {credentials: 'same-origin'} to send cookie for the current domain
		fetchOptions: {}
	},

	initialize: function(url, options) {
		// Inherits options from geojson-vt!
// 		this._slicer = geojsonvt(geojson, options);
		this._url = url;
		L.VectorGrid.prototype.initialize.call(this, options);
	},

	// 🍂method setUrl(url: String, noRedraw?: Boolean): this
	// Updates the layer's URL template and redraws it (unless `noRedraw` is set to `true`).
	setUrl: function(url, noRedraw) {
		this._url = url;

		if (!noRedraw) {
			this.redraw();
		}

		return this;
	},

	_getSubdomain: L.TileLayer.prototype._getSubdomain,

	_isCurrentTile : function(coords, tileBounds) {

		if (!this._map) {
			return true;
		}

		var zoom = this._map._animatingZoom ? this._map._animateToZoom : this._map._zoom;
		var currentZoom = zoom === coords.z;

		var tileBounds = this._tileCoordsToBounds(coords);
		var currentBounds = this._map.getBounds().overlaps(tileBounds); 

		return currentZoom && currentBounds;

	},

	_getVectorTilePromise: function(coords, tileBounds) {
		var data = {
			s: this._getSubdomain(coords),
			x: coords.x,
			y: coords.y,
			z: coords.z
// 			z: this._getZoomForUrl()	/// TODO: Maybe replicate TileLayer's maxNativeZoom
		};
		if (this._map && !this._map.options.crs.infinite) {
			var invertedY = this._globalTileRange.max.y - coords.y;
			if (this.options.tms) { // Should this option be available in Leaflet.VectorGrid?
				data['y'] = invertedY;
			}
			data['-y'] = invertedY;
		}

		if (!this._isCurrentTile(coords, tileBounds)) {
			return Promise.resolve({layers:[]});
		}

		var tileUrl = L.Util.template(this._url, L.extend(data, this.options));

		return fetch(tileUrl, this.options.fetchOptions).then(function(response){

			if (!response.ok || !this._isCurrentTile(coords)) {
				return {layers:[]};
			} 

			return response.blob().then( function (blob) {

				var reader = new FileReader();
				return new Promise(function(resolve){
					reader.addEventListener("loadend", function() {
						// reader.result contains the contents of blob as a typed array
						// blob.type === 'application/x-protobuf'
						var pbf = new index( reader.result );
						return resolve(new VectorTile( pbf ));

					});
					reader.readAsArrayBuffer(blob);
				});
			});

		}.bind(this)).then(function(json){

			// Normalize feature getters into actual instanced features
			for (var layerName in json.layers) {
				var feats = [];

				for (var i=0; i<json.layers[layerName].length; i++) {
					var feat = json.layers[layerName].feature(i);
					feat.geometry = feat.loadGeometry();
					feats.push(feat);
				}

				json.layers[layerName].features = feats;
			}

			return json;
		});
	}
});


// 🍂factory L.vectorGrid.protobuf(url: String, options)
// Instantiates a new protobuf VectorGrid with the given URL template and options
L.vectorGrid.protobuf = function (url, options) {
	return new L.VectorGrid.Protobuf(url, options);
};

var workerCode = __$strToBlobUri("'use strict';\n\n// calculate simplification data using optimized Douglas-Peucker algorithm\n\nfunction simplify(coords, first, last, sqTolerance) {\n    var maxSqDist = sqTolerance;\n    var mid = (last - first) >> 1;\n    var minPosToMid = last - first;\n    var index;\n\n    var ax = coords[first];\n    var ay = coords[first + 1];\n    var bx = coords[last];\n    var by = coords[last + 1];\n\n    for (var i = first + 3; i < last; i += 3) {\n        var d = getSqSegDist(coords[i], coords[i + 1], ax, ay, bx, by);\n\n        if (d > maxSqDist) {\n            index = i;\n            maxSqDist = d;\n\n        } else if (d === maxSqDist) {\n            // a workaround to ensure we choose a pivot close to the middle of the list,\n            // reducing recursion depth, for certain degenerate inputs\n            // https://github.com/mapbox/geojson-vt/issues/104\n            var posToMid = Math.abs(i - mid);\n            if (posToMid < minPosToMid) {\n                index = i;\n                minPosToMid = posToMid;\n            }\n        }\n    }\n\n    if (maxSqDist > sqTolerance) {\n        if (index - first > 3) { simplify(coords, first, index, sqTolerance); }\n        coords[index + 2] = maxSqDist;\n        if (last - index > 3) { simplify(coords, index, last, sqTolerance); }\n    }\n}\n\n// square distance from a point to a segment\nfunction getSqSegDist(px, py, x, y, bx, by) {\n\n    var dx = bx - x;\n    var dy = by - y;\n\n    if (dx !== 0 || dy !== 0) {\n\n        var t = ((px - x) * dx + (py - y) * dy) / (dx * dx + dy * dy);\n\n        if (t > 1) {\n            x = bx;\n            y = by;\n\n        } else if (t > 0) {\n            x += dx * t;\n            y += dy * t;\n        }\n    }\n\n    dx = px - x;\n    dy = py - y;\n\n    return dx * dx + dy * dy;\n}\n\nfunction createFeature(id, type, geom, tags) {\n    var feature = {\n        id: typeof id === 'undefined' ? null : id,\n        type: type,\n        geometry: geom,\n        tags: tags,\n        minX: Infinity,\n        minY: Infinity,\n        maxX: -Infinity,\n        maxY: -Infinity\n    };\n    calcBBox(feature);\n    return feature;\n}\n\nfunction calcBBox(feature) {\n    var geom = feature.geometry;\n    var type = feature.type;\n\n    if (type === 'Point' || type === 'MultiPoint' || type === 'LineString') {\n        calcLineBBox(feature, geom);\n\n    } else if (type === 'Polygon' || type === 'MultiLineString') {\n        for (var i = 0; i < geom.length; i++) {\n            calcLineBBox(feature, geom[i]);\n        }\n\n    } else if (type === 'MultiPolygon') {\n        for (i = 0; i < geom.length; i++) {\n            for (var j = 0; j < geom[i].length; j++) {\n                calcLineBBox(feature, geom[i][j]);\n            }\n        }\n    }\n}\n\nfunction calcLineBBox(feature, geom) {\n    for (var i = 0; i < geom.length; i += 3) {\n        feature.minX = Math.min(feature.minX, geom[i]);\n        feature.minY = Math.min(feature.minY, geom[i + 1]);\n        feature.maxX = Math.max(feature.maxX, geom[i]);\n        feature.maxY = Math.max(feature.maxY, geom[i + 1]);\n    }\n}\n\n// converts GeoJSON feature into an intermediate projected JSON vector format with simplification data\n\nfunction convert(data, options) {\n    var features = [];\n    if (data.type === 'FeatureCollection') {\n        for (var i = 0; i < data.features.length; i++) {\n            convertFeature(features, data.features[i], options, i);\n        }\n\n    } else if (data.type === 'Feature') {\n        convertFeature(features, data, options);\n\n    } else {\n        // single geometry or a geometry collection\n        convertFeature(features, {geometry: data}, options);\n    }\n\n    return features;\n}\n\nfunction convertFeature(features, geojson, options, index) {\n    if (!geojson.geometry) { return; }\n\n    var coords = geojson.geometry.coordinates;\n    var type = geojson.geometry.type;\n    var tolerance = Math.pow(options.tolerance / ((1 << options.maxZoom) * options.extent), 2);\n    var geometry = [];\n    var id = geojson.id;\n    if (options.promoteId) {\n        id = geojson.properties[options.promoteId];\n    } else if (options.generateId) {\n        id = index || 0;\n    }\n    if (type === 'Point') {\n        convertPoint(coords, geometry);\n\n    } else if (type === 'MultiPoint') {\n        for (var i = 0; i < coords.length; i++) {\n            convertPoint(coords[i], geometry);\n        }\n\n    } else if (type === 'LineString') {\n        convertLine(coords, geometry, tolerance, false);\n\n    } else if (type === 'MultiLineString') {\n        if (options.lineMetrics) {\n            // explode into linestrings to be able to track metrics\n            for (i = 0; i < coords.length; i++) {\n                geometry = [];\n                convertLine(coords[i], geometry, tolerance, false);\n                features.push(createFeature(id, 'LineString', geometry, geojson.properties));\n            }\n            return;\n        } else {\n            convertLines(coords, geometry, tolerance, false);\n        }\n\n    } else if (type === 'Polygon') {\n        convertLines(coords, geometry, tolerance, true);\n\n    } else if (type === 'MultiPolygon') {\n        for (i = 0; i < coords.length; i++) {\n            var polygon = [];\n            convertLines(coords[i], polygon, tolerance, true);\n            geometry.push(polygon);\n        }\n    } else if (type === 'GeometryCollection') {\n        for (i = 0; i < geojson.geometry.geometries.length; i++) {\n            convertFeature(features, {\n                id: id,\n                geometry: geojson.geometry.geometries[i],\n                properties: geojson.properties\n            }, options, index);\n        }\n        return;\n    } else {\n        throw new Error('Input data is not a valid GeoJSON object.');\n    }\n\n    features.push(createFeature(id, type, geometry, geojson.properties));\n}\n\nfunction convertPoint(coords, out) {\n    out.push(projectX(coords[0]));\n    out.push(projectY(coords[1]));\n    out.push(0);\n}\n\nfunction convertLine(ring, out, tolerance, isPolygon) {\n    var x0, y0;\n    var size = 0;\n\n    for (var j = 0; j < ring.length; j++) {\n        var x = projectX(ring[j][0]);\n        var y = projectY(ring[j][1]);\n\n        out.push(x);\n        out.push(y);\n        out.push(0);\n\n        if (j > 0) {\n            if (isPolygon) {\n                size += (x0 * y - x * y0) / 2; // area\n            } else {\n                size += Math.sqrt(Math.pow(x - x0, 2) + Math.pow(y - y0, 2)); // length\n            }\n        }\n        x0 = x;\n        y0 = y;\n    }\n\n    var last = out.length - 3;\n    out[2] = 1;\n    simplify(out, 0, last, tolerance);\n    out[last + 2] = 1;\n\n    out.size = Math.abs(size);\n    out.start = 0;\n    out.end = out.size;\n}\n\nfunction convertLines(rings, out, tolerance, isPolygon) {\n    for (var i = 0; i < rings.length; i++) {\n        var geom = [];\n        convertLine(rings[i], geom, tolerance, isPolygon);\n        out.push(geom);\n    }\n}\n\nfunction projectX(x) {\n    return x / 360 + 0.5;\n}\n\nfunction projectY(y) {\n    var sin = Math.sin(y * Math.PI / 180);\n    var y2 = 0.5 - 0.25 * Math.log((1 + sin) / (1 - sin)) / Math.PI;\n    return y2 < 0 ? 0 : y2 > 1 ? 1 : y2;\n}\n\n/* clip features between two axis-parallel lines:\n *     |        |\n *  ___|___     |     /\n * /   |   \____|____/\n *     |        |\n */\n\nfunction clip(features, scale, k1, k2, axis, minAll, maxAll, options) {\n\n    k1 /= scale;\n    k2 /= scale;\n\n    if (minAll >= k1 && maxAll < k2) { return features; } // trivial accept\n    else if (maxAll < k1 || minAll >= k2) { return null; } // trivial reject\n\n    var clipped = [];\n\n    for (var i = 0; i < features.length; i++) {\n\n        var feature = features[i];\n        var geometry = feature.geometry;\n        var type = feature.type;\n\n        var min = axis === 0 ? feature.minX : feature.minY;\n        var max = axis === 0 ? feature.maxX : feature.maxY;\n\n        if (min >= k1 && max < k2) { // trivial accept\n            clipped.push(feature);\n            continue;\n        } else if (max < k1 || min >= k2) { // trivial reject\n            continue;\n        }\n\n        var newGeometry = [];\n\n        if (type === 'Point' || type === 'MultiPoint') {\n            clipPoints(geometry, newGeometry, k1, k2, axis);\n\n        } else if (type === 'LineString') {\n            clipLine(geometry, newGeometry, k1, k2, axis, false, options.lineMetrics);\n\n        } else if (type === 'MultiLineString') {\n            clipLines(geometry, newGeometry, k1, k2, axis, false);\n\n        } else if (type === 'Polygon') {\n            clipLines(geometry, newGeometry, k1, k2, axis, true);\n\n        } else if (type === 'MultiPolygon') {\n            for (var j = 0; j < geometry.length; j++) {\n                var polygon = [];\n                clipLines(geometry[j], polygon, k1, k2, axis, true);\n                if (polygon.length) {\n                    newGeometry.push(polygon);\n                }\n            }\n        }\n\n        if (newGeometry.length) {\n            if (options.lineMetrics && type === 'LineString') {\n                for (j = 0; j < newGeometry.length; j++) {\n                    clipped.push(createFeature(feature.id, type, newGeometry[j], feature.tags));\n                }\n                continue;\n            }\n\n            if (type === 'LineString' || type === 'MultiLineString') {\n                if (newGeometry.length === 1) {\n                    type = 'LineString';\n                    newGeometry = newGeometry[0];\n                } else {\n                    type = 'MultiLineString';\n                }\n            }\n            if (type === 'Point' || type === 'MultiPoint') {\n                type = newGeometry.length === 3 ? 'Point' : 'MultiPoint';\n            }\n\n            clipped.push(createFeature(feature.id, type, newGeometry, feature.tags));\n        }\n    }\n\n    return clipped.length ? clipped : null;\n}\n\nfunction clipPoints(geom, newGeom, k1, k2, axis) {\n    for (var i = 0; i < geom.length; i += 3) {\n        var a = geom[i + axis];\n\n        if (a >= k1 && a <= k2) {\n            newGeom.push(geom[i]);\n            newGeom.push(geom[i + 1]);\n            newGeom.push(geom[i + 2]);\n        }\n    }\n}\n\nfunction clipLine(geom, newGeom, k1, k2, axis, isPolygon, trackMetrics) {\n\n    var slice = newSlice(geom);\n    var intersect = axis === 0 ? intersectX : intersectY;\n    var len = geom.start;\n    var segLen, t;\n\n    for (var i = 0; i < geom.length - 3; i += 3) {\n        var ax = geom[i];\n        var ay = geom[i + 1];\n        var az = geom[i + 2];\n        var bx = geom[i + 3];\n        var by = geom[i + 4];\n        var a = axis === 0 ? ax : ay;\n        var b = axis === 0 ? bx : by;\n        var exited = false;\n\n        if (trackMetrics) { segLen = Math.sqrt(Math.pow(ax - bx, 2) + Math.pow(ay - by, 2)); }\n\n        if (a < k1) {\n            // ---|-->  | (line enters the clip region from the left)\n            if (b > k1) {\n                t = intersect(slice, ax, ay, bx, by, k1);\n                if (trackMetrics) { slice.start = len + segLen * t; }\n            }\n        } else if (a > k2) {\n            // |  <--|--- (line enters the clip region from the right)\n            if (b < k2) {\n                t = intersect(slice, ax, ay, bx, by, k2);\n                if (trackMetrics) { slice.start = len + segLen * t; }\n            }\n        } else {\n            addPoint(slice, ax, ay, az);\n        }\n        if (b < k1 && a >= k1) {\n            // <--|---  | or <--|-----|--- (line exits the clip region on the left)\n            t = intersect(slice, ax, ay, bx, by, k1);\n            exited = true;\n        }\n        if (b > k2 && a <= k2) {\n            // |  ---|--> or ---|-----|--> (line exits the clip region on the right)\n            t = intersect(slice, ax, ay, bx, by, k2);\n            exited = true;\n        }\n\n        if (!isPolygon && exited) {\n            if (trackMetrics) { slice.end = len + segLen * t; }\n            newGeom.push(slice);\n            slice = newSlice(geom);\n        }\n\n        if (trackMetrics) { len += segLen; }\n    }\n\n    // add the last point\n    var last = geom.length - 3;\n    ax = geom[last];\n    ay = geom[last + 1];\n    az = geom[last + 2];\n    a = axis === 0 ? ax : ay;\n    if (a >= k1 && a <= k2) { addPoint(slice, ax, ay, az); }\n\n    // close the polygon if its endpoints are not the same after clipping\n    last = slice.length - 3;\n    if (isPolygon && last >= 3 && (slice[last] !== slice[0] || slice[last + 1] !== slice[1])) {\n        addPoint(slice, slice[0], slice[1], slice[2]);\n    }\n\n    // add the final slice\n    if (slice.length) {\n        newGeom.push(slice);\n    }\n}\n\nfunction newSlice(line) {\n    var slice = [];\n    slice.size = line.size;\n    slice.start = line.start;\n    slice.end = line.end;\n    return slice;\n}\n\nfunction clipLines(geom, newGeom, k1, k2, axis, isPolygon) {\n    for (var i = 0; i < geom.length; i++) {\n        clipLine(geom[i], newGeom, k1, k2, axis, isPolygon, false);\n    }\n}\n\nfunction addPoint(out, x, y, z) {\n    out.push(x);\n    out.push(y);\n    out.push(z);\n}\n\nfunction intersectX(out, ax, ay, bx, by, x) {\n    var t = (x - ax) / (bx - ax);\n    out.push(x);\n    out.push(ay + (by - ay) * t);\n    out.push(1);\n    return t;\n}\n\nfunction intersectY(out, ax, ay, bx, by, y) {\n    var t = (y - ay) / (by - ay);\n    out.push(ax + (bx - ax) * t);\n    out.push(y);\n    out.push(1);\n    return t;\n}\n\nfunction wrap(features, options) {\n    var buffer = options.buffer / options.extent;\n    var merged = features;\n    var left  = clip(features, 1, -1 - buffer, buffer,     0, -1, 2, options); // left world copy\n    var right = clip(features, 1,  1 - buffer, 2 + buffer, 0, -1, 2, options); // right world copy\n\n    if (left || right) {\n        merged = clip(features, 1, -buffer, 1 + buffer, 0, -1, 2, options) || []; // center world copy\n\n        if (left) { merged = shiftFeatureCoords(left, 1).concat(merged); } // merge left into center\n        if (right) { merged = merged.concat(shiftFeatureCoords(right, -1)); } // merge right into center\n    }\n\n    return merged;\n}\n\nfunction shiftFeatureCoords(features, offset) {\n    var newFeatures = [];\n\n    for (var i = 0; i < features.length; i++) {\n        var feature = features[i],\n            type = feature.type;\n\n        var newGeometry;\n\n        if (type === 'Point' || type === 'MultiPoint' || type === 'LineString') {\n            newGeometry = shiftCoords(feature.geometry, offset);\n\n        } else if (type === 'MultiLineString' || type === 'Polygon') {\n            newGeometry = [];\n            for (var j = 0; j < feature.geometry.length; j++) {\n                newGeometry.push(shiftCoords(feature.geometry[j], offset));\n            }\n        } else if (type === 'MultiPolygon') {\n            newGeometry = [];\n            for (j = 0; j < feature.geometry.length; j++) {\n                var newPolygon = [];\n                for (var k = 0; k < feature.geometry[j].length; k++) {\n                    newPolygon.push(shiftCoords(feature.geometry[j][k], offset));\n                }\n                newGeometry.push(newPolygon);\n            }\n        }\n\n        newFeatures.push(createFeature(feature.id, type, newGeometry, feature.tags));\n    }\n\n    return newFeatures;\n}\n\nfunction shiftCoords(points, offset) {\n    var newPoints = [];\n    newPoints.size = points.size;\n\n    if (points.start !== undefined) {\n        newPoints.start = points.start;\n        newPoints.end = points.end;\n    }\n\n    for (var i = 0; i < points.length; i += 3) {\n        newPoints.push(points[i] + offset, points[i + 1], points[i + 2]);\n    }\n    return newPoints;\n}\n\n// Transforms the coordinates of each feature in the given tile from\n// mercator-projected space into (extent x extent) tile space.\nfunction transformTile(tile, extent) {\n    if (tile.transformed) { return tile; }\n\n    var z2 = 1 << tile.z,\n        tx = tile.x,\n        ty = tile.y,\n        i, j, k;\n\n    for (i = 0; i < tile.features.length; i++) {\n        var feature = tile.features[i],\n            geom = feature.geometry,\n            type = feature.type;\n\n        feature.geometry = [];\n\n        if (type === 1) {\n            for (j = 0; j < geom.length; j += 2) {\n                feature.geometry.push(transformPoint(geom[j], geom[j + 1], extent, z2, tx, ty));\n            }\n        } else {\n            for (j = 0; j < geom.length; j++) {\n                var ring = [];\n                for (k = 0; k < geom[j].length; k += 2) {\n                    ring.push(transformPoint(geom[j][k], geom[j][k + 1], extent, z2, tx, ty));\n                }\n                feature.geometry.push(ring);\n            }\n        }\n    }\n\n    tile.transformed = true;\n\n    return tile;\n}\n\nfunction transformPoint(x, y, extent, z2, tx, ty) {\n    return [\n        Math.round(extent * (x * z2 - tx)),\n        Math.round(extent * (y * z2 - ty))];\n}\n\nfunction createTile(features, z, tx, ty, options) {\n    var tolerance = z === options.maxZoom ? 0 : options.tolerance / ((1 << z) * options.extent);\n    var tile = {\n        features: [],\n        numPoints: 0,\n        numSimplified: 0,\n        numFeatures: 0,\n        source: null,\n        x: tx,\n        y: ty,\n        z: z,\n        transformed: false,\n        minX: 2,\n        minY: 1,\n        maxX: -1,\n        maxY: 0\n    };\n    for (var i = 0; i < features.length; i++) {\n        tile.numFeatures++;\n        addFeature(tile, features[i], tolerance, options);\n\n        var minX = features[i].minX;\n        var minY = features[i].minY;\n        var maxX = features[i].maxX;\n        var maxY = features[i].maxY;\n\n        if (minX < tile.minX) { tile.minX = minX; }\n        if (minY < tile.minY) { tile.minY = minY; }\n        if (maxX > tile.maxX) { tile.maxX = maxX; }\n        if (maxY > tile.maxY) { tile.maxY = maxY; }\n    }\n    return tile;\n}\n\nfunction addFeature(tile, feature, tolerance, options) {\n\n    var geom = feature.geometry,\n        type = feature.type,\n        simplified = [];\n\n    if (type === 'Point' || type === 'MultiPoint') {\n        for (var i = 0; i < geom.length; i += 3) {\n            simplified.push(geom[i]);\n            simplified.push(geom[i + 1]);\n            tile.numPoints++;\n            tile.numSimplified++;\n        }\n\n    } else if (type === 'LineString') {\n        addLine(simplified, geom, tile, tolerance, false, false);\n\n    } else if (type === 'MultiLineString' || type === 'Polygon') {\n        for (i = 0; i < geom.length; i++) {\n            addLine(simplified, geom[i], tile, tolerance, type === 'Polygon', i === 0);\n        }\n\n    } else if (type === 'MultiPolygon') {\n\n        for (var k = 0; k < geom.length; k++) {\n            var polygon = geom[k];\n            for (i = 0; i < polygon.length; i++) {\n                addLine(simplified, polygon[i], tile, tolerance, true, i === 0);\n            }\n        }\n    }\n\n    if (simplified.length) {\n        var tags = feature.tags || null;\n        if (type === 'LineString' && options.lineMetrics) {\n            tags = {};\n            for (var key in feature.tags) { tags[key] = feature.tags[key]; }\n            tags['mapbox_clip_start'] = geom.start / geom.size;\n            tags['mapbox_clip_end'] = geom.end / geom.size;\n        }\n        var tileFeature = {\n            geometry: simplified,\n            type: type === 'Polygon' || type === 'MultiPolygon' ? 3 :\n                type === 'LineString' || type === 'MultiLineString' ? 2 : 1,\n            tags: tags\n        };\n        if (feature.id !== null) {\n            tileFeature.id = feature.id;\n        }\n        tile.features.push(tileFeature);\n    }\n}\n\nfunction addLine(result, geom, tile, tolerance, isPolygon, isOuter) {\n    var sqTolerance = tolerance * tolerance;\n\n    if (tolerance > 0 && (geom.size < (isPolygon ? sqTolerance : tolerance))) {\n        tile.numPoints += geom.length / 3;\n        return;\n    }\n\n    var ring = [];\n\n    for (var i = 0; i < geom.length; i += 3) {\n        if (tolerance === 0 || geom[i + 2] > sqTolerance) {\n            tile.numSimplified++;\n            ring.push(geom[i]);\n            ring.push(geom[i + 1]);\n        }\n        tile.numPoints++;\n    }\n\n    if (isPolygon) { rewind(ring, isOuter); }\n\n    result.push(ring);\n}\n\nfunction rewind(ring, clockwise) {\n    var area = 0;\n    for (var i = 0, len = ring.length, j = len - 2; i < len; j = i, i += 2) {\n        area += (ring[i] - ring[j]) * (ring[i + 1] + ring[j + 1]);\n    }\n    if (area > 0 === clockwise) {\n        for (i = 0, len = ring.length; i < len / 2; i += 2) {\n            var x = ring[i];\n            var y = ring[i + 1];\n            ring[i] = ring[len - 2 - i];\n            ring[i + 1] = ring[len - 1 - i];\n            ring[len - 2 - i] = x;\n            ring[len - 1 - i] = y;\n        }\n    }\n}\n\nfunction geojsonvt(data, options) {\n    return new GeoJSONVT(data, options);\n}\n\nfunction GeoJSONVT(data, options) {\n    options = this.options = extend(Object.create(this.options), options);\n\n    var debug = options.debug;\n\n    if (debug) { console.time('preprocess data'); }\n\n    if (options.maxZoom < 0 || options.maxZoom > 24) { throw new Error('maxZoom should be in the 0-24 range'); }\n    if (options.promoteId && options.generateId) { throw new Error('promoteId and generateId cannot be used together.'); }\n\n    var features = convert(data, options);\n\n    this.tiles = {};\n    this.tileCoords = [];\n\n    if (debug) {\n        console.timeEnd('preprocess data');\n        console.log('index: maxZoom: %d, maxPoints: %d', options.indexMaxZoom, options.indexMaxPoints);\n        console.time('generate tiles');\n        this.stats = {};\n        this.total = 0;\n    }\n\n    features = wrap(features, options);\n\n    // start slicing from the top tile down\n    if (features.length) { this.splitTile(features, 0, 0, 0); }\n\n    if (debug) {\n        if (features.length) { console.log('features: %d, points: %d', this.tiles[0].numFeatures, this.tiles[0].numPoints); }\n        console.timeEnd('generate tiles');\n        console.log('tiles generated:', this.total, JSON.stringify(this.stats));\n    }\n}\n\nGeoJSONVT.prototype.options = {\n    maxZoom: 14,            // max zoom to preserve detail on\n    indexMaxZoom: 5,        // max zoom in the tile index\n    indexMaxPoints: 100000, // max number of points per tile in the tile index\n    tolerance: 3,           // simplification tolerance (higher means simpler)\n    extent: 4096,           // tile extent\n    buffer: 64,             // tile buffer on each side\n    lineMetrics: false,     // whether to calculate line metrics\n    promoteId: null,        // name of a feature property to be promoted to feature.id\n    generateId: false,      // whether to generate feature ids. Cannot be used with promoteId\n    debug: 0                // logging level (0, 1 or 2)\n};\n\nGeoJSONVT.prototype.splitTile = function (features, z, x, y, cz, cx, cy) {\n    var this$1 = this;\n\n\n    var stack = [features, z, x, y],\n        options = this.options,\n        debug = options.debug;\n\n    // avoid recursion by using a processing queue\n    while (stack.length) {\n        y = stack.pop();\n        x = stack.pop();\n        z = stack.pop();\n        features = stack.pop();\n\n        var z2 = 1 << z,\n            id = toID(z, x, y),\n            tile = this$1.tiles[id];\n\n        if (!tile) {\n            if (debug > 1) { console.time('creation'); }\n\n            tile = this$1.tiles[id] = createTile(features, z, x, y, options);\n            this$1.tileCoords.push({z: z, x: x, y: y});\n\n            if (debug) {\n                if (debug > 1) {\n                    console.log('tile z%d-%d-%d (features: %d, points: %d, simplified: %d)',\n                        z, x, y, tile.numFeatures, tile.numPoints, tile.numSimplified);\n                    console.timeEnd('creation');\n                }\n                var key = 'z' + z;\n                this$1.stats[key] = (this$1.stats[key] || 0) + 1;\n                this$1.total++;\n            }\n        }\n\n        // save reference to original geometry in tile so that we can drill down later if we stop now\n        tile.source = features;\n\n        // if it's the first-pass tiling\n        if (!cz) {\n            // stop tiling if we reached max zoom, or if the tile is too simple\n            if (z === options.indexMaxZoom || tile.numPoints <= options.indexMaxPoints) { continue; }\n\n        // if a drilldown to a specific tile\n        } else {\n            // stop tiling if we reached base zoom or our target tile zoom\n            if (z === options.maxZoom || z === cz) { continue; }\n\n            // stop tiling if it's not an ancestor of the target tile\n            var m = 1 << (cz - z);\n            if (x !== Math.floor(cx / m) || y !== Math.floor(cy / m)) { continue; }\n        }\n\n        // if we slice further down, no need to keep source geometry\n        tile.source = null;\n\n        if (features.length === 0) { continue; }\n\n        if (debug > 1) { console.time('clipping'); }\n\n        // values we'll use for clipping\n        var k1 = 0.5 * options.buffer / options.extent,\n            k2 = 0.5 - k1,\n            k3 = 0.5 + k1,\n            k4 = 1 + k1,\n            tl, bl, tr, br, left, right;\n\n        tl = bl = tr = br = null;\n\n        left  = clip(features, z2, x - k1, x + k3, 0, tile.minX, tile.maxX, options);\n        right = clip(features, z2, x + k2, x + k4, 0, tile.minX, tile.maxX, options);\n        features = null;\n\n        if (left) {\n            tl = clip(left, z2, y - k1, y + k3, 1, tile.minY, tile.maxY, options);\n            bl = clip(left, z2, y + k2, y + k4, 1, tile.minY, tile.maxY, options);\n            left = null;\n        }\n\n        if (right) {\n            tr = clip(right, z2, y - k1, y + k3, 1, tile.minY, tile.maxY, options);\n            br = clip(right, z2, y + k2, y + k4, 1, tile.minY, tile.maxY, options);\n            right = null;\n        }\n\n        if (debug > 1) { console.timeEnd('clipping'); }\n\n        stack.push(tl || [], z + 1, x * 2,     y * 2);\n        stack.push(bl || [], z + 1, x * 2,     y * 2 + 1);\n        stack.push(tr || [], z + 1, x * 2 + 1, y * 2);\n        stack.push(br || [], z + 1, x * 2 + 1, y * 2 + 1);\n    }\n};\n\nGeoJSONVT.prototype.getTile = function (z, x, y) {\n    var this$1 = this;\n\n    var options = this.options,\n        extent = options.extent,\n        debug = options.debug;\n\n    if (z < 0 || z > 24) { return null; }\n\n    var z2 = 1 << z;\n    x = ((x % z2) + z2) % z2; // wrap tile x coordinate\n\n    var id = toID(z, x, y);\n    if (this.tiles[id]) { return transformTile(this.tiles[id], extent); }\n\n    if (debug > 1) { console.log('drilling down to z%d-%d-%d', z, x, y); }\n\n    var z0 = z,\n        x0 = x,\n        y0 = y,\n        parent;\n\n    while (!parent && z0 > 0) {\n        z0--;\n        x0 = Math.floor(x0 / 2);\n        y0 = Math.floor(y0 / 2);\n        parent = this$1.tiles[toID(z0, x0, y0)];\n    }\n\n    if (!parent || !parent.source) { return null; }\n\n    // if we found a parent tile containing the original geometry, we can drill down from it\n    if (debug > 1) { console.log('found parent tile z%d-%d-%d', z0, x0, y0); }\n\n    if (debug > 1) { console.time('drilling down'); }\n    this.splitTile(parent.source, z0, x0, y0, z, x, y);\n    if (debug > 1) { console.timeEnd('drilling down'); }\n\n    return this.tiles[id] ? transformTile(this.tiles[id], extent) : null;\n};\n\nfunction toID(z, x, y) {\n    return (((1 << z) * y + x) * 32) + z;\n}\n\nfunction extend(dest, src) {\n    for (var i in src) { dest[i] = src[i]; }\n    return dest;\n}\n\nvar identity = function(x) {\n  return x;\n};\n\nvar transform = function(transform) {\n  if (transform == null) { return identity; }\n  var x0,\n      y0,\n      kx = transform.scale[0],\n      ky = transform.scale[1],\n      dx = transform.translate[0],\n      dy = transform.translate[1];\n  return function(input, i) {\n    if (!i) { x0 = y0 = 0; }\n    var j = 2, n = input.length, output = new Array(n);\n    output[0] = (x0 += input[0]) * kx + dx;\n    output[1] = (y0 += input[1]) * ky + dy;\n    while (j < n) { output[j] = input[j], ++j; }\n    return output;\n  };\n};\n\nvar bbox = function(topology) {\n  var t = transform(topology.transform), key,\n      x0 = Infinity, y0 = x0, x1 = -x0, y1 = -x0;\n\n  function bboxPoint(p) {\n    p = t(p);\n    if (p[0] < x0) { x0 = p[0]; }\n    if (p[0] > x1) { x1 = p[0]; }\n    if (p[1] < y0) { y0 = p[1]; }\n    if (p[1] > y1) { y1 = p[1]; }\n  }\n\n  function bboxGeometry(o) {\n    switch (o.type) {\n      case \"GeometryCollection\": o.geometries.forEach(bboxGeometry); break;\n      case \"Point\": bboxPoint(o.coordinates); break;\n      case \"MultiPoint\": o.coordinates.forEach(bboxPoint); break;\n    }\n  }\n\n  topology.arcs.forEach(function(arc) {\n    var i = -1, n = arc.length, p;\n    while (++i < n) {\n      p = t(arc[i], i);\n      if (p[0] < x0) { x0 = p[0]; }\n      if (p[0] > x1) { x1 = p[0]; }\n      if (p[1] < y0) { y0 = p[1]; }\n      if (p[1] > y1) { y1 = p[1]; }\n    }\n  });\n\n  for (key in topology.objects) {\n    bboxGeometry(topology.objects[key]);\n  }\n\n  return [x0, y0, x1, y1];\n};\n\nvar reverse = function(array, n) {\n  var t, j = array.length, i = j - n;\n  while (i < --j) { t = array[i], array[i++] = array[j], array[j] = t; }\n};\n\nvar feature = function(topology, o) {\n  return o.type === \"GeometryCollection\"\n      ? {type: \"FeatureCollection\", features: o.geometries.map(function(o) { return feature$1(topology, o); })}\n      : feature$1(topology, o);\n};\n\nfunction feature$1(topology, o) {\n  var id = o.id,\n      bbox = o.bbox,\n      properties = o.properties == null ? {} : o.properties,\n      geometry = object(topology, o);\n  return id == null && bbox == null ? {type: \"Feature\", properties: properties, geometry: geometry}\n      : bbox == null ? {type: \"Feature\", id: id, properties: properties, geometry: geometry}\n      : {type: \"Feature\", id: id, bbox: bbox, properties: properties, geometry: geometry};\n}\n\nfunction object(topology, o) {\n  var transformPoint = transform(topology.transform),\n      arcs = topology.arcs;\n\n  function arc(i, points) {\n    if (points.length) { points.pop(); }\n    for (var a = arcs[i < 0 ? ~i : i], k = 0, n = a.length; k < n; ++k) {\n      points.push(transformPoint(a[k], k));\n    }\n    if (i < 0) { reverse(points, n); }\n  }\n\n  function point(p) {\n    return transformPoint(p);\n  }\n\n  function line(arcs) {\n    var points = [];\n    for (var i = 0, n = arcs.length; i < n; ++i) { arc(arcs[i], points); }\n    if (points.length < 2) { points.push(points[0]); } // This should never happen per the specification.\n    return points;\n  }\n\n  function ring(arcs) {\n    var points = line(arcs);\n    while (points.length < 4) { points.push(points[0]); } // This may happen if an arc has only two points.\n    return points;\n  }\n\n  function polygon(arcs) {\n    return arcs.map(ring);\n  }\n\n  function geometry(o) {\n    var type = o.type, coordinates;\n    switch (type) {\n      case \"GeometryCollection\": return {type: type, geometries: o.geometries.map(geometry)};\n      case \"Point\": coordinates = point(o.coordinates); break;\n      case \"MultiPoint\": coordinates = o.coordinates.map(point); break;\n      case \"LineString\": coordinates = line(o.arcs); break;\n      case \"MultiLineString\": coordinates = o.arcs.map(line); break;\n      case \"Polygon\": coordinates = polygon(o.arcs); break;\n      case \"MultiPolygon\": coordinates = o.arcs.map(polygon); break;\n      default: return null;\n    }\n    return {type: type, coordinates: coordinates};\n  }\n\n  return geometry(o);\n}\n\nvar stitch = function(topology, arcs) {\n  var stitchedArcs = {},\n      fragmentByStart = {},\n      fragmentByEnd = {},\n      fragments = [],\n      emptyIndex = -1;\n\n  // Stitch empty arcs first, since they may be subsumed by other arcs.\n  arcs.forEach(function(i, j) {\n    var arc = topology.arcs[i < 0 ? ~i : i], t;\n    if (arc.length < 3 && !arc[1][0] && !arc[1][1]) {\n      t = arcs[++emptyIndex], arcs[emptyIndex] = i, arcs[j] = t;\n    }\n  });\n\n  arcs.forEach(function(i) {\n    var e = ends(i),\n        start = e[0],\n        end = e[1],\n        f, g;\n\n    if (f = fragmentByEnd[start]) {\n      delete fragmentByEnd[f.end];\n      f.push(i);\n      f.end = end;\n      if (g = fragmentByStart[end]) {\n        delete fragmentByStart[g.start];\n        var fg = g === f ? f : f.concat(g);\n        fragmentByStart[fg.start = f.start] = fragmentByEnd[fg.end = g.end] = fg;\n      } else {\n        fragmentByStart[f.start] = fragmentByEnd[f.end] = f;\n      }\n    } else if (f = fragmentByStart[end]) {\n      delete fragmentByStart[f.start];\n      f.unshift(i);\n      f.start = start;\n      if (g = fragmentByEnd[start]) {\n        delete fragmentByEnd[g.end];\n        var gf = g === f ? f : g.concat(f);\n        fragmentByStart[gf.start = g.start] = fragmentByEnd[gf.end = f.end] = gf;\n      } else {\n        fragmentByStart[f.start] = fragmentByEnd[f.end] = f;\n      }\n    } else {\n      f = [i];\n      fragmentByStart[f.start = start] = fragmentByEnd[f.end = end] = f;\n    }\n  });\n\n  function ends(i) {\n    var arc = topology.arcs[i < 0 ? ~i : i], p0 = arc[0], p1;\n    if (topology.transform) { p1 = [0, 0], arc.forEach(function(dp) { p1[0] += dp[0], p1[1] += dp[1]; }); }\n    else { p1 = arc[arc.length - 1]; }\n    return i < 0 ? [p1, p0] : [p0, p1];\n  }\n\n  function flush(fragmentByEnd, fragmentByStart) {\n    for (var k in fragmentByEnd) {\n      var f = fragmentByEnd[k];\n      delete fragmentByStart[f.start];\n      delete f.start;\n      delete f.end;\n      f.forEach(function(i) { stitchedArcs[i < 0 ? ~i : i] = 1; });\n      fragments.push(f);\n    }\n  }\n\n  flush(fragmentByEnd, fragmentByStart);\n  flush(fragmentByStart, fragmentByEnd);\n  arcs.forEach(function(i) { if (!stitchedArcs[i < 0 ? ~i : i]) { fragments.push([i]); } });\n\n  return fragments;\n};\n\nfunction extractArcs(topology, object$$1, filter) {\n  var arcs = [],\n      geomsByArc = [],\n      geom;\n\n  function extract0(i) {\n    var j = i < 0 ? ~i : i;\n    (geomsByArc[j] || (geomsByArc[j] = [])).push({i: i, g: geom});\n  }\n\n  function extract1(arcs) {\n    arcs.forEach(extract0);\n  }\n\n  function extract2(arcs) {\n    arcs.forEach(extract1);\n  }\n\n  function extract3(arcs) {\n    arcs.forEach(extract2);\n  }\n\n  function geometry(o) {\n    switch (geom = o, o.type) {\n      case \"GeometryCollection\": o.geometries.forEach(geometry); break;\n      case \"LineString\": extract1(o.arcs); break;\n      case \"MultiLineString\": case \"Polygon\": extract2(o.arcs); break;\n      case \"MultiPolygon\": extract3(o.arcs); break;\n    }\n  }\n\n  geometry(object$$1);\n\n  geomsByArc.forEach(filter == null\n      ? function(geoms) { arcs.push(geoms[0].i); }\n      : function(geoms) { if (filter(geoms[0].g, geoms[geoms.length - 1].g)) { arcs.push(geoms[0].i); } });\n\n  return arcs;\n}\n\nfunction planarRingArea(ring) {\n  var i = -1, n = ring.length, a, b = ring[n - 1], area = 0;\n  while (++i < n) { a = b, b = ring[i], area += a[0] * b[1] - a[1] * b[0]; }\n  return Math.abs(area); // Note: doubled area!\n}\n\nvar bisect = function(a, x) {\n  var lo = 0, hi = a.length;\n  while (lo < hi) {\n    var mid = lo + hi >>> 1;\n    if (a[mid] < x) { lo = mid + 1; }\n    else { hi = mid; }\n  }\n  return lo;\n};\n\nvar untransform = function(transform) {\n  if (transform == null) { return identity; }\n  var x0,\n      y0,\n      kx = transform.scale[0],\n      ky = transform.scale[1],\n      dx = transform.translate[0],\n      dy = transform.translate[1];\n  return function(input, i) {\n    if (!i) { x0 = y0 = 0; }\n    var j = 2,\n        n = input.length,\n        output = new Array(n),\n        x1 = Math.round((input[0] - dx) / kx),\n        y1 = Math.round((input[1] - dy) / ky);\n    output[0] = x1 - x0, x0 = x1;\n    output[1] = y1 - y0, y0 = y1;\n    while (j < n) { output[j] = input[j], ++j; }\n    return output;\n  };\n};\n\nvar slicers = {};\r\nvar options;\r\n\r\nonmessage = function (e) {\r\n	if (e.data[0] === 'slice') {\r\n		// Given a blob of GeoJSON and some topojson/geojson-vt options, do the slicing.\r\n		var geojson = e.data[1];\r\n		options     = e.data[2];\r\n\r\n		if (geojson.type && geojson.type === 'Topology') {\r\n			for (var layerName in geojson.objects) {\r\n				slicers[layerName] = geojsonvt(\r\n					feature(geojson, geojson.objects[layerName])\r\n				, options);\r\n			}\r\n		} else {\r\n			slicers[options.vectorTileLayerName] = geojsonvt(geojson, options);\r\n		}\r\n\r\n	} else if (e.data[0] === 'get') {\r\n		// Gets the vector tile for the given coordinates, sends it back as a message\r\n		var coords = e.data[1];\r\n\r\n		var tileLayers = {};\r\n		for (var layerName in slicers) {\r\n			var slicedTileLayer = slicers[layerName].getTile(coords.z, coords.x, coords.y);\r\n\r\n			if (slicedTileLayer) {\r\n				var vectorTileLayer = {\r\n					features: [],\r\n					extent: options.extent,\r\n					name: options.vectorTileLayerName,\r\n					length: slicedTileLayer.features.length\r\n				};\r\n\r\n				for (var i in slicedTileLayer.features) {\r\n					var feat = {\r\n						geometry: slicedTileLayer.features[i].geometry,\r\n						properties: slicedTileLayer.features[i].tags,\r\n						type: slicedTileLayer.features[i].type	// 1 = point, 2 = line, 3 = polygon\r\n					};\r\n					vectorTileLayer.features.push(feat);\r\n				}\r\n				tileLayers[layerName] = vectorTileLayer;\r\n			}\r\n		}\r\n		postMessage({ layers: tileLayers, coords: coords });\r\n	}\r\n};\n//# sourceMap" + "pingURL=slicerWebWorker.js.worker.map\n", "text/plain; charset=us-ascii", false);

// The geojson/topojson is sliced into tiles via a web worker.
// This import statement depends on rollup-file-as-blob, so that the
// variable 'workerCode' is a blob URL.

/*
 * 🍂class VectorGrid.Slicer
 * 🍂extends VectorGrid
 *
 * A `VectorGrid` for slicing up big GeoJSON or TopoJSON documents in vector
 * tiles, leveraging [`geojson-vt`](https://github.com/mapbox/geojson-vt).
 *
 * 🍂example
 *
 * ```
 * var geoJsonDocument = {
 * 	type: 'FeatureCollection',
 * 	features: [ ... ]
 * };
 *
 * L.vectorGrid.slicer(geoJsonDocument, {
 * 	vectorTileLayerStyles: {
 * 		sliced: { ... }
 * 	}
 * }).addTo(map);
 *
 * ```
 *
 * `VectorGrid.Slicer` can also handle [TopoJSON](https://github.com/mbostock/topojson) transparently:
 * ```js
 * var layer = L.vectorGrid.slicer(topojson, options);
 * ```
 *
 * The TopoJSON format [implicitly groups features into "objects"](https://github.com/mbostock/topojson-specification/blob/master/README.md#215-objects).
 * These will be transformed into vector tile layer names when styling (the
 * `vectorTileLayerName` option is ignored when using TopoJSON).
 *
 */

L.VectorGrid.Slicer = L.VectorGrid.extend({

	options: {
		// 🍂section
		// Additionally to these options, `VectorGrid.Slicer` can take in any
		// of the [`geojson-vt` options](https://github.com/mapbox/geojson-vt#options).

		// 🍂option vectorTileLayerName: String = 'sliced'
		// Vector tiles contain a set of *data layers*, and those data layers
		// contain features. Thus, the slicer creates one data layer, with
		// the name given in this option. This is important for symbolizing the data.
		vectorTileLayerName: 'sliced',

		extent: 4096,	// Default for geojson-vt
		maxZoom: 14  	// Default for geojson-vt
	},

	initialize: function(geojson, options) {
		L.VectorGrid.prototype.initialize.call(this, options);

		// Create a shallow copy of this.options, excluding things that might
		// be functions - we only care about topojson/geojsonvt options
		var options = {};
		for (var i in this.options) {
			if (i !== 'rendererFactory' &&
				i !== 'vectorTileLayerStyles' &&
				typeof (this.options[i]) !== 'function'
			) {
				options[i] = this.options[i];
			}
		}

// 		this._worker = new Worker(window.URL.createObjectURL(new Blob([workerCode])));
		this._worker = new Worker(workerCode);

		// Send initial data to worker.
		this._worker.postMessage(['slice', geojson, options]);

	},


	_getVectorTilePromise: function(coords) {

		var _this = this;

		var p = new Promise( function waitForWorker(res) {
			_this._worker.addEventListener('message', function recv(m) {
				if (m.data.coords &&
				    m.data.coords.x === coords.x &&
				    m.data.coords.y === coords.y &&
				    m.data.coords.z === coords.z ) {

					res(m.data);
					_this._worker.removeEventListener('message', recv);
				}
			});
		});

		this._worker.postMessage(['get', coords]);

		return p;
	},

});


L.vectorGrid.slicer = function (geojson, options) {
	return new L.VectorGrid.Slicer(geojson, options);
};

L.Canvas.Tile = L.Canvas.extend({

	initialize: function (tileCoord, tileSize, options) {
		L.Canvas.prototype.initialize.call(this, options);
		this._tileCoord = tileCoord;
		this._size = tileSize;

		this._initContainer();
		this._container.setAttribute('width', this._size.x);
		this._container.setAttribute('height', this._size.y);
		this._layers = {};
		this._drawnLayers = {};
		this._drawing = true;

		if (options.interactive) {
			// By default, Leaflet tiles do not have pointer events
			this._container.style.pointerEvents = 'auto';
		}
	},

	getCoord: function() {
		return this._tileCoord;
	},

	getContainer: function() {
		return this._container;
	},

	getOffset: function() {
		return this._tileCoord.scaleBy(this._size).subtract(this._map.getPixelOrigin());
	},

	onAdd: L.Util.falseFn,

	addTo: function(map) {
		this._map = map;
	},

	removeFrom: function (map) {
		delete this._map;
	},

	_onClick: function (e) {
		var point = this._map.mouseEventToLayerPoint(e).subtract(this.getOffset()), layer, clickedLayer;

		for (var id in this._layers) {
			layer = this._layers[id];
			if (layer.options.interactive && layer._containsPoint(point) && !this._map._draggableMoved(layer)) {
				clickedLayer = layer;
			}
		}
		if (clickedLayer)  {
			L.DomEvent.fakeStop(e);
			this._fireEvent([clickedLayer], e);
		}
	},

	_onMouseMove: function (e) {
		if (!this._map || this._map.dragging.moving() || this._map._animatingZoom) { return; }

		var point = this._map.mouseEventToLayerPoint(e).subtract(this.getOffset());
		this._handleMouseHover(e, point);
	},

	/// TODO: Modify _initPath to include an extra parameter, a group name
	/// to order symbolizers by z-index

	_updateIcon: function (layer) {
		if (!this._drawing) { return; }

		var icon = layer.options.icon,
		    options = icon.options,
		    size = L.point(options.iconSize),
		    anchor = options.iconAnchor ||
		        	 size && size.divideBy(2, true),
		    p = layer._point.subtract(anchor),
		    ctx = this._ctx,
		    img = layer._getImage();

		if (img.complete) {
			ctx.drawImage(img, p.x, p.y, size.x, size.y);
		} else {
			L.DomEvent.on(img, 'load', function() {
				ctx.drawImage(img, p.x, p.y, size.x, size.y);
			});
		}

		this._drawnLayers[layer._leaflet_id] = layer;
	}
});


L.canvas.tile = function(tileCoord, tileSize, opts){
	return new L.Canvas.Tile(tileCoord, tileSize, opts);
};

// Aux file to bundle everything together
//# sourceMappingURL=Leaflet.VectorGrid.js.map

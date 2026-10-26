// Oʻahu tract-by-year table: Landsat surface temperature + greenness (NDVI)
// Paste into the Google Earth Engine Code Editor (code.earthengine.google.com) and press Run.
// Then open the Tasks tab and click RUN on each export. CSVs land in Google Drive.
//
// Output 1 (oahu_tract_year): one row per census tract per year
//   GEOID    census tract ID
//   year
//   lst_c    median warm-season land surface temperature, deg C, averaged over the tract
//   ndvi     median warm-season greenness (-1 to 1), averaged over the tract
//   n_clear  average number of cloud-free observations behind each pixel (data quality)
// Output 2 (oahu_tract_elevation): one row per tract with mean elevation in metres

// ---------- settings ----------
var START_YEAR = 1999;   // Landsat 7 launch; earlier Hawaiʻi coverage is likely sparse
var END_YEAR = 2025;
var MONTH_START = 6;     // warm season window; widen if n_clear is low
var MONTH_END = 10;

// ---------- areas: census tracts, City & County of Honolulu ----------
var tracts = ee.FeatureCollection('TIGER/2020/TRACT')
  .filter(ee.Filter.eq('STATEFP', '15'))
  .filter(ee.Filter.eq('COUNTYFP', '003'));

// ---------- Landsat Collection 2 Level 2 ----------
function clearMask(img) {
  var qa = img.select('QA_PIXEL');
  return qa.bitwiseAnd(1 << 1).eq(0)        // dilated cloud
    .and(qa.bitwiseAnd(1 << 3).eq(0))       // cloud
    .and(qa.bitwiseAnd(1 << 4).eq(0));      // cloud shadow
}

function prep(img, redBand, nirBand, thermalBand) {
  var red = img.select(redBand).multiply(0.0000275).add(-0.2);
  var nir = img.select(nirBand).multiply(0.0000275).add(-0.2);
  var ndvi = nir.subtract(red).divide(nir.add(red)).rename('ndvi');
  var lst = img.select(thermalBand).multiply(0.00341802).add(149.0)
    .subtract(273.15).rename('lst_c');     // Kelvin -> Celsius
  return lst.addBands(ndvi)
    .updateMask(clearMask(img))
    .copyProperties(img, ['system:time_start']);
}

function prepL57(img) { return prep(img, 'SR_B3', 'SR_B4', 'ST_B6'); }
function prepL89(img) { return prep(img, 'SR_B4', 'SR_B5', 'ST_B10'); }

var landsat = ee.ImageCollection('LANDSAT/LT05/C02/T1_L2').map(prepL57)
  .merge(ee.ImageCollection('LANDSAT/LE07/C02/T1_L2').map(prepL57))
  .merge(ee.ImageCollection('LANDSAT/LC08/C02/T1_L2').map(prepL89))
  .merge(ee.ImageCollection('LANDSAT/LC09/C02/T1_L2').map(prepL89))
  .filterBounds(tracts)
  .filter(ee.Filter.calendarRange(MONTH_START, MONTH_END, 'month'));

// ---------- summarize per tract per year ----------
var years = ee.List.sequence(START_YEAR, END_YEAR);

var perYear = years.map(function (y) {
  y = ee.Number(y).toInt();
  var col = landsat.filter(ee.Filter.calendarRange(y, y, 'year'));
  var result = ee.Algorithms.If(
    col.size().gt(0),
    col.median()
      .addBands(col.select('lst_c').count().rename('n_clear'))
      .reduceRegions({
        collection: tracts,
        reducer: ee.Reducer.mean(),
        scale: 30,
        tileScale: 4
      })
      .map(function (f) { return f.setGeometry(null).set('year', y); }),
    ee.FeatureCollection([])
  );
  return ee.FeatureCollection(result);
});

var table = ee.FeatureCollection(perYear).flatten();

Export.table.toDrive({
  collection: table,
  description: 'oahu_tract_year',
  fileFormat: 'CSV',
  selectors: ['GEOID', 'year', 'lst_c', 'ndvi', 'n_clear']
});

// ---------- fixed control: elevation ----------
var elevation = ee.Image('USGS/SRTMGL1_003').select('elevation');

var elevTable = elevation.reduceRegions({
  collection: tracts,
  reducer: ee.Reducer.mean().setOutputs(['elev_m']),
  scale: 30,
  tileScale: 4
}).map(function (f) { return f.setGeometry(null); });

Export.table.toDrive({
  collection: elevTable,
  description: 'oahu_tract_elevation',
  fileFormat: 'CSV',
  selectors: ['GEOID', 'elev_m']
});

// ---------- quick look ----------
Map.centerObject(ee.Geometry.Point([-157.95, 21.45]), 10);
Map.addLayer(
  landsat.filter(ee.Filter.calendarRange(2023, 2023, 'year')).median().select('lst_c'),
  {min: 20, max: 50, palette: ['blue', 'yellow', 'red']},
  'Surface temp 2023 (C)'
);
Map.addLayer(tracts.style({color: 'black', fillColor: '00000000', width: 1}), {}, 'Tracts');

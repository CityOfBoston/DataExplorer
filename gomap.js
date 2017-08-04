// When locator icon in datatable is clicked, go to that spot on the map
// the below function is not used
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var zip = $el.data("zip");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    zip: zip,
    nonce: Math.random()
  });
});

// to tell what kind of device is running
$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
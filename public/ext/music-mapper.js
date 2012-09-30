function colourCountry(isoCode, colour) {
    var paths = document
            .getElementById('map')
            .contentDocument
            .getElementById('gGBR')
            .getElementsByTagName('path');

    for (var i = 0; i < paths.length; i++) {
        paths[i].style.fill = colour;
    }
}

window.addEventListener('load', function() {
    var wall = new Masonry(document.getElementById('artists'));

    colourCountry('GBR', 'lime');
});

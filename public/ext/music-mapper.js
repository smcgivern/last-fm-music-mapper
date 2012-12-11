var divisionSize = 5,
    colourPalette = [
        ['#f7fcb9', '#addd8e', '#31a354'],
        ['#ffffcc', '#c2e699', '#78c679', '#238443'],
        ['#ffffcc', '#c2e699', '#78c679', '#31a354', '#006837'],
        ['#ffffcc', '#d9f0a3', '#addd8e', '#78c679', '#31a354', '#006837'],
        ['#ffffcc', '#d9f0a3', '#addd8e', '#78c679', '#41ab5d', '#238443',
         '#005a32'],
        ['#ffffe5', '#f7fcb9', '#d9f0a3', '#addd8e', '#78c679', '#41ab5d',
         '#238443', '#005a32'],
        ['#ffffe5', '#f7fcb9', '#d9f0a3', '#addd8e', '#78c679', '#41ab5d',
         '#238443', '#006837', '#004529']
    ];

window.addEventListener('load', function() {
    var artists = document.getElementById('artists'),
        wall = new Masonry(artists),
        map = document.getElementById('map').contentDocument,
        countryNames = artists.getElementsByClassName('country-name'),
        highlightWrapper = function(a, c) { return function() {
            highlightArtists(a, c);
        };};

    mapCountries(artists, map, divisionSize, colourPalette);

    for (var i = 0; i < countryNames.length; i++) {
        var isoCodes = countryNames[i].className.match(/[A-Z]{3}/);

        if (isoCodes) {
            countryNames[i]
                .addEventListener('click',  highlightWrapper(artists,
                                                             isoCodes[0]));
        }
    }
});

function mapCountries(root, map, divisionSize, colourPalette) {
    var countries = countryPlayCounts(root),
        playCounts = objectValues(countries),
        divisions = numberOfDivisions(playCounts, divisionSize, colourPalette),
        colours = colourPalette.filter(function(x) {
            return x.length == divisions;
        })[0];

    for (var country in countries) {
        if (countries.hasOwnProperty(country)) {
            var i = paletteIndex(countries[country], playCounts, divisions);

            colourCountry(map, root, country, colours[i]);
        }
    }
}

function countryPlayCounts(root) {
    var countries = {},
        lis = root.getElementsByTagName('li');

    for (var i = 0; i < lis.length; i++) {
        var li = lis[i];

        li.className.split(/\s+/).forEach(function(className) {
            var components = className.split('-');

            if (components[0] == 'country') {
                if (!countries[components[1]] > 0) {
                    countries[components[1]] = 0;
                }

                countries[components[1]] = countries[components[1]] +
                    +li.getElementsByClassName('playcount')[0].textContent;
            }
        });
    }

    return countries;
}

// Calculates the number of divisions needed for the values, given divisionSize.
// Will only pick values which match the length of a sub-array of colourPalette.
//
function numberOfDivisions(values, divisionSize, colourPalette) {
    var idealNumber = Math.floor(unique(values).length / divisionSize),
        potentialNumbers = colourPalette.map(function (x) { return x.length; });

    potentialNumbers.sort(numeric);

    if (potentialNumbers.indexOf(idealNumber) > -1) {
        return idealNumber;
    } else if (idealNumber > potentialNumbers[-1]) {
        return potentialNumbers[-1];
    } else {
        return potentialNumbers[0];
    }
}

function paletteIndex(value, values, numberOfDivisions) {
    var uniqueValues = unique(values).sort(numeric),
        rangeSize = Math.floor(uniqueValues.length / numberOfDivisions);

    return Math.floor(numberOfDivisions * uniqueValues.indexOf(value)
                      / uniqueValues.length);
}

function colourCountry(root, artists, isoCode, colour) {
    var paths = root
            .getElementById('g' + isoCode)
            .getElementsByTagName('path');

    for (var i = 0; i < paths.length; i++) {
        paths[i].style.fill = colour;
        paths[i].addEventListener('click', function() {
            highlightArtists(artists, isoCode);
        });
    }
}

function highlightArtists(root, isoCode) {
    var lis = root.getElementsByTagName('li');

    for (var i = 0; i < lis.length; i++) {
        var li = lis[i];

        removeHighlight(li);

        if (li.className.indexOf('country-' + isoCode) > -1) {
            li.className = li.className + ' highlight';

            window.setTimeout(function(li) { removeHighlight(li); }, 30000, li);
        }
    }
}


function removeHighlight(element) {
    if (element.className.indexOf('highlight') > -1) {
        element.className = element.className.replace('highlight', '').trim();
    }
}

function numeric(a, b) { return a - b; }

function unique(array) {
    return array.slice(0).filter(function(element, index, array) {
        return (index == array.indexOf(element));
    });
}

function objectValues(object) {
    var values = [];

    for (var key in object) {
        if (object.hasOwnProperty(key)) {
            values.push(object[key]);
        }
    }

    return values;
}

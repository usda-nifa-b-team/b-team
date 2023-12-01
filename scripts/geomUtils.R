# From https://en.wikipedia.org/wiki/Longitude#Length_of_a_degree_of_longitude
hortis.WGS84a = 6378137;
hortis.WGS84b = 6356752.3142;
hortis.WGS84e2 = (hortis.WGS84a * hortis.WGS84a - hortis.WGS84b * hortis.WGS84b) / (hortis.WGS84a * hortis.WGS84a);

# Length in metres for a degree of longitude at given latitude
hortis.longitudeLength <- function (latitude) {
    latrad <- pi * latitude / 180;
    sinrad <- sin(latrad);
    return (pi * hortis.WGS84a * cos(latrad) / (180 * sqrt(1 - hortis.WGS84e2 * sinrad * sinrad)))
}

#Length in metres for a degree of latitude at given latitude
hortis.latitudeLength = function (latitude) {
    latrad <- pi * latitude / 180;
    sinrad <- sin(latrad);
    return (pi * hortis.WGS84a * (1 - hortis.WGS84e2) / (180 * (1 - hortis.WGS84e2 * sinrad * sinrad) ^ 1.5));
}

hortis.longToLat <- function (lng, lat) {
    longLength <- hortis.longitudeLength(lat);
    latLength <- hortis.latitudeLength(lat);
    return (lng * longLength / latLength);
}

CREATE TABLE bridges (
    bridgeId           INTEGER PRIMARY KEY,
    bridgeOwner        TEXT,
    bridgeRoute        TEXT,
    bridgeNumber       TEXT,
    bridgeName         TEXT,
    bridgeMp           REAL,
    bridgeCounty       TEXT,
    bridgeMunicipality TEXT,
    bridgeLatitude     REAL,
    bridgeLongitude    REAL
);

CREATE TABLE stations (
    stationId          INTEGER PRIMARY KEY,
    stationName        TEXT,
    stationLatitude    REAL,
    stationLongitude   REAL,
    stationAddress     TEXT,
    stationCity        TEXT,
    stationState       TEXT,
    stationCounty      TEXT,
    stationHours       TEXT,
    stationPhoneNumber TEXT,
    stationTypeGas     TEXT
);

{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipAPI";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = "Functions to extract data and interact with web APIs.";
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipU
    jsonlite
    httr
    flipTransformations
    flipTime
    zip
    flipFormat
    odbc
    gganimate
    knitr
    mime
    stringr
    readxl
    rhtmlMetro
    arrow
    RMySQL
    lubridate
    zeallot
    DBI
    RGoogleAnalytics
    RPostgres
    RJSONIO
    haven
    openxlsx
    rgeolocate
    curl
    git2r
  ];
}

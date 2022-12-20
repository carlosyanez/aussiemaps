[![R](https://github.com/carlosyanez/aussiemaps/actions/workflows/r.yml/badge.svg)](https://github.com/carlosyanez/aussiemaps/actions/workflows/r.yml)

aussiemaps
================

<img src="https://github.com/carlosyanez/aussiemaps/raw/master/img/hexSticker.png" width = "175" height = "200" align="right" />

**aussiemaps** provides Australian LGA, suburbs and Postal Area maps
directly in R, without any need to download and process shapefiles.

The data for the package has been created using the scripts provided
[here](https://github.com/carlosyanez/Australian_Polygons). The sf
objects contain (Suburb,LGA, Postal Area) intersection polygons, which
allow to select, filter and re-construct any of those geographic units.

## Installation and pre-requisites

First install required packages,then, install this package from github

```
install.packages("tidyverse","sf","lwgeom","devtools")
devtools::install_github("carlosyanez/aussiemaps")
```
Alternatively, install from r-universe

```
install.packages("tidyverse","sf","lwgeom","devtools")

# Enable this universe
options(repos = c(
    carlosyanez = 'https://carlosyanez.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('aussiemaps')

```

This package does not contain the [data files](https://github.com/carlosyanez/aussiemaps.data), which will be downloaded the first time you call the library. Alternatively, you can download them directly:

```
devtools::install_github("carlosyanez/aussiemaps.data")

```


## Not for you?

The development of this package is still in progress.It also intends to
provide granular suburb/postal area maps, so it may not be suitable for
some use cases. There are other great Australian maps packages, such as:

-   [ozmaps](https://mdsumner.github.io/ozmaps/), which has good
    nation-wide, states and LGA maps, as well as electoral divisions.
    This package is on CRAN.
-   [absmaps](https://github.com/wfmackey/absmaps), which provides maps
    for all ABS statistical divisions.

## Issues? bugs? Ideas?

If you find something that is not quite right, please post an issue. If
you have any ideas, or if you want to contribute, [please let me know](https://twitter.com/messages/25712933-3805104374?recipient_id=25712933&text=Hello%20world)!

## To Do

At the moment, this package only provides maps for
[internal](https://en.wikipedia.org/wiki/States_and_territories_of_Australia)
states and territories.

## Credits

-   LGA and localities maps have been sourced from
    [data.gov.au](htttp:///data.gov.au) and used under CC0-BY licence.
-   Postal areas have constructed using definitions and shape files from
    the [Australian Bureau of Statistics (ABS)](https://www.abs.gov.au/)

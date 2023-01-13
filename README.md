[![R-CMD-check](https://github.com/carlosyanez/aussiemaps/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/carlosyanez/aussiemaps/actions/workflows/R-CMD-check.yaml)
[![aussiemaps status badge](https://carlosyanez.r-universe.dev/badges/aussiemaps)](https://carlosyanez.r-universe.dev)

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

You can install this package from Github

```
remotes::install_github("carlosyanez/aussiemaps")
```
Alternatively, install from r-universe

```

# Enable this universe
options(repos = c(
    carlosyanez = 'https://carlosyanez.r-universe.dev',
    CRAN = 'https://cloud.r-project.org'))

# Install some packages
install.packages('aussiemaps')

```

Please note that no data is included in the package installation - data files  will be downloaded and cached as needed.


## Not for you?

The development of this package is still in progress.It also intends to
provide granular suburb/postal area maps, so it may not be suitable for
some use cases. There are other great Australian maps packages, such as:

-   [ozmaps](https://mdsumner.github.io/ozmaps/), which has good
    nation-wide, states and LGA maps, as well as electoral divisions.
    This package is on CRAN.
-   [absmapsdata](https://github.com/wfmackey/absmapsdata), which provides maps
    for all ABS statistical divisions.

## Issues? bugs? Ideas?

If you find something that is not quite right, please post an issue. If
you have any ideas, or if you want to contribute, [please let me know](https://fosstodon.org/@carlosyanez)!

## To Do

At the moment, this package only provides maps for
[internal](https://en.wikipedia.org/wiki/States_and_territories_of_Australia)
states and territories.

## Credits

-   Data in this package has been created  from the [Australian Bureau of Statistics (ABS)](https://www.abs.gov.au/) geopgraphic boundary files, available at the ABS's website and at htttp://data.gov.au

## Acknowledgment of Country

The author of this package acknowledges the Aboriginal and Torres Strait Islander people as the traditional custodians of Australia, and pays respect to their Elders past, present and emerging.

aussiemaps
================

**aussiemaps** provides Australian LGA, suburbs and Postal Area maps
directly in R, without any need to download and process shapefiles.

The data for the package has been created using the scripts provided
[here](https://github.com/carlosyanez/Australian_Polygons). The sf
objects in this contain (Suburb,LGA, Postal Area) intersection polygons,
which allow to select, filter and re-construct any of those geographic
units.

## Installation and pre-requisites

First install required packages

`install.packages("tidyverse","sf","lwgeom","devtools")`

then, install this package from github

`devtools::install_github("carlosyanez/aussiemaps")`

## How to use

The main function in this package is **load\_map()**, which retrieves a
selected portion of the map based on a filter “tibble” and an
aggregation attribute.

Filter tibbles can be created any combination of attributes available on
**list\_aggregations**, as long as *State* is present. Another way to
create a filter tibble is to subset **locations.table**, which is
included in package.

Aggregations are optional, if not added, will retrieve the granular
polygons directly.

The below plot shows and example on how to retrieve different sf
objects, with different aggregations (postal area 3004 covers parts of
two different suburs. South Yarra is shared across two LGAs).

![](map1.png)

The package also contains a number of pre-defined “regions”. These are
either official (e.g. Grampians Region) or *unofficial* (e.g. Limestone
coast) divisions. These regions can also cross state lines (e.g. Gold
Coast - Tweed Heads). To find out the existing regions, run
*list\_regions()*. With the region name, *get\_region()* will produce
the corresponding filter tibble.

![](map2.png)

Since the loaded objects are sf objects, they can be easily merge with
data frames or tibbles to add additional data for visualisation.

![](map3.png)

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
you have any ideas, or want to collaborate please let me know!

## To Do

At the moment, this package only provides maps for
[internal](https://en.wikipedia.org/wiki/States_and_territories_of_Australia)
states and territories.

## Credits

-   LGA and localities maps have been sourced from
    [data.gov.au](htttp:///data.gov.au), and used under CC0-BY licence.
-   Postal areas have constructed using definitions and shape files from
    the [Australian Bureau of Statistics (ABS)](https://www.abs.gov.au/)

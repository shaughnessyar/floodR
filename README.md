# floodR

Flood water is often a mixture of water originating from various sources including rain, groundwater, municipal water, etc. Hydrograph separations delineate the various flow componants during floods. Calculating these flow componants typically requires a formula to be applied over each of the floods individually. There are currently methods to automate hydrograph separations, however, these methods reply on the shape of the hydrograph to delinate flow componants, typically drawing lines accross inflection points in the hydrograph to estimate runnoff fractions. These "graphical" methods, although quick to implement, are often less accurate than chemical hydrograph separations. `floodR` offers tools to streamline the chemical hydrograph separation process, preserving the accuracy of a chemical hydrograph separation with the ease of the graphical method. 

## Installation

You can install package from github with:


``` r
# install.packages("devtools")
devtools::install_github("shaughnessyar/floodR")
```

## Progress

This package is currently under development, and will be updated as features are completed. 

* Select floods interactively using selector gadget. Status: draft on GitHub
* Merge water quality data with USGS data. Status: in develop development
* Hydrograph separations. Status: draft on GitHub



---
title: "var_loc"
author: "Valentina Vaney"
format: html
editor: visual
---


### Function for finding FVS inputs: VAR and LOC

all FVS input files need two identifier keys so that it can calibrate model based on location of property. This script gets you both the variant and location codes needed for input generation. Use these in step 3 when creating FVS input.

#### Install Required Packages


::: {.cell}

```{.r .cell-code}
required_packages <- c("sf", "tidyverse", "ggplot2", "rnaturalearth", 
                       "soilDB", "dplyr", "sp", "odbc", "tigris", 
                       "lwgeom", "data.table","knitr")

# Function to check and install packages
install_if_missing <- function(packages) {
  installed <- installed.packages()
  for (pkg in packages) {
    if (!pkg %in% installed[, "Package"]) {
      install.packages(pkg)
    }
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load the libraries
library(sf)
```

::: {.cell-output .cell-output-stderr}

```
Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.3.1; sf_use_s2() is TRUE
```


:::

```{.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}

```
Warning: package 'lubridate' was built under R version 4.4.1
```


:::

::: {.cell-output .cell-output-stderr}

```
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.1     ✔ tibble    3.2.1
✔ lubridate 1.9.3     ✔ tidyr     1.3.1
✔ purrr     1.0.2     
```


:::

::: {.cell-output .cell-output-stderr}

```
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```


:::

```{.r .cell-code}
library(ggplot2)
library(rnaturalearth)
library(soilDB)
library(dplyr)
library(sp)
library(odbc)
library(tigris)
```

::: {.cell-output .cell-output-stderr}

```
To enable caching of data, set `options(tigris_use_cache = TRUE)`
in your R script or .Rprofile.
```


:::

```{.r .cell-code}
library(lwgeom)
```

::: {.cell-output .cell-output-stderr}

```
Linking to liblwgeom 3.0.0beta1 r16016, GEOS 3.12.1, PROJ 9.3.1

Attaching package: 'lwgeom'

The following object is masked from 'package:sf':

    st_perimeter
```


:::

```{.r .cell-code}
library(data.table)
```

::: {.cell-output .cell-output-stderr}

```

Attaching package: 'data.table'

The following objects are masked from 'package:lubridate':

    hour, isoweek, mday, minute, month, quarter, second, wday, week,
    yday, year

The following objects are masked from 'package:dplyr':

    between, first, last

The following object is masked from 'package:purrr':

    transpose
```


:::
:::


### load the FVS variant map


::: {.cell}

```{.r .cell-code}
# Read variant map
variant_map <- read_sf("FVSVariantMap20210525/FVS_Variants_and_Locations.shp")

variant_map <- variant_map %>%
  filter(FVSVariant != "AK" & FVSVariant != "HI")

# Read USA shapefile
usa <- ne_states(country = "united states of america", returnclass = "sf")
usa <- usa %>%
  filter(name != "Alaska" & name != "Hawaii")

#set albers projection
utm_crs <- st_crs(32616) 
```
:::


### Create the function that will extract var and loc for your shapefile


::: {.cell}

```{.r .cell-code}
plot_variant_map <- function(your_shape) {
  
  # Transform variant map and USA shapefiles to UTM
  variant_map <- st_transform(variant_map, utm_crs)
  
  # If input is a string (path to shapefile), read the shapefile
  if (is.character(your_shape)) {
    your_shape <- read_sf(your_shape)
  }
  
  # Transform your shapefile to UTM
  your_shape <- st_transform(your_shape, utm_crs)
  
  # Find intersection between your shape and variant map
  var_shp <- st_intersection(your_shape, variant_map)
  
  # Extract unique VARIANT and LOCATION
  VARIANT <- unique(var_shp$FVSVariant)
  LOCATION <- unique(var_shp$FVSLocCode)
  
  # Create a data frame with VARIANT and LOCATION
  result <- data.frame(VARIANT = VARIANT, LOCATION = LOCATION)
  
  return(result)
}
```
:::


###  Upload your shape file here

Between the double quotes in green delete whats in there and click tab until you find the shape file of interest. Don't forget that this is a remote repo and if you want your shape file of choice to be easily accessible upload it to this repository ;)


::: {.cell}

```{.r .cell-code}
hello <- read_sf("property/HILTON_STANDS.shp") #insert your shape file here ;)
plot_variant_map(hello)
```

::: {.cell-output .cell-output-stderr}

```
Warning: attribute variables are assumed to be spatially constant throughout
all geometries
```


:::

::: {.cell-output .cell-output-stdout}

```
  VARIANT LOCATION
1      NE      922
```


:::
:::


Tips for Users
================
Dana Seidel
December 19, 2018

# Getting Started

Watch this [demo video](https://youtu.be/ub9ApzhYano) I created for
you\! I hope it will answer most of your questions and give you a sense
of the intended workflow and proper usage of the app.

# General usage tips

The app has four main functions:

1.  Load your data in the form of a .csv
2.  Extract new environmental variables to your data
3.  Fit logistic regression models to your data
4.  Build and map predictive risk layers from regression models

You can also download extracted data, your predicted risk layer, and a
short report including regression results and a plot from within the
app.

## Bare minimum requirements

This app is intended for use with CWD surveillance data from the
province of Alberta. For full functionality, at a minimum the dataset
you provide needs to have 3 columns - a column of cwd status in 1’s and
0’s and two columns specifying x and y coordinates values, projected in
UTM 12N NAD83. With these 3 columns alone, you could then extract all 14
environmental varaibles provided within the app and listed on the
`Information` tab. With these 14 variables you could then fit a
regression and make a predicted map If you are solely interested in the
app’s extraction tool, it is possible to extract (and download)
variables provided only x and y coordinate columns - without a cwd
status variable however regression and prediction is of course
impossible.

Of course, if columns for deer sex, species, time since 1st positive,
harvest method opr proximity to previous positives (all known to be
biologically relevant) are not provided, one cannot fit models or build
predictive layers including these important variables. Therefore the
recommended minimum user-provided dataset includes: *cwd status, x
coordinate in UTM 12N, y coordinate in UTM 12N, time since 2000,
proximity to previous cwd+ cases in km, deer species, deer sex, and
harvest method*.

### Calculating +Proximity variable

Most of the recommended variables listed above are included in or easily
derived from the raw surveillance data collected each year by GovAB,
however proximity to previous CWD+ cases requires some additional
analysis and, ideally, access to all previous known cases in the region
This is one of the most highly predictive variables in estimating CWD
risk and yet it’s the only one the app cannot calculate for you. I’ve
included some code below that can help users calculate their own
proximity values in R - it requires that you provide positives with a
date column and x and y coordinates. Ideally this is run using a
complete dataset of historic surveillance in Alberta and bordering
Saskatchewan WMZs to get the most accurate available values.

``` r
# install.packaages(c("sf", "dplyr"))
library(sf)
library(dplyr)

# Parameters are as follows:
#
# df_positives -- a dataframe of all known CWD+ cases in the regions.
# points -- a dataframe of all surveillance points of interest
#
# Both dataframes must contain columns named "date", "x" and "y" exactly
# referring to the date and x and y coordinates in the UTM12N projected crs when and where it was collected
#
# Depending on the length of your dataframe this can take several minutes. 
# For the historic dataset with ~49000 points, this took roughly 9.5 mins

calc_prox <- function(points, df_positives) {

  # turn these data frames into spatial objects.
  pts <- points %>% st_as_sf(., coords = c("x", "y"), remove = F, crs = 26912)
  pos <- df_positives %>% st_as_sf(., coords = c("x", "y"), remove = F, crs = 26912)

  # intialize an empty column
  pts$prox <- NA

  # loop through points
  for (i in 1:nrow(pts)) {

    # filter those positives that been identified before the surveillance pt
    prev_pos <- filter(pos, date <= pts[i, "date"])
    
    # find distance btwn surveillance pt and all known positives 
    dist <- st_distance(pts[i, ], prev_pos)
    
    # save the minimum distance in this set
    pts[i, "prox"] <- min(dist)/1000  # make sure you convert to km
  }

  return(st_set_geometry(pts, NULL))
}
```

##### Why is this calculation not available in app?

Primarily, I opted not to include this function in app because the app
places no checks on the data that is supplied by the user and in order
to produce accurate results the proximity calculation must use a
complete or near complete dataset of historically known positive cases.
I considered storing such a list of positives internal to the app to
provide a baseline for accurate estimates but that too would require
code ensuring no duplicates between the user-provided and the internal
dataset and still would not ensure accuracy or completeness of users
code and thus the proximity estimates. Additionally, given the highly
sensitive nature of such data, we are cautious about storing it publicly
even on a secure
server.

##### You may be tempted to calculate your proximity column simply by extracting values from the proximity raster provided in the app - but you’ll note that is not allowed in app. Why not?

Simply stated, a spatial raster can have no time component while your
data must in order for an accurate risk assessment through time. You’ll
see that the code above requires a date column – this is becasue `+prox`
is calculated as the minimum distance between each point and all
*previous* CWD+ cases. The raster is only one snap shot in time –
proximity of each grid cell to known positive cases – where as
surveillance data is collected through time, and CWD spread is a
temporal process. Therefore when calculating `+prox` before a risk
assessment, you must take into account distances to only those CWD+
cases that exist before a given surveillance point, not all those that
may come in the future.

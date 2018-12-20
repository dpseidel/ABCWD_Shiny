Tips for Maintainers
================
Dana Seidel
December 19, 2018

App Requirements
================

When deployed on shinyapps.io, this app requires at least an **xx-large** instance. Without it the app will crash when users try to build a predictions layer because of memory requirements.

Deployment
==========

This app was deployed using the following script:

``` r
# Given the appropriate token and secret
# rsconnect::setAccountInfo(name='fw-habitat-aep', TOKEN, SECRET)

rsconnect::deployApp(
  account = "fw-habitat-aep",
  appFileManifest = "manifest.txt",
  appName = "ABCWD_Shiny",
  appTitle = "Alberta CWD risk assessment app"
)

# requires significant RAM to calculate predictions layer, so update the configuration
rsconnect::configureApp(account = "fw-habitat-aep", "ABCWD_Shiny", size = "xxlarge")
```

Raster maintainance
===================

The app as-is is designed to need little to no maintanance however the rasters underlying the predictions may become out of date (e.g. the prox+ raster may need to be updated with new cases), or simply users may become interested in adding more variables/rasters for consideration.

When developing the app I considered building in functionality to allow users to load their own rasters, roughly eliminating the need for any raster maintanance at all; however, I ultimately decided that this was unadvisable within the Shiny web app framework. This decision was primarily due to the fact that raster files can come in multiple file formats, projections, extents, and resolutions and are generally very large. All of these characteristics can require memory-expensive resampling procedures to ensure correct performance within the app. This makes it hard not only to load them quickly, but also difficult and very slow to ensure that any given user-loaded raster will play nicely with the other rasters already stored internal to the app. I determined that adding this custom-upload feature would result in a loss of consistent performance for users which I wanted to avoid to ensure the long term success of the app.

Fortunately, it is trivial for any maintainer to update these rasters on the server side, eliminating the need to handle loadling, storage, or resampling on the backend. Below are instructions for easy updating of the app's internal rasters.

Adding new rasters:
-------------------

I have left comments in both the `server.r` and `ui.r` files to indicate the 3-5 line changes that are necessary to include a new raster. Additionally, below I provide step by step instructions for updating or adding a new raster.

For reference, all rasters in the app must have matching extent, projection, and resolution. The app's rasters currently follow the following specifications:

-   extent: `c(272584.6, 5427085, 572784.6, 6093785) # xmin, ymin, xmax, ymax coordinates`
-   resolution: `c(100, 100) # 100m by 100m resolution`
-   projection: [EPSG:26912](https://epsg.io/26912) i.e. NAD83 / UTM zone 12N

### Steps:

1.  Identify the raster/variable you want to add.

2.  Ensure that the raster file has the projection, extent, and resolution of raster files already in use by the app. To ensure that your rasters stack properly with existing rasters, I encourage you to use the following code in R for a quick resampling procedure before loading the raster into the `data/rasters/` directory and testing/redeploying the app:

    ``` r
    # install.packages("gdalUtils")
    library(gdalUtils)

    gdalwarp(
      srcfile = "Your_Raster_File_to_Resample",
      dstfile = "data/rasters/Your_New_Raster_File.tif",
      # desired projection
      t_srs = "EPSG:26912",
      # desired resolution
      tr = c(100, 100),
      # desired extent, xmin, ymin, xmax, ymax coordinates
      te = c(272584.6, 5427085, 572784.6, 6093785),
      output_Raster = T, overwrite = T, verbose = T
    )

    # if the script above is still not leading to compatible raster files, consider the function at
    # https://github.com/csaybar/Dorado/blob/8f4d25c0ff03c1431c916934e3e697b34d2b9550/R/resample_R.R
    # it will you allow you to feed in both the raster you hope to resample and
    # an original raster from within the rasters folder for a perfect match!

    # if the resulting raster is still very large...
    # you might consider looking into compression with gdal_translate(..., co = "COMPRESS=LZW")
    ```

3.  Save the resampled raster file in the `data/rasters/` folder. If you simply want to update an existing variable, overwrite the original file in the rasters directory, test, and redeploy!
    -   **Note:** If you want to update an existing variable but prefer to give the raster file a new name continue on through step 6.
    -   **2nd Note:** the necessary directory is not included on Github (it's git-ignored) but, as a maintaner and for proper deployment, you will need to have the complete `data` directory (containing the WMUs shapefiles and the `rasters` directory with all original/necessary rasters) in the main directory of you local version of this repo. It's inside this `rasters` folder that you need to add the new raster. If you think you should have a copy of this data directory and you don't please contact Anne Hubbs.
4.  Pick a variable name that you will use to identify this raster/variable throughout the app. Ideally it should be short but intuitive and not contain any special characters or spaces.

5.  Now in `server.R`, add the variable/raster name to [lines 316-321](https://github.com/dpseidel/ABCWD_Shiny/blob/64843a0516682ec65a04ccd0b4757d8706fa0dc6/server.R#L316-L321). **Important**: Rasters are read in alphabetical order from the rasters folder according to file name, so be very careful to ensure the names vector you are changing here matches the order in which the rasters are ultimately read in - ensuring all rasters get their correct name and future predictions maps are made with the correct variables.

6.  If your changes in step 5 change the order placement of the +prox raster (i.e. make it anything other than the 11th raster read in) update [line 435](https://github.com/dpseidel/ABCWD_Shiny/blob/64843a0516682ec65a04ccd0b4757d8706fa0dc6/server.R#L435).

7.  Now in `ui.R`, add the variable name to the appropriate category of the named vector `variable_list` at [lines 11-17](https://github.com/dpseidel/ABCWD_Shiny/blob/64843a0516682ec65a04ccd0b4757d8706fa0dc6/ui.R#L11-L17).

8.  (Optional but recommended) Add the variable name to the appropriate input category in [lines 79-120](https://github.com/dpseidel/ABCWD_Shiny/blob/64843a0516682ec65a04ccd0b4757d8706fa0dc6/ui.R#L79-L120) of `ui.R`, mirroring the options formatting of the other variables: `` `Title` = "variable_name" ``.

9.  Test extraction and map tabs locally

10. Re-Deploy!

### Calculating a new proximity raster:

The raster most likely to need updating is the prox+ raster (named `e_min.tif` in the rasters folder). This raster can be calculated most easily in ArcGIS using the Euclidean distance tool and a shapefile with all CWD+ cases in space. Alternatively, I recommend creating this in R using the `sf`, `fasterize`, and `raster` packages or in python using the `scipy.spatial` library. I'm happy to provide code or further recommendations as needed.

Final thoughts of encouragement
-------------------------------

Building this app taught me tons about shiny and R and efficiency and more, and even though I may not always be the direct maintainer of the app I hope I have built something useful to people and not too daunting for others to maintain. My hope in making the source code public and leaving these tips is that you, future maintaner, are inspired to mess around and learn too. The beauty of version control is that you can't break anything irrevocably. Good luck and thank you for continuing to care about this project!

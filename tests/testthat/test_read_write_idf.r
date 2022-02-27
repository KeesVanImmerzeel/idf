#library(idf)

context("Reading and writing raster files")

f <- system.file("extdata", "test.idf", package = "idf")
r <- read_raster(f)
ft <- system.file("extdata", "test.tif", package = "idf")
rt <- read_raster(ft, EPSG = "EPSG:28992") # Force crs

fn <- "test_out.idf" # In folder tests\testthat
if (file.exists(fn))
      file.remove(fn)
fnt <- "test_out.tif" # In folder in folder tests\testthat
if (file.exists(fnt))
      file.remove(fnt)

test_that("Test that idf and tif files result in identical objects after reading.",
          {
                expect_equal( identical(terra::crs(r), terra::crs(rt)), TRUE)
          })

test_that("Test that .write.idf and write_raster indeed writes idf- or tif files.",
          {
                expect_silent(write_raster(r, fn, overwrite = TRUE))
                expect_error(
                      write_raster(r, fn),
                      "Error in .write.idf: file exists; use overwrite=TRUE"
                )
          })
if (file.exists(fn))
      file.remove(fn)
if (file.exists(fnt))
      file.remove(fnt)

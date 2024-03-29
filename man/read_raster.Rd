% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_write_idf.r
\name{read_raster}
\alias{read_raster}
\title{Methods to create a terra::SpatRaster object with the ability to read idf files.}
\usage{
read_raster(x, EPSG = "EPSG:28992", e = NULL, funstr = NULL, ...)
}
\arguments{
\item{x}{filename (character)}

\item{EPSG}{coordinate reference system like "EPSG:4326" (character)}

\item{e}{Terra extent object}

\item{funstr}{Optional function to modify raster values. Denote the raster with 'x' (or character between square brackets) (character)}

\item{...}{Additional arguments as for 'terra::rast' function. For an idf-raster, only the optional 'EPSG' argument is used.}
}
\value{
terra::SpatRaster
}
\description{
The function 'read_raster' is able to create a terra::SpatRaster object from an
idf-file if an idf-filename is specified (exctension '.idf', like in 'foo.idf').
Otherwhise the function equals the function 'terra::rast()'.
}
\examples{
f <- system.file("extdata", "test.idf", package="idf")
r <- read_raster(f)

f <- system.file("extdata", "test.tif", package="idf")
r <- read_raster(f)

f <- system.file("extdata", "heads.tif", package="idf")
r <- read_raster(f)
names(r)

f1 <- system.file("extdata", "HEAD_20080401_l1.idf", package="idf")
f2 <- system.file("extdata", "HEAD_20080501_l1.idf", package="idf")
f <- c(f1, f2)
r <- read_raster(f)

r <- read_raster(f, funstr="x*100")
}

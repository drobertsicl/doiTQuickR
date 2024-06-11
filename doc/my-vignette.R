## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(imager)
library(ggplot2)
library(fs)
library(MSnbase)
library(doiTQuickR)

## -----------------------------------------------------------------------------
system2(command = "where", args = paste0("/r ", normalizePath("C:/Users"), " msconvert.exe"))

## -----------------------------------------------------------------------------
msconexe <- "C:/Users/eatmo/AppData/Local/Apps/ProteoWizard 3.0.24094.d2966db 64-bit/msconvert.exe"

## -----------------------------------------------------------------------------
convertR("C:/TQ data/IMG_Febr15-NEG-lipids-DCIS-!B-105-r2.raw/", outpath = "C:/TQ data", msconvert = msconexe)

## -----------------------------------------------------------------------------
data <- MSnbase::readSRMData("C:/TQ data/IMG_Febr15-NEG-lipids-DCIS-!B-105-r2.mzML")

## -----------------------------------------------------------------------------
datamatrix <- getData(data)

## -----------------------------------------------------------------------------
imgDims <- estDims("C:/TQ data/IMG_Febr15-NEG-lipids-DCIS-!B-105-r2.raw/")

## -----------------------------------------------------------------------------
dimslist <- estDims2(nrow(datamatrix))

## -----------------------------------------------------------------------------
plotlist <- plotDims(dimslist, datamatrix)

## -----------------------------------------------------------------------------
plotlist[[6]]
plotlist[[7]]

## -----------------------------------------------------------------------------
imgDims <- c(290,457)

## -----------------------------------------------------------------------------
# this is commented out so that I can actually build my vignette, but if you really want to see an error you can run it yourself
#xydims <- getPos("C:/TQ data/IMG_Febr15-NEG-lipids-DCIS-!B-105-r2.raw/")

## -----------------------------------------------------------------------------
test <- ionPlot(datamatrix, imgDims, log = TRUE, compound = "tic", title = "Look at this fake TIC image")
print(test)

## -----------------------------------------------------------------------------
test <- ionPlot(datamatrix, imgDims, log = TRUE, compound = 7, title = "This is the 7th ion. What is the 7th ion? I don't know")
print(test)

## -----------------------------------------------------------------------------
# first we'll get the precursor and product m/z values
details <- getDaughters(data)
# then we'll get the names
ion_names <- getNames("C:/TQ data/IMG_Febr15-NEG-lipids-DCIS-!B-105-r2.raw/")
print(ion_names)

## -----------------------------------------------------------------------------
ion_names[1] <- "pantothenic acid"
# then let's stick it onto our details dataframe
details <- cbind(details, ion_names)

## -----------------------------------------------------------------------------
# these are also commented out because it's an interactive input and will just freeze on building
#roi_bg <- roiSelect(datamatrix, imgDims, log = TRUE)

# so we'll do it the insanely stupid way and load in a csv of the actual pixel index values we got

roi_bg <- read.csv("C:/TQ data/roi_bg.txt")
roi_bg <- as.numeric(roi_bg[,2])

## -----------------------------------------------------------------------------
# as above, run the commented code not the uncommented code
#roi_tissue <- roiSelect(datamatrix, imgDims, log = TRUE)

roi_tissue <- read.csv("C:/TQ data/roi_tissue.txt")
roi_tissue <- as.numeric(roi_tissue[,2])

## -----------------------------------------------------------------------------
# let's make a data frame for a box plot, using the 7th ion
boxplot_df <- data.frame(intensity = c(datamatrix[roi_bg,7], datamatrix[roi_tissue,7]), type = c(rep("Background", length(roi_bg)), rep("Tissue", length(roi_tissue))))

nolog <- ggplot(boxplot_df,
aes(x = type,
y = intensity, fill = type)) +
geom_boxplot(width = 0.5, outlier.shape = NA) + geom_jitter(size = 0.5, alpha = 0.2)

logtrans <- ggplot(boxplot_df,
aes(x = type,
y = log(intensity+1), fill = type)) +
geom_boxplot(width = 0.5, outlier.shape = NA) + geom_jitter(size = 0.5, alpha = 0.2)

print(nolog)
print(logtrans)


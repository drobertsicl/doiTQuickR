
#' Get region of interest using point selection
#'
#' @param image Data frame of your imaging data, scans as rows
#' @param dims x,y dimension pair of your pixel dimensions
#' @param log Whether to log transform your image, defaults to FALSE
#' @param flipy Whether to mirror the y axis, defaults to FALSE
#' @param invert Whether to invert pixel values, defaults to false
#' @param compound Which column to visualise for ROI selection, defaults to "tic" (sum of all)
#'
#' @return Numeric vector of scan numbers in ROI
#' @export
#'
#' @examples
roiSelect <- function(image, dims, log = FALSE, flipy = FALSE, invert = FALSE, compound = "tic") {
  cimgplot <- matrix(data = NA, nrow = dims[1], ncol = dims[2])
  
  if(compound == "tic"){
    cimgplot <- sapply(1:prod(dims), function(i) {
      sum(image[i,])})
    cimgplot <- imager::as.cimg(cimgplot, x = dims[1], y = dims[2])
    if(flipy == TRUE){
      cimgplot <- mirror(cimgplot, axis = "y")
    }
  } else{
    cimgplot <- sapply(1:prod(dims), function(i) {
      image[i,compound]})
    cimgplot <- imager::as.cimg(cimgplot, x = dims[1], y = dims[2])
    if(flipy == TRUE){
      cimgplot <- mirror(cimgplot, axis = "y")
    }
  }
  
  
  
  X11()
  plot.new()
  
  if(invert == TRUE){
    cimgplot <- cimgplot*-1
  }
  
  # standard
  plot(cimgplot)
  # log tic
  if(log == TRUE){
    plot(log(cimgplot + 1))
  }
  
  selectedPoints <- gatepoints::fhs(as.data.frame(cimgplot), mark = TRUE)
  return(as.numeric(selectedPoints))
}

#' Plot ion images
#'
#' @param image Data frame of your imaging data, scans as rows
#' @param dims x,y dimension pair of your pixel dimensions
#' @param log Whether to log transform your image, defaults to FALSE
#' @param flipy Whether to mirror the y axis, defaults to FALSE
#' @param compound Which ion to plot (either numeric by order, or "tic" for sum of all)
#' @param pal Which viridis palette to use, defaults to A (magma)
#' @param title Title label to add to plot
#' @param type Defaults to relative, using x,y grid coordinates derived from dimensions, set to "abs" for instrument reported values
#' @param xvals Vector of absolute x coordinate values
#' @param yvals Vector of absolute y coordinate values
#' @param addpix Whether to duplicate the final xvals and yvals values to account for a "missing" pixel, defaults to TRUE
#'
#' @return
#' @export
#'
#' @examples data <- MSnbase::readSRMData("path/to/file.mzML")
#' datamatrix <- getData(data)
#' dimlist <- estDims2(260432)
#' plotlist <- plotDims(input_data = datamatrix, dims_list = dimlist)
#' plotlist <- plotDims(input_data = datamatrix, dims_list = dimlist)
#' xydims <- getPos(filename = "path/to/file.raw/")
#' plot <- ionPlot(datamatrix, numScans, log = TRUE, compound = "tic", title = "test", xvals = xydims[,1], yvals = xydims[,2])
#' plot
ionPlot <- function(image, dims, log = FALSE, flipy = FALSE, compound = NULL, pal = "A", title = NULL, type = "rel", xvals = NULL, yvals = NULL, addpix = TRUE) {
  cimgplot <- matrix(data = NA, nrow = dims[1], ncol = dims[2])
  
  if(compound == "tic"){
    cimgplot <- sapply(1:prod(dims), function(i) {
      sum(image[i,])})
    cimgplot <- imager::as.cimg(cimgplot, x = dims[1], y = dims[2])
    if(flipy == TRUE){
      cimgplot <- mirror(cimgplot, axis = "y")
    }
  } else{
    cimgplot <- sapply(1:prod(dims), function(i) {
      image[i,compound]})
    cimgplot <- imager::as.cimg(cimgplot, x = dims[1], y = dims[2])
    if(flipy == TRUE){
      cimgplot <- mirror(cimgplot, axis = "y")
    }
  }

  # standard
  if(log == FALSE){
    if(type == "abs"){
      cimgplot <- as.data.frame(cimgplot)
      if(addpix != TRUE){
        cimgplot$x <- xvals
        cimgplot$y <- yvals
      }else{
        cimgplot$x <- c(xvals, xvals[length(xvals)])
        cimgplot$y <- c(yvals, yvals[length(yvals)])
      }
    }
    p <- ggplot(data = as.data.frame(cimgplot)) +
      geom_raster(aes(x = x, y = rev(y), fill = (value))) +
      scale_fill_viridis_c(option = pal, name = "Intensity (A.U)") + coord_fixed() +
      scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),
                                                           trans=scales::reverse_trans()) + xlab(NULL) + ylab(NULL) +
      ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }
  # log
  if(log == TRUE){
    if(type == "abs"){
      cimgplot <- as.data.frame(cimgplot)
      if(addpix != TRUE){
        cimgplot$x <- xvals
        cimgplot$y <- yvals
      }else{
        cimgplot$x <- c(xvals, xvals[length(xvals)])
        cimgplot$y <- c(yvals, yvals[length(yvals)])
      }
    }
    p <- ggplot(data = as.data.frame(cimgplot)) +
      geom_raster(aes(x = x, y = rev(y), fill = log(value+1))) +
      scale_fill_viridis_c(option = pal, name = "Intensity (A.U)") + coord_fixed() +
      scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0),
                                                           trans=scales::reverse_trans()) + xlab(NULL) + ylab(NULL) +
      ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  }
}
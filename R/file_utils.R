#' Convert TQ .raw to .mzml
#'
#' @param filename Path to the desired .raw folder
#' @param outpath Path to export the .mzml file to
#' @param msconvert Path to your proteowizard msconvert.exe
#' @param args Additional command line arguments, fille by default
#'
#' @return Returns nothing in the session, .mzml will be exported to given folder
#' @export
#'
#' @examples 
#' msconexe <- "path/to/msconvert.exe"
#' convertR(filename = "C:/some_tq_data.raw", outpath = "C:/my_tq_data", msconvert = msconexe)
convertR <- function(
    filename = NULL,
    outpath = NULL,
    msconvert = NULL,
    args = c("--mzML",
             "--64",
             "--zlib",
             "--chromatogramFilter \"index [1,100]\""
    )){
  
  # strip trailing / if it exists
  
  if(substr(filename, nchar(filename), nchar(filename)) == "/"){
    filename <- substr(filename, 1, (nchar(filename)-1))
  }
  
  # if no output folder provided, set to default wd
  if(is.null(outpath) == TRUE){
    args <- c(args, "-o ", shQuote(getwd(), type = "cmd"))
  } else {
    args <- c(args, "-o ", shQuote(outpath, type = "cmd"))
  }
  tempout <- NULL
  system2(command = msconvert, args = c(shQuote(filename, type = "cmd"), args))
}

#' Get a data matrix from SRM data
#'
#' @param srm_in Input data from an MSNbase SRM object
#' @param addpix Whether to make up for a "missing pixel" or not, defaults to TRUE
#'
#' @return Matrix of TQ data, scans in rows, ions in columns
#' @export
#'
#' @examples 
#' data <- MSnbase::readSRMData("C:/my_tq_data/my_file.mzML")
#' datamatrix <- getData(data)
getData <- function(srm_in, addpix = TRUE){
  datamatrix <- sapply(1:length(srm_in), function(i) {
    (srm_in[i]@intensity)
  })
  
  if(addpix == TRUE){
    datamatrix <- rbind(datamatrix, datamatrix[(nrow(datamatrix)-1),])
  }
  datamatrix <- datamatrix[,srm_in@featureData@data$chromatogramIndex]
  return(datamatrix)
}

#' Get details on parent/daughter ion values
#'
#' @param srm_in Input data from an MSNbase SRM object
#'
#' @return Data frame of precursor and product m/z values
#' @export
#'
#' @examples
#' data <- MSnbase::readSRMData("C:/my_tq_data/my_file.mzML")
#' temp_df <- getDaughters(data)
getDaughters <- function(srm_in){
  precursor_mz <- srm_in@featureData@data$precursorIsolationWindowTargetMZ
  product_mz <- srm_in@featureData@data$productIsolationWindowTargetMZ
  tempdf <- data.frame(precursor_mz = precursor_mz, product_mz = product_mz)
  return(tempdf)
}

#' Get names of ion channels from acquisition file
#'
#' @param filename Path to the desired .raw folder
#'
#' @return Array of strings of >2 characters in length
#' @export
#'
#' @examples
#' compund_names <- getNames("C:/some_tq_data.raw")
getNames <- function(filename = NULL){
  
  # if folder is provided, set filename to _FUNC001.CMP within that folder
  if(fs::is_dir(filename) == TRUE){
    
    # add trailing slash if not there
    
    if(substr(filename, nchar(filename), nchar(filename)) != "/"){
      filename <- paste0(filename, "/")
    }
    
    filename <- paste0(filename, "_FUNC001.CMP")
  }
  
  system2(command = "strings", args = c(shQuote(filename), " -n 2"), stdout = TRUE)
}
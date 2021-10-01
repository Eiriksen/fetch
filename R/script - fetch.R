#' function for simplified reading of files
#'
#' Tries to automaticaly handle .txt, .csv, .xls, .xlsx and google sheet links
#'
#' @param file Filename or URL for google sheet
#' @param type File type. Usually not necessary since this is checked automatically.
#' @param skip How many rows to skip in the file
#' @param range Only for google sheets
#' @param dec decimal separator
#' @param sep value separator
#' @param sheet for google sheets and excel sheets. Specify which sheet in the file to read.
#' @param col_types for google sheets. Specify variable types for columns. See documentation for read_sheet() in googlesheets4
#' @export
read.data <- function(file, type, skip=0, range=NULL, dec=".", sep, sheet=NULL, col_types=NULL){

  file <- glue::glue(file)
  filename <- glue::glue(file)

  # If type is not specified, check if the filename is a google sheets file
  if(missing(type) & grepl("docs.google.com", filename, fixed=T))
  {
    type <- "googlesheet"
  }
  # - if not a google sheets, check filetype from filename
  if (missing(type)){
    type <- tools::file_ext(filename)
  }


  if (type == "xlsx" | type == "xls")
  {
    df = readxl::read_excel(filename, skip=skip, sheet=sheet)
  }
  else if (type == "csv")
  {
    if(missing(sep)) sep=","
    df = read.csv(filename, head=T, sep=sep, dec=dec, skip=skip)
  }
  else if (type == "googlesheet")
  {
    df = googlesheets4::read_sheet(file, skip=skip, range=range, sheet=sheet, col_types=col_types)
  }
  else if (type == "txt")
  {
    if(missing(sep)) sep="\t"
    df = read.table(file=file, skip=skip, sep=sep, dec=dec, header=T)
  }
  else
  {
    stop("Filetype not specified or compatible")
    break
  }
  return(df)
}


#' Function for automatic file saving
#'
#' Automatically saves save .txt, .csv, .xls, .xlsx
#'
#' @export
save.data = function(df, filename){
  require(openxlsx)
  require(glue)
  require(tools)

  filename=glue::glue(filename)

  type = tools::file_ext(filename)
  if (type=="")
  {
    type="csv"
    filename=glue("{filename}.csv")
  }
  else if (type == "xlsx" || type == "xls")
  {
    readxl::write.xlsx(df, filename)
  }
  else if (type == "csv")
  {
    write.table(df, file=filename, sep=",", row.names=F, quote=F)
  }
  else if (type == "txt")
  {
    write.table(df, file=filename, sep="\t", row.names=F, quote=F)
  }
  else
  {
  stop("Filetype not supported by save.data() yet")
  }

}

#' Synonym function of read.data
#' @export
fetch <- function(file, type, skip=0, range=NULL, dec=".", sep, sheet=NULL, col_types=NULL, pattern_filename, return=T){

  if (!missing(pattern_filename)){
    if(missing(file)) file="."
    list_filenames <- list.files(path=file,pattern=pattern_filename, recursive = F)
    list_filepaths <- paste(file,"/",list_filenames,sep="")
    list_files <- list()

    for(filepath in list_filepaths){
      message(glue::glue("Reading {filepath}"))
      filename <- tools::file_path_sans_ext(basename(filepath))
      if (return==T){
        list_files[[filename]] <- read.data(filepath,type,skip,range,dec,sep,sheet,col_types)
      }
      else {
        assign_global(filename, read.data(filepath,type,skip,range,dec,sep,sheet,col_types))
      }
    }

    if(return==T) return(list_files)
  }
  else{
    read.data(file,type,skip,range,dec,sep,sheet,col_types)
  }
}

#https://stackoverflow.com/questions/53178659/r-create-a-global-variable-with-the-name-of-a-functions-input
assign_global <- function(name, value) {
  assign(name, value, envir = .GlobalEnv)
}

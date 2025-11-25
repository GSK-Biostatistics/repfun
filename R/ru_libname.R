#' Automate retrieval of data sets/frames in specified folder.
#'
#' Provide a folder containing data sets/frames and have a list of function calls returned that can be used to quickly access individual data sets/frames.
#'
#' @param datapath Location of reporting data sets.
#'
#' @return List of function calls for use in quickly accessing individual data sets/frames.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' adamdata <- repfun::ru_libname(datdir)
#' adamdata$adae.rda() %>% dplyr::filter(TRTEMFL=='Y') -> teae
#' print(head(teae[,c(1:10)]),10)
#'
#' @import haven
#' @export
#'
ru_libname <- function(datapath) {
  #print(paste0("RU_LIBNAME: ", "Start or RU_LIBNAME"))
  data_list <- list()
  dataname_list <- list()
  datasets <- list.files(path=datapath, pattern="\\.sas7bdat$")
  if (length(datasets) > 0) for (j in 1:length(datasets)) {
    datasetname <- stringr::str_replace(datasets[j], "[.]sas7bdat", "")
    fullname <- paste(datapath, datasets[j], sep="/")
    statement <- paste0(datasetname, "_func <- function(x=", base::toString(shQuote(fullname)), ") { haven::read_sas(x) }" )
    eval(parse(text=statement))
    data_list[j] <- list(base::get(paste0(datasetname, "_func")))
    dataname_list[j] <- datasetname
  }
  this_inc <- length(datasets)
  datasets <- list.files(path=datapath, pattern="\\.rda$")
  if (length(datasets) > 0) for (j in 1:length(datasets)) {
    datasetname <- stringr::str_replace(datasets[j], "[.]rda", "")
    fullname <- paste(datapath, datasets[j], sep="/")
    statement <- paste0(datasetname, "_rda_func <- function(x=", base::toString(shQuote(fullname)), ") { base::get(base::load(x)) }" )
    eval(parse(text=statement))
    data_list[j + this_inc] <- list(base::get(paste0(datasetname, "_rda_func")))
    dataname_list[j + this_inc] <- paste0(datasetname, ".rda")
  }
  this_inc <- this_inc + length(datasets)
  datasets <- list.files(path=datapath, pattern="\\.rds$")
  if (length(datasets) > 0) for (j in 1:length(datasets)) {
    datasetname <- stringr::str_replace(datasets[j], "[.]rds", "")
    fullname <- paste(datapath, datasets[j], sep="/")
    statement <- paste0(datasetname, "_rds_func <- function(x=", base::toString(shQuote(fullname)), ") { base::readRDS(x) }" )
    eval(parse(text=statement))
    data_list[j + this_inc] <- list(base::get(paste0(datasetname, "_rds_func")))
    dataname_list[j + this_inc] <- paste0(datasetname, ".rds")
  }
  this_inc <- this_inc + length(datasets)
  datasets <- list.files(path=datapath, pattern="\\.json$")
  if (length(datasets) > 0) for (j in 1:length(datasets)) {
    datasetname <- stringr::str_replace(datasets[j], "[.]json", "")
    fullname <- paste(datapath, datasets[j], sep="/")
    statement <- paste0(datasetname, "_json_func <- function(x=", base::toString(shQuote(fullname)), ") { jsonlite::fromJSON(x) }" )
    eval(parse(text=statement))
    data_list[j + this_inc] <- list(base::get(paste0(datasetname, "_json_func")))
    dataname_list[j + this_inc] <- paste0(datasetname, ".json")
  }
  this_inc <- this_inc + length(datasets)
  datasets <- list.files(path=datapath, pattern="\\.parquet$")
  if (length(datasets) > 0) for (j in 1:length(datasets)) {
    datasetname <- stringr::str_replace(datasets[j], "[.]parquet", "")
    fullname <- paste(datapath, datasets[j], sep="/")
    statement <- paste0(datasetname, "_parquet_func <- function(x=", base::toString(shQuote(fullname)), ") { arrow::read_parquet(x) }" )
    eval(parse(text=statement))
    data_list[j + this_inc] <- list(base::get(paste0(datasetname, "_parquet_func")))
    dataname_list[j + this_inc] <- paste0(datasetname, ".parquet")
  }
  this_inc <- this_inc + length(datasets)
  datasets <- list.files(path=datapath, pattern="\\.xpt$")
  if (length(datasets) > 0) for (j in 1:length(datasets)) {
    datasetname <- stringr::str_replace(datasets[j], "[.]xpt", "")
    fullname <- paste(datapath, datasets[j], sep="/")
    statement <- paste0(datasetname, "_xpt_func <- function(x=", base::toString(shQuote(fullname)), ") { haven::read_xpt(x) }" )
    eval(parse(text=statement))
    data_list[j + this_inc] <- list(base::get(paste0(datasetname, "_xpt_func")))
    dataname_list[j + this_inc] <- paste0(datasetname, ".xpt")
  }
  this_inc <- this_inc + length(datasets)

  write2xpt <- function(dsetin, name=NULL) {
    if (is.null(name)) name <- as.character(base::substitute(dsetin))
    this_list_name <- unlist(base::strsplit(deparse(sys.call(0)), "\\$"))[1]
    this_name <- paste0(name, ".xpt")
    this_full_name <- paste0(datapath, "/", this_name)
    if (requireNamespace("xportr", quietly = TRUE)) {
      xportr::xportr_write(dsetin, this_full_name)
    } else {
      haven::write_xpt(dsetin, this_full_name)
    }
    this_list <- get(this_list_name)
    if (! (this_name%in% names(this_list))){
      this_statement <- paste0(this_list_name, "[[", shQuote(this_name), "]] <<- function(x=",
                               base::toString(shQuote(this_full_name)), ") { haven::read_xpt(x) }")
      base::eval(base::parse(text=this_statement))
    }
  }
  data_list[this_inc + 1] <- list(base::get("write2xpt"))
  dataname_list[this_inc + 1] <- "write2xpt"

  write2rds <- function(dsetin, name=NULL) {
    if (is.null(name)) name <- as.character(base::substitute(dsetin))
    this_list_name <- unlist(base::strsplit(deparse(sys.call(0)), "\\$"))[1]
    this_name <- paste0(name, ".rds")
    this_full_name <- paste0(datapath, "/", this_name)
    base::saveRDS(dsetin, file = this_full_name, compress = TRUE)
    this_list <- get(this_list_name)
    if (! (this_name%in% names(this_list))){
      this_statement <- paste0(this_list_name, "[[", shQuote(this_name), "]] <<- function(x=",
                               base::toString(shQuote(this_full_name)), ") { base::readRDS(x) }")
      base::eval(base::parse(text=this_statement))
    }
  }
  data_list[this_inc + 2] <- list(base::get("write2rds"))
  dataname_list[this_inc + 2] <- "write2rds"
  write2rda <- function(dsetin, name=NULL) {
    if (is.null(name)) name <- as.character(base::substitute(dsetin))
    this_list_name <- unlist(base::strsplit(deparse(sys.call(0)), "\\$"))[1]
    this_name <- paste0(name, ".rda")
    this_full_name <- paste0(datapath, "/", this_name)
    base::save(dsetin, file = this_full_name, compress = "bzip2")
    this_list <- get(this_list_name)
    if (! (this_name%in% names(this_list))){
      this_statement <- paste0(this_list_name, "[[", shQuote(this_name), "]] <<- function(x=",
                               base::toString(shQuote(this_full_name)), ") { base::load(x) }")
      base::eval(base::parse(text=this_statement))
    }
  }
  data_list[this_inc + 3] <- list(base::get("write2rda"))
  dataname_list[this_inc + 3] <- "write2rda"
  write2json <- function(dsetin, name=NULL) {
    if (is.null(name)) name <- as.character(base::substitute(dsetin))
    this_list_name <- unlist(base::strsplit(deparse(sys.call(0)), "\\$"))[1]
    this_name <- paste0(name, ".json")
    this_full_name <- paste0(datapath, "/", this_name)
    jsonlite::write_json(dsetin, this_full_name, pretty = TRUE)
    this_list <- get(this_list_name)
    if (! (this_name%in% names(this_list))){
      this_statement <- paste0(this_list_name, "[[", shQuote(this_name), "]] <<- function(x=",
                               base::toString(shQuote(this_full_name)), ") { jsonlite::fromJSON(x) }")
      base::eval(base::parse(text=this_statement))
    }
  }
  data_list[this_inc + 4] <- list(base::get("write2json"))
  dataname_list[this_inc + 4] <- "write2json"
  write2parquet <- function(dsetin, name=NULL) {
    if (is.null(name)) name <- as.character(base::substitute(dsetin))
    this_list_name <- unlist(base::strsplit(deparse(sys.call(0)), "\\$"))[1]
    this_name <- paste0(name, ".parquet")
    this_full_name <- paste0(datapath, "/", this_name)
    arrow::write_parquet(dsetin, this_full_name)
    this_list <- get(this_list_name)
    if (! (this_name%in% names(this_list))){
      this_statement <- paste0(this_list_name, "[[", shQuote(this_name), "]] <<- function(x=",
                               base::toString(shQuote(this_full_name)), ") { arrow::read_parquet(x) }")
      base::eval(base::parse(text=this_statement))
    }
  }
  data_list[this_inc + 5] <- list(base::get("write2parquet"))
  dataname_list[this_inc + 5] <- "write2parquet"

  deletedata <- function(datanames, types=c("rds", "xpt", "sas7bdat", "rda", "json", "parquet")) {
    this_list_name <- unlist(base::strsplit(deparse(sys.call(0)), "\\$"))[1]
    this_list <- get(this_list_name)
    for (i in 1:length(datanames)) {
      for (j in 1:length(types)) {
        this_name <- paste0(datanames[i], ".", types[j])
        this_full_name <- paste0(datapath, "/", this_name)

        if (types[j] == "sas7bdat") this_name <- datanames[i]
        if ((this_name %in% names(this_list))){
          if (file.exists(this_full_name)) {
            file.remove(this_full_name)
          }
          this_statement <- paste0(this_list_name, "[[", shQuote(this_name), "]] <<- NULL")
          base::eval(base::parse(text=this_statement))
        }
      }
    }
  }
  data_list[this_inc + 6] <- list(base::get("deletedata"))
  dataname_list[this_inc + 6] <- "delete"

  names(data_list) <- unlist(dataname_list)
  data_list
}

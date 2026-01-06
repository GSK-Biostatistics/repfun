#' Assign Big N to Data Frame.
#'
#' Merge input data with population data to keep only subjects which are in population sub data,
#'
#' @param dsetin The data set that will be merged with the population data set.
#' @param dsetinpop The population data set.
#' @param subjidvars Variable(s) that define a unique subject.
#' @param subpop A sub-population expression where variables are on DSETINPOP.
#' @param pop The population expression (SAFFL=='Y').
#' @param keeppopvars Variables to keep on the population data set.
#'
#' @return A data frame based on the incoming data frame but restricted to the population of interest with relevant population variables added.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' outdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"outdir")
#' dir.create(outdir,showWarnings=FALSE)
#' fmtdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"fmtdir")
#' dir.create(fmtdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POP="SAFFL",
#'                  D_POPLBL="Safety",
#'                  D_POPDATA=repfun::adsl,
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'   dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                  ifelse(TRT01A=='Xanomeline Low Dose',2,3)),
#'          SAFFL=ifelse((row_number() %% 10) == 0,'N',SAFFL))
#' attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
#' attr(G_POPDATA$SAFFL,"label") <- 'Safety Population Flag'
#' adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL)
#' adae2 <- repfun::ru_getdata(adae, G_POPDATA, c("STUDYID", "USUBJID"),
#'                     keeppopvars=c("TRT01AN", "TRT01A"))
#'
#' @export
#'
ru_getdata <- function(dsetin,
                       dsetinpop=rfenv$G_POPDATA,
                       subjidvars=c("STUDYID", "USUBJID"),
                       subpop=rfenv$G_SUBPOP,
                       pop=rfenv$G_POP,
                       keeppopvars=rfenv$G_KEEPPOPVARS) {
  #print(paste0("RU_GETDATA: ", "Start or RU_GETDATA"))

  rfenv$an.error.occured <- FALSE
  tryCatch( { force(dsetinpop) }
            , error = function(e) {rfenv$an.error.occured <- TRUE})

  if (rfenv$an.error.occured) dsetinpop <- NULL

  rfenv$an.error.occured <- FALSE
  tryCatch( { force(pop)}
            , error = function(e) {rfenv$an.error.occured <- TRUE})

  if (rfenv$an.error.occured) pop <- NULL

  rfenv$an.error.occured <- FALSE
  tryCatch( { force(subpop)}
            , error = function(e) {rfenv$an.error.occured <- TRUE})

  if (rfenv$an.error.occured) subpop <- NULL

  rfenv$an.error.occured <- FALSE
  tryCatch( { force(keeppopvars)}
            , error = function(e) {rfenv$an.error.occured <- TRUE})

  if (rfenv$an.error.occured) keeppopvars <- NULL

  df_gdata <- dsetin
  df_pop_data <- dsetinpop

  if (!is.null(pop)) {
    # df_pop_data <- subset(df_pop_data, (df_pop_data[pop] == 1) || (df_pop_data[pop] == "Y"))
    # df_pop_data <- subset(df_pop_data, (df_pop_data[pop] == "Y"))
    df_pop_data <- df_pop_data %>% dplyr::filter(!! as.name(pop) == 'Y')
  }

  if (!is.null(subpop)) {
    # print(paste0("base::subset(df_pop_data,", substitute(subpop), ")"))
    df_pop_data <- eval(parse(text=paste0("base::subset(df_pop_data,", substitute(subpop), ")")))
  }

  if (! is.null(df_pop_data) && ! is.null(subjidvars) && subjidvars[1] != "") {
    var_pop_vars <- base::unique(c(unlist(subjidvars), unlist(keeppopvars)))
    var_pop_vars <- base::setdiff(var_pop_vars, base::names(df_gdata))
    var_pop_vars <- base::unique(c(unlist(subjidvars), unlist(var_pop_vars)))
    df_pop_data <- df_pop_data %>% dplyr::select(dplyr::all_of(var_pop_vars))
    df_gdata <- base::merge(df_gdata, df_pop_data, by=subjidvars, all.x=FALSE, all.y=FALSE)

    #df_gdata <- ru_labels(df_gdata, base::labels(dsetin))
    #df_gdata <- ru_labels(df_gdata, base::labels(df_pop_data))

    df_gdata <- ru_labels(df_gdata, lapply(dsetin,function(x){attr(x,"label")}))
    df_gdata <- ru_labels(df_gdata, lapply(df_pop_data,function(x){attr(x,"label")}))

  }
  return(df_gdata)
}

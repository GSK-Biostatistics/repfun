#' Stack Columns of a Dataframe into New Column
#'
#' Pass in a dataframe and columns to stack and have new dataframe returned that contains the stacked columns.
#'
#' @param dsetin Name of incoming dataframe with columns to have stacked.
#' @param sepc Separator character between the stacked columns.
#' @param splitc Split character between stacked columns.
#' @param varsin List of variables to be stacked.
#' @param varout Name of stacked column in dataframe.
#' @param varlabel Label for new stacked column.
#'
#' @return The incoming dataframe with columns stacked as requested.
#'
#' @author Chris Rook, \email{christopher.x.rook@gsk.com} \cr
#'         Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' rfenv <- if (exists('rfenv') && is.environment(get('rfenv'))){
#'              rfenv
#'          } else {
#'              rfenv <- new.env(parent = emptyenv())
#'              rfenv$G_DEBUG <- 0
#'              rfenv
#'          }
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- rfenv$G_POPDATA %>%
#'   dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                  ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'   repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
#' adae <- rfenv$adamdata$adae.rda() %>%
#'         dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
#' aesum_p <- repfun::ru_freq(adae,
#'                    dsetindenom=G_POPDATA,
#'                    countdistinctvars=c('STUDYID','USUBJID'),
#'                    groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
#'                    anyeventvars = c('AEBODSYS','AEDECOD'),
#'                    anyeventvalues = c('ANY EVENT','ANY EVENT'),
#'                    groupbyvarsdenom=c('TRT01AN'),
#'                    resultstyle="NUMERPCT",
#'                    totalforvar=c('TRT01AN'),
#'                    totalid=99,
#'                    totaldecode='Total',
#'                    codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'                    varcodelistpairs=c(""),
#'                    codelistnames=list(),
#'                    resultpctdps=0) %>%
#' repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
#'           groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
#'           acrossvar="TRT01AN", acrossvarlabel="TRT01A",
#'           acrossvarprefix=c("tt_ac", "tt_p")) %>%
#' dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%
#' dplyr::rename(ord2=tt_summarylevel) %>%
#' dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%
#' dplyr::select(-c(starts_with('tt_p'),starts_with('ord'))) %>%
#' repfun::ru_stackvar(varsin=c('AEBODSYS','AEDECOD'),varout='SYSPREF',
#'                     varlabel='Body System/Preferred Term')
#'
#' @import tidyr
#' @export
#'
ru_stackvar <- function(dsetin,
                        sepc         = '/',          # Separator character
                        splitc       = '\\line',     # Split character
                        varsin       = NULL,         # List of variables to be stacked
                        varout       = NULL,         # Name of stack variable
                        varlabel     = NULL          # Label of stack variable
) {

  ##===================================##
  ## Stack variables, create new ones. ##
  ##===================================##
  if (!is.null(sepc) && !is.null(splitc)) {sepsplit <- paste0(sepc,splitc)
  } else if (!is.null(sepc)) {sepsplit <- sepc
  } else if (!is.null(splitc)) {sepsplit <- splitc}

  ##===========================##
  ## Create stacked variable.  ##
  ##===========================##
  dsetin %>% tidyr::unite(!! varout, !!! rlang::syms(varsin) ,sep=sepsplit, remove=FALSE) -> tmpdf

  ##===============================##
  ## Update label on new variable. ##
  ##===============================##
  if (!is.null(varlabel)){
    attr(tmpdf[[varout]],'label') <- varlabel
  } else {
    curlbls <- lapply(tmpdf, attr, "label")
    attr(tmpdf[[varout]],'label') <- as.character(paste(curlbls[varsin],collapse=sepsplit))
  }

  return(tmpdf)
}

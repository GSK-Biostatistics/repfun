#' Add a Page Number Column to an Existing Dataframe
#'
#' Take incoming dataframe and add page number variable to it accounting for grouping variables, stacked variables, and no-split variables.
#'
#' @param dsetin The dataframe for which a paging variable will be added.
#' @param grpvars Grouping variables used in the output (used for nosplitvars).
#' @param stackvars Specify stacked grouping variables (reduces # of page lines available for data).
#' @param varlabls Apply labels to outgoing dataframe.
#' @param rowsprbdy Number of rows in the body of the report.
#' @param startpaging Set to zero on first call, and > 0 on recalls to fix widows.
#' @param lastbygrp Set to true if this page is processing the last value of the grouping variables (used when footnote is applied only to last page).
#' @param fpage Setting to 'last' indicates that footnotes are only displayed on the last page.
#' @param nftnotes Enter the number of footnotes (determines # of page lines available for data).
#' @param nosplitvars Setting to true requires all values of the last grouping/stackvar must be on the same page (if possible).
#' @param npgvars Number of page-by variables for this report (reduces # of page lines available for data).
#'
#' @return A dataframe based on the incoming dataframe but with a paging variable added.
#'
#' @author Chris Rook, \email{cr883296@gmail.com} \cr
#'         Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com}
#'
#' @examples
#' library(repfun)
#' library(dplyr)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir");
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
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'  dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                 ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'  repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 001 (n)'))
#' adae <- repfun:::rfenv$adamdata$adae.rda() %>%
#'         dplyr::inner_join(G_POPDATA, by=c('STUDYID','USUBJID','SAFFL','TRT01A'))
#' aesum_t <- repfun::ru_freq(
#'              adae,
#'              dsetindenom=G_POPDATA,
#'              countdistinctvars=c('STUDYID','USUBJID'),
#'              groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
#'              anyeventvars = c('AEBODSYS','AEDECOD'),
#'              anyeventvalues = c('ANY EVENT','ANY EVENT'),
#'              groupbyvarsdenom=c('TRT01AN'),
#'              resultstyle="NUMERPCT",
#'              totalforvar=c('TRT01AN'),
#'              totalid=99,
#'              totaldecode='Total',
#'              codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'              varcodelistpairs=c(""),
#'              codelistnames=list(),
#'              resultpctdps=0) %>%
#'  repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
#'            groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
#'            acrossvar="TRT01AN",
#'            acrossvarlabel="TRT01A",
#'            acrossvarprefix=c("tt_ac", "tt_p")) %>%
#'  dplyr::mutate(ord1=ifelse(tt_summarylevel==0,0,1)) %>%
#'  dplyr::rename(ord2=tt_summarylevel) %>%
#'  dplyr::arrange(ord1,AEBODSYS,ord2,AEDECOD) %>%
#'  dplyr::select(-c(starts_with('tt_p'),starts_with('ord')))
#'
#' aesum_p3 <- repfun::ru_addpage(aesum_t,grpvars=c('AEBODSYS'),rowsprbdy=35,
#'                     nosplitvars=TRUE)
#' print(head(aesum_p3,10))
#'
#' @export
#'
ru_addpage <- function(dsetin,
                       grpvars=NULL,
                       stackvars=NULL,
                       varlabls=NULL,
                       rowsprbdy=NULL,
                       startpaging=0,
                       lastbygrp=FALSE,
                       fpage='all',
                       nftnotes=0,
                       nosplitvars=FALSE,
                       npgvars=0){

  if (rfenv$G_DEBUG>0){message(paste0('rowsprbdy: ',rowsprbdy))}

  ##=============
  ## Initialize
  ##=============
  if (startpaging==0){
    rfenv$PAGING_ITERATIONS <- 0
    if (is.null(varlabls)){varlabls=lapply(dsetin,function(x){attr(x,"label")})}
  }

  if (rfenv$G_DEBUG>0){message(paste0('Iterating ... ',rfenv$PAGING_ITERATIONS))}

  ##==============================
  ## Count number of iterations.
  ##==============================
  if (rfenv$PAGING_ITERATIONS>200){
      if (rfenv$G_DEBUG>0) {print('***** MAXIMUM NUMBER OF PAGING ITERATIONS REACHED (200).  REQUEST DOES NOT FIT ON A PAGE.  HINT: REMOVE NOWIDOWVAR. *****')}
      return(dsetin)
  } else {
      rfenv$PAGING_ITERATIONS <- rfenv$PAGING_ITERATIONS + 1
      if (rfenv$PAGING_ITERATIONS>200){
        if (rfenv$G_DEBUG>0) {print('***** MAXIMUM NUMBER OF PAGING ITERATIONS REACHED (200).  REQUEST DOES NOT FIT ON A PAGE. HINT: NOWIDOWVAR HAS BEEN DISABLED. *****')}
        dsetin %>% select(-c(PAGEVAR,catid,catn,widow)) -> dsetin
        df <- repfun::ru_addpage(dsetin,
                         grpvars=grpvars,
                         stackvars=stackvars,
                         varlabls=varlabls,
                         rowsprbdy=rowsprbdy,
                         startpaging=0,
                         lastbygrp=lastbygrp,
                         fpage=fpage,
                         nftnotes=nftnotes,
                         nosplitvars=FALSE,
                         npgvars=npgvars)
        return(df)
      }
  }

  #=============================================================
  # Check if this is a first call or repeat call to fix widows.
  #=============================================================
  if (startpaging>0){
    dsetin %>% dplyr::filter((PAGEVAR < startpaging) | (PAGEVAR==startpaging & widow==0)) %>% select(-c('widow','catid','catn')) -> splitgood
    dsetin %>% dplyr::filter((PAGEVAR > startpaging) | (PAGEVAR==startpaging & widow==1)) %>% select(-c('PAGEVAR','widow','catid','catn')) -> dsetin
  }

  #===============================================================================================================================================
  # Find extra lines and incorporate into paging.
  # Extra line is generated whenever a return character exists in one of the variables.  Find the maximum number per line.  Add to it # vars for
  # the first stackvar level considering all but the last variable.  These will also hold return characters (inserted in next section).
  #===============================================================================================================================================

  #=======================================
  # Number of linefeeds in current value.
  #=======================================
  dsetin %>% dplyr::mutate(dplyr::across(dplyr::everything(),.fns=list(nlin=~1 + stringr::str_count(.,'\\n')),.names="NLINES_{col}")) -> dsetin
  dsetin['TOTLINES'] <- apply(dsetin[, grepl('NLINES_',names(dsetin))], 1, max)
  dsetin %>% dplyr::select(-dplyr::contains('NLINES_')) -> dsetin

  #===========================================================
  # Add number of linefeeds from stackvars (processed below).
  #===========================================================
  if (!is.null(stackvars)){
    catvars <- stackvars
    nstckvrsl1 <- length(catvars) - npgvars
  } else if (!is.null(grpvars)){
    catvars <- grpvars
    nstckvrsl1 <- 0
  }

  #===============================================================
  # Update TOTLINES on the first row when stackvars is requested.
  #===============================================================
  if (!is.null(catvars)){
    dsetin %>% dplyr::group_by(!!! rlang::syms(catvars)) %>% dplyr::mutate(LINEWTHINGRP=dplyr::row_number(),TOTLINES=ifelse(dplyr::row_number()==1,TOTLINES+nstckvrsl1,TOTLINES)) %>% dplyr::ungroup() -> dsetin
  }

  #============================================================================================================================================================
  # If a row starts on one page and ends on another we push to the next page (where it ends). But we cannot find these cases unless we page first, then check.
  # This is done below, page first using STARTLINE and ENDLINE (which are cumulative) then update TOTLINES and repage for a tighter fit. By updating TOTLINES
  # on this row we push the last line (plus) to the next page and avoid overfilling a single page.
  #============================================================================================================================================================
  dsetin %>%
    dplyr::mutate(ENDLINE=cumsum(TOTLINES), STARTLINE=ENDLINE-TOTLINES+1) %>%
    dplyr::mutate(PAGESTARTLINE=1 + floor(STARTLINE/(rowsprbdy+1)), PAGEENDLINE=1 + floor(ENDLINE/(rowsprbdy+1)),
           TOTLINES=ifelse(!(PAGESTARTLINE==PAGEENDLINE),2*TOTLINES-1,TOTLINES)) %>%
    dplyr::mutate(CUMTOTLINES=cumsum(TOTLINES),PAGEVAR=(startpaging) + 1 + floor(CUMTOTLINES/(rowsprbdy+(1/10**5)))) %>%
    dplyr::group_by(PAGEVAR) %>% mutate(MINCUMTOTLINES=min(CUMTOTLINES)) %>% dplyr::ungroup() %>%
    dplyr::mutate(PAGEVAR=ifelse((PAGEVAR==max(PAGEVAR) & (!is.null(lastbygrp)) & (lastbygrp==TRUE) & (!is.null(fpage)) & (fpage=='last') & (nftnotes>0)),
                          PAGEVAR + floor((CUMTOTLINES-MINCUMTOTLINES+1)/(rowsprbdy+1-nftnotes+1)),
                          PAGEVAR)) %>%
    dplyr::select(-dplyr::contains('LINE')) -> dsetin

  #===================================================
  # Add back the good pages if this is a repage call.
  #===================================================
  if (startpaging>0){
    dsetin <- rbind(splitgood,dsetin)
  }

  #=======================================
  # Check for widows when catvars is set.
  #=======================================
  if (!is.null(catvars)){
    dsetin %>% dplyr::group_by(!!! rlang::syms(catvars)) %>% dplyr::mutate(catid=dplyr::row_number(), catn=dplyr::n()) %>% dplyr::ungroup() %>% dplyr::group_by(PAGEVAR) %>%
    # =========================================================================================================================================
    # Widow Definition: Last row of page ***AND*** first row of group (excluding last grouping variable) ***AND*** group has more than 1 row.
    # =========================================================================================================================================
    dplyr::mutate(widow=ifelse(dplyr::row_number()==dplyr::n() & catid==1 & catn>1,1,0)) %>% dplyr::ungroup() -> dsetin

    #==========================================================================================
    # No split vars definition: Unique value of all vbls in catvars is on more than one page.
    #==========================================================================================
    if (nosplitvars==TRUE){
      dsetin %>% dplyr::group_by(!!! rlang::syms(catvars)) %>% dplyr::mutate(widow=ifelse(!(min(PAGEVAR)==max(PAGEVAR)),1,widow)) %>% dplyr::ungroup() -> dsetin
    }

    #============================================================================
    # If no widows, proceed with labeling, otherwise invoke this function again.
    #============================================================================
    if (max(dsetin$widow)==0 | nosplitvars==FALSE){
      dsetin %>% dplyr::select(-c('catid','catn','widow')) -> dsetin
    } else {
      widowpage <- min(dsetin[dsetin$widow==1,c('PAGEVAR')])

      #=================
      # Recursive call.
      #=================
      dsetin <- ru_addpage(dsetin=dsetin,
                           grpvars=grpvars,
                           stackvars=stackvars,
                           varlabls=varlabls,
                           rowsprbdy=rowsprbdy,
                           startpaging=widowpage,
                           lastbygrp=lastbygrp,
                           fpage=fpage,
                           nftnotes=nftnotes,
                           nosplitvars=nosplitvars,
                           npgvars=npgvars)
    }
  }

  ##======================================
  ## Return if recursion limit reached.
  ##======================================
  if (rfenv$PAGING_ITERATIONS>200){
      return(dsetin)
  }

  #====================
  # Relabel variables.
  #====================
  varlabls['PAGEVAR'] <- 'Page Variable'
  #label(dsetin) = as.list(varlabls[match(names(dsetin), names(varlabls))])
  dsetin <- ru_labels(dsetin,varlabels=varlabls)

  return(dsetin)
}

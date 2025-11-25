#' Align Columns for Reporting
#'
#' Pass in a data frame alignment criteria to have columns aligned for reporting.
#'
#' @param dsetin The data set holding the columns to align.
#' @param varsin Columns to align.
#' @param byvars Set of group-by variables.
#' @param alignment Type of alignment.
#' @param compresschryn Compress by removing leading and trailing spaces for the resulting aligned column?
#' @param ncspaces Number of spaces between N and Percent.
#'
#' @return A data frame based having the requested columns aligned for reporting.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{cr883296@gmail.com}
#'
#' @examples
#'
#' #=================
#' # Adverse Events
#' #=================
#' library(repfun)
#' library(dplyr)
#' datdir <- file.path(gsub("\\","/",tempdir(),fixed=TRUE),"datdir")
#' dir.create(datdir,showWarnings=FALSE)
#' repfun::copydata(datdir)
#' repfun::rs_setup(D_POP="SAFFL",
#'                  D_POPLBL="Safety",
#'                  D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>% dplyr::mutate(TRT01AN=
#'                      ifelse(TRT01A=='Placebo',1,
#'                      ifelse(TRT01A=='Xanomeline Low Dose',2,3)))
#' attr(G_POPDATA$TRT01AN,"label") <- 'Actual Treatment for Period 01 (n)'
#' adae <- repfun:::rfenv$adamdata$adae.rda() %>% dplyr::select(-SAFFL) %>%
#'         repfun::ru_getdata(G_POPDATA, c("STUDYID", "USUBJID"),
#'         keeppopvars=c("TRT01AN", "TRT01A"))
#' aesum_t <- repfun::ru_freq(adae,
#'               dsetindenom=G_POPDATA,
#'               countdistinctvars=c('STUDYID','USUBJID'),
#'               groupbyvarsnumer=c('TRT01AN','TRT01A','AEBODSYS','AEDECOD'),
#'               anyeventvars = c('AEBODSYS','AEDECOD'),
#'               anyeventvalues = c('ANY EVENT','ANY EVENT'),
#'               groupbyvarsdenom=c('TRT01AN'),
#'               resultstyle="NUMERPCT",
#'               totalforvar=c('TRT01AN'),
#'               totalid=99,
#'               totaldecode='Total',
#'               codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'               varcodelistpairs=c(""),
#'               codelistnames=list(),
#'               resultpctdps=0) %>%
#'            repfun::ru_denorm(varstodenorm=c("tt_result", "PERCENT"),
#'                     groupbyvars=c("tt_summarylevel", "AEBODSYS", "AEDECOD"),
#'                     acrossvar="TRT01AN",
#'                     acrossvarlabel="TRT01A",
#'                     acrossvarprefix=c("tt_ac", "tt_p"))
#' print('Before Aligning')
#' print(head(aesum_t[,grep('(AEBODSYS|AEDECOD|tt_ac)',names(aesum_t))],20))
#' aesum_t_a <- repfun::ru_align(aesum_t, "tt_ac:")
#' print('After Aligning')
#' print(head(aesum_t_a[,grep('(AEBODSYS|AEDECOD|tt_ac)',names(aesum_t_a))],20))
#'
#' #===========================
#' # Baseline Characteristics
#' #===========================
#' repfun::rs_setup(D_POP="SAFFL",
#'                  D_POPLBL="Safety",
#'                  D_POPDATA=repfun::adsl %>% dplyr::filter(SAFFL =='Y'),
#'                  D_SUBJID=c("STUDYID","USUBJID"),
#'                  R_DICTION=NULL,
#'                  R_OTHERDATA=NULL,
#'                  R_INPUTDATA=NULL,
#'                  R_RAWDATA=NULL,
#'                  R_SDTMDATA=NULL,
#'                  R_ADAMDATA=datdir)
#' G_POPDATA <- repfun:::rfenv$G_POPDATA %>%
#'              dplyr::mutate(TRT01AN=ifelse(TRT01A=='Placebo',1,
#'                             ifelse(TRT01A=='Xanomeline Low Dose',2,3))) %>%
#'   repfun::ru_labels(varlabels=list('TRT01AN'='Actual Treatment for Period 01 (n)'))
#' demstats_t <- repfun::ru_sumstats(G_POPDATA,
#'                  analysisvars=c("AGE","TRTDURD"),
#'                  groupbyvars=c("STUDYID","TRT01AN"),
#'                  codedecodevarpairs=c("TRT01AN", "TRT01A"),
#'                  totalforvar="TRT01AN", totalid=99,
#'                  totaldecode="Total",
#'                  statsinrowsyn = "Y",
#'                  analysisvardps=list("AGE"=1,"TRTDURD"=2),
#'                  statslist=c("n", "mean", "median", "sd", "min", "max")) %>%
#'   repfun::ru_denorm(varstodenorm=c("tt_result"),
#'             groupbyvars=c("tt_avid", "tt_avnm", "tt_svid", "tt_svnm"),
#'             acrossvar="TRT01AN", acrossvarlabel="TRT01A",
#'             acrossvarprefix=c("tt_ac"))
#'
#' print('Before Aligning')
#' print(head(demstats_t,10))
#' demstats_t_a <- repfun::ru_align(demstats_t, "tt_ac:", ncspaces=10)
#' print('After Aligning')
#' print(head(demstats_t_a,10))
#'
#' @export
#'
ru_align <- function (dsetin,
                      varsin,
                      byvars=NULL,
                      alignment="Right",
                      compresschryn="Y",
                      ncspaces=1) {

  #if (G_DEBUG>0) print(paste0("RU_ALIGN: ", "Start or RU_ALIGN"))

  if (nrow(dsetin) < 1) return(as.data.frame(dsetin))
  alignment <- tolower(alignment)
  if (alignment == "right") {alignment.1 <- "left"}
  if (alignment == "left") {alignment.1 <- "right"}

  varsin <- ru_expvarlist(dsetin, varsin)

  compresschryn <- toupper(compresschryn)
  dp <- "."
  ndspchar <- "/"
  if (length(byvars) < 1 || (length(byvars) == 1 && (is.na(byvars) || is.null(byvars)) || byvars == "" )) nobyvars <- TRUE else nobyvars <- FALSE

  dsetin.1 <- dplyr::mutate(dsetin, seq__=dplyr::row_number())

  for (i in 1:length(varsin)) {
    if (nobyvars) selectvars <- c(unlist(varsin[i]), "seq__") else selectvars <- c(byvars, unlist(varsin[i]), "seq__")

    v.this.var <- unlist(varsin[i])
    d.selsub <- dsetin.1 %>% dplyr::select(dplyr::all_of(selectvars))
    this_var <- d.selsub[[v.this.var]]
    p11__ <- stringr::str_split(this_var, "[(]", simplify = TRUE)
    p21__ <- stringr::str_split(p11__[, 1], "[.]", simplify=TRUE)
    p22__ <- if (ncol(p21__) > 1) {p21__[, 2]} else {rep("", nrow(p11__))}
    p21__ <- p21__[, 1]

    p31__ <- stringr::str_split(p11__[, 1], "[/]", simplify=TRUE)
    p32__ <- if (ncol(p31__) > 1) p31__[, 2] else rep("", nrow(p11__))
    p31__ <- p31__[, 1]

    slash__ <- ifelse(stringr::str_detect(p11__[, 1], "/"), 1, 0)
    colon__ <- ifelse(stringr::str_detect(p11__[, 1], "."), 1, 0)

    if (ncol(p11__) > 1) {
      p41__ <- stringr::str_split(p11__[, 2], "[,]", simplify=TRUE)
      p42__ <- if (ncol(p41__) > 1) p41__[, 2] else rep("", nrow(p11__))
      p41__ <- p41__[, 1]
    } else {
      p41__ <- rep("", nrow(p11__))
      p42__ <- rep("", nrow(p11__))
    }
    d.all_rows <- cbind(p21__, p22__, p31__, p32__, p41__, p42__, d.selsub$seq__, slash__, colon__)
    colnames(d.all_rows) <- c("p21__", "p22__", "p31__", "p32__", "p41__", "p42__", "seq__", "slash__", "colon__")
    d.all_rows <- as.data.frame(d.all_rows) %>% dplyr::mutate(
      p1__=stringr::str_trim(ifelse(slash__ == 1, p31__, p21__)),
      p2__=stringr::str_trim(ifelse(slash__ == 1, p32__, p22__)),
      p3__=stringr::str_trim(p41__),
      p4__=stringr::str_trim(p42__)
    ) %>% dplyr::select(seq__, p1__, p2__, p3__, p4__, slash__, colon__)

    if (nobyvars) {
      d.part_width <- d.all_rows %>% dplyr::summarize(w1__=max(nchar(p1__), na.rm = TRUE),
                                                      w2__=max(nchar(p2__), na.rm = TRUE), w3__=max(nchar(p3__), na.rm = TRUE), w4__=max(nchar(p4__), na.rm = TRUE),
                                                      s__=max(slash__, na.rm = TRUE), c__=max(colon__, na.rm = TRUE))

      d.width <- merge(x = d.selsub, y = d.part_width, all.x = TRUE, all.y=TRUE)
    } else {
      d.part_width <- merge(d.all_rows, d.selsub, by="seq__") %>% dplyr::group_by(!! rlang::sym(byvars)) %>% dplyr::summarize(w1__=max(nchar(p1__), na.rm = TRUE),
                                                                                                                              w2__=max(nchar(p2__), na.rm = TRUE), w3__=max(nchar(p3__), na.rm = TRUE), w4__=max(nchar(p4__), na.rm = TRUE),
                                                                                                                              s__=max(slash__, na.rm = TRUE), c__=max(colon__, na.rm = TRUE)) %>% dplyr::ungroup()
      d.width <- merge(x = d.selsub, y = d.part_width, by = byvars, all.x = TRUE, all.y=TRUE)
    }

    # d.all_rows <- ru_fillna(d.all_rows)
    d.width.1 <- merge(x = d.width, y = d.all_rows, by = c("seq__"), all.x = TRUE, all.y=TRUE)
    # print(d.width.1)
    if (nobyvars) selectvars.1 <- c("seq__", v.this.var)
    else selectvars.1 <- c(byvars, "seq__", v.this.var)
    d.width.1 <- d.width.1 %>% dplyr::mutate(var__=ifelse(! is.na(w1__) & w1__ > 0, stringr::str_pad(p1__, w1__, "left"), ""),
                                      sep__=ifelse(slash__ == "1", ndspchar, ifelse(colon__== "1" & ! p1__=="" & ! p2__=="", ".", " ")),
                                      p3__=ifelse(! is.na(w3__) & w3__> 0 & ! p3__ == "", paste0("(", p3__), p3__),
                                      var__=ifelse(! is.na(w2__) & w2__ > 0, paste0(var__, sep__, stringr::str_pad(p2__, w2__, "right")), var__),
                                      var__=ifelse(! is.na(w3__) & w3__ > 0, paste0(var__, " ", stringr::str_pad(p3__, w3__ + ncspaces, alignment.1)), var__),
                                      var__=ifelse(! is.na(w4__) & w4__ > 0, paste0(var__, ", ", stringr::str_pad(p4__, w4__, alignment)), var__),
                                      !! v.this.var := var__
    ) %>% dplyr::select(dplyr::all_of(selectvars.1))
    dsetin.1 <- dsetin.1[, which(! (names(dsetin.1) %in% c(v.this.var)))]
    if (i == 1) d.out <- d.width.1
    else d.out <- merge(x = d.out, y = d.width.1, by = c("seq__"), all.x = TRUE, all.y=FALSE)
  }
  d.out <- merge(x = dsetin.1, y = d.out, by = c(byvars, "seq__"), all.x = TRUE, all.y=FALSE) %>% dplyr::select(-seq__)
  #d.out <- ru_labels(d.out, base::labels(dsetin))
  d.out <- ru_labels(d.out, lapply(dsetin,function(x){attr(x,"label")}))

  #if (G_DEBUG>0) print(paste0("RU_ALIGN: ", "End of RU_ALIGN"))

  return(as.data.frame(d.out))
}

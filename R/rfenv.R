rfenv <- new.env(parent = emptyenv())

rfenv$G_DEBUG <- 0
#rfenv$PATH <- paste0("C:/Users/",Sys.getenv("USERNAME"),"/OneDrive - GSK/Documents/GitHub/repfun")
rfenv$PATH <- paste0(getwd())

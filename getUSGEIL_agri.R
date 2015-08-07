

rProf <- TRUE
compareWithFCL <- TRUE
nOfOutl <- 20
nOfYears <- 3

suppressPackageStartupMessages(library(dplyr))

token       <- "218d0df9-bc99-4a6a-994f-82f583b5f063"
module_name <- "trade_prevalidation"
domain      <- "trade"
dataset     <- "ct_raw_tf"
reportVar   <- "reportingCountryM49"
partnerVar  <- "partnerCountryM49"
itemVar     <- "measuredItemHS"
eleVar      <- "measuredElementTrade"
yearVar     <- "timePointYears"

startMoment <- format(Sys.time(), "%Y%m%d%H%M")


# Settings for developer's station
if(Sys.getenv("DEBUG_MODE") %in% c("", "TRUE", "T", "1")) {
  
  if(length(lapply(dir(file.path("r_modules", module_name, "R"), 
                       full.names = T), 
                   source)) == 0) stop("Files for sourcing not found")
  
  output_dir <- file.path(Sys.getenv("HOME"), 
                          module_name,
                          paste0(Sys.getenv("USER"), startMoment))
  
  faosws::GetTestEnvironment(
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = token)
  
  reporters <- swsContext.datasets[[1]]@dimensions[[reportVar]]@keys
  
} 



f <- function() {
  heads <- getReporterAgriData(c(getCountryCode("^Germa")$code, 
                                 getCountryCode("Isra")$code, 
                                 getCountryCode("^USA\\(")$code),
                               selectElems(unit %in% c("kg", "US$", "l", "m3", "t", "1000 t", "head", "1000 head"),
                                           backflow == F), 
                               2011)
  
  saveRDS(heads, "USGEIL_agri.rds")
  
  heads
  
}

# > dim(data)
# [1] 186493      7
# GetData.processNorm Res
# rows <- lapply(data$data, function(listElement){
# 49420687 dups
# 90301 mb allocated
# 89950 mb released 
# 3300 sec

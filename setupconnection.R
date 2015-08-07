
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
  
  if(length(lapply(dir(file.path(Sys.getenv("HOME"),"sws", "sws_r_api",  "r_modules", module_name, "R"), 
                       full.names = T), 
                   source)) == 0) stop("Files for sourcing not found")
  
  output_dir <- file.path(Sys.getenv("HOME"), 
                          module_name,
                          paste0(Sys.getenv("USER"), startMoment))
  
  faosws::GetTestEnvironment(
    # baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    # baseUrl = "https://hqlprsws2.hq.un.fao.org:8181/",
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    #token = "349ce2c9-e6bf-485d-8eac-00f6d7183fd6") # Token for QA)
    token = "f5e52626-a015-4bbc-86d2-6a3b9f70950a") # Second token for QA
    # token = token)
  
} 

# Settings for working environment
if(Sys.getenv("DEBUG_MODE") %in% c("FALSE", "F", "0")) {
  
  
  output_dir <- file.path(Sys.getenv("R_SWS_SHARE_PATH"),
                          module_name,
                          paste0(Sys.getenv("USER"), startMoment))
  
  
}

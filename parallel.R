### Parallel

library(doParallel)
library(foreach)
registerDoParallel(cores=detectCores(all.tests=TRUE))


startLMYear <- 2005
startReportYear <- 2010
endYear <- 2012

items <- getAllItems()$code

f <- tempfile()
Rprof(f)

startTime <- Sys.time()

x <- plyr::ldply(items[sample(seq_along(items), 100)], function(item) {
  getCommodityFullData(item, 
                       element = selectElems(unit %in% c("kg", "US$"), backflow == F), 
                       year = startLMYear:endYear)
  
}, .progress = "text", .parallel = F)

diffTime <- Sys.time() - startTime
# Time difference of 57.42297 mins


Rprof(NULL)

summaryRprof(f)$by.total[1:20,]

unlink(f)

db1 <- src_sqlite("comtrade100.sqlite3", create = T)
ct100_sqlite <- copy_to(db1, x, temporary = F, indexes = 
                          list("reportingCountryM49", "partnerCountryM49", "measuredElementTrade",
                               "measuredItemHS", "timePointYears"))


db1 <- src_sqlite("comtrade100.sqlite3", create = F)
startTime <- Sys.time()
x <- as.data.frame(tbl(db1, "x"), n = -1)
diffTime <- Sys.time() - startTime
# Time difference of 0.9398198 secs

source("/home/sas/sws/sws_r_api/r_modules/trade_prevalidation/vignettes/setupconnection.R")

tlsql <- function(select = NULL, where = NULL) {
  
  if(!is.null(select)) select <- list(select)
  
  faosws::GetTableData(schemaName = "ess",
                       tableName = "ct_tariffline_adhoc", 
                       whereClause = where, 
                       selectColumns = select)
}

for (rep in c('376', '842', '276')) {
  startMoment <- Sys.time()
  data <- try(
#     tlsql(select = "rep, tyear as year, flow, comm as hs, prt, weight, qty, qunit, tvalue as value", 
#           where = paste0("where rep in ('", rep, "') and tyear = '2011'")))
  tlsql(select = "flow, comm as hs, prt, weight, qty, qunit, tvalue as value", 
          where = paste0("where rep in ('", rep, "') and tyear = '2011'")))
  dur <- Sys.time() - startMoment
  if(!inherits(data, "try-error")) {
    print("Success")
    saveRDS(data, paste0("tariff2011_", rep, ".rds"))
  } else print("Failed")
  print(dur)
}

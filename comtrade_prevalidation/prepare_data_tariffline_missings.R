suppressPackageStartupMessages(library(dplyr))

source(file.path(Sys.getenv("HOME"), "r_adhoc", "trade_prevalid_testing", "setupconnection.R"))

subdir <- "OrangeBook"
sourcedir <- "tradeR"

if(length(lapply(dir(file.path(Sys.getenv("HOME"), "r_adhoc", "privateFAO", subdir, sourcedir), 
                     full.names = T), 
                 source)) == 0) stop("Files for sourcing not found")

trade_src <- src_postgres("sws_data", "localhost", 5432, "trade", .pwd, 
                          options = "-c search_path=ess")

missingIndicator <- function(indicator, missingCases) {
  indicator %in% missingCases}
  
  
data_db <- tbl(trade_src, sql("select * from ess.agri"))

  data <- data_db %>% 
  filter(year == "2011") %>% 
  collect() %>% 
  mutate(no_quant = missingIndicator(weight, NA) &
           missingIndicator(qty, NA)) %>% 
  left_join(getComtradeM49(), by = c("reporter" = "code"))

hs2_miss <- data %>% 
  group_by(name, hs2) %>% 
  summarize(n_of_miss = sum(no_quant) / n()) %>% 
  group_by(name) %>% 
  mutate(me_miss_reporter = median(n_of_miss),
         max_miss_reporter = max(n_of_miss)) %>% 
  ungroup() %>% 
  mutate(id = row_number())

saveRDS(hs2_miss, 
        file.path(Sys.getenv("HOME"), 
                  "r_adhoc", 
                  "trade_prevalid_testing",
                  "comtrade_prevalidation",
                  "tariffline_hs2_missing_quant.Rds"))
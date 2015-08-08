suppressPackageStartupMessages(library(dplyr))

missingIndicator <- function(indicator, missingCases) {
  indicator %in% missingCases}
# Where we are?

# In FAO

hostname <- Sys.getenv("HOSTNAME")
if(!is.element(hostname, c("matrunichstation", "sasmobile2.sasdomain"))) 
  stop("Uknown workstation. Aborting.")

if(hostname == "matrunichstation") {
  source(file.path(Sys.getenv("HOME"), "r_adhoc", "trade_prevalid_testing", "setupconnection.R"))
  projects_dir <- "r_adhoc"
}

# At home
if(hostname == "sasmobile2.sasdomain") projects_dir <- "fao"

subdir <- "OrangeBook"
sourcedir <- "tradeR"

if(length(lapply(dir(file.path(Sys.getenv("HOME"), projects_dir, "privateFAO", subdir, sourcedir), 
                     full.names = T), 
                 source)) == 0) stop("Files for sourcing not found")

if(hostname == "matrunichstation") {
  trade_src <- src_postgres("sws_data", "localhost", 5432, "trade", .pwd, 
                            options = "-c search_path=ess")
  
  data_db <- tbl(trade_src, sql("select * from ess.agri"))
}

if(hostname == "sasmobile2.sasdomain") {
  data_db <- readRDS(file.path(Sys.getenv("HOME"), 
                               projects_dir, 
                               "trade_prevalid_testing",
                               "tariffline_agri_full.Rds"))
  
}

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


## Full agri data 
data_db %>% 
  collect() %>% 
saveRDS(file.path(Sys.getenv("HOME"), 
                  projects_dir, 
                  "trade_prevalid_testing",
                  "tariffline_agri_full.Rds"))

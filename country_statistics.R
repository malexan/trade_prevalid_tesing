startMoment <- Sys.time()

data <- getReporterFullData(getCountryCode("Kenya")$code,
                            element = selectElems(unit %in% c("kg", "US$"), backflow == F),
                            year = 2012)

Sys.time() - startMoment


data0 <- getCommodityFullData(100110, 
                              element = selectElems(unit %in% c("kg", "US$"), backflow == F), 
                              year = 2012)

startMoment <- Sys.time()
data1 <- getComtradeData(842, getAllCountryCodes("partner"), 2011, getAllItems()$code,
                         element = selectElems(unit %in% c("kg"), backflow == F))

Sys.time() - startMoment

startMoment <- Sys.time()
Rprof()
data1value <- getComtradeData(842, getAllCountryCodes("partner"), 2011, getAllItems()$code,
                         element = selectElems(unit %in% c("US$"), backflow == F))
Rprof(NULL)
Sys.time() - startMoment

data <- dplyr::bind_rows(data1, data1value)

startMoment <- Sys.time()
isr <- getReporterFullData(getCountryCode("Isra")$code, 
                           selectElems(unit %in% c("kg", "US$"), backflow == F), 
                           2011)
Sys.time() - startMoment


startMoment <- Sys.time()
ge <- getReporterFullData(getCountryCode("^Germa")$code, 
                           selectElems(unit %in% c("kg", "US$"), backflow == F), 
                           2011)
Sys.time() - startMoment

data0 <- bind_rows(data, isr, ge) %>% 
  humanizeComtrade()

data0 %>% 
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%  
  group_by(reporter) %>% 
  summarize(
    n = n(),
    noquant = sum((is.na(weight) | weight == 0) & cost > 0, na.rm = T),
    novalue = sum(weight > 0 & (is.na(cost) | cost == 0), na.rm = T),
    noquantprop   = round(noquant / n, 3),
    novalueprop   = novalue / n)

saveRDS(data0, "us_ge_il_2011.rds")





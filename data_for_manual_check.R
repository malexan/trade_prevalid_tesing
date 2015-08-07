startLMYear <- 1990
startReportYear <- 2010
endYear <- 2012
matchingFlows <- .2
matchingFlowsMarketShare <- .1

data0 <- getCommodityFullData(100110, 
                              element = selectElems(unit %in% c("kg", "US$"), backflow == F), 
                              year = startLMYear:endYear)


## UV and aggr UV

data <- data0 %>%
  humanizeComtrade() %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate(selftrade = reporter == partner,
         noquant = weight == 0 & cost > 0,
         novalue = weight > 0 & cost == 0,
         uv = ifelse(noquant | novalue, NA, cost / weight)) %>%
  group_by(dir, back, item, hs, year) %>%
  mutate(uv_global = median(uv, na.rm = T)) %>%
  group_by(dir, item, back, hs, year, reporter) %>%
  mutate(uv_reporter = median(uv, na.rm = T)) %>%
  ungroup() %>%
  mutate(magnor = magnitudeOrder(uv, uv_reporter))


## LM residuals

data <- data %>%
  left_join(data %>%
              filter(!selftrade,
                     !noquant,
                     !novalue,
                     magnor ==1) %>%
              select(dir, back, item, hs, reporter, partner, year, uv) %>%
              group_by(dir, back, item, hs, reporter, partner) %>%
              do(broom_augment(lm(uv ~ year, data = .))) %>%
              ungroup() %>%
              select(-uv, -.hat, -.fitted, -.resid),
            by = c("reporter", "partner", "year", "dir", "back", "item", "hs"))

## mirrored UV

data <- data %>%
  group_by(year, item, hs, back) %>%
  mutate(uv_mirr = mirroredVariable(reporter, partner, dir, uv)) %>%
  ungroup()

## Running mean
data <- data %>%
  group_by(reporter, partner, dir, back, item, hs) %>%
  arrange(year) %>% # But without arrange runmean is the same. How could it be?
  mutate(uv_roll = caTools::runmean(uv, k = 3, alg = "C", endrule = "mean")) %>%
  ungroup()

### Checks of order infuence on runmean()

# data %>%
#   filter(!selftrade,
#          !noquant,
#          !novalue,
#          magnor ==1,
#          reporter == "Albania") %>%
#   select(reporter, partner, year, dir, back, hs, item, uv, uv_roll) %>%
#   reshape2::melt(measure.vars = c("uv", "uv_roll")) %>% 
#   ggplot(aes(year, value, color = variable)) +  geom_path() +
#   facet_wrap(~reporter +  partner + dir + back, scales = "free")
    
## Out of order

data <- data %>% 
  group_by(year, item, hs, back) %>%
  mutate(q_mirr = mirroredVariable(reporter, partner, dir, weight),
         v_mirr = mirroredVariable(reporter, partner, dir, cost),
         q_match = abs(weight / q_mirr - 1) < matchingFlows,
         v_match = abs(cost / v_mirr - 1) < matchingFlows,
         full_match = q_match & v_match) %>% 
  group_by(dir, add = T) %>%
  mutate(totalvalue = sum(cost, na.rm = T),
            matchvalue = sum(cost[full_match], na.rm = T),
            matchshare = matchvalue / totalvalue,
            enoughshare = matchshare > matchingFlowsMarketShare,
         uv_min = min(uv[enoughshare], na.rm = T),
         uv_max = max(uv[enoughshare], na.rm = T)) %>% 
  ungroup() %>% 
  mutate(outofrange = uv > uv_max | uv < uv_min)

# data1 %>% 
#   filter(outofrange)
    
## Prepare for Tomas

cleanCountryNames <- function(name) {
  name <- stringr::str_replace_all(name, "\\(.*\\)", "")
  name <- stringr::str_trim(name)
  name
}



data <- data %>%
  # filter(year >= 2010) %>%
  mutate(quantity = weight / 1000,
         value    = cost / 1000,
         flow = factor(dir, levels = c("in", "out"), labels = c("import", "export")),
         reporter = cleanCountryNames(reporter),
         partner  = cleanCountryNames(partner)) %>%
  select(reporter, partner, year, flow, hs, item, quantity, value, 
         uv, uv_global, uv_reporter, uv_mirr, uv_roll3 = uv_roll,
         no_quantity = noquant, no_value = novalue, 
         selftrade, outofrange,
         magnit_ord = magnor,  
         influence = .cooksd, residual = .std.resid) %>% 
  arrange(year, flow, reporter, partner) # %>%
#   as.data.frame() %>%
#   XLConnect::writeWorksheetToFile(file.path(output_dir, "comtrade.xlsx"), data = ., sheet = "Sheet1")

# saveRDS(data, "data.rds")
saveRDS(data, "r_modules/trade_prevalidation/vignettes/comtrade_prevalidation/data.rds")

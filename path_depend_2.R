startLMYear <- 2010
startReportYear <- 2010
endYear <- 2012

data0 <- getCommodityFullData(100110, 
                              element = selectElems(unit %in% c("kg", "US$"), backflow == F), 
                              year = startLMYear:endYear)




### Mirroring of quantities and values

ct <- data0 %>%
  humanizeComtrade() %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate(selftrade = reporter == partner,
         rule3 = weight == 0 & cost > 0,
         rule3_1 = weight > 0 & cost == 0,
         uv = ifelse(rule3 | rule3_1, NA, cost / weight)) %>%
  reshape2::melt(measure.vars = c("weight", "cost")) %>%
  group_by(year, back, item, hs, variable) %>%
  mutate(value_mirr = mirroredVariable(reporter, partner, dir, value),
         value_diff = (value - value_mirr) / value) %>%
  ungroup()

# Results

ct %>%
  ggplot(aes(value_diff, fill = variable)) +
  geom_density(alpha = .3) +
  facet_grid(year ~ dir, scales = "free") + 
  scale_x_log10(labels = scales::percent)

ct %>%
  ggplot(aes(value_diff, fill = variable)) +
  geom_density(alpha = .3) +
  facet_wrap( ~ dir, scales = "free") + 
  scale_x_log10()


ct %>%
  filter(!selftrade, !rule3, !rule3_1) %>%
  ggplot(aes(dir, value_diff, fill = variable)) +
  geom_boxplot(alpha = .3) +
  facet_wrap( ~ dir, scales = "free") + 
  scale_y_log10(labels = scales::percent)


ct %>%
  filter(dir == "in", !selftrade, !rule3, !rule3_1) %>%
  ggplot(aes(value, value_mirr)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ variable, scales = "free") +
  scale_y_log10() + 
  scale_x_log10()


ct %>%
  filter(variable == "cost", !selftrade, !rule3, !rule3_1) %>%
  ggplot(aes(value, value_mirr)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ dir, scales = "free") +
  scale_y_log10() + 
  scale_x_log10()

ct %>%
  filter(variable == "cost", dir == "in", !selftrade, !rule3, !rule3_1) %>%
  lm(value ~ value_mirr, data = .) %>%
  summary

ct %>%
  filter(variable == "cost", dir == "out", !selftrade, !rule3, !rule3_1) %>%
  lm(value ~ value_mirr, data = .) %>%
  summary

ct %>%
  filter(variable == "cost", dir == "out", !selftrade, !rule3, !rule3_1) %>%
  lm(value_diff ~ as.factor(year), data = .) %>%
  summary
  

ct %>%
  filter(variable == "cost", dir == "out", !selftrade, !rule3, !rule3_1) %>%
  ggplot() +
  geom_boxplot(aes(partner, value_diff)) + 
  scale_y_log10(labels = scales::percent)

ct %>%
  filter(variable == "cost", dir == "out", !selftrade, !rule3, !rule3_1) %>%
  group_by(partner) %>%
  summarize(diff = median(value_diff, na.rm = T)) %>%
  top_n(40, diff) %>%
  ggplot() + geom_bar(aes(reorder(partner, diff), diff), stat = "identity")+
  coord_flip()

### SD of different UV methods

# Calculate UV
ct <- ct %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate(selftrade = reporter == partner,
         rule3 = weight == 0 & cost > 0,
         rule3_1 = weight > 0 & cost == 0,
         uv = ifelse(rule3 | rule3_1, NA, cost / weight)) 


# Other types of UVs

ct <- ct %>%
  group_by(dir, item, hs, year) %>%
  mutate(uv_global = median(uv, na.rm = T)) %>%
  group_by(dir, item, hs, year, reporter) %>%
  mutate(uv_reporter = median(uv, na.rm = T))


# Lets draw

ct %>%
  select(-hs, -weight, -cost) %>%
  reshape2::melt(measure.vars = c("uv_global", "uv_reporter"),
                 value.name = 'uv_agg', variable.name = "uv_type") %>% 
  mutate(uv_diff = abs(uv_agg - uv)) %>%
  ggplot(aes(uv_type, uv_diff, fill = uv_type)) + geom_boxplot() + 
  facet_wrap(~item + year + dir, scales = "free_y") + 
  scale_y_log10()


ct %>%
  select(-hs, -weight, -cost) %>%
  reshape2::melt(measure.vars = c("uv_global", "uv_reporter"),
                 value.name = 'uv_agg', variable.name = "uv_type") %>% 
  mutate(uv_diff = abs(uv_agg - uv)) %>%
  ggplot(aes(uv_diff, fill = uv_type)) + geom_density(alpha = .3) + 
  facet_wrap(~item + year + dir) + 
  scale_x_log10()



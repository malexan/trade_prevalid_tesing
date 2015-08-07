# Save agricult codes in the DB

agriCodes <- data.frame(hs6 = getAgriHSCodes(), 
                        stringsAsFactors = F)

RPostgreSQL::dbWriteTable(trade_src$con, "agricodes",
                          agriCodes, 
                          row.names = F,
                          overwrite = T)

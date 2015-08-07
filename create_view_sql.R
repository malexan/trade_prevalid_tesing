# tbl from dplyr adds SELECT ALL, so change it with DBI functions

tbl(trade_src, sql("CREATE VIEW ess.tariffline AS SELECT 
tyear AS year, 
rep AS reporter, 
prt AS partner,
flow,
weight,
qty,
qunit,
tvalue AS value 
FROM ess.ct_tariffline_adhoc;"))

DBI::dbSendQuery(trade_src$con, "CREATE VIEW ess.agri AS SELECT 
tariffline.year, tariffline.reporter, tariffline.partner,
tariffline.hs, tariffline.flow, tariffline.weight, 
tariffline.qty, tariffline.qunit, tariffline.value,
substring(tariffline.hs from 1 for 2) as hs2, 
substring(tariffline.hs from 1 for 4) as hs4, 
agricodes.hs6
from tariffline
join agricodes on agricodes.hs6 = substring(tariffline.hs from 1 for 6);")

DBI::dbSendQuery(trade_src$con, "drop view ess.agri")

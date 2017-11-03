######################################################################################################
# R code for calculating SPAN MSFC
# Anders D. Sleire - sleire@gmail.com
######################################################################################################

library(RODBC)
library(ggplot2)
library(reshape2)
source("C:/Users/AndersS/Dropbox/sleire/hedgeR/R/msfc_class.R")
source("C:/Users/AndersS/Dropbox/sleire/hedgeR/R/msfc.R")

# import portfolio data from UNIKEE/VIZ/[VizDataWareHouse]
con<-odbcConnect("connectUNIKEE/VIZ")

sql <- "
declare @tdate as date
set @tdate = getdate() -1

SELECT [Include] = 'TRUE'
,a.[SpanDate] as tdate
,a.[ProdName]
,b.FromDate
,b.ToDate
,a.[Closing]
,a.[Span] * -1 as Span
FROM [BECustom].[dbo].[Span] a
inner join [VizPrices].[dbo].[Products] b
on a.ProdName = b.ProdName
where a.SpanDate = @tdate
and a.ProdName like '%ENOQ%'
order by b.FromDate
"

bench <- sqlQuery(con,sql)
odbcClose(con); rm(sql,con)

# inputs to msfc
tdate <- as.Date(bench$tdate[1])
include <- bench$Include
f <- bench$Span
sdate <- as.Date(bench$FromDate)
edate <- as.Date(bench$ToDate)

# create MSFC
res <- msfc(tdate,include,sdate,edate,f)

# plot res
plot(res)
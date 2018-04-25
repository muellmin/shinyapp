library(rActus)
library(rflPortfolio)
library(rflContracts)
data("ModelBank")
bank=Portfolio()
import(bank, ModelBank)

BalanceSheet = Tree(list(
  branches = list(
    BS = c("Assets","Liab"),
    Assets = c("MortVar", "MortFixed"),
    Liab = c("Savings", "MonMarket", "CapMarket")
  ),
  leafs = list(
    MortVar = "104",
    MortFixed = c("105", "106"),
    Savings = "101",
    MonMarket = "102",
    CapMarket = "103"
  )
))

ad="2016-01-02T00"
yc.tnr <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
yc.rts <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
yc.ch <- YieldCurve(
  what = list(
    MarketObjectCode = "YC_CH_EIDGENOSSEN",
    Nodes = list(ReferenceDate = ad,
                 Tenors = yc.tnr, Rates = yc.rts)))
rf <- RFConn()
add(rf, yc.ch)
eng <- DcEngine(list(DiscountingSpread=0.01,
                     RiskFactorObjectLink="YC_CH_EIDGENOSSEN"))
set(bank, rf)
set(eng, rf)


set(BalanceSheet,what=list(business=list(
  Savings=get(get(bank,"101"),"all"),
  MonMarket=get(get(bank,"102"),"all"),
  CapMarket=get(get(bank,"103"),"all"),
  MortVar=get(get(bank,"104"),"all"),
  MortFixed=get(get(bank,"105"),"all")
)))


targets=diag(c(1.08,1.15,1.1,1.1,1.1))
colnames(targets) =
  c("Savings", "MonMarket", "CapMarket", "MortVar", "MortFixed")
rownames(targets) = colnames(targets)
targets

by=timeSequence(substring(ad,1,10),"2020-06-01",by="1 year")
years = as.character(2016:2019)

newBusiness(ptf=bank, tree=BalanceSheet, by=by, rf=rf, refType="nominal")
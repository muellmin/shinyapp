#*************************************************************
# Copyright (c) 2018 - present by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

# load required libraries
library(rActus)
library(rflPortfolio)
library(rflSimulation)

# define bank
data("ModelBank")
bank=Portfolio()
import(bank, ModelBank)

# balance sheet
BalanceSheet = Tree(list(
  branches = list( 
    BS = c("Assets","Liabilities"),
    Assets = c("Mortgages"),
    Liabilities = c("SavingsAccounts","MoneyMarket","Lending")
  ),
  leafs=list(
    Mortgages = c("104","105","106"), 
    SavingsAccounts = "101",
    MoneyMarket = "102",
    Lending = "103"
  )
))

# analysis date
ad="2016-01-02T00"

# define the economic environment, i.e. risk factor model
# in this simple case, the economic environment is defined through a
# single Yield Curve - the Euro-Area Yield Curve.
# 
# This defines the yield curve observed at the analysis date. 
yc.tnr <- c("3M", "1Y", "2Y", "5Y", "7Y", "10Y")
yc.rts <- c(-0.28, -0.26, -0.21, 0.03, 0.20, 0.42)/100
yc.ch <- YieldCurve(
  what = list(
    MarketObjectCode = "YC_CH_EIDGENOSSEN",
    Nodes = list(ReferenceDate = ad, 
                 Tenors = yc.tnr, Rates = yc.rts)))
rf=RFConn()
add(rf,yc.ch)
set(bank,rf)

# --------------------------------------------------------------------
#
# "liquidation view" analysis
events=events(bank,ad,rf)
value(events,by=ad,type="nominal",tree=BalanceSheet)

eng=DcEngine()
set(eng,list(RiskFactorObjectLink="YC_CH_EIDGENOSSEN"))
set(eng,rf)
value(events,by=ad,type="markToModel",method=eng,tree=BalanceSheet)


# --------------------------------------------------------------------
#
# "going concern view" analysis

# first, define a scenario for the continuation of your business
# A. what type of business (in the sense of an actus contract)
#    will be used on the various accounts in the tree in future?
templates=list(
  SavingsAccounts=get(get(bank,"101"),"all"),
  MoneyMarket=get(get(bank,"102"),"all"),
  Lending=get(get(bank,"103"),"all"),
  Mortgages=get(get(bank,"104"),"all")
)

# B. what are the business targets (in the sense of a multiplier 
#    applied to currention notional values) for the various accounts 
#    in the tree?
# Note: (currently only on diagonal)
growth=diag(c(1,1.1,1.1,1.1)) # i.e. no growth but continue with same notional amounts
colnames(growth)=c("SavingsAccounts","MoneyMarket","Lending","Mortgages")
rownames(growth)=colnames(growth)

# now define a time axis for the business simulation
by=timeSequence(substring(ad,1,10),"2020-06-01",by="1 year")

# therewith, we are able to compute a new portfolio containing no
# only current business but also the future business contingent to
# the business scenario and risk factors
newcts=newbiz(bank,BalanceSheet,by,growth,templates,rf)

# check what new contracts where added
get(newcts,what="ContractID")
get(newcts,what="ContractType")

# check only for certain period or certain contract id
get(newcts,what="ContractType",filter=list(periods="2018-01-02"))
get(newcts,what="ContractType",filter=list(contracts="MoneyMarket"))

# get new contracts for certain period
get(newcts,what="contracts",filter=list(periods="2018-01-02"))

# for analysis, combine current bank portfolio and future business in list
bank.dyn=c(bank,newcts)
set(bank.dyn,rf)

# compute analytics
liquidity(bank.dyn,by,"marginal",tree=BalanceSheet)
income(bank.dyn,by,"marginal",revaluation.gains=FALSE,tree=BalanceSheet)
value(bank.dyn,type="nominal",by=by,tree=BalanceSheet)
value(bank.dyn,type="markToModel",by=by,method=eng,tree=BalanceSheet)

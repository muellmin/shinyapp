
rm(list= ls())

source(file = "Code/Funktionen.R")

# libraries
library(rActus)
library(rflPortfolio)
library(rflContracts)
library(pdfetch)


#-- BS der Banken in einer Liste abspeichern ----

list.filenames = list.files(path = "Bank_BS_Data/")
list.filenames

# create an empty list that will serve as a container to receive the incoming files
list.data = list()

# create a loop to read in your data
for (i in 1:length(list.filenames))
{
  list.data[[i]] = read.csv(paste("Bank_BS_Data/", list.filenames[i], sep = ""), header = T, sep = ";")
}

# add the names of your data to the list
names(list.data) = list.filenames

#----------------------------- Analyse ----

res.list = AnalysisWithStresstest(list.data = list.data,timeBucketLength = 4,shift = -0.1)

res.list2 = AnalysisWithoutStresstest(list.data = list.data,timeBucketLength = 8)

res.list3 = Stresstest(list.data = list.data)

yc.ecb = YieldCurveECB(ad = "2018-03-14T00",plot.yc = TRUE,type = "SR")


#----------------------------- Plots -----

#------------ plot liquidity

barplot(res.list[["213800QILIUD4ROSUO03"]]$liq[1,],main="Annual liquidity",
        names.arg=names(res.list[["213800QILIUD4ROSUO03"]]$liq[1,]),las=2)

barplot(res.list[["391200BODWBDLTH1TS43"]]$liq[1,],main="Annual liquidity",
        names.arg=names(res.list[["391200BODWBDLTH1TS43"]]$liq[1,]),las=2)

barplot(res.list[["529900HNOAA1KXQJUQ27"]]$liq[1,],main="Annual liquidity",
        names.arg=names(res.list[["529900HNOAA1KXQJUQ27"]]$liq[1,]),las=2)


#------------ plot Zins-Einkommen 

barplot(res.list[["213800QILIUD4ROSUO03"]]$inc.nom[1,],main="Annual Interest Rate Income",
        names.arg=names(res.list[["213800QILIUD4ROSUO03"]]$inc.nom[1,]),las=2)
barplot(res.list[["213800QILIUD4ROSUO03"]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains",
        names.arg=names(res.list[["213800QILIUD4ROSUO03"]]$inc.reval[1,]),las=2)

barplot(res.list[["391200BODWBDLTH1TS43"]]$inc.nom[1,],main="Annual Interest Rate Income",
        names.arg=names(res.list[["391200BODWBDLTH1TS43"]]$inc.nom[1,]),las=2)
barplot(res.list[["391200BODWBDLTH1TS43"]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains",
        names.arg=names(res.list[["391200BODWBDLTH1TS43"]]$inc.reval[1,]),las=2)

barplot(res.list[["529900HNOAA1KXQJUQ27"]]$inc.nom[1,],main="Annual Interest Rate Income",
        names.arg=names(res.list[["529900HNOAA1KXQJUQ27"]]$inc.nom[1,]),las=2)
barplot(res.list[["529900HNOAA1KXQJUQ27"]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains",
        names.arg=names(res.list[["529900HNOAA1KXQJUQ27"]]$inc.reval[1,]),las=2)

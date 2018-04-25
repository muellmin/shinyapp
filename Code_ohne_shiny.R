library(rActus)
library(rflPortfolio)
library(rflContracts)
library(pdfetch)


list.filenames <- list.files(path = "C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Kontrakte_EZB/")
list.filenames
list.data<-list()
for (i in 1:length(list.filenames))
{
  list.data[[i]] <- read.csv(paste("C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Kontrakte_EZB/", list.filenames[i], sep = ""), header = T, sep = ";")
}

names(list.data) <- list.filenames

length(list.data)

bs.fun = function(list.data, ad = NA)
  #--------- Variablen Erklärung
  # list.data = Liste der BS der einzelnen Banken
  # ad = Analysedatum, ad = NA --> heutiges Datum
{
  #Datum definieren
  if(is.na(ad)){
    path.ad = as.character(Sys.Date())
    ad = paste(path.ad,"T00", sep = "")
  }
  
  
  #Analyse jedes Bankacc#
  yc.ecb = YieldCurveECB(ad = NA ,plot.yc = FALSE,type = "SR")
  rf = RFConn()
  add(rf, yc.ecb)
  
  #------------ Anzahl Contracts in jedem Datensatz
  anz.contract = rep(NA,length(list.data))
  for(i in 1:length(list.data)){
    anz.contract[i] = length(list.data[[i]]$ContractRole)
  }
  
  #------------ Leere Liste für die einzelnen Resultate
  res.list = rep(list(NA),length(list.data))
  
  #------------ For-Schlaufe um alle BS zu analysieren
  
  
  #for(i in 1:length(list.data)){
    i <- 1
    #------------ Bank-Portfolio erstellen 
    bank=Portfolio()
    import(bank, list.data[[i]])
    ids=get(bank,"ids")
    
    #------------ Risikofaktormodell an Bank-Portfolio anhängen 
    set(bank, rf)
    
    #------------ DiscountEngine für jeden Contract definieren
    eng.list = rep(list(NA),anz.contract[i])
    for(j in 1:length(eng.list)){
      eng.list[[j]] = DcEngine(list(DiscountingSpread=list.data[[i]]$Valuation_DiscountingSpread[j],
                                    RiskFactorObjectLink=list.data[[i]]$Valuation_RiskFactorObjektLink[j]))
      
      # Risikofaktormodell an DiscountEngine anhängen
      set(eng.list[[j]],rf)
    }
    
    
    #------------ Balance Sheet definieren 
    
    id.A = which(list.data[[i]]$ContractRole=="RPA")
    id.L = which(list.data[[i]]$ContractRole=="RPL")
    
    SubAcc.A = list.data[[i]]$SubAccount[id.A]
    SubAcc.L = list.data[[i]]$SubAccount[id.L]
    SubAcc = list.data[[i]]$SubAccount
    
    names(SubAcc) = list.data[[i]]$ContractID
    
    List.Acc = list(rep(list(NA),length(unique(SubAcc))))
    for(k in 1:length(unique(SubAcc))){
      List.Acc[[k]] = names(SubAcc[which(SubAcc == unique(SubAcc)[k])])
    }
    List.Acc
    names(List.Acc) = unique(SubAcc)
    
    BalanceSheet = Tree(list(
      branches = list(
        BS = c("Assets","Liabilities"),
        Assets = unique(SubAcc.A),
        Liabilities = unique(SubAcc.L)
      ),
      leafs = List.Acc
    ))
    
    
    #------------ Events generieren 
    ev <- events(bank, ad, rf)
    
    evs.df = as.data.frame(ev)
    #BalanceSheet table
    BS.table <- value(bank,"nominal",tree=BalanceSheet)
    
    BS.table$markToModel <- value(bank,"markToModel",eng.list,tree=BalanceSheet)
    
    print(BS.table)
    #Resultate in Liste abspreichern
    res.list[[i]] <-  BS.table
    
  }
  names(res.list) <- unlist(strsplit(names(list.data), "[.]"))[seq(1,2*length(list.data),2)]
  
  return(res.list)
  print(res.list)
}



l <- bs.fun(list.data)
l


trans.matrix <- function(list.data){
  res.list = rep(list(NA),length(list.data))
  for(i in 1:length(list.data)){
    
    id.A = which(list.data[[i]]$ContractRole=="RPA")
    id.L = which(list.data[[i]]$ContractRole=="RPL")
    
    SubAcc.A = list.data[[i]]$SubAccount[id.A]
    SubAcc.L = list.data[[i]]$SubAccount[id.L]
    
    acc.names <- c(unique(SubAcc.A), unique(SubAcc.L))
    dim <- length(acc.names)
    targets <- diag(round(runif(dim, min = 1, max = 1.1),3))
    rownames(targets) <- colnames(targets) <- acc.names
    
    res.list[[i]] <- targets
  }
  names(res.list) <- unlist(strsplit(names(list.data), "[.]"))[seq(1,2*length(list.data),2)]
  return(res.list)
}
t <- trans.matrix(list.data)
length(t)

lt <- list()

v1 <- list(c("Loan.L", 0.3))
v2 <- list(c("Mortgage", -0.3))

lt <- c(lt, v1)
lt <- c(lt, v2)
lt
length(lt)
lt[[1]][1]

tf <- colnames(t[[1]]) == lt[[1]][1]

diag(t[[1]])[tf] <-  diag(t[[1]])[tf]+as.numeric(lt[[1]][2])
t[[1]]

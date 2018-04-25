
#Funktion für das BalanceSheet und Übergangsmatrix
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
  
  
  for(i in 1:length(list.data)){
    
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
    
 
    #Resultate in Liste abspreichern
    res.list[[i]] <-  BS.table
    
  }
  names(res.list) <- unlist(strsplit(names(list.data), "[.]"))[seq(1,2*length(list.data),2)]
  
  return(res.list)
  print(res.list)
}

#Übergangsmatrix

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





#-----------------------------------------------------------------------------
#Funktionen von BA Neff/Iseli
#----- Funktion 1
AnalysisWithStresstest = function(list.data, ad = NA, timeBucketLength = 8, shift = 0.07) 
  #--------- Variablen Beschreibung
  # list.data = Liste der BS der einzelnen Banken
  # ad = Analysedatum, ad = NA --> heutiges Datum
  # timeBucketLength = Länge des Analysezeitraums
  # shift = Anpassung der Zinsstrukturkurve (1 Wert --> Paralleler Shift, Vektor --> Anpassung aller Zinsen einzeln)
{
  #------------ Librarys
  library(rActus)
  library(rflPortfolio)
  library(rflContracts)
  library(pdfetch)
  
  
  if(is.na(ad)){
    path.ad = as.character(Sys.Date())
    ad = paste(path.ad,"T00", sep = "")
  }
  
  #------------ Marktumgebung definieren: Zinsstrukturkurve 
  
  yc.ecb = YieldCurveECB(ad = ad,plot.yc = FALSE,type = "SR")
  
  #------------ Risikofaktormodell definieren
  rf = RFConn()
  add(rf, yc.ecb)
  
  
  #------------ Anzahl Contracts in jedem Datensatz
  # anz.contract = rep(NA,length(list.data))
  # for(i in 1:length(list.data)){
  #   anz.contract[i] = length(list.data[[i]]$ContractRole)
  # }
  
  #------------ Leere Liste für die einzelnen Resultate
  res.list = rep(list(NA),length(list.data))
  lei.vec = rep(NA,length(list.data))
  
  #------------ For-Schlaufe um alle BS zu analysieren
  
  for(i in 1:length(list.data)){
    
    #------------ Bank-Portfolio erstellen 
    bank=Portfolio()
    import(bank, list.data[[i]])
    ids=get(bank,"ids")
    bank
    
    #------------ Risikofaktormodell an Bank-Portfolio anhängen 
    set(bank, rf)
    
    
    #------------ Löschen der Child-Contracte aus dem Datensatz
    C.vec = c("C1","C2")
    C.ids = numeric()
    for(j in 1:length(C.vec)){
      C.ids = c(C.ids,grep(C.vec[j],list.data[[i]]$ContractID))
    }
    if(length(C.ids)==0){
      BS.data = list.data[[i]]
    }else{
      BS.data = list.data[[i]][-C.ids,]
    }
    
    #------------ DiscountEngine für jeden Contract definieren
    eng.list = rep(list(NA),length(ids))
    
    for(j in 1:length(eng.list)){
      eng.list[[j]] = DcEngine(list(DiscountingSpread=BS.data$Valuation_DiscountingSpread[j],
                                    RiskFactorObjectLink=BS.data$Valuation_RiskFactorObjektLink[j]))
      
      # Risikofaktormodell an DiscountEngine anhängen
      set(eng.list[[j]],rf)
    }
    names(eng.list) = BS.data$ContractID
    
    #------------ Balance Sheet definieren 
    BS.ContractRole = unique(BS.data$ContractRole)
    BS.SubAcc.list = rep(list(NA),length(BS.ContractRole))
    for(j in 1:length(BS.SubAcc.list)){
      BS.SubAcc.list[[j]] = BS.data$SubAccount[which(BS.data$ContractRole==BS.ContractRole[j])]
    }
    names(BS.SubAcc.list) = BS.ContractRole
    id.RPA = grep("RPA",names(BS.SubAcc.list))
    id.RPL = grep("RPL",names(BS.SubAcc.list))
    id.Offbalance = c(grep("RFL",names(BS.SubAcc.list)),
                      grep("PFL",names(BS.SubAcc.list))
    )
    
    temp = character()
    for(j in 1:length(id.Offbalance)){
      temp = c(temp,BS.SubAcc.list[[id.Offbalance[j]]])
    }
    BS.SubAcc.list = BS.SubAcc.list[-id.Offbalance]
    BS.SubAcc.list$Offbalance = temp
    names(BS.SubAcc.list)[id.RPA] = "Assets"
    names(BS.SubAcc.list)[id.RPL] = "Liabilities"
    
    #BS.names = rep(NA,length(unique(BS.data$ContractRole)))
    
    branche.list = list(names(BS.SubAcc.list))
    for(j in 1:length(BS.SubAcc.list)){
      branche.list[[j+1]] = unique(BS.SubAcc.list[[j]])
    }
    names(branche.list) = c("BS",names(BS.SubAcc.list))
    
    
    SubAcc = BS.data$SubAccount
    names(SubAcc) = BS.data$ContractID
    
    List.Acc = list(rep(list(NA),length(unique(SubAcc))))
    for(k in 1:length(unique(SubAcc))){
      List.Acc[[k]] = names(SubAcc[which(SubAcc == unique(SubAcc)[k])])
    }
    List.Acc
    names(List.Acc) = unique(SubAcc)
    
    # Balancesheet
    BalanceSheet = Tree(list(
      branches = branche.list,
      leafs = List.Acc
    ))
    
    
    #------------ Events generieren 
    ev = events(bank, ad, rf)
    
    #------------ Liquidität
    by=timeSequence(substring(ad,1,10),length.out=timeBucketLength+2,by="1 year")
    
    h = timeBucketLength 
    years = as.character(as.numeric(format(as.Date(ad),"%Y")):(as.numeric(format(as.Date(ad),"%Y"))+h))
    
    liq = round(liquidity(bank,by,"marginal",tree=BalanceSheet)[,-1],2)
    colnames(liq) = years
    liq = as.matrix(liq)
    
    #------------ Value 
    # Nominal
    val.nom = round(value(bank,"nominal",tree=BalanceSheet),2)
    
    # MarkToModel
    val.mark.con = matrix(NA,nrow = length(ids)+1,ncol = 1)
    for(j in 1:length(ids)){
      val.mark.con[j+1,] = value(bank,"markToModel",eng.list[ids[j]],select = list(ContractID = ids[j]))
    }
    colnames(val.mark.con) = "markToModel"
    RowNames = c("BS",ids)
    rownames(val.mark.con) = RowNames
    val.mark.con[1,] = sum(val.mark.con[2:(length(ids)+1),])
    
    val.mark = matrix(NA,nrow = length(BalanceSheet$leafs)+length(BalanceSheet$branches),ncol = 1)
    rownames(val.mark) = rownames(val.nom)
    colnames(val.mark) = "markToModel"
    for(j in 1:length(names(List.Acc))){
      val.mark[grep(names(List.Acc)[j],rownames(val.mark)),] = sum(val.mark.con[List.Acc[[j]],])
    }
    val.mark["BS",] = val.mark.con["BS",]
    
    for(j in 2:length(BalanceSheet$branches)){
      val.mark[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),] = sum(na.omit(val.mark[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(val.mark)),]))
    }
    val.mark = round(val.mark,2)
    
    #------------ Zins- Einkommen 
    # Ohne Neubewertung
    inc.nom = round(income(bank,by,"marginal", revaluation.gains=FALSE,tree=BalanceSheet)[,-1],2)
    colnames(inc.nom) = years
    inc.nom = as.matrix(inc.nom)
    
    # # Mit Neubewertung
    # inc.reval.con = matrix(NA,nrow = length(ids)+1,ncol = length(years))
    # for(j in 1:length(ids)){
    #   inc.reval.con[j+1,] = income(bank,by,"marginal",eng.list[[ids[j]]], revaluation.gains=TRUE,select = list(ContractID=ids[j]))
    # }
    # colnames(inc.reval.con) = years
    # rownames(inc.reval.con) = c("BS",ids)
    # for(j in 1:length(colnames(inc.reval.con))){
    #   inc.reval.con[1,j] = sum(inc.reval.con[2:(length(ids)+1),j])
    # }
    # 
    # inc.reval = matrix(NA,nrow = length(BalanceSheet$leafs)+3,ncol = length(years))
    # 
    # rownames(inc.reval) = rownames(inc.nom)
    # colnames(inc.reval) = years
    # for(j in 1:length(names(List.Acc))){
    #   for(k in 1:length(colnames(inc.reval))){
    #     
    #     inc.reval[grep(names(List.Acc)[j],rownames(inc.reval)),k] = sum(inc.reval.con[List.Acc[[j]],k])
    #     
    #   }
    # }
    # inc.reval["BS",] = inc.reval.con["BS",]
    # 
    # for(j in 2:length(BalanceSheet$branches)){
    #   for(k in 1:length(colnames(inc.reval))){
    #     inc.reval[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),k] = sum(na.omit(inc.reval[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(inc.reval)),k]))
    #   }
    # }
    # inv.reval = round(inc.reval,2)
    
    
    #------------ Equity - (Assets minus Liabilities) 
    equity=val.mark["BS",]
    
    ## equity-ratio
    equity.ratio = equity/val.mark["BS.Assets",]
    
    
    #------------ Netto-Profit 
    # positiv income - negativ income
    #profit.reval = inc.reval["BS",]
    profit.nominal = inc.nom["BS",]
    
    #------------ Stresstest (Parallele-Verschiebung der Zinsstrukturkurve) 
    set(yc.ecb, list(Nodes = list(ReferenceDate = ad, 
                                  Tenors = get(yc.ecb,what = "Tenors"), 
                                  Rates = get(yc.ecb,what = "Rates") + shift)))  
    
    # Events generieren 
    ev = events(bank, ad, rf)
    
    # Liquidity shocked
    liq.shocked = round(liquidity(bank,by,"marginal",tree=BalanceSheet)[,-1],2)
    colnames(liq.shocked) = years
    liq.shocked = as.matrix(liq.shocked)
    
    # Val.mark.shocked
    val.mark.con = matrix(NA,nrow = length(ids)+1,ncol = 1)
    for(j in 1:length(ids)){
      val.mark.con[j+1,] = value(bank,"markToModel",eng.list[ids[j]],select = list(ContractID = ids[j]))
    }
    colnames(val.mark.con) = "markToModel"
    RowNames = c("BS",ids)
    rownames(val.mark.con) = RowNames
    val.mark.con[1,] = sum(val.mark.con[2:(length(ids)+1),])
    
    val.mark.shocked = matrix(NA,nrow = length(BalanceSheet$leafs)+length(BalanceSheet$branches),ncol = 1)
    rownames(val.mark.shocked) = rownames(val.nom)
    colnames(val.mark.shocked) = "markToModel"
    for(j in 1:length(names(List.Acc))){
      val.mark.shocked[grep(names(List.Acc)[j],rownames(val.mark.shocked)),] = sum(val.mark.con[List.Acc[[j]],])
    }
    val.mark.shocked["BS",] = val.mark.con["BS",]
    
    for(j in 2:length(BalanceSheet$branches)){
      val.mark.shocked[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),] = sum(na.omit(val.mark.shocked[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(val.mark.shocked)),]))
    }
    val.mark.shocked = round(val.mark.shocked,2)
    
    # Equity shocked
    equity.shocked=val.mark.shocked["BS.Assets",]+val.mark.shocked["BS.Liabilities",] # note the negative sign of value on the liability side
    ## equity-ratio shocked
    equity.ratio.shocked = equity.shocked/val.mark.shocked["BS.Assets",]
    
    # Income shocked
    # Ohne Neubewertung
    inc.nom.shocked = round(income(bank,by,"marginal", revaluation.gains=FALSE,tree=BalanceSheet)[,-1],2)
    colnames(inc.nom.shocked) = years
    inc.nom.shocked = as.matrix(inc.nom.shocked)
    
    # # Mit Neubewertung
    # inc.reval.con = matrix(NA,nrow = length(ids)+1,ncol = length(years))
    # for(j in 1:length(ids)){
    #   inc.reval.con[j+1,] = income(bank,by,"marginal",eng.list[[ids[j]]], revaluation.gains=TRUE,select = list(ContractID=ids[j]))
    # }
    # colnames(inc.reval.con) = years
    # rownames(inc.reval.con) = c("BS",ids)
    # for(j in 1:length(colnames(inc.reval.con))){
    #   inc.reval.con[1,j] = sum(inc.reval.con[2:(length(ids)+1),j])
    # }
    # 
    # inc.reval.shocked = matrix(NA,nrow = length(BalanceSheet$leafs)+3,ncol = length(years))
    # 
    # rownames(inc.reval.shocked) = rownames(inc.nom.shocked)
    # colnames(inc.reval.shocked) = years
    # for(j in 1:length(names(List.Acc))){
    #   for(k in 1:length(colnames(inc.reval.shocked))){
    #     
    #     inc.reval.shocked[grep(names(List.Acc)[j],rownames(inc.reval.shocked)),k] = sum(inc.reval.con[List.Acc[[j]],k])
    #     
    #   }
    # }
    # inc.reval.shocked["BS",] = inc.reval.con["BS",]
    # for(j in 2:length(BalanceSheet$branches)){
    #   for(k in 1:length(colnames(inc.reval.shocked))){
    #     inc.reval.shocked[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),k] = sum(na.omit(inc.reval.shocked[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(inc.reval.shocked)),k]))
    #   }
    # }
    # inc.reval.shocked = round(inc.reval.shocked,2)
    
    # Profit shocked
    profit.nom.shocked = inc.nom.shocked["BS",]
    #profit.shocked = inc.reval.shocked["BS",]
    
    set(yc.ecb, list(Nodes = list(ReferenceDate = ad, 
                                  Tenors = get(yc.ecb,what = "Tenors"), 
                                  Rates = get(yc.ecb,what = "Rates") - shift))) 
    
    lei.vec[i] = list.data[[i]]$LegalEntityIDRecordCreator[1]
    
    #------------ Alle Resultate werden in einer Liste abgespeichert 
    res.list[[i]] = list(lei = list.data[[i]]$LegalEntityIDRecordCreator[1],
                         liq = liq,
                         liq.shocked = liq.shocked,
                         val.nom = val.nom, 
                         val.mark = val.mark,
                         inc.nom = inc.nom, 
                         #inc.reval = inc.reval,
                         equity = equity,
                         equity.ratio = equity.ratio,
                         Netto.Profit.Nominal = profit.nominal,
                         #Netto.Profit.Reval = profit.reval,
                         val.mark.shocked = val.mark.shocked,
                         equity.shocked = equity.shocked,
                         equity.ratio.shocked = equity.ratio.shocked, 
                         #Netto.Profit.shocked = profit.shocked,
                         Netto.Profit.Nominal.shocked = profit.nom.shocked, 
                         inc.nom.shocked = inc.nom.shocked
                         #inc.reval.shocked = inc.reval.shocked
    )
  }
  names(res.list) = lei.vec
  
  return(res.list)
}

#----- Funktion 2
AnalysisWithoutStresstest = function(list.data, ad = NA, timeBucketLength = 8)
  #--------- Variablen Beschreibung
  # list.data = Liste der BS der einzelnen Banken
  # ad = Analysedatum, ad = NA --> heutiges Datum
  # timeBucketLength = Länge des Analysezeitraums
{
  #------------ Librarys
  library(rActus)
  library(rflPortfolio)
  library(rflContracts)
  library(pdfetch)
  
  
  if(is.na(ad)){
    path.ad = as.character(Sys.Date())
    ad = paste(path.ad,"T00", sep = "")
  }
  
  #------------ Marktumgebung definieren: Zinsstrukturkurve 
  
  yc.ecb = YieldCurveECB(ad = ad,plot.yc = FALSE,type = "SR")
  
  #------------ Risikofaktormodell definieren
  rf = RFConn()
  add(rf, yc.ecb)
  
  
  #------------ Anzahl Contracts in jedem Datensatz
  # anz.contract = rep(NA,length(list.data))
  # for(i in 1:length(list.data)){
  #   anz.contract[i] = length(list.data[[i]]$ContractRole)
  # }
  
  #------------ Leere Liste für die einzelnen Resultate
  res.list = rep(list(NA),length(list.data))
  lei.vec = rep(NA,length(list.data))
  
  #------------ For-Schlaufe um alle BS zu analysieren
  
  for(i in 1:length(list.data)){
    
    #------------ Bank-Portfolio erstellen 
    bank=Portfolio()
    import(bank, list.data[[i]])
    ids=get(bank,"ids")
    bank
    
    #------------ Risikofaktormodell an Bank-Portfolio anhängen 
    set(bank, rf)
    
    
    #------------ Löschen der Child-Contracte aus dem Datensatz
    C.vec = c("C1","C2")
    C.ids = numeric()
    for(j in 1:length(C.vec)){
      C.ids = c(C.ids,grep(C.vec[j],list.data[[i]]$ContractID))
    }
    if(length(C.ids)==0){
      BS.data = list.data[[i]]
    }else{
      BS.data = list.data[[i]][-C.ids,]
    }
    
    #------------ DiscountEngine für jeden Contract definieren
    eng.list = rep(list(NA),length(ids))
    
    for(j in 1:length(eng.list)){
      eng.list[[j]] = DcEngine(list(DiscountingSpread=BS.data$Valuation_DiscountingSpread[j],
                                    RiskFactorObjectLink=BS.data$Valuation_RiskFactorObjektLink[j]))
      
      # Risikofaktormodell an DiscountEngine anhängen
      set(eng.list[[j]],rf)
    }
    names(eng.list) = BS.data$ContractID
    
    #------------ Balance Sheet definieren 
    BS.ContractRole = unique(BS.data$ContractRole)
    BS.SubAcc.list = rep(list(NA),length(BS.ContractRole))
    for(j in 1:length(BS.SubAcc.list)){
      BS.SubAcc.list[[j]] = BS.data$SubAccount[which(BS.data$ContractRole==BS.ContractRole[j])]
    }
    names(BS.SubAcc.list) = BS.ContractRole
    id.RPA = grep("RPA",names(BS.SubAcc.list))
    id.RPL = grep("RPL",names(BS.SubAcc.list))
    id.Offbalance = c(grep("RFL",names(BS.SubAcc.list)),
                      grep("PFL",names(BS.SubAcc.list))
    )
    
    temp = character()
    for(j in 1:length(id.Offbalance)){
      temp = c(temp,BS.SubAcc.list[[id.Offbalance[j]]])
    }
    BS.SubAcc.list = BS.SubAcc.list[-id.Offbalance]
    BS.SubAcc.list$Offbalance = temp
    names(BS.SubAcc.list)[id.RPA] = "Assets"
    names(BS.SubAcc.list)[id.RPL] = "Liabilities"
    
    #BS.names = rep(NA,length(unique(BS.data$ContractRole)))
    
    branche.list = list(names(BS.SubAcc.list))
    for(j in 1:length(BS.SubAcc.list)){
      branche.list[[j+1]] = unique(BS.SubAcc.list[[j]])
    }
    names(branche.list) = c("BS",names(BS.SubAcc.list))
    
    
    SubAcc = BS.data$SubAccount
    names(SubAcc) = BS.data$ContractID
    
    List.Acc = list(rep(list(NA),length(unique(SubAcc))))
    for(k in 1:length(unique(SubAcc))){
      List.Acc[[k]] = names(SubAcc[which(SubAcc == unique(SubAcc)[k])])
    }
    List.Acc
    names(List.Acc) = unique(SubAcc)
    
    # Balancesheet
    BalanceSheet = Tree(list(
      branches = branche.list,
      leafs = List.Acc
    ))
    
    
    #------------ Events generieren 
    ev = events(bank, ad, rf)
    
    #------------ Liquidität
    by=timeSequence(substring(ad,1,10),length.out=timeBucketLength+2,by="1 year")
    
    h = timeBucketLength 
    years = as.character(as.numeric(format(as.Date(ad),"%Y")):(as.numeric(format(as.Date(ad),"%Y"))+h))
    
    liq = round(liquidity(bank,by,"marginal",tree=BalanceSheet)[,-1],2)
    colnames(liq) = years
    liq = as.matrix(liq)
    
    #------------ Value 
    # Nominal
    val.nom = round(value(bank,"nominal",tree=BalanceSheet),2)
    
    # MarkToModel
    val.mark.con = matrix(NA,nrow = length(ids)+1,ncol = 1)
    for(j in 1:length(ids)){
      val.mark.con[j+1,] = value(bank,"markToModel",eng.list[ids[j]],select = list(ContractID = ids[j]))
    }
    colnames(val.mark.con) = "markToModel"
    RowNames = c("BS",ids)
    rownames(val.mark.con) = RowNames
    val.mark.con[1,] = sum(val.mark.con[2:(length(ids)+1),])
    
    val.mark = matrix(NA,nrow = length(BalanceSheet$leafs)+length(BalanceSheet$branches),ncol = 1)
    rownames(val.mark) = rownames(val.nom)
    colnames(val.mark) = "markToModel"
    for(j in 1:length(names(List.Acc))){
      val.mark[grep(names(List.Acc)[j],rownames(val.mark)),] = sum(val.mark.con[List.Acc[[j]],])
    }
    val.mark["BS",] = val.mark.con["BS",]
    
    for(j in 2:length(BalanceSheet$branches)){
      val.mark[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),] = sum(na.omit(val.mark[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(val.mark)),]))
    }
    val.mark = round(val.mark,2)
    
    #------------ Zins- Einkommen 
    # Ohne Neubewertung
    inc.nom = round(income(bank,by,"marginal", revaluation.gains=FALSE,tree=BalanceSheet)[,-1],2)
    colnames(inc.nom) = years
    inc.nom = as.matrix(inc.nom)
    
    # # Mit Neubewertung
    # inc.reval.con = matrix(NA,nrow = length(ids)+1,ncol = length(years))
    # for(j in 1:length(ids)){
    #   inc.reval.con[j+1,] = income(bank,by,"marginal",eng.list[[ids[j]]], revaluation.gains=TRUE,select = list(ContractID=ids[j]))
    # }
    # colnames(inc.reval.con) = years
    # rownames(inc.reval.con) = c("BS",ids)
    # for(j in 1:length(colnames(inc.reval.con))){
    #   inc.reval.con[1,j] = sum(inc.reval.con[2:(length(ids)+1),j])
    # }
    # 
    # inc.reval = matrix(NA,nrow = length(BalanceSheet$leafs)+3,ncol = length(years))
    # 
    # rownames(inc.reval) = rownames(inc.nom)
    # colnames(inc.reval) = years
    # for(j in 1:length(names(List.Acc))){
    #   for(k in 1:length(colnames(inc.reval))){
    #     
    #     inc.reval[grep(names(List.Acc)[j],rownames(inc.reval)),k] = sum(inc.reval.con[List.Acc[[j]],k])
    #     
    #   }
    # }
    # inc.reval["BS",] = inc.reval.con["BS",]
    # 
    # for(j in 2:length(BalanceSheet$branches)){
    #   for(k in 1:length(colnames(inc.reval))){
    #     inc.reval[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),k] = sum(na.omit(inc.reval[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(inc.reval)),k]))
    #   }
    # }
    # inv.reval = round(inc.reval,2)
    
    
    #------------ Equity - (Assets minus Liabilities) 
    equity=val.mark["BS",]
    
    ## equity-ratio
    equity.ratio = equity/val.mark["BS.Assets",]
    
    
    #------------ Netto-Profit 
    # positiv income - negativ income
    #profit.reval = inc.reval["BS",]
    profit.nominal = inc.nom["BS",]
    
    lei.vec[i] = list.data[[i]]$LegalEntityIDRecordCreator[1]
    
    #------------ Alle Resultate werden in einer Liste abgespeichert 
    res.list[[i]] = list(lei = list.data[[i]]$LegalEntityIDRecordCreator[1],
                         liq = liq, 
                         val.nom = val.nom, 
                         val.mark = val.mark,
                         inc.nom = inc.nom, 
                         #inc.reval = inc.reval,
                         equity = equity,
                         equity.ratio = equity.ratio,
                         Netto.Profit.Nominal = profit.nominal
                         #Netto.Profit.Reval = profit.reval
    )
  }
  names(res.list) = lei.vec
  
  return(res.list)
}

#----- Funktion 3
Stresstest = function(list.data, ad = NA, timeBucketLength = 8, shift = 0.07)
  #--------- Variablen Beschreibung
  # list.data = Liste der BS der einzelnen Banken
  # ad = Analysedatum, ad = NA --> heutiges Datum
  # timeBucketLength = Länge des Analysezeitraums
  # shift = Anpassung der Zinsstrukturkurve (1 Wert --> Paralleler Shift, Vektor --> Anpassung aller Zinsen einzeln)
{
  #------------ Librarys
  library(rActus)
  library(rflPortfolio)
  library(rflContracts)
  library(pdfetch)
  
  
  if(is.na(ad)){
    path.ad = as.character(Sys.Date())
    ad = paste(path.ad,"T00", sep = "")
  }
  
  #------------ Marktumgebung definieren: Zinsstrukturkurve 
  
  yc.ecb = YieldCurveECB(ad = ad,plot.yc = FALSE,type = "SR")
  
  #------------ Risikofaktormodell definieren
  rf = RFConn()
  add(rf, yc.ecb)
  
  
  #------------ Anzahl Contracts in jedem Datensatz
  # anz.contract = rep(NA,length(list.data))
  # for(i in 1:length(list.data)){
  #   anz.contract[i] = length(list.data[[i]]$ContractRole)
  # }
  
  #------------ Leere Liste für die einzelnen Resultate
  res.list = rep(list(NA),length(list.data))
  lei.vec = rep(NA,length(list.data))
  
  #------------ For-Schlaufe um alle BS zu analysieren
  
  for(i in 1:length(list.data)){
    
    #------------ Bank-Portfolio erstellen 
    bank=Portfolio()
    import(bank, list.data[[i]])
    ids=get(bank,"ids")
    bank
    
    #------------ Risikofaktormodell an Bank-Portfolio anhängen 
    set(bank, rf)
    
    
    #------------ Löschen der Child-Contracte aus dem Datensatz
    C.vec = c("C1","C2")
    C.ids = numeric()
    for(j in 1:length(C.vec)){
      C.ids = c(C.ids,grep(C.vec[j],list.data[[i]]$ContractID))
    }
    if(length(C.ids)==0){
      BS.data = list.data[[i]]
    }else{
      BS.data = list.data[[i]][-C.ids,]
    }
    
    #------------ DiscountEngine für jeden Contract definieren
    eng.list = rep(list(NA),length(ids))
    
    for(j in 1:length(eng.list)){
      eng.list[[j]] = DcEngine(list(DiscountingSpread=BS.data$Valuation_DiscountingSpread[j],
                                    RiskFactorObjectLink=BS.data$Valuation_RiskFactorObjektLink[j]))
      
      # Risikofaktormodell an DiscountEngine anhängen
      set(eng.list[[j]],rf)
    }
    names(eng.list) = BS.data$ContractID
    
    #------------ Balance Sheet definieren 
    BS.ContractRole = unique(BS.data$ContractRole)
    BS.SubAcc.list = rep(list(NA),length(BS.ContractRole))
    for(j in 1:length(BS.SubAcc.list)){
      BS.SubAcc.list[[j]] = BS.data$SubAccount[which(BS.data$ContractRole==BS.ContractRole[j])]
    }
    names(BS.SubAcc.list) = BS.ContractRole
    id.RPA = grep("RPA",names(BS.SubAcc.list))
    id.RPL = grep("RPL",names(BS.SubAcc.list))
    id.Offbalance = c(grep("RFL",names(BS.SubAcc.list)),
                      grep("PFL",names(BS.SubAcc.list))
    )
    
    temp = character()
    for(j in 1:length(id.Offbalance)){
      temp = c(temp,BS.SubAcc.list[[id.Offbalance[j]]])
    }
    BS.SubAcc.list = BS.SubAcc.list[-id.Offbalance]
    BS.SubAcc.list$Offbalance = temp
    names(BS.SubAcc.list)[id.RPA] = "Assets"
    names(BS.SubAcc.list)[id.RPL] = "Liabilities"
    
    #BS.names = rep(NA,length(unique(BS.data$ContractRole)))
    
    branche.list = list(names(BS.SubAcc.list))
    for(j in 1:length(BS.SubAcc.list)){
      branche.list[[j+1]] = unique(BS.SubAcc.list[[j]])
    }
    names(branche.list) = c("BS",names(BS.SubAcc.list))
    
    
    SubAcc = BS.data$SubAccount
    names(SubAcc) = BS.data$ContractID
    
    List.Acc = list(rep(list(NA),length(unique(SubAcc))))
    for(k in 1:length(unique(SubAcc))){
      List.Acc[[k]] = names(SubAcc[which(SubAcc == unique(SubAcc)[k])])
    }
    List.Acc
    names(List.Acc) = unique(SubAcc)
    
    # Balancesheet
    BalanceSheet = Tree(list(
      branches = branche.list,
      leafs = List.Acc
    ))
    
    
    #------------ Events generieren 
    ev = events(bank, ad, rf)
    
    #------------ Liquidität
    by=timeSequence(substring(ad,1,10),length.out=timeBucketLength+2,by="1 year")
    
    h = timeBucketLength 
    years = as.character(as.numeric(format(as.Date(ad),"%Y")):(as.numeric(format(as.Date(ad),"%Y"))+h))
    
    set(yc.ecb, list(Nodes = list(ReferenceDate = ad, 
                                  Tenors = get(yc.ecb,what = "Tenors"), 
                                  Rates = get(yc.ecb,what = "Rates") + shift)))  
    
    # Events generieren 
    ev = events(bank, ad, rf)
    
    # Liquidity shocked
    liq.shocked = round(liquidity(bank,by,"marginal",tree=BalanceSheet)[,-1],2)
    colnames(liq.shocked) = years
    liq.shocked = as.matrix(liq.shocked)
    
    # Nominal
    val.nom = round(value(bank,"nominal",tree=BalanceSheet),2)
    
    # Val.mark.shocked
    val.mark.con = matrix(NA,nrow = length(ids)+1,ncol = 1)
    for(j in 1:length(ids)){
      val.mark.con[j+1,] = value(bank,"markToModel",eng.list[ids[j]],select = list(ContractID = ids[j]))
    }
    colnames(val.mark.con) = "markToModel"
    RowNames = c("BS",ids)
    rownames(val.mark.con) = RowNames
    val.mark.con[1,] = sum(val.mark.con[2:(length(ids)+1),])
    
    val.mark.shocked = matrix(NA,nrow = length(BalanceSheet$leafs)+length(BalanceSheet$branches),ncol = 1)
    rownames(val.mark.shocked) = rownames(val.nom)
    colnames(val.mark.shocked) = "markToModel"
    for(j in 1:length(names(List.Acc))){
      val.mark.shocked[grep(names(List.Acc)[j],rownames(val.mark.shocked)),] = sum(val.mark.con[List.Acc[[j]],])
    }
    val.mark.shocked["BS",] = val.mark.con["BS",]
    
    for(j in 2:length(BalanceSheet$branches)){
      val.mark.shocked[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),] = sum(na.omit(val.mark.shocked[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(val.mark.shocked)),]))
    }
    val.mark.shocked = round(val.mark.shocked,2)
    
    # Equity shocked
    equity.shocked=val.mark.shocked["BS.Assets",]+val.mark.shocked["BS.Liabilities",] # note the negative sign of value on the liability side
    ## equity-ratio shocked
    equity.ratio.shocked = equity.shocked/val.mark.shocked["BS.Assets",]
    
    # Income shocked
    # Ohne Neubewertung
    inc.nom.shocked = round(income(bank,by,"marginal", revaluation.gains=FALSE,tree=BalanceSheet)[,-1],2)
    colnames(inc.nom.shocked) = years
    inc.nom.shocked = as.matrix(inc.nom.shocked)
    
    # # Mit Neubewertung
    # inc.reval.con = matrix(NA,nrow = length(ids)+1,ncol = length(years))
    # for(j in 1:length(ids)){
    #   inc.reval.con[j+1,] = income(bank,by,"marginal",eng.list[[ids[j]]], revaluation.gains=TRUE,select = list(ContractID=ids[j]))
    # }
    # colnames(inc.reval.con) = years
    # rownames(inc.reval.con) = c("BS",ids)
    # for(j in 1:length(colnames(inc.reval.con))){
    #   inc.reval.con[1,j] = sum(inc.reval.con[2:(length(ids)+1),j])
    # }
    # 
    # inc.reval.shocked = matrix(NA,nrow = length(BalanceSheet$leafs)+3,ncol = length(years))
    # 
    # rownames(inc.reval.shocked) = rownames(inc.nom.shocked)
    # colnames(inc.reval.shocked) = years
    # for(j in 1:length(names(List.Acc))){
    #   for(k in 1:length(colnames(inc.reval.shocked))){
    #     
    #     inc.reval.shocked[grep(names(List.Acc)[j],rownames(inc.reval.shocked)),k] = sum(inc.reval.con[List.Acc[[j]],k])
    #     
    #   }
    # }
    # inc.reval.shocked["BS",] = inc.reval.con["BS",]
    # for(j in 2:length(BalanceSheet$branches)){
    #   for(k in 1:length(colnames(inc.reval.shocked))){
    #     inc.reval.shocked[paste("BS.",names(BalanceSheet$branches)[j],sep = ""),k] = sum(na.omit(inc.reval.shocked[grep(paste("BS.",names(BalanceSheet$branches)[j],sep = ""),rownames(inc.reval.shocked)),k]))
    #   }
    # }
    # inc.reval.shocked = round(inc.reval.shocked,2)
    
    # Profit shocked
    profit.nom.shocked = inc.nom.shocked["BS",]
    #profit.shocked = inc.reval.shocked["BS",]
    
    set(yc.ecb, list(Nodes = list(ReferenceDate = ad, 
                                  Tenors = get(yc.ecb,what = "Tenors"), 
                                  Rates = get(yc.ecb,what = "Rates") - shift))) 
    
    lei.vec[i] = list.data[[i]]$LegalEntityIDRecordCreator[1]
    
    #------------ Alle Resultate werden in einer Liste abgespeichert 
    res.list[[i]] = list(lei = list.data[[i]]$LegalEntityIDRecordCreator[1],
                         liq.shocked = liq.shocked,
                         val.mark.shocked = val.mark.shocked,
                         equity.shocked = equity.shocked,
                         equity.ratio.shocked = equity.ratio.shocked, 
                         #Netto.Profit.shocked = profit.shocked,
                         Netto.Profit.Nominal.shocked = profit.nom.shocked, 
                         inc.nom.shocked = inc.nom.shocked
                         #inc.reval.shocked = inc.reval.shocked
    )
  }
  names(res.list) = lei.vec
  
  return(res.list)
}

#----- Funktion 4
YieldCurveECB = function(ad = NA,type = "SR",plot.yc = FALSE,lines = FALSE,shift.list)
  #--------- Variablen Beschreibung
  # ad = Analysedatum, ad = NA --> heutiges Datum
  # plot.yc = FALSE -> kein Plot, TRUE -> Plot
  # type = ("SR"|"IF"), SR = Spot Rates, IF = Instantaneous Forward Rates
  # shift_Plot = Plot der Zinskurve mit definiertem Shift
  # shift = Anpassung der Zinsstrukturkurve (1 Wert --> Paralleler Shift, Vektor --> Anpassung aller Zinsen einzeln)
{
  #------------ Librarys
  library(rActus)
  library(rflPortfolio)
  library(rflContracts)
  library(pdfetch)
  
  
  if(is.na(ad)){
    path.ad = as.character(Sys.Date())
    ad = paste(path.ad,"T00", sep = "")
  }
  
  #------------ Marktumgebung definieren: Zinsstrukturkurve (Spot Rates)
  
  yc = timeSeries(
    pdfetch_ECB(c("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_3M", 
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_6M",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_9M",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_1Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_2Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_3Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_5Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_7Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_10Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_15Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_20Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_25Y",
                  "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_30Y")))    
  
  if(type == "IF"){
    yc = timeSeries(
      pdfetch_ECB(c("YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_3M", 
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_6M",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_9M",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_1Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_2Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_3Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_5Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_7Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_10Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_15Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_20Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_25Y",
                    "YC.B.U2.EUR.4F.G_N_A.SV_C_YM.IF_30Y")))    
  }
  
  
  yc.ea = as.data.frame(yc)
  
  if(is.na(yc.ea[ad,]$YC.B.U2.EUR.4F.G_N_A.SV_C_YM.SR_3M)){
    yc.ea = yc.ea[dim(yc.ea)[1],]
  }else{
    yc.ea = yc.ea[ad,]
  }
  
  colnames(yc.ea)=paste("", c("3M","6M","9M","1Y","2Y","3Y","5Y","7Y","10Y","15Y","20Y","25Y","30Y"),
                        sep="")
  
  yc.tnr = colnames(yc.ea)
  yc.rts = yc.ea/100
  
  yc.ecb = YieldCurve(
    what = list(
      MarketObjectCode = "YC_ECB",
      Nodes = list(ReferenceDate = ad, 
                   Tenors = yc.tnr, Rates = yc.rts)))
  
  if(plot.yc == TRUE){
    tenors = c(3/12,6/12,9/12,1,2,3,5*12/12,7*12/12,
               10*12/12,15*12/12,20*12/12,25*12/12,30*12/12)
    
    plot(tenors,get(yc.ecb,what = "Rates"),type = "l",xlab = "Tenors",ylab = "Rates",
         lwd = 2,las = 2,xaxt = "n",main = paste("YC_ECB",", Date: ",ad))
    axis(1,at=tenors,labels = F)
    text(tenors,par("usr")[3]-0.001,labels = get(yc.ecb,what = "Tenors"),xpd = T)
  }
  
  
  if(lines == TRUE){
    
    range.max = numeric()
    range.min = numeric()
    for(j in 1:length(shift.list)){
      range.max[j] = max(get(yc.ecb,what = "Rates")+shift.list[[j]])
      range.min[j] = min(get(yc.ecb,what = "Rates")+shift.list[[j]])
    }
    
    tenors = c(3/12,6/12,9/12,1,2,3,5*12/12,7*12/12,
               10*12/12,15*12/12,20*12/12,25*12/12,30*12/12)
    
    plot(tenors,get(yc.ecb,what = "Rates"),type = "l",xlab = "Tenors",ylab = "Rates",
         lwd = 2,las = 2,xaxt = "n",main = paste("YC_ECB",", Date: ",ad),
         ylim = range(range.max,range.min,get(yc.ecb,what = "Rates")))
    
    axis(1,at=tenors,labels = F)
    text(tenors,par("usr")[3]-0.001,labels = get(yc.ecb,what = "Tenors"),xpd = T)
    
    for(j in 1:length(shift.list)){
      lines(tenors,get(yc.ecb,what = "Rates")+shift.list[[j]],col = j+1,lwd = 2)
    }
    legend("topleft",inset = 0.01 ,legend = paste("Scenario ",1:length(shift.list)),
           col = 2:(length(shift.list)+1),lty = 1,lwd = 2,box.lty = 0,cex = 0.7)
    
    
  }
  
  return(yc.ecb)
}

#----- Funktion 5
MC.Liquidity = function(list.data, ad = NA, timeBucketLength = 8,shock.data)
  #--------- Variablen Beschreibung
  # list.data = Liste der BS der einzelnen Banken
  # ad = Analysedatum, ad = NA --> heutiges Datum
  # timeBucketLength = Länge des Analysezeitraums
  # shock.data = Liste der einzelnen Shocks für die Zinskurve
{
  #------------ Librarys
  library(rActus)
  library(rflPortfolio)
  library(rflContracts)
  library(pdfetch)
  
  
  if(is.na(ad)){
    path.ad = as.character(Sys.Date())
    ad = paste(path.ad,"T00", sep = "")
  }
  
  #------------ Marktumgebung definieren: Zinsstrukturkurve 
  
  yc.ecb = YieldCurveECB(ad = ad,plot.yc = FALSE,type = "SR")
  
  #------------ Risikofaktormodell definieren
  rf = RFConn()
  add(rf, yc.ecb)
  
  
  #------------ Leere Liste für die einzelnen Resultate
  res.list = rep(list(NA),length(list.data))
  lei.vec = rep(NA,length(list.data))
  
  
  #------------ For-Schlaufe um alle BS zu analysieren
  for(i in 1:length(list.data)){
    
    #------------ Bank-Portfolio erstellen 
    bank=Portfolio()
    import(bank, list.data[[i]])
    ids=get(bank,"ids")
    bank
    
    #------------ Risikofaktormodell an Bank-Portfolio anhängen 
    set(bank, rf)
    
    
    #------------ Löschen der Child-Contracte aus dem Datensatz
    C.vec = c("C1","C2")
    C.ids = numeric()
    for(j in 1:length(C.vec)){
      C.ids = c(C.ids,grep(C.vec[j],list.data[[i]]$ContractID))
    }
    if(length(C.ids)==0){
      BS.data = list.data[[i]]
    }else{
      BS.data = list.data[[i]][-C.ids,]
    }
    
    #------------ DiscountEngine für jeden Contract definieren
    eng.list = rep(list(NA),length(ids))
    
    for(j in 1:length(eng.list)){
      eng.list[[j]] = DcEngine(list(DiscountingSpread=BS.data$Valuation_DiscountingSpread[j],
                                    RiskFactorObjectLink=BS.data$Valuation_RiskFactorObjektLink[j]))
      
      # Risikofaktormodell an DiscountEngine anhängen
      set(eng.list[[j]],rf)
    }
    names(eng.list) = BS.data$ContractID
    
    #------------ Balance Sheet definieren 
    BS.ContractRole = unique(BS.data$ContractRole)
    BS.SubAcc.list = rep(list(NA),length(BS.ContractRole))
    for(j in 1:length(BS.SubAcc.list)){
      BS.SubAcc.list[[j]] = BS.data$SubAccount[which(BS.data$ContractRole==BS.ContractRole[j])]
    }
    names(BS.SubAcc.list) = BS.ContractRole
    id.RPA = grep("RPA",names(BS.SubAcc.list))
    id.RPL = grep("RPL",names(BS.SubAcc.list))
    names(BS.SubAcc.list)[id.RPA] = "Assets"
    names(BS.SubAcc.list)[id.RPL] = "Liabilities"
    
    BS.names = rep(NA,length(unique(BS.data$ContractRole)))
    
    branche.list = list(names(BS.SubAcc.list))
    for(j in 1:length(BS.SubAcc.list)){
      branche.list[[j+1]] = unique(BS.SubAcc.list[[j]])
    }
    names(branche.list) = c("BS",names(BS.SubAcc.list))
    
    
    SubAcc = BS.data$SubAccount
    names(SubAcc) = BS.data$ContractID
    
    List.Acc = list(rep(list(NA),length(unique(SubAcc))))
    for(k in 1:length(unique(SubAcc))){
      List.Acc[[k]] = names(SubAcc[which(SubAcc == unique(SubAcc)[k])])
    }
    #List.Acc
    names(List.Acc) = unique(SubAcc)
    
    # Balancesheet
    BalanceSheet = Tree(list(
      branches = branche.list,
      leafs = List.Acc
    ))
    
    #------------ Events generieren 
    ev = events(bank, ad, rf)
    
    #------------ Liquidität
    by=timeSequence(substring(ad,1,10),length.out=timeBucketLength+2,by="1 year")
    
    h = timeBucketLength 
    years = as.character(as.numeric(format(as.Date(ad),"%Y")):(as.numeric(format(as.Date(ad),"%Y"))+h))
    
    liq = round(liquidity(bank,by,"marginal")[-1],2)
    names(liq) = years
    
    #------------ Liquiditäts - Stresstest über alle Shocks
    for(j in 1:nrow(shock.data)){
      set(yc.ecb, list(Nodes = list(ReferenceDate = ad, 
                                    Tenors = get(yc.ecb,what = "Tenors"), 
                                    Rates = get(yc.ecb,what = "Rates") + shock.data[j,3:15]))) 
      
      # Liquidität 
      ev = events(bank, ad, rf)
      liq = rbind(liq,round(liquidity(bank,by,"marginal")[-1],2))
      rownames(liq)[j+1] = shock.data$ID[j]
      
      set(yc.ecb, list(Nodes = list(ReferenceDate = ad, 
                                    Tenors = get(yc.ecb,what = "Tenors"), 
                                    Rates = get(yc.ecb,what = "Rates") - shock.data[j,3:15]))) 
    }
    
    
    
    lei.vec[i] = list.data[[i]]$LegalEntityIDRecordCreator[1]
    
    #------------ Alle Resultate werden in einer Liste abgespeichert 
    res.list[[i]] = list(lei = list.data[[i]]$LegalEntityIDRecordCreator[1],
                         MC.liq = liq
    )
  }
  names(res.list) = lei.vec
  
  return(res.list)
}

#----- Funktion 6
Hist.MC.Liquidity = function(res.list)
  #--------- Variablen Beschreibung
  # res.list = MC.Liquidity Resultat EINER Bank! 
{
  par(mfrow = c(3,ceiling(ncol(res.list$MC.liq)/3)))
  for(i in 1:ncol(res.list$MC.liq)){
    hist(res.list$MC.liq[,i],main = colnames(res.list$MC.liq)[i],xlab = "EUR")
    # VaR 5% und 1%
    abline(v = quantile(res.list$MC.liq[,i],0.05), col = "red")
    abline(v = quantile(res.list$MC.liq[,i],0.01), col = "blue")
  }
}

#----- Funktion 7
Box.MC.Liquidity = function(res.list)
  #--------- Variablen Beschreibung
  # res.list = MC.Liquidity Resultat EINER Bank! 
{
  par(mfrow = c(3,ceiling(ncol(res.list$MC.liq)/3)))
  for(i in 1:ncol(res.list$MC.liq)){
    boxplot(res.list$MC.liq[,i],main = colnames(res.list$MC.liq)[i])
  }
}

#----- Funktion 8
Barplot.BS = function(data,id.year = 1)
  #--------- Variablen Beschreibung
  # data = Resultate welche dargestellt werden sollen
  # id.year = Index des Jahres welches dargestellt werden soll
{
  
  
  vec = c("BS","BS.Assets","BS.Liabilities","BS.Offbalance")
  id.x = numeric()
  for(i in 1:length(vec)){
    id.x[i] = which(rownames(data) == vec[i])
  }
  
  data.sort = rbind(data[id.x,],data[-id.x,])
  
  col = character()
  y.pos = numeric()
  for(i in 1:length(data.sort[,id.year])){
    if(data.sort[i,id.year] < 0){
      col[i] = "tomato"
      y.pos[i] = 0
    }else{
      col[i] = "lightgreen"
      y.pos[i] = data.sort[i,id.year]
    }
  }
  
  
  xx = barplot(data.sort[,id.year],col = col,ylim = c(min(data.sort[,id.year]),max(data.sort[,id.year])+20000),
               axis.lty = 2,space = 1,xaxt = "n",las = 2,main = colnames(data)[id.year])
  
  axis(side = 1,at = xx,labels = F)
  text(x = xx, y = y.pos, label = data.sort[,id.year], pos = 3, cex = 0.7, col = "black")
  text(x = xx, par("usr")[3]-2000,srt = 25,adj = 1,xpd = T,labels = rownames(data.sort),cex = 0.7)
}
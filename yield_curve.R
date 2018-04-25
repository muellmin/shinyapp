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

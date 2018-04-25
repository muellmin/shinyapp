library(shiny)
library(shinydashboard)
library(htmlwidgets)
library(shinyIncubator)

library(dplyr)
library(plyr)

library(leaflet)
library(rgdal)
library(shinyjs)
library(DT)

library(rActus)
library(rflPortfolio)
library(rflContracts)
library(pdfetch)
#install.packages("rJava")
library(rJava)

setwd("C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/")
rm(list= ls())

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "BA_FS2018",
                                    tags$li(class = "dropdown", 
                                            tags$a(href = "#shiny-tab-home","Home", "data-toggle" = "tab")),
                                    tags$li(class = "dropdown", 
                                            tags$a(href = "#shiny-tab-network","Network", "data-toggle" = "tab")),
                                    tags$li(class = "dropdown", 
                                            tags$a(href = "#shiny-tab-bank","Bank infomation", "data-toggle" = "tab")),
                                    tags$li(class = "dropdown", 
                                            tags$a(href = "#shiny-tab-contracts","Contracts", "data-toggle" = "tab")),
                                    tags$li(class = "dropdown", 
                                            tags$a(href = "#shiny-tab-market","Market", "data-toggle" = "tab")),
                                    tags$li(class = "dropdown", 
                                            tags$a(href = "#shiny-tab-stress","Stresstests", "data-toggle" = "tab")),
                                    tags$li(class = "dropdown",
                                            tags$a(href="https://www.zhaw.ch/de/engineering/institute-zentren/idp/", target="_blank", 
                                                   tags$img(height = "40px", src="https://digitalcollection.zhaw.ch/image/logo_zhaw_ohne_byline_medium.png")
                                            ))
                    ),
                    dashboardSidebar(
                      sidebarMenu(id = "tabs",
                                  br(),
                                  dateInput('date', label = 'Date input', value = Sys.Date(), format = "yyyy-mm-dd"),
                                  div(style="display:inline-block",actionButton('update', 'Update all data', icon = icon("refresh"), width = "200px"), style="display:center-align"),
                                  br(),
                                  menuItem("Home", tabName = "home", icon = icon("home")),
                                  menuItem("Network", tabName = "network", icon = icon("connectdevelop"),
                                           radioButtons(inputId = "mapView", label = "View", choices = c("show only map", "show banks", "show connections"), selected = "show banks")),
                                  menuItem("Bank information", tabName = "bank", icon = icon("building"),
                                           radioButtons(inputId = "bankView", label = "View", choices = c("overview", "show one bank", "show multiple banks", "normal vs shocked"), selected = "overview"),
                                           sliderInput("timeBucketLength", "time bucket length:", min = 1, max = 10, value = 8, step = 1),
                                           radioButtons(inputId = "stressTest1", label = "Stresstest", choices = c("no", "yes"), selected = "no"),
                                           conditionalPanel(
                                             condition = "input.bankView == 'show one bank'",
                                             selectInput(inputId = "bankList1", label = "List of banks:", choices = NULL)),
                                           conditionalPanel(
                                             condition = "input.bankView == 'show multiple banks'",
                                             selectInput(inputId = "bankList21", label = "First bank:", choices = NULL),
                                             selectInput(inputId = "bankList22", label = "Second bank:", choices = NULL)),
                                           conditionalPanel(
                                             condition = "input.bankView == 'normal vs shocked'",
                                             selectInput(inputId = "bankList2", label = "List of banks:", choices = NULL)),
                                           conditionalPanel(
                                             condition = "input.stressTest1 == 'yes' || input.bankView == 'normal vs shocked'",
                                             radioButtons(inputId = "scenario", label = "Scenarios:", choices = c("scenario 1", "scenario 2", "scenario 3"), selected = "scenario 1"))),
                                  menuItem("Contracts", tabName = "contracts", icon = icon("file")),
                                  menuItem("Market", tabName = "market", icon = icon("globe")),
                                  menuItem("Stresstests", tabName = "stress", icon = icon("cogs"))
                      )
                    ),
                    dashboardBody(
                      includeCSS("code/www/custom.css"),
                      tabItems(
                        tabItem("home",
                                tags$h1("Real-Time Monitoring of Financial Systems with ACTUS"),
                                br(),
                                tags$b("Betreuer: Wolfgang Breymann"),br(),
                                tags$b("Nebenbetreuer: Nils Andri Bundi"),
                                br(),
                                br(),
                                tags$p("Financial systems are complex networks of banks (as network nodes) and payment obligations among 
                                       these banks (as network edges). In such a system, two main sources of risk are considered; (1) the risk of banks to default individually due to 
                                       some exogenous shock to their capital buffer, and (2) the risk of default contagion or, in other words, the risk of the default of one bank 
                                       spreading to another bank in the network due to shared network links. The last financial crisis has shown that the latter risk can be 
                                       considerable, yet current regulations such as capital requirements or the liquidity coverage ratio focus on assessing the health of banks 
                                       individually and thereby disregard network effects.
                                       In this Bachelor Thesis, we aim at developing a graphical interface that allows to dynamically specify simple toy bank networks with nodes 
                                       consisting of a set of ACTUS contracts that give rise to payment obligations among banks. These 
                                       obligations may be dependent upon some market risk factors (e.g. interest payments in variable 
                                       rate bonds) which build the source for exogenous shocks to the toy bank network. Based on such a 
                                       set-up the students should develop and implement meaningful monitoring and visualization tools 
                                       such as total payment obligations between any two banks (net liquidity) that allows the regulator 
                                       to assess in real-time the financial health of the network. Finally, the effect of adding new ACTUS 
                                       contracts among any two banks in the network should be analyzed.")),
                        tabItem("network",
                                fluidPage(
                                  tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
                                  leafletOutput("mymap")
                                )),
                        tabItem("bank", uiOutput("bankViewUI")),
                        tabItem("contracts", uiOutput("contractViewUI")),
                        tabItem("market", uiOutput("marketViewUI")),
                        tabItem("stress", uiOutput("stressViewUI"))
                                ),
                      br(),
                      br(),
                      hr(),
                      tags$footer("by Denis Iseli, Dominique Neff und Mino Müller, WI15b, ZHAW", align = "center")
                      )
)

server <- function(input, output, session) {
  source("C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Code/Funktionen.R")
  
  selDate <- reactive({
    path.ad = as.character(input$date)
    ad = paste(path.ad,"T00", sep = "")
    return(ad)
  })
  
  ImportBankCSV <- reactive({
    list.filenames <- list.files(path = "C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Bank_Info_Data/")
    list.filenames
    list.data<-list()
    for (i in 1:length(list.filenames))
    {
      list.data[[i]] <- read.csv(paste("C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Bank_Info_Data/", list.filenames[i], sep = ""), header = T, sep = ";")
    }
    
    names(list.data) <- list.filenames
    return(list.data)                    
  })
  
  ImportContractCSV <- reactive({
    list.filenames <- list.files(path = "C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Bank_BS_Data/")
    list.filenames
    list.data<-list()
    for (i in 1:length(list.filenames))
    {
      list.data[[i]] <- read.csv(paste("C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Bank_BS_Data/", list.filenames[i], sep = ""), header = T, sep = ";")
    }
    
    names(list.data) <- list.filenames
    return(list.data)                    
  })

  ImportBankDf <- reactive(
    ldply(ImportBankCSV(), data.frame)
  )
  
  ImportContractDF <- reactive(
    ldply(ImportContractCSV(), data.frame)
  )
  
  BankList <- reactive(
    ImportBankDf()$BankName
  )
  
  output$bankOverview <- renderDataTable({
    temp <- ImportBankDf()
    data <- data.frame(temp$BankName, temp$LegalEntityID, temp$Street, temp$Number, temp$ZipCode, temp$City, temp$Country)
    colnames(data) <- c("BankName", "LegalEntityID", "Street", "Number", "ZipCode", "City", "Country")
    return(data)
  })
  #--------------------------------------
  #Teil EZB
  values <- reactiveValues()
  values$shockTM <- list()
  values$TM.shocked <- list()
  
  ImportContractCSVezb <- reactive({
    list.filenames <- list.files(path = "C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Kontrakte_EZB/")
    list.filenames
    list.data<-list()
    for (i in 1:length(list.filenames))
    {
      list.data[[i]] <- read.csv(paste("C:/Users/Mino/Documents/ZHAW/EZB/shinyapp/Kontrakte_EZB/", list.filenames[i], sep = ""), header = T, sep = ";")
    }
    
    names(list.data) <- list.filenames
    return(list.data)                    
  })
  #Übersicht BalanceSheet
  bankOverview.BS <- reactive(
    bs.fun(ImportContractCSVezb())
  )

  output$BS.1 <- renderTable({
    x <- as.data.frame(bankOverview.BS()[[1]])
    colnames(x) <- c("nominal", "markToModel")
    return(x)
  }, rownames = TRUE)
  output$BS.2 <- renderTable({
    x <- as.data.frame(bankOverview.BS()[[2]])
    colnames(x) <- c("nominal", "markToModel")
    return(x)
  }, rownames = TRUE)
  output$BS.3 <- renderTable({
    x <- as.data.frame(bankOverview.BS()[[3]])
    colnames(x) <- c("nominal", "markToModel")
    return(x)
  }, rownames = TRUE)
  
  #Erstellung TransitionsMatrix
  bankOverview.TM <- reactive({
    temp <- trans.matrix(ImportContractCSVezb())
    isolate( values$TM.shocked <- temp)
    return(temp)
  })
  
  output$TS.1 <- renderTable({
    x <- as.data.frame(bankOverview.TM()[1])
    colnames(x) <- rownames(x)
    return(x)
  }, rownames = TRUE)
  output$TS.2 <- renderTable({
    x <- as.data.frame(bankOverview.TM()[2])
    colnames(x) <- rownames(x)
    return(x)
  }, rownames = TRUE)
  output$TS.3 <- renderTable({
    x <- as.data.frame(bankOverview.TM()[3])
    colnames(x) <- rownames(x)
    return(x)
  }, rownames = TRUE)
  
  AccNames <- reactive({
    
    result <- NULL
    for(i in 1:length(t)){
      result <- c(result, rownames(bankOverview.TM()[[i]]))
    }
    unique(result)
  })

  
  getShockTransMatrix <- observeEvent(input$addShock,{
    temp <- list(c(input$KontoName, input$SchocktransM))
    values$shockTM <- c(values$shockTM, temp)
    print(values$shockTM)
    isolate(values$shockTM)
  })
  
  calcMatrix <- observeEvent(input$calShock, {
    
    temp <- values$shockTM
    temp.m <- bankOverview.TM()
    for(p in 1:length(temp.m)){
      for(i in 1:length(temp)){
      
        tf <- colnames(temp.m[[p]]) == temp[[i]][1]
      
        diag(temp.m[[p]])[tf] <-  diag(temp.m[[p]])[tf]+as.numeric(temp[[i]][2])
        
      }
      
    }
    isolate(values$TM.shocked <- temp.m)
    isolate(values$shockTM <- list())
  })
  
  output$TS.shock.1 <- renderTable({
    x <- as.data.frame(values$TM.shocked[[1]])
    colnames(x) <- rownames(x)
    return(x)
  }, rownames = TRUE)
  output$TS.shock.2 <- renderTable({
    x <- as.data.frame(values$TM.shocked[[2]])
    colnames(x) <- rownames(x)
    return(x)
  }, rownames = TRUE)
  output$TS.shock.3 <- renderTable({
    x <- as.data.frame(values$TM.shocked[[3]])
    colnames(x) <- rownames(x)
    return(x)
  }, rownames = TRUE)
  
#------------------------------------------
  
  
  
  output$contractOverview <- renderDataTable(
    ImportContractDF(),
    options = list(scrollX = TRUE)
  )
  
  output$selectUI1 <- renderText({
    return(BankList())
  })
  
  timeBucketLength <- reactive({
    as.integer(input$timeBucketLength)
  })
  
  res.list.without <- reactive({
    withProgress(message = 'calculation in progress', value = 0, {
      res.list.without <- rep(list(NA), 10)
      for (i in 1:10){
        res.list.without[[i]] <- AnalysisWithoutStresstest(list.data = ImportContractCSV(), ad = selDate(), timeBucketLength = i)
        incProgress(1/10, detail = paste("end of part", i))
      }
    })
    return(res.list.without)
  })
  
  res.list.with <- reactive({
    withProgress(message = 'calculation in progress', value = 0, {
      for (i in 1:1){
        res.list.with <- AnalysisWithStresstest(list.data = ImportContractCSV(), ad = selDate(), timeBucketLength = timeBucketLength(), shift = scenario())
        incProgress(1/1, detail = paste("end of part", 1))
      }
    })
    return(res.list.with)
  })
  
  res.list.vs <- reactive({
    withProgress(message = 'calculation in progress', value = 0, {
      for (i in 1:1){
        res.list.vs <- AnalysisWithStresstest(list.data = ImportContractCSV(), ad = selDate(), timeBucketLength = timeBucketLength(), shift = scenario())
        incProgress(1/1, detail = paste("end of part", 1))
      }
    })
    return(res.list.vs)
  })
  
  res.list <- reactive({
    res.list <- NULL
    if (input$stressTest1 == "no"){
      res.list <- res.list.without()[[timeBucketLength()]]
    } else if (input$stressTest1 == "yes"){
      res.list <- res.list.with()
    }
    return(res.list)
  })
  
  scenario <- reactive({
    scenario <- NULL
    if (input$scenario == "scenario 1"){
      scenario <- input$scen1
    } else if (input$scenario == "scenario 2"){
      scenario <- input$scen2
    } else if (input$scenario == "scenario 3"){
      scenario <- input$scen3
    }
    return(scenario)
  })
  
  # res.list.3 <- reactive({
  #           Stresstest(list.data = ImportContractCSV(), ad = selDate(), timeBucketLength = 8, shift = 0.07)
  # })
  
  ContractList <- reactive({
    res.list.1()
  })
  
  observeEvent(input$update, {
    updateSelectInput(session, "bankList1", choices = BankList())
    updateSelectInput(session, "bankList21", choices = BankList())
    updateSelectInput(session, "bankList22", choices = BankList())
    updateSelectInput(session, "bankList2", choices = BankList())
  })
  
  B1 <- reactive({
    x <- grep(input$bankList1,ImportBankDf()$BankName)
    y <- as.character(ImportBankDf()[x,"LegalEntityID"])
    return(y)
  })
  
  B2 <- reactive({
    x <- grep(input$bankList21,ImportBankDf()$BankName)
    y <- as.character(ImportBankDf()[x,"LegalEntityID"])
    return(y)
  })
  
  B3 <- reactive({
    x <- grep(input$bankList22,ImportBankDf()$BankName)
    y <- as.character(ImportBankDf()[x,"LegalEntityID"])
    return(y)
  })
  
  B4 <- reactive({
    x <- grep(input$bankList2,ImportBankDf()$BankName)
    y <- as.character(ImportBankDf()[x,"LegalEntityID"])
    return(y)
  })
  
  # Leaflet TEST
  output$mymap <- renderLeaflet({
    banks <- ImportBankDf()
    
    leaflet() %>% setView(lng = 8.729535, lat = 47.29502, zoom = 6) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      
      addCircles(lng = ~Long, lat = ~Lat, weight = 1, radius = 20000, popup = paste("<b>", banks$BankName, "</b><br>",
                                                                                    "LEI:", banks$LegalEntityID, "<br>",
                                                                                    "Longitude:", banks$Long, "<br>",
                                                                                    "Latitude:", banks$Lat), data = banks) %>%
      
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"))
  })
  
  output$bankViewUI <- renderUI(if (input$bankView == "show one bank") {
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 100px}"))),
      infoBoxOutput("BankName1", width = 12),
      valueBoxOutput("BankInfo1", width = 3),
      valueBoxOutput("BankInfo2", width = 3),
      valueBoxOutput("BankInfo3", width = 3),
      valueBoxOutput("BankInfo4", width = 3),
      tabBox(width = 12, side = "left",
             tabPanel("Liquidity", dataTableOutput(outputId = "T1T1", height = "450px")),
             tabPanel("Income nom", dataTableOutput(outputId = "T1T2", height = "450px")),
             tabPanel("Income reval", dataTableOutput(outputId = "T1T3", height = "450px")),
             tabPanel("BalanceSheet", tableOutput("BS.1")),#Teil EZB
             tabPanel("TransitionsMatrix", tableOutput("TS.1"))), #Teil EZB
      tabBox(width = 12, side = "left",
             tabPanel("Annual liquidity", plotOutput(outputId = "S1T1", height = "450px")),
             tabPanel("Annual Interest Rate Income", plotOutput(outputId = "S1T2", height = "450px")),
             tabPanel("Annual Interest Rate Income With Revaluation Gains", plotOutput(outputId = "S1T3", height = "450px"))))
  } else if (input$bankView == "overview") {
    fluidPage(
      dataTableOutput("bankOverview")
      )
  } else if (input$bankView == "show multiple banks") {
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 100px}"))),
      infoBoxOutput("BankName21", width = 6),
      infoBoxOutput("BankName22", width = 6),
      
      valueBoxOutput("BankInfo211", width = 3),
      valueBoxOutput("BankInfo212", width = 3),
      
      valueBoxOutput("BankInfo221", width = 3),
      valueBoxOutput("BankInfo222", width = 3),
      
      valueBoxOutput("BankInfo213", width = 3),
      valueBoxOutput("BankInfo214", width = 3),
      
      valueBoxOutput("BankInfo223", width = 3),
      valueBoxOutput("BankInfo224", width = 3),
      tabBox(width = 6, side = "left",
             tabPanel("Liquidity", dataTableOutput(outputId = "T2T1", height = "450px")),
             tabPanel("Income nom", dataTableOutput(outputId = "T2T2", height = "450px")),
             tabPanel("Income reval", dataTableOutput(outputId = "T2T3", height = "450px")),
             tabPanel("BalanceSheet", tableOutput("BS.2")), #Teil EZB),
             tabPanel("TransitionsMatrix", tableOutput("TS.2"))), #Teil EZB
      tabBox(width = 6, side = "left",
             tabPanel("Liquidity", dataTableOutput(outputId = "T3T1", height = "450px")),
             tabPanel("Income nom", dataTableOutput(outputId = "T3T2", height = "450px")),
             tabPanel("Income reval", dataTableOutput(outputId = "T3T3", height = "450px")),
             tabPanel("BalanceSheet", tableOutput("BS.3")), #Teil EZB),
             tabPanel("TransitionsMatrix", tableOutput("TS.3"))), #Teil EZB
      tabBox(width = 6, side = "left",
             tabPanel("Annual liquidity", plotOutput(outputId = "M1T1", height = "450px")),
             tabPanel("Annual Interest Rate Income", plotOutput(outputId = "M1T2", height = "450px")),
             tabPanel("Annual Interest Rate Income With Revaluation Gains", plotOutput(outputId = "M1T3", height = "450px"))),
      tabBox(width = 6, side = "left",
             tabPanel("Annual liquidity", plotOutput(outputId = "M2T1", height = "450px")),
             tabPanel("Annual Interest Rate Income", plotOutput(outputId = "M2T2", height = "450px")),
             tabPanel("Annual Interest Rate Income With Revaluation Gains", plotOutput(outputId = "M2T3", height = "450px"))))
  } else if (input$bankView == "normal vs shocked") {
    fluidRow(
      tags$head(tags$style(HTML(".small-box {height: 100px}"))),
      infoBoxOutput("BankName301", width = 6),
      infoBoxOutput("BankName302", width = 6),
      
      valueBoxOutput("BankInfo311", width = 3),
      valueBoxOutput("BankInfo312", width = 3),
      
      valueBoxOutput("BankInfo321", width = 3),
      valueBoxOutput("BankInfo322", width = 3),
      
      valueBoxOutput("BankInfo313", width = 3),
      valueBoxOutput("BankInfo315", width = 3),
      
      valueBoxOutput("BankInfo314", width = 3),
      valueBoxOutput("BankInfo316", width = 3),
      tabBox(width = 6, side = "left",
             tabPanel("Annual liquidity", plotOutput(outputId = "VS1T1", height = "450px")),
             tabPanel("Annual Interest Rate Income", plotOutput(outputId = "VS1T2", height = "450px")),
             tabPanel("Annual Interest Rate Income With Revaluation Gains", plotOutput(outputId = "VS1T3", height = "450px"))),
      tabBox(width = 6, side = "left",
             tabPanel("Annual liquidity", plotOutput(outputId = "VS2T1", height = "450px")),
             tabPanel("Annual Interest Rate Income", plotOutput(outputId = "VS2T2", height = "450px")),
             tabPanel("Annual Interest Rate Income With Revaluation Gains", plotOutput(outputId = "VS2T3", height = "450px"))))
  })
  
  output$BankName1 <- renderInfoBox(
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 25px;", input$bankList1),
      subtitle = paste("LEI: ", B1(), sep = ""),
      icon = icon("building"),
      color = "light-blue"
    )
  )
  
  output$BankName21 <- renderInfoBox(
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 25px;", input$bankList21),
      subtitle = paste("LEI: ", B2(), sep = ""),
      icon = icon("building"),
      color = "light-blue"
    )
  )
  
  output$BankName22 <- renderInfoBox(
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 25px;", input$bankList22),
      subtitle = paste("LEI: ", B3(), sep = ""),
      icon = icon("building"),
      color = "light-blue"
    )
  )
  
  output$BankInfo1 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B1()]]$equity.ratio, 4)*100, "%"),
      subtitle = "equity ratio",
      icon = icon("percent"),
      color = "light-blue"
    )
  )
  
  output$BankInfo2 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(sum(res.list()[[B1()]]$liq), 2), "EUR"),
      subtitle = "liquidity 2018",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo3 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B1()]]$val.nom[1], 2),"EUR"),
      subtitle = "nominal value",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo4 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B1()]]$val.mark[1], 2),"EUR"),
      subtitle = "net present value",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankName301 <- renderInfoBox(
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 25px;", input$bankList2, "(not shocked)"),
      subtitle = paste("LEI: ", B4(), sep = ""),
      icon = icon("building"),
      color = "light-blue"
    )
  )
  
  output$BankName302 <- renderInfoBox(
    infoBox(
      title = NULL,
      value = tags$p(style = "font-size: 25px;", input$bankList2, "(shocked)"),
      subtitle = paste("LEI: ", B4(), sep = ""),
      icon = icon("building"),
      color = "light-blue"
    )
  )
  
  output$BankInfo311 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", B4()),
      subtitle = "LegalEntityID",
      icon = icon("info"),
      color = "light-blue"
    )
  )
  
  output$BankInfo321 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", B4()),
      subtitle = "LegalEntityID",
      icon = icon("info"),
      color = "light-blue"
    )
  )
  
  output$BankInfo312 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list.vs()[[B4()]]$equity.ratio, 4)*100,"%"),
      subtitle = "equity ratio",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo322 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list.vs()[[B4()]]$equity.ratio, 4)*100,"%"),
      subtitle = "equity ratio",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo313 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list.vs()[[B4()]]$equity, 2), "EUR"),
      subtitle = "equity",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo314 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list.vs()[[B4()]]$equity.shocked, 2), "EUR"),
      subtitle = "equity shocked",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo315 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list.vs()[[B4()]]$Netto.Profit.Reval, 2), "EUR"),
      subtitle = "netto profit reval",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo316 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list.vs()[[B4()]]$Netto.Profit.shocked, 2), "EUR"),
      subtitle = "netto profit shocked",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$S1T1 <- renderPlot(
    barplot(res.list()[[B1()]]$liq[1,],main="Annual liquidity", names.arg=names(res.list()[[B1()]]$liq[1,]),las=2)
  )
  
  output$S1T2 <- renderPlot(
    barplot(res.list()[[B1()]]$inc.nom[1,],main="Annual Interest Rate Income", names.arg=names(res.list()[[B1()]]$liq[1,]),las=2)
  )
  
  output$S1T3 <- renderPlot(
    barplot(res.list()[[B1()]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains", names.arg=names(res.list()[[B1()]]$liq[1,]),las=2)
  )
  
  output$BankInfo211 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B2()]]$equity.ratio, 4)*100, "%"),
      subtitle = "equity ratio",
      icon = icon("percent"),
      color = "light-blue"
    )
  )
  
  output$BankInfo212 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(sum(res.list()[[B2()]]$liq), 2), "EUR"),
      subtitle = "liquidity 2018",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo221 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B3()]]$equity.ratio, 4)*100, "%"),
      subtitle = "equity ratio",
      icon = icon("percent"),
      color = "light-blue"
    )
  )
  
  output$BankInfo222 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(sum(res.list()[[B3()]]$liq), 2), "EUR"),
      subtitle = "liquidity 2018",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo213 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B2()]]$val.nom[1], 2),"EUR"),
      subtitle = "nominal vlaue",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo214 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B2()]]$val.mark[1], 2),"EUR"),
      subtitle = "net present value",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo223 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B3()]]$val.nom[1], 2),"EUR"),
      subtitle = "nominal vlaue",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$BankInfo224 <- renderValueBox(
    valueBox(
      value = tags$p(style = "font-size: 25px;", round(res.list()[[B3()]]$val.mark[1], 2),"EUR"),
      subtitle = "net present value",
      icon = icon("eur", lib = "glyphicon"),
      color = "light-blue"
    )
  )
  
  output$M1T1 <- renderPlot(
    barplot(res.list()[[B2()]]$liq[1,],main="Annual liquidity", names.arg=names(res.list()[[B2()]]$liq[1,]),las=2)
  )
  
  output$M1T2 <- renderPlot(
    barplot(res.list()[[B2()]]$inc.nom[1,],main="Annual Interest Rate Income", names.arg=names(res.list()[[B2()]]$liq[1,]),las=2)
  )
  
  output$M1T3 <- renderPlot(
    barplot(res.list()[[B2()]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains", names.arg=names(res.list()[[B2()]]$liq[1,]),las=2)
  )
  
  output$M2T1 <- renderPlot(
    barplot(res.list()[[B3()]]$liq[1,],main="Annual liquidity", names.arg=names(res.list()[[B3()]]$liq[1,]),las=2)
  )
  
  output$M2T2 <- renderPlot(
    barplot(res.list()[[B3()]]$inc.nom[1,],main="Annual Interest Rate Income", names.arg=names(res.list()[[B3()]]$liq[1,]),las=2)
  )
  
  output$M2T3 <- renderPlot(
    barplot(res.list()[[B3()]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains", names.arg=names(res.list()[[B3()]]$liq[1,]),las=2)
  )
  
  output$VS1T1 <- renderPlot(
    barplot(res.list.vs()[[B4()]]$liq[1,],main="Annual liquidity", names.arg=names(res.list.vs()[[B4()]]$liq[1,]),las=2)
  )
  
  output$VS1T2 <- renderPlot(
    barplot(res.list.vs()[[B4()]]$inc.nom[1,],main="Annual Interest Rate Income", names.arg=names(res.list.vs()[[B4()]]$liq[1,]),las=2)
  )
  
  output$VS1T3 <- renderPlot(
    barplot(res.list.vs()[[B4()]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains", names.arg=names(res.list.vs()[[B4()]]$liq[1,]),las=2)
  )
  
  output$VS2T1 <- renderPlot(
    barplot(res.list.vs()[[B4()]]$liq[1,],main="Annual liquidity", names.arg=names(res.list.vs()[[B4()]]$liq[1,]),las=2)
  )
  
  output$VS2T2 <- renderPlot(
    barplot(res.list.vs()[[B4()]]$inc.nom[1,],main="Annual Interest Rate Income", names.arg=names(res.list.vs()[[B4()]]$liq[1,]),las=2)
  )
  
  output$VS2T3 <- renderPlot(
    barplot(res.list.vs()[[B4()]]$inc.reval[1,],main="Annual Interest Rate Income With Revaluation Gains", names.arg=names(res.list.vs()[[B4()]]$liq[1,]),las=2)
  )
  
  output$MA1 <- renderPlot({
    YieldCurveECB(ad = selDate(),plot.yc = TRUE)
  })
  
  output$contractViewUI <- renderUI(
    fluidPage(
      dataTableOutput("contractOverview"))
  )
  
  output$marketViewUI <- renderUI(
    fluidPage(
      plotOutput("MA1"))
  )
  
  output$stressViewUI <- renderUI(
    fluidPage(
      fluidRow(
        h4("Stresstest für Zinsmarkt"),
        column(width = 4,
             h4("Scenario 1"),
             numericInput(inputId = "scen1", label = "Shift:", value = 0.07, step = 0.01)),
        column(width = 4,
             h4("Scenario 2"),
             numericInput(inputId = "scen2", label = "Shift:", value = -0.07, step = 0.01)),
        column(width = 4,
             h4("Scenario 3"),
             numericInput(inputId = "scen3", label = "Shift:", value = 0, step = 0.01))
#------------------------------------------         
 #Teil EZB     
      ),
      fluidRow(
        br(),
        br(),
        h4("Stresstest für Liquiditätsmarkt"),
        column(width = 4,
               selectInput("KontoName", "Account:", choices = AccNames()),
               actionButton("addShock", "Add Shock")),
        column(width = 4,
             numericInput(inputId = "SchocktransM", label = "Shift:", value = -0.2, step = 0.01))

        
      
      
      ),
 
      fluidRow(
        br(),
        br(),
        h4("Simulation und Darstellung des Stresstests"),
        br(),
        column(width = 12,
               actionButton("calShock", "Run Liquiditystress")),
        br(),
        tabBox(width = 12, side = "left",
               tabPanel(paste("Trans.Matrix", BankList()[1]), tableOutput("TS.shock.1")),
               tabPanel(paste("Trans.Matrix", BankList()[2]), tableOutput("TS.shock.2")),
               tabPanel(paste("Trans.Matrix", BankList()[3]), tableOutput("TS.shock.3")))
      )
 
    )
  )
  
  output$T1T1 <- renderDataTable(
    res.list()[[B1()]]$liq,
    options = list(scrollX = TRUE)
  )
  
  output$T1T2 <- renderDataTable(
    res.list()[[B1()]]$inc.nom,
    options = list(scrollX = TRUE)
  )
  
  output$T1T3 <- renderDataTable(
    res.list()[[B1()]]$inc.reval,
    options = list(scrollX = TRUE)
  )
  
  output$T2T1 <- renderDataTable(
    res.list()[[B2()]]$liq,
    options = list(scrollX = TRUE)
  )
  
  output$T2T2 <- renderDataTable(
    res.list()[[B2()]]$inc.nom,
    options = list(scrollX = TRUE)
  )
  
  output$T2T3 <- renderDataTable(
    res.list()[[B2()]]$inc.reval,
    options = list(scrollX = TRUE)
  )
  
  
  output$T3T1 <- renderDataTable(
    res.list()[[B3()]]$liq,
    options = list(scrollX = TRUE)
  )
  
  output$T3T2 <- renderDataTable(
    res.list()[[B3()]]$inc.nom,
    options = list(scrollX = TRUE)
  )
  
  output$T3T3 <- renderDataTable(
    res.list()[[B3()]]$inc.reval,
    options = list(scrollX = TRUE)
  )
  
  
}


shinyApp(ui = ui, server = server)
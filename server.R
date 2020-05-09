
shinyServer(
  
  function(input, output){

    ###################################
    output$dygraph<- renderDygraph({
      start <- as.Date("2000-01-01")
      end <- Sys.Date() # get untill today date
      getSymbols(c("AAPL","^IXIC","IBM","SBUX","NXPI","SFIX","JNJ"), src = "yahoo", from = start, to = end)
      aapl=AAPL$AAPL.Close;
      nsQ=IXIC$IXIC.Close;
      iBM=IBM$IBM.Close;
      sBUX=SBUX$SBUX.Close;
      nXPI=NXPI$NXPI.Close;
      sFIX=SFIX$SFIX.Close;
      jNJ=JNJ$JNJ.Close;
      
      stock1<-switch(input$stock_1,"Apple Inc."=aapl,"IBM"=iBM,"Starbucks Corporation"=sBUX,
                     "NXP Semiconductors N.V."=nXPI,
                     "Netflix"=sFIX,"Johnson & Johnson"=jNJ)
      
      # create single dataset
      stocks <- cbind(stock1, nsQ)
      dygraph(stocks)
      # customised chart
      
      stock_dyg <- dygraph(stocks, main =paste(input$stock_1, "NASDAQ", sep = "  v.s  "), width = "100%", height = "100%") %>%
        dySeries("IXIC.Close", axis = "y2") %>% 
        dyAxis("y", 
               label = input$stock_1,drawGrid = TRUE) %>%
        dyAxis("y2", 
               label = "NASDAQ",
               valueRange = c(min(min(stock1),min(nsQ)), max(max(stock1),max(nsQ))),
               independentTicks = TRUE) %>%
        dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE, axisLineColor = "navy", gridLineColor = "lightblue")%>%
        dyRangeSelector(dateWindow = c(start, end)) %>%
        dyRoller()
      stock_dyg
      ######################################################3
      
      
    })
    
  output$table <- renderDataTable({ 
    start <- as.Date("2000-01-01")
    end <- Sys.Date() # get untill today date
    getSymbols(c("AAPL","^IXIC","IBM","SBUX","NXPI","SFIX","JNJ"), src = "yahoo", from = start, to = end)
    aapl=AAPL$AAPL.Close;
    nsQ=IXIC$IXIC.Close;
    iBM=IBM$IBM.Close;
    sBUX=SBUX$SBUX.Close;
    nXPI=NXPI$NXPI.Close;
    sFIX=SFIX$SFIX.Close;
    jNJ=JNJ$JNJ.Close;
    
    stock1<-switch(input$stock_1,"Apple Inc."=aapl,"IBM"=iBM,"Starbucks Corporation"=sBUX,
                   "NXP Semiconductors N.V."=nXPI,
                   "Netflix"=sFIX,"Johnson & Johnson"=jNJ)
    
    stock1=diff(stock1)/stock1[-length(stock1)]
    nsQ=diff(nsQ)/nsQ[-length(nsQ)]
    
    # create single dataset
    stocks <- na.omit(cbind(stock1, nsQ))
    row.names(stocks) <- c(paste( "NASDAQ",input$stock_1, sep = " On "),paste(input$stock_1,"NASDAQ", sep = " On "))
    # Spliover mean
    VAR_mean_results <- summary(VAR(stocks,lag.max = 1,type = "both"))
    
    Resvar <-resid(VAR(stocks,lag.max = 1,type = "both"))
    
    # mean s&P on apple retuen
    mean_spli_S_A <- VAR_mean_results$varresult[[1]]$coefficients[2]
    mean_spli_A_S <- VAR_mean_results$varresult$IXIC.Close$coefficients[2]
    corr_S_A=VAR_mean_results$corres[1,2]
    
    # sd s&P on apple retuen
    # univariate normal GARCH(1,1) for each series
    garch11.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), 
                                                                                            model = "sGARCH"), distribution.model = "norm")
    # dcc specification - GARCH(1,1) for conditional correlations# Spillover of volatility on each Others
    dcc.garch.spec <- dccspec(uspec = multispec( replicate(ncol(Resvar), garch11.spec) ), 
                              dccOrder = c(1,1),distribution = "mvnorm")
    dcc.garch.spec
    dcc.fit = dccfit(dcc.garch.spec, data = Resvar)
    dcc.fcst=dccforecast(dcc.fit,n.ahead=1)
    FH.V.Cov=as.data.frame(dcc.fcst@mforecast$H)
    sd_spli_S_A <- FH.V.Cov[1,1]
    sd_spli_A_S <- FH.V.Cov[1,2]
    resultsData <- round(data.frame("SpillOver in Mean"=c(mean_spli_S_A,mean_spli_A_S),"SpillOver in Volatility"=c(sd_spli_S_A,sd_spli_A_S), "Correlation"=corr_S_A),digits = 4)
    row.names(resultsData) <- c(paste( "NASDAQ",input$stock_1, sep = " On "),paste(input$stock_1,"NASDAQ", sep = " On "))
    resultsData
    
},options = list( lengthChange = FALSE,searching = FALSE,lengthMenu=FALSE,className=FALSE))
  
})


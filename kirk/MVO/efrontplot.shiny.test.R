input <- list()
input$stock_MODI=TRUE
input$stock_MGF=TRUE
input$stock_MEE=FALSE
input$stock_FCEL=FALSE
input$stock_OII=FALSE

input$stock_MAT=TRUE
input$stock_EMN=TRUE
input$stock_LEG=FALSE
input$stock_AAPL=FALSE
input$stock_UTR=FALSE

input$stock_AMAT=TRUE
input$stock_AMGN=TRUE
input$stock_CAT=FALSE
input$stock_DD=FALSE
input$stock_G=FALSE


input$is.sum=TRUE
input$is.lo=FALSE
input$is.box=FALSE
input$is.group=TRUE
input$is.turnover=FALSE
input$is.propcost=FALSE


input$sum=1
input$upper=NULL
input$lower=NULL
input$upper.group=1
input$lower.group=0
input$toc=NULL
input$ptc=NULL

source("efrontplot.shiny.R")
res <- do.call(efrontplot.shiny,list(
  
                                  list.stock=list(input$stock_MODI,
                                    input$stock_MGF,
                                    input$stock_MEE,
                                    input$stock_FCEL,
                                    input$stock_OII,
                                    
                                    input$stock_MAT,
                                    input$stock_EMN,
                                    input$stock_LEG,
                                    input$stock_AAPL,
                                    input$stock_UTR,
                                    
                                    input$stock_AMAT,
                                    input$stock_AMGN,
                                    input$stock_CAT,
                                    input$stock_DD,
                                    input$stock_G)
                                  ,
                               
                                  list.cset=list(input$is.sum,
                                    input$is.lo,
                                    input$is.box,
                                    input$is.group,
                                    input$is.turnover,
                                    input$is.propcost)
                                  ,
                               
                                  list.arg=list(sum=input$sum, 
                                    upper=input$upper,
                                    lower=input$lower, 
                                    upper.group=input$upper.group, 
                                    lower.group=input$lower.group,
                                    toc=input$toc,
                                    ptc=input$ptc)
                                  )  )



# efrontPlot(returns=res$returns, cset=res$cset, rf = .003, npoints = 50,wts.plot = T,bar.ylim = c(-1,4))





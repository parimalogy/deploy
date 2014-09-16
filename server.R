library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
    
    
    
#     
# calcs <- function(Base){
#     
#     list(Base,p,Anal)           
# }    
    
    datadump <- reactive({
        inFile <- input$file1
         if (is.null(inFile))
             return(NULL)
        read.csv(inFile$datapath, header=input$header, sep=input$sep)
        
        })
    
    output$contents <- renderTable({
        datadump()[,1:2]
        
    })
    
    output$summary <- renderDataTable({
        Base <- datadump()
        Base <- Base[,1:2]
        colnames(Base)[1] <- "Date"
        Base$Date <- strptime(Base$Date,"%d - %m - %Y")
        Base <- Base[with(Base,order(Date)),]
        Base$Ret <- log((c(Base[-1,2],NA))/Base[,2])
        S20 <- Base[nrow(Base),2]
        temp2 <- Base[-nrow(Base),3]
        Base <- Base[-nrow(Base),]
        fin1 <- Base[(nrow(Base)-19):(nrow(Base)),]        #Latest 20d
        fin2 <- Base[(nrow(Base)-59):(nrow(Base)-20),]       #Regime40day
        
        m1 <- mean(fin1$Ret);   s1 <- sd(fin1$Ret);     SE1 <- s1/sqrt(nrow(fin1))  #Latest 20d
        m2 <- mean(fin2$Ret);   s2 <- sd(fin2$Ret);     SE2 <- s2/sqrt(nrow(fin2))  #Regime 40d
        m3 <- mean(Base$Ret);   s3 <- sd(Base$Ret);     SE3 <- s3/sqrt(nrow(Base)) #all
        phi <- rnorm(1000)
        d <- seq(20,1,-1)
        S0 <- mint <- maxt <- rnk <- rep(0,length(d))
        for (i in 1:(length(d))){
            S0[i] <- Base[(as.numeric(nrow(Base))-d[i]+1),2]
            simu <- S0[i]*exp(((m2-(((s2^2)/2)))*d[i])-(s2*phi*sqrt(d[i])))
            rnk[i] <- length(simu[simu<S20])/length(simu) 
        }

        Anal <- data.frame(Type=c("Prior20","Regime","All"),
                           mean=c(round(m1,6),round(m2,6),round(m3,6)),
                           SD=c(round(s1,6),round(s2,6),round(s3,6)),
                           StartDates=c(min(fin1$Date),min(fin2$Date),min(Base$Date)),
                           EndDates=c(max(fin1$Date),max(fin2$Date),max(Base$Date)))        
    }, options=list(bPaginate = FALSE,bFilter = FALSE))
    
    output$plot <- renderPlot({
        if (is.null(datadump()))
            return(NULL)
        Base <- datadump()
        Base <- Base[,1:2]
        colnames(Base)[1] <- "Date"
        Base$Date <- strptime(Base$Date,"%d - %m - %Y")
        Base <- Base[with(Base,order(Date)),]
        Base$Ret <- log((c(Base[-1,2],NA))/Base[,2])
        S20 <- Base[nrow(Base),2]
        temp2 <- Base[-nrow(Base),3]
        Base <- Base[-nrow(Base),]
        fin1 <- Base[(nrow(Base)-19):(nrow(Base)),]        #Latest 20d
        fin2 <- Base[(nrow(Base)-59):(nrow(Base)-20),]       #Regime40day
        
        m1 <- mean(fin1$Ret);   s1 <- sd(fin1$Ret);     SE1 <- s1/sqrt(nrow(fin1))  #Latest 20d
        m2 <- mean(fin2$Ret);   s2 <- sd(fin2$Ret);     SE2 <- s2/sqrt(nrow(fin2))  #Regime 40d
        m3 <- mean(Base$Ret);   s3 <- sd(Base$Ret);     SE3 <- s3/sqrt(nrow(Base)) #all
        phi <- rnorm(1000)
        d <- seq(20,1,-1)
        S0 <- mint <- maxt <- rnk <- rep(0,length(d))
        for (i in 1:(length(d))){
            S0[i] <- Base[(as.numeric(nrow(Base))-d[i]+1),2]
            simu <- S0[i]*exp(((m2-(((s2^2)/2)))*d[i])-(s2*phi*sqrt(d[i])))
            rnk[i] <- length(simu[simu<S20])/length(simu) 
        }
        p <- qplot(d,rnk,
                   xlab="No. of days", 
                   ylab="Percentage Rank",
                   main="Spot Relative to Simulation")+ ylim(0,1)
        print(p)
   })
   
   output$projection <- renderPlot({
       if (is.null(datadump()))
           return(NULL)
       Base <- datadump()
       Base <- Base[,1:2]
       colnames(Base)[1] <- "Date"
       Base$Date <- strptime(Base$Date,"%d - %m - %Y")
       Base <- Base[with(Base,order(Date)),]
       Base$Ret <- log((c(Base[-1,2],NA))/Base[,2])
       S20 <- Base[nrow(Base),2]
       temp2 <- Base[-nrow(Base),3]
       Base <- Base[-nrow(Base),]
       fin1 <- Base[(nrow(Base)-19):(nrow(Base)),]        #Latest 20d
       fin2 <- Base[(nrow(Base)-59):(nrow(Base)-20),]       #Regime40day
       
       m1 <- mean(fin1$Ret);   s1 <- sd(fin1$Ret);     SE1 <- s1/sqrt(nrow(fin1))  #Latest 20d
       m2 <- mean(fin2$Ret);   s2 <- sd(fin2$Ret);     SE2 <- s2/sqrt(nrow(fin2))  #Regime 40d
       m3 <- mean(Base$Ret);   s3 <- sd(Base$Ret);     SE3 <- s3/sqrt(nrow(Base)) #all
       phi <- rnorm(1000)
       switch(input$variable,
              abc={
                  mt <- m1; st <- s1          
              },
              pqr={
                  mt <- m2; st <- s2  
              })
#        if(input$variable="abc"){
#           
#        } 
#        if(input$variable="pqr"){
#            mt <- m2; st <- s2  
#        }
       dt <- input$days
       simu <- S20*exp(((mt-(((st^2)/2)))*dt)-(st*phi*sqrt(dt)))
       yt <- quantile(simu,c(0.1,0.25,0.5,0.75,0.9))
       q <- qplot(seq(1,1,1),yt, xlab="", 
                  ylab="Simulated Levels",
                  main=paste("Projection levels for",dt, "days"))+
             geom_hline(yintercept=yt)+ylim(min(simu),max(simu))+
    scale_y_continuous(breaks = c(yt[1],yt[2],yt[3],yt[4],yt[5]))+
    geom_text(aes(label=round(yt,4)),vjust=1.5)
       print(q)
       
   })
})


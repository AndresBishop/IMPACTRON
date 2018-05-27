library(DT)
library(shiny)
library(splitstackshape)
library(plotly)
library(leaflet)
library(htmltools)
library(htmlwidgets)
packageVersion('plotly')

#spain <- read.delim("data/analysis_txtn_qc_segmented.txt")
spain <- readRDS("data/spain.rds")
estaspain <- read.delim("data/statistics.txt")
#estaspain <- read.delim("data/statistics.txt")
biz<-as.character(names(spain)[c(5,7,9,11,13,15,17,19,21,22,23,24,25,26)])
biz2<-as.character(names(spain)[c(17,19,23,26)])
biz3<-as.character(names(estaspain)[c(4:83)])
stationsspain <- read.delim2 ("data/estaciones.txt", sep = "")
#stations<-rename(stations, c("AG87540A"="station","X.62.73"="longitud", "X.35.97"="latitud"))
df<-stationsspain[grep("SP", stationsspain$station), , drop = FALSE]
df$lng <- as.numeric(as.character(df$lng ))
df$lat <- as.numeric(as.character(df$lat ))
# Defines UI 


shinyServer (function(input, output){
  
  output$ui <- renderUI({
    # Depending on input$stratos, generate an appropiate
    # selectInput and send it to the client.
    
    switch(input$stratos,
           "None"=selectInput("dynamic", "Choose a variable to display", choices = as.list(biz), 
                              selected = "dtx"),
           'Season'= selectInput("dynamic", "Choose a variable to  display", choices = as.list(biz2)),
           'Month'= selectInput("dynamic", "Choose a variable to  display", as.list(biz2)),
           'Segment'= selectInput("dynamic", "Choose a variable to display", as.list(biz2))
    )})

  # Depending on number of segments, generate an appropiate
  # selectInput to summarize and send it to the client.
  
  # Output the data

############################
# Scatter Plot (tab1)
############################
  
  output$scatter <- renderPlot({
    
    data<-reactive({d<-#subset(spain[c(paste("qc", input$var1, sep = ""), input$var1)], spain$code == input$stationscatter)
      #subset(d[input$var1], d$paste("qc", input$var1, sep = "") != 1)
      subset(spain[,c(input$var1)],spain$code == input$stationscatter)
    #subset(d[,c(input$var1)],spain$code == input$stationscatter)
    })
    datavar<-reactive({d<-subset(spain[,c(paste("qc",input$var1,sep = ""))],spain$code == input$stationscatter)
    })
    datayear<-reactive({d<-subset(spain[,c("year")],spain$code == input$stationscatter)
    })
    
    if  (input$var1== "dtx"){
    plot(data(), xaxt="n", type="p", pch=ifelse(datavar()==1,11,21), bg=ifelse(datavar()==4,"blue",ifelse(datavar()==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(-3, 3))
    }
    else if(input$var1== "dtn"){
      plot(data(), xaxt="n", type="p", pch=ifelse(datavar()==1,11,21), bg=ifelse(datavar()==4,"blue",ifelse(datavar()==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(-3, 3))
    }
    else if(input$var1== "dtm"){
      plot(data(), xaxt="n", type="p", pch=ifelse(spain$qcder==1,11,21), bg=ifelse(spain$qcder==4,"blue",ifelse(spain$qcder==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(-3, 3))
    }
    else if(input$var1== "drn"){
      plot(data(), xaxt="n", type="p", pch=ifelse(spain$qcder==1,11,21), bg=ifelse(spain$qcder==4,"blue",ifelse(spain$qcder==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(-3, 3))
    }
    else if(input$var1== "pc01"){
      plot(data(), xaxt="n", type="p", pch=ifelse(datavar()==1,11,21), bg=ifelse(datavar()==4,"blue",ifelse(datavar()==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(0, 20))
    }
    else if(input$var1== "pc02"){
      plot(data(), xaxt="n", type="p", pch=ifelse(datavar()==1,11,21), bg=ifelse(datavar()==4,"blue",ifelse(datavar()==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(0, 20))
    }
    else{
      plot(data(), xaxt="n", type="p", pch=ifelse(spain$qcder==1,11,21), bg=ifelse(spain$qcder==4,"blue",ifelse(spain$qcder==3,"orange","red")), ylab=input$var1, xlab="Time", ylim=c(-45, 45))
    }
    rbPal <- c('blue','orange','red')
    legend(x = "topright", inset=c(0,-0.30), xpd = TRUE,bty="n",title="QC values",legend=c("ok","suspicious","very suspicious"),col =rbPal,pch=20)
    abline(h=0, lty=3, lwd=4, col="black")
    
    #draw segments mean line
    
    data23<-reactive({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
    return(d)})
    longit<-length(data23())
    inicio<-0
    numberofrows<-reactive({d<-subset(spain[,29],spain$code == input$stationscatter )
    return(d)})
    tablenor<-table(numberofrows())
    for(i in 1:longit){
      elementtable<-tablenor[i]+inicio
      watson<-tablenor[i]
      dataotro<-reactive({d<-subset(spain[,input$var1],spain$segment == i & spain$code == input$stationscatter)
      return(d)})
      segments(inicio,mean(dataotro(),na.rm = TRUE),elementtable, col = "red", lwd = 4, lty=3)
      inicio<-inicio+watson+1
    }
    
  })
  
  ############################
  # All Stations (tab3)
  ############################
  
  output$scatterx <- renderPlotly({
  data<-reactive({a<-subset(estaspain, select = c("season", "segment", input$var))
  return(a)})
  datacuadro<-as.data.frame(data())
  p<-ggplot(data = datacuadro, aes(x=season, y=datacuadro[3]))  + geom_boxplot(aes(fill=season)) + 
    geom_hline(yintercept=c(0), linetype="dotted") +
    theme(legend.text=element_text(size=12)) +
    theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),axis.line=element_blank(),
          axis.text.x=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank())+
          
    ylab(paste(input$var,collapse = " "))
  ggplotly(p)
  })
  
############################
# Box Plot (tab2)
############################ 
  
  output$boxplot <- renderPlotly({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$dynamic))
      return()
    
    # Get the data and return the appropiate plot
    
    else if(input$stratos=='Season'){
      data<-reactive({a<-subset(spain, select = c("season", "year", "month", "day", "segment", input$dynamic),
                                spain$code == input$station)
      return(a)})
      datacuadro<-as.data.frame(data())
      labels<-c("1"="Segment 1", "2"="Segment 2", "3"="Segment 3", "4"="Segment 4", "5"="Segment 5", "6"="Segment 6", "7"="Segment 7")
      p<-ggplot(data = datacuadro, aes(x=season, y=datacuadro[6]))  + geom_boxplot(aes(fill=season)) + 
                facet_wrap(~ segment, ncol = 2, labeller=labeller(segment = labels)) +  
                coord_cartesian(ylim=c(-3, 3)) + scale_y_continuous(breaks=seq(-3, 3, 1)) +
                theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),axis.title.x=element_blank()) +
                ylab(paste(input$dynamic,collapse = " "))
      ggplotly(p)
      
    } 
    else if(input$stratos=='Month'){
      data<-reactive({a<-subset(spain, select = c("season", "year", "month", "day", "segment", input$dynamic), 
                                spain$code == input$station)
      return(a)})
      datacuadro<-as.data.frame(data())
      labels<-c("1"="Segment 1", "2"="Segment 2", "3"="Segment 3", "4"="Segment 4", "5"="Segment 5", "6"="Segment 6", "7"="Segment 7")
      p<-ggplot(data = datacuadro, aes(x=as.factor(month), y=datacuadro[6]))  + geom_boxplot(aes(fill=as.factor(month))) +  
                facet_wrap(~ segment, ncol = 2, labeller=labeller(segment = labels)) +  
                coord_cartesian(ylim=c(-3, 3)) + scale_y_continuous(breaks=seq(-3, 3, 1)) +
                scale_x_discrete(breaks=c("3", "4","5","6","7","8","9","10",
                                          "11","12","1","2"),
                                labels=c("MAR", "APR","MAY","JUN","JUL","AUG","SEP","OCT",
                                        "NOV","DEC","JAN","FEB"))+
                ylab(paste(input$dynamic,collapse = " ")) + xlab("month")+
                theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(), legend.position="none",
                      axis.title.x=element_blank())
      ggplotly(p)
    } 
    else {
      data<-reactive({a<-subset(spain, select = c("season", "year", "month", "day", "segment", input$dynamic), spain$code == input$station)
      return(a)})
      datacuadro<-as.data.frame(data())
      p<-ggplot(data = datacuadro, aes(x=as.factor(segment), y=datacuadro[6]))  + geom_boxplot(aes(fill=as.character(segment))) +
                coord_cartesian(ylim=c(-3, 3)) + scale_y_continuous(breaks=seq(-3, 3, 1)) +
                ylab(paste(input$dynamic,collapse = " ")) + xlab("segment")+
                theme(panel.grid.minor=element_blank(),panel.grid.major=element_blank(),axis.title.x=element_blank()) +
                scale_fill_discrete(name="segment")
        
      ggplotly()
    }
  })
  
############################
#Summaries for Scatter Plot
############################

  output$resumenscatter1<-renderPrint({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$var1))
      return(summary(subset(spain[,17],spain$code == input$stationscatter)))
    
    # Get the data and return the summary
    datasegments<-reactive({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
    return(d)})
    df<-as.vector(datasegments())
    data<-reactive({subset(spain[,input$var1],spain$code == input$stationscatter & spain$segment == df[1])})
    summary(data(),digits = 3)
   
  })
  

    datasumaria<-observe({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
    return(d)
    })
    fg<-length(datasumaria)
    

#Summaries for each segment
 
    
    output$plots <- renderUI({
      datasumaria<-reactive({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
      return(d)
      })
      fgh<-length(datasumaria())
      plot_output_list <- lapply(1:fgh, function(i) {
        summaryname <- paste("summary", i, sep="")
        verbatimTextOutput(summaryname)
      })
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
 
  for (i in 1:fg) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      summaryname <- paste("summary", my_i, sep="")
      output[[summaryname]] <- renderPrint({
        datasegments<-reactive({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
        return(d)})
        df<-as.vector(datasegments())
        data<-reactive({subset(spain[,input$var1],spain$code == input$stationscatter & spain$segment == df[my_i])})
        summary(data(),digits = 3) 
      }) 
    }) 
  }
    

#Title for the Summaries

    
    output$title <- renderUI({
      datasumaria<-reactive({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
      return(d)
      })
      fgh<-length(datasumaria())
      plot_output_list <- lapply(1:fgh, function(i) {
        stringname <- paste("string", i, sep="")
        verbatimTextOutput(stringname)
      })
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    for (i in 1:fg) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        stringname <- paste("string", my_i, sep="")
        output[[stringname]] <- renderPrint({
          pasta<-paste("Summary segment", my_i, sep=" ",":")
          writeLines(pasta)
         
        }) 
      }) 
    }
    
############################
#Summaries for Scatter Plot
############################
   
  output$summaryx<-renderPrint({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$var1))
      return(summary(subset(spain[,17],spain$code == input$stationscatter)))
    
    # Get the data and return the summary
    datasegments<-reactive({d<-unique(subset(spain[,29], spain$code == input$stationscatter ))
    return(d)})
    df<-as.vector(datasegments())
    data<-reactive({subset(spain[,input$var1],spain$code == input$stationscatter & spain$segment == df[2] )})
    summary(data(),digits = 3)
    
  })
  
  ############################
  #Summaries for All Stations
  ############################
  output$resumenx<-renderPrint({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$var))
      return(summary(subset(estaspain[,8])))
    
    # Get the data and return the summary
    data<-reactive({subset(estaspain[,input$var],estaspain$season == c("ANU","JJA"))})
    summary(data(),digits = 2)
    
  })
  
    ############################
    #Table
    ############################
  output$mytable<- DT::renderDataTable({
    x<-reactive({subset(estaspain[1:8],estaspain$code == input$stationsDT)})
    xz<-DT::datatable(x())
    xz
   # data()
  })
  
  
  ############################
  #Map
  ############################
  
  tcu_map <-"https://api.mapbox.com/styles/v1/kairocafe/cjdbx69g5c6mb2ska05ru6nw9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoia2Fpcm9jYWZlIiwiYSI6ImNpZ2J6Zmo4aTFyYmJ0am01NDZlbGk3bGcifQ.btP13aTsFPT7MrmdWB8New"  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  output$MapPlot1 <- renderLeaflet({
    
    leaflet(data = df) %>% addTiles(urlTemplate = tcu_map) %>%
      addCircleMarkers(data=df, ~lng , ~lat, layerId=~station, popup=~station, radius=8 , color="black",  fillColor="purple", stroke = TRUE, fillOpacity = 0.8, clusterOptions = markerClusterOptions(fillColor = "blue"))%>%
      addMiniMap(
        toggleDisplay = TRUE)%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom out",
        onClick=JS("function(btn, map){ map.setView([30, -10], 4); }")))%>%
          addScaleBar(options = scaleBarOptions(maxWidth = 200, metric = TRUE, imperial = FALSE,
                                                updateWhenIdle = TRUE))
       })
  
  # store the click
  observeEvent(input$MapPlot1_marker_click,{
    data_of_click$clickedMarker <- input$MapPlot1_marker_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot=renderPlotly({
    my_place=data_of_click$clickedMarker$id
    if(is.null(my_place)){my_place="SP80020A"} 
    data<-reactive({d<-subset(spain[,c(input$var1)],spain$code == my_place)
    })
    plot(data()) 
    data<-reactive({a<-subset(spain, select = c("season", "year", "month", "day", "segment", input$var2), spain$code == my_place)
    return(a)})
    datacuadro<-as.data.frame(data())
    p<-ggplot(data = datacuadro, aes(x=as.factor(segment), y=datacuadro[6]))  + geom_boxplot(aes(fill=as.character(segment))) +
      coord_cartesian(ylim=c(-3, 3)) + scale_y_continuous(breaks=seq(-3, 3, 1)) +
      ylab(paste(input$var2,collapse = " ")) + xlab("Segment")+
      theme(legend.position="none") +
      scale_fill_discrete(name="segment")
    
    ggplotly()
  })

})


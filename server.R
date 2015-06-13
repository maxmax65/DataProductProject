library(shiny)
library(maps)
library(sp)
library(Hmisc)
## PopEtaAnno<-read.csv("PopAgeYear.csv")
PopEtaAnno<-read.csv("PopolazionePerComuneEdAnno.csv", stringsAsFactors = FALSE)
AvgPopCitta<-summarize(PopEtaAnno$Total, by = PopEtaAnno$Comune, mean)
names(AvgPopCitta)<-c("Comune", "Total")
AvgPopCitta$Range<-cut2(AvgPopCitta$Total, g = 10)


ProvRM<-readRDS("ProvRM_map.RDS")

shinyServer(function(input, output){
    
    output$town_plot<-renderPlot(width=600, height=700, {
    city<-input$town
    typeOfInfo<-input$M_F_MF
    
    paste("You have selected ", city, sep="")
    
    series1<-PopEtaAnno$Male[PopEtaAnno$Comune==city]
    series2<-PopEtaAnno$Female[PopEtaAnno$Comune==city]
    perc_ser1<-round(series1/(series1+series2),3)*100
    perc_ser2<-round(series2/(series1+series2),3)*100

    layout(matrix(c(1,1,1,1,2,3,3,3,3,4), nrow = 2, ncol = 5, byrow=TRUE))
    par(cex=0.8, cex.lab=1.2, cex.main=1.5, cex.sub=1.2)       
    switch(typeOfInfo,
               "Male+Female" = {
                lim_y<- round(1.2*max(series1 + series2),0)

                par(mar=c(4,3,3,0))
                barplot(matrix(c(series1,series2),
                               ncol=10, byrow=TRUE), col=c("lightblue", "pink"),
                        names.arg = 2002:2011, axisnames = TRUE,
                        xlim=c(0,12),
                        ylim=c(0,lim_y),
                        main = paste("Male and Female population in the town of ",
                                     city, sep=""), xlab="Year")
                y1<-series1/2
                y2<-series1 + series2/2
                
                text(rep(seq(0.5,11.3,by=1.2),2), c(y1, y2),
                     c(series1,series2), cex=1.2, srt=90)
                text(rep(seq(0.9,11.7,by=1.2),2), c(y1, y2),
                     paste("(",c(perc_ser1,perc_ser2),"%)", sep=""), srt=90)
                par(mar=c(4,0,0,0))
                plot(1,1,type = "n", axes = FALSE,ann = FALSE)
                legend("left", legend=c("Female", "Male"), fill=c("pink", "lightblue"), cex=1.3)
            },
            "Male" = {
                lim_y<- round(1.2*max(series1, series2),0)
                par(mar=c(4,3,3,0))
                barplot(series1, col="lightblue",
                        names.arg = 2002:2011,
                        ylim=c(0, lim_y),
                        xlim=c(0,12),
                        main = paste("Male population in the town of ",
                                     city, sep=""), xlab="Year")
                y1<-series1/2
                text(seq(0.7,11.5,by=1.2), y1, series1, cex=1.2, srt=90)
                par(mar=c(4,0,0,0))
                plot(1,1,type = "n", axes = FALSE,ann = FALSE)
                legend("left", legend="Male", fill="lightblue", cex=1.3)
            },
           "Female" = {
               lim_y<- round(1.2*max(series1, series2),0)
               
               par(mar=c(4,3,3,0))
               barplot(series2, col="pink",
                       names.arg = 2002:2011,
                       xlim=c(0,12),
                       ylim=c(0,lim_y),
                       main = paste("Female population in the town of ",
                                    city, sep=""), xlab="Year")
               y1<-series2/2
               text(seq(0.7,11.5,by=1.2), y1, series2, cex=1.2, srt=90)
               par(mar=c(4,0,0,0))
               plot(1,1,type = "n", axes = FALSE,ann = FALSE)
               legend("left", legend="Female", fill="pink", cex=1.3)
           }

    )       
    par(mar=c(0,0,4,0))
    plot(ProvRM, col="white")
    Municipality_map<-subset(ProvRM, NAME_3==city)
    rangePop<-mean(PopEtaAnno$Total)
    colori<-heat.colors(10)
    plot(Municipality_map,
         col=colori[AvgPopCitta$Range[AvgPopCitta$Comune==city]],
         add=TRUE)
    box(col="red")
    title("Map of municipalities of Roma province")
    text(12.1, 41.5, city, cex = 2, col="red")
    text(12.1, 41.45,
         paste("2002-2011 average population; ",
               round(AvgPopCitta$Total[AvgPopCitta$Comune==city],0),
               sep=""),
         cex = 1.3, col="red")
    
    })
})
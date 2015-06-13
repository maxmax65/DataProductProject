## Getting and cleaning data

    ## read the file with demographics
    PopAnno<-read.csv("PopolazionePerComuneEdAnno.csv", stringsAsFactors = FALSE)
    ## translate same variable names
    names(POP)[3:6]<-c("Year", "Total", "Male","Female")
    ## save the file
    write.csv(PopAnno, "PopolazionePerComuneEdAnno.csv", row.names = FALSE)

    ## read the file with the borders of Italian "Comuni"
    ## load("/Users/massimilianotesta/Dropbox/DataSpecialization/Projects/DevelopingDataProducts/ITA_adm3.RData")
    ## select the "Comuni" in the province of Roma (NAME_2=="Roma")
    ProvRM<-subset(gadm, gadm$NAME_2=="Roma")
    ## check the consistency of "Comuni" names in the POP data.frame and in the map
    town_POP<-sort(unique(POP$Comune))
    town_map<-sort(ProvRM$NAME_3)
    cbind(town_POP[! town_POP %in% town_map], town_map[! town_POP %in% town_map])
    ## there are 12 names that are different
    ## aling the names of the map to those of the data.frame
    ProvRM$NAME_3<-gsub(" Da ", " da ", ProvRM$NAME_3)
    ProvRM$NAME_3<-gsub(" Di ", " di ", ProvRM$NAME_3)
    ProvRM$NAME_3<-gsub(" Dei ", " dei ", ProvRM$NAME_3)
    ProvRM$NAME_3<-gsub(" Nel ", " nel ", ProvRM$NAME_3)
    ProvRM$NAME_3<-gsub("Sant' ", "Sant'", ProvRM$NAME_3)
    ProvRM$NAME_3<-gsub("Monte Compatri", "Montecompatri", ProvRM$NAME_3)
    town_map<-sort(ProvRM$NAME_3)
    cbind(town_POP[! town_POP %in% town_map], town_map[! town_POP %in% town_map])
    
    
    ## save it in a .RDS file to use in the shiny app
    saveRDS(ProvRM, "ProvRM_map.RDS")

        
    
## Exploratory analysis
plot(POP$Year[POP$Comune=="Anzio"],
     POP$Male[POP$Comune=="Anzio"],
     type = "b", lty=4)
points(POP$Year[POP$Comune=="Anzio"],
       POP$Female[POP$Comune=="Anzio"],
       col="red", type = "b", lty=4)

pie(c(POP$Male[POP$Comune=="Anzio" & POP$Year==2006],
      POP$Female[POP$Comune=="Anzio" & POP$Year==2006]),
        labels = c("Male", "Female"), col=c("lightblue", "pink"))
text(x = .15, y= .35, "45%")
text(x = -.15, y= -.35, "55%")


## barplot sovrapposti
barplot(matrix(c(POP$Male[POP$Comune=="Anzio"],
                 POP$Female[POP$Comune=="Anzio"]),
               ncol=10, byrow=TRUE), col=c("lightblue", "pink"))

## barplot adiancenti
barplot(matrix(c(POP$Male[POP$Comune=="Anzio"],
                 POP$Female[POP$Comune=="Anzio"]),
               ncol=10, byrow=TRUE), col=c("lightblue", "pink"),
        names.arg = POP$Year[POP$Comune=="Anzio"],
        beside=TRUE)

## mappa dei comuni con colorazione di uno di questi
## library(Hmisc); ranges<-cut2(POP$Total, g = 10, right = TRUE)
plot(ProvRM, ranges, col=ranges)


## variare la presentazione del barplot in base alla selezione "beside"

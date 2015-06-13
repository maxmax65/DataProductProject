# ui.R
library(shiny)
PopEtaAnno<-read.csv("PopAgeYear.csv")
towns<-sort(as.character(unique(PopEtaAnno$Comune)))
rm(PopEtaAnno)

shinyUI(fluidPage(
    titlePanel("Population of Roma province"),
    sidebarLayout(
        sidebarPanel("This site shows the trend from 2002 to 2011 of the female and/or male population
                     resident in the towns of Roma province, highlighting in red the relevant territory
                     in the province map",
            br(),
            br(),
            br(),
            helpText("Help: select the type of info and/or the town you are interested in
                     and the barplot and/or the map below will be updated accordingly"),
            br(),
            br(),            
            radioButtons("M_F_MF", label="Choose the info to show:",
                        choices = c("Female", "Male", "Male+Female"),
                        select="Male+Female"),
            selectInput("town", 
                        label = "Choose a town to display",
                        choices = towns,
                        selected = towns[towns=="Roma"]),
            ## sliderInput("range", label = "Range of interest",
            ##                       min = 0, max = 100, value = c(0, 100))
            br(),
            br(),
            br(),
            br(),            
            "Data dowloaded from:",
            br(),
            "- demographics: ",
            a("CSV - Popolazione residente nei comuni (anni 2002-2011)",
                      href="http://www.dati.gov.it/dataset/provincia-di-roma_03a8a38e-6084-4103-bb5a-2f011bb458d5"),
            br(),
            "- map: ",
            a("Global Administrative Areas (Country: Italy; Format: R data)",href="http://gadm.org/download"),
            br(),
            "and elaborated by the author"
            
        ),
        mainPanel(
            plotOutput("town_plot")
        )
    )
)
)
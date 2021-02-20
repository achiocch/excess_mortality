#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)


tmp2 = tempfile(fileext = ".xlsx")
download.file(url = "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile",
              destfile = tmp2, mode="wb")


AllDeath_KW_AG_Ins = readxl::read_excel(tmp2, sheet = "D_2016_2021_KW_AG_Ins", skip = 8, na = c("NA", "X"))
AllDeath_KW_AG_Ins = as.data.frame(AllDeath_KW_AG_Ins)

colnames(AllDeath_KW_AG_Ins)[2] = "year"
AllDeath_KW_AG_Ins$year = as.numeric(AllDeath_KW_AG_Ins$year)




## main plot function

plotweeks= function(filterAge, filteryears, target, Dataframe){
    if(length(filteryears)==0){
        filterYears_cleaned = unique(c(target,Dataframe$year))
    } else {
        filterYears_cleaned = unique(c(target,filteryears))
    }



    submessage = ""

    if("Insgesamt" %in% filterAge & length(filterAge)>1){
        filterAge = "Insgesamt"
        submessage= ("mehrere Altersgruppen zusammen mit Insgesamt ausgewählt, diese werden  nicht getrennt angezeigt")
    }

    idxage = Dataframe$`unter … Jahren` %in% filterAge
    idxyear = Dataframe$year %in% filterYears_cleaned

    dataset_raw = Dataframe[idxage&idxyear,]

    dataset=dataset_raw[,-c(1,3)] %>% group_by(year) %>% summarize_all(sum)


    meanyears= filterYears_cleaned[!filterYears_cleaned%in% target]

    tmpdf=dataset[dataset$year %in% meanyears,-1]

    means=apply(tmpdf, 2, mean, na.rm=T)
    sds=apply(tmpdf, 2, sd, na.rm=T)

    upper95=means + 1.96*sds
    lower95=means - 1.96*sds

    ylims=c(min(unlist(c(dataset[,-1],lower95)), na.rm=T),
    max(unlist(c(dataset[,-1],upper95)), na.rm=T))

    ylims = ylims*c(0.8,1.2)

    plot(y=0, x=0, xlim=c(1,53),
         ylim=ylims,
         type="n", ylab="Tote pro woche", xlab="Kalenderwoche",
         main=paste("Altersgruppe:", paste0(filterAge, collapse="; ")))
    mtext(submessage, 3)


    polygon(x=c(1:52, 52:1), y=c(lower95[1:52], upper95[52:1]),
            col ="#87ceeb80", border = NA)

    for (i in sort(meanyears)){
        tmpdf=dataset[dataset$year==i,-1]
        lines(x=1:52,y=tmpdf[1:52], col="skyblue", lwd="2")}

    lines(x=1:52,y=means[1:52], col="dodgerblue4", lwd="2")

    coltarget=c("red", "purple", "pink")
    for(i in 1:length(target)){
        tmpdf=dataset[dataset$year==target[i],-1]
        lines(x=1:52,y=tmpdf[1:52], col=coltarget[i], lwd="2")
    }


    legend("topleft", legend = c(target,
                                 paste("mean",paste(range(meanyears),
                                                    collapse="-"),
                                       "95% CI")),
           col=c(coltarget[1:length(target)], "skyblue"), lwd=2,bty="n")
}

#
# plotweeks(filterAge = c("30-35", "35-40"),
#           filteryears= c(),
#           target=c(2020),
#           Dataframe =AllDeath_KW_AG_Ins)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wöchentliche Sterberaten in Deutschland"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("AgeGroup",
                               h3("Altersgruppe"),
                               choices = unique(AllDeath_KW_AG_Ins$`unter … Jahren`),
                               selected = "Insgesamt"),
            checkboxGroupInput("Years",
                               h3("Jahre Mittelwert"),
                               choices = unique(AllDeath_KW_AG_Ins$year),
                               selected = c(2016:2019)),
            checkboxGroupInput("target",
                               h3("Jahre Vergleich"),
                               choices = unique(AllDeath_KW_AG_Ins$year),
                               selected = c(2020:2019))

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Weeekplot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Weeekplot <- renderPlot({

        dataset = reactiveValues(data = AllDeath_KW_AG_Ins)

        # draw the histogram with the specified number of bins
        plotweeks(filterAge = input$AgeGroup,
                  filteryears= input$Years,
                  target=input$target,
                  Dataframe =dataset$data)

    })
}

# Run the application
shinyApp(ui = ui, server = server)

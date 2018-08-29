
library(ggplot2)
library(shiny)
library(plotly)
library(stringr)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(randomForest)
library(pROC)
library(ggmosaic)
library(devtools)
library(PPtreeViz)
library(devtools)
library(graph)
library(PairViz)
library(PPforest)
source("shinyplots.R")

data.sources = list.files(pattern="*.Rdata")
for(i in 1:length(data.sources)){
  load(data.sources[i])
}

ppf <- ppf_fish
imp <- impo_fish
rf <- rf_fish

shinyUI(fluidPage(
  titlePanel('PPforest visualization'),
  # Some custom CSS for a smaller font for preformatted text
  tags$head(tags$style(
    HTML("
         pre, table.table {
         font-size: smaller;
         }
         ")
    )),
  
  tabsetPanel(
    tabPanel(
      "Individual cases",
      
      fluidRow(column(width = 12,
                      radioButtons(inputId="paropt", label="Parallel", 
                                   choices=c("Parallel","Enhanced parallel"), selected = "Parallel",
                                   inline = TRUE)
                      
      )),
      fluidRow(column(
        width = 11,
        plotlyOutput("parallel", height = 400)
      )),
      fluidRow(
        column(
          width = 4, align = "center", offset = 2,
          plotlyOutput("mdsplot", height = 400)
        ), column(
          width = 4,align = "center",
          plotlyOutput("sideplot", height = 400)
        )
        
      ),
      
       if(n.class>2){
      if(n.class==3){
        fluidRow(
          column(width = 4,
                 plotlyOutput("ternaryplot", height = 400)))
      }else{
      fluidRow(
        column(width = 11,
               plotlyOutput("ternaryplot", height = 400)))
      }
         }
    )
    ,

    tabPanel(
      "Models",
      
      fluidRow(
        column(width = 7,
               plotlyOutput("importancetree",height = 500)), column(
                 width = 4,
                 plotOutput("plottree",height = 350)
                 , plotlyOutput('boxtreeerror',height = 150)
               )
      ),
      fluidRow(
        if(sum(ppf[[8]][[1]]$Tree.Struct[,4]!=0) == 1){
          column(width = 4,
                 plotlyOutput("plotdensity",height = 400))
        }else if(sum(ppf[[8]][[1]]$Tree.Struct[,4]!=0) == 2){
          column(width = 7,
                 plotlyOutput("plotdensity",height = 400))
        }else{
          column(width = 11,
                 plotlyOutput("plotdensity",height = 400))
        }

      ),
      fluidRow(
        if(sum(ppf[[8]][[1]]$Tree.Struct[,4]!=0) == 1 ){
          column(width = 4,
                 plotlyOutput("plotmosaic",height = 400))
        }else if(sum(ppf[[8]][[1]]$Tree.Struct[,4]!=0) == 2 ){
          column(width = 7,
                 plotlyOutput("plotmosaic",height = 400))
        }else{
          column(width = 11,
                 plotlyOutput("plotmosaic",height = 400))
        }

      )
    )
    ,
    tabPanel(
      "Performance comparison",
      fluidRow(column(width = 12,sidebarPanel(
        selectizeInput(
          'xcol', 'Select Class', as.character(lev) , selected = lev,multiple =
            TRUE
        ), actionButton("goButton", "Go!")
      ))),
      fluidRow(
        column(width = 4,
               plotlyOutput("siderfpp",height = 400)),
        column(width = 4,
               plotlyOutput("plot_rocpp",height = 400)),
        column(width = 4,
               plotlyOutput("plot_oobpp",height = 400))

      ),
      fluidRow(
        column(width = 4,
               plotlyOutput("siderf",height = 400)),
        column(width = 4,
               plotlyOutput("plot_rocrf",height = 400)),
        column(width = 4,
               plotlyOutput("plot_oobrf",height = 400))
      ),

      fluidRow(column(width = 12,
                      radioButtons(inputId="importance", label="Importance", 
                                   choices=c("PPforest impo","PPforest avgtr","Permuted"), selected="PPforest impo",
                                   inline = TRUE)
  
      )),

      fluidRow(
        column(width = 4, align = "center", offset = 2,
               plotlyOutput("plot_impopp",height = 400)),

        column(width = 4,
               plotlyOutput("plot_imporf",height = 400))

      ),
      fluidRow(column(width = 6,align = "center",
                      tableOutput('tablepp')),
               column(width = 6,
                      tableOutput('tablerf')))
    )
    
  )
    )
)
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(DT)
library(networkD3)

####
### data wrangling

hospital_dat <- read_csv('data/hospital_characteristics.csv')
statelist <- unique(hospital_dat$State)
zipcode <- unique(hospital_dat$`ZIP Code`)
#hospital_dat %>%
#    arrange(State, `Hospital overall rating`, `Patient Experience Rating`) -> hospital_dat
# hospital_dat$Hospital_NAME <- str_sub(hospital_names, start = 1, end = -10)
# hospital_dat$State <- str_sub(hospital_names, start = -2)
# hospital_dat$Zipcode <-  str_sub(hospital_names, start = -8, end = -4)
# hospital_dat %>%
#     select(Hospital_NAME, State, Zipcode,
#             H_STAR_RATING, `Hospital overall rating`) -> hospital_dat


# Define UI 
shinyUI(fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    navbarPage("Hospital Look UP", theme = shinytheme("lumen"),
        tabPanel("Find your hospital", fluid = TRUE, icon = icon("table"),
                        
    fluidRow(
        column(3, offset = 1,
               h3("Select States", align = "center"),
               selectInput(inputId = "cstate",
                           label = "Select your current state",
                           choices = statelist ,
                           selected = "MN",
                           width = "400px"),
               selectInput(inputId = "fstate",
                           label = "Select the state you are interested",
                           choices = statelist ,
                           selected = "MN",
                           width = "400px"),
               helpText("By clicking one hospital from the table, 
                        it will show top 10 most similar hospitals comparing to it from the state you are interested in, 
                        regarding hospital measures and patient expirence assessments. Longer edge indicates higher similarity."),
        ),
        column(6,class = "well", offset = 1,
               h3("Recommendations", align = "center"),
               forceNetworkOutput(outputId="networkd3", width = "100%", height = "400px")
               
        ),
        hr(),
        fluidRow(
            column(12, align = "center",
                   DT::dataTableOutput("outtable",width = "90%")
                   )
            
        ),
        hr()
    # ),
    # # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         width = 4, 
    #         titlePanel(h4("Select hospitals in the region")),
    #         
    # 
    #     # Select Event
    #     selectInput(inputId = "cstate",
    #                 label = "Select your current state",
    #                 choices = statelist ,
    #                 selected = "MN",
    #                 width = "200px"),
    #     selectInput(inputId = "fstate",
    #                 label = "Select the state you are interested",
    #                 choices = statelist ,
    #                 selected = "MN",
    #                 width = "200px"),
    #         
    #     #actionButton("calsim", "Find similar hospitals", class = "btn-success"),
    #         ),
    #     mainPanel(width = 8,
    #         div(dataTableOutput("outtable"), style = "font-size:80%"),
    #         hr(),
    #         h3("Recommendations", align = "center"),
    #         withSpinner(forceNetworkOutput(outputId="networkd3", width = "100%", height = "400px"))
    #     )
     )    
    ),
    tabPanel("Geograph", fluid = TRUE, icon = icon("globe-americas"),
         fluidRow(
             column(12, align = "center",
                         withSpinner(plotOutput("geomaps",width = "100%", height = 550,
                                    dblclick = "map_dblclick",
                                    brush = brushOpts(
                                        id = "map_brush",
                                        resetOnNew = TRUE))
                                    )
                    
         )),
         hr(),
         helpText("Only plot the first 5 hospitals if selections greater than 5. Users can zoom in for more detailed regions."),
         h3("Rating comparisons for selected hospitals", align = "center"),
         br(),
         fluidRow(
             column(6, offset = 1,class = "well",
                plotOutput("splot", width = "100%", height = 400)
             
         ),
            column(5, 
                   dataTableOutput("stable", width = "100%")
                
            )
         )
             )
    )
    
    )
)

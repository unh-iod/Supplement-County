#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinylogs)
library(gt)

use_tracking()

# Define UI for application that draws a histogram
fluidPage(
  useShinyjs(), 
  tags$style(type="text/css",
             "body {font-size: max(1em, 20px) !important;
                     font-family: Helvetica;}",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             #".navbar {background-color: #FFFFFF !important; }",
             ".navbar-default .navbar-brand {color: #000000 !important;}",
             #"a {color: #000000 !important;}",
             "span {color: #000000 !important;}",
             ".navbar-default .navbar-brand {color: #000000 !important;}",
             ".navbar .container-fluid {background-color: ##EEEEEE !important;
                                         border-color: ##EEEEEE !important;
                                         margin: 2px;}",
             "ul.nav {list-style-position:inside !important;}",
             ".navbar li {background-color: #F6B52C;
                       border: 1px solid #F6B52C;
                       border-radius: 5px;
                       margin: 2px;}",
             ".navbar #container li.active a {border-color:#E0A122;
                                   background-color:#E0A122;}",
             ".navbar #container li:hover {border-color:#E0A122;
                                   background-color:#E0A122 !important;}",
             ".navbar a {color: #000000 !important;}",
             "span {color: #000000 !important;}",
             "#Downloader {background-color: #0D4D8C;
                            color: #FFFFFF;}",
             "#Downloader3 {background-color: #0D4D8C;
                            color: #FFFFFF;}",
             "#Downloader5 {background-color: #0D4D8C;
                            color: #FFFFFF;}",
             "#Downloader6 {background-color: #0D4D8C;
                            color: #FFFFFF;}",
             "#Downloader8 {background-color: #0D4D8C;
                            color: #FFFFFF;}",
             "#Downloader9 {background-color: #0D4D8C;
                            color: #FFFFFF;}",
             "#Downloader13 {background-color: #0D4D8C;
                            color: #FFFFFF;}"),
  # Application title
  navbarPage(id = "container",strong(""),
             tabPanel(strong("Prevalence and Population"), fluid = TRUE,
                      h3("Source: American Community Survey (ACS) 5-year"),
                      sidebarLayout(
                        sidebarPanel(
                          h2("Customize this table:"),
                          p(actionLink("Link2Disclaimer1","Disclaimer",style = "text-decoration: underline;")),
                          hr(),
                          selectInput("Disability1",h3("Disability"),
                                      choices = c("Disability",
                                                  "Hearing Disability",
                                                  "Seeing Disability",
                                                  "Cognitive Disability",
                                                  "Ambulatory Disability",
                                                  "Self-Care Disability",
                                                  "Independent Living Disability")),
                          selectInput("State1",h3("State"),
                                      choices = c("All",uniques$state)),
                          selectInput("Gender1",h3("Gender"),
                                      choices=c("All",uniques$gender)),
                          selectInput("Agegroup1",h3("Age Group"),
                                      choices=c("All",uniques$agegroup)),
                          hr(),
                          selectInput("Year1",h3("Year"),
                                      choices=data_years$acs,
                                      selected = 1)
                        ),
                        mainPanel(
                          fluidRow(
                            p("This section presents statistics on the United States civilian population and people with disabilities, within national, state, and county locations. The principal source of these data is the U.S. Census Bureau, specifically the American Community Survey 5-Year Estimates, summarized at data.census.gov tables B18101 - B18107. In the tables presented here,  statistics for people with disabilities (disability status or disability type) are based on having responded ‘yes’ to a series of questions within the American Community Survey (see the Disability Statistics Compendium Glossary for more details).")),
                          downloadButton("Downloader", "Download XLSX"),
                          h2(strong(textOutput("mTitle1"))),
                          fluidRow(
                            htmlOutput("TStable")  %>% withSpinner(color="#0D4D8C")
                          ),
                          p(textOutput("Citation1")),
                          p(paste("Disclaimer:",disclaimer))
                        )
                      )
             ),
             tabPanel(strong("Employment"), fluid = TRUE,
                      h3("Source: American Community Survey (ACS) 5-year"),
                      sidebarLayout(
                        sidebarPanel(
                          h2("Customize this table:"),
                          p(actionLink("Link2Disclaimer2","Disclaimer",style = "text-decoration: underline;")),
                          hr(),
                          selectInput("Measure3",h3("Measure"),
                                      choices = c("Employment to Population Ratio" = "E2PR",
                                                  "Labor Force Particiaption" = "LFP",
                                                  "Unemployment Rate" = "UNEMP")),
                          selectInput("Disability3",h3("Disability"),
                                      choices = c("Disability"#,
                                                  #"Hearing Disability",
                                                  #"Seeing Disability",
                                                  #"Cognitive Disability",
                                                  #"Ambulatory Disability",
                                                  #"Self-Care Disability",
                                                  #"Independent Living Disability"
                                      )),
                          selectInput("State3",h3("State"),
                                      choices = c("All",uniques$state)),
                          hr(),
                          selectInput("Year3",h3("Year"),
                                      choices=data_years$acs,
                                      selected = 1)
                        ),
                        mainPanel(
                          fluidRow(
                            p("This section presents statistics on employment of the United States civilian population by disability status, within national, state, and county locations. The principal source of these data is the U.S. Census Bureau, specifically the American Community Survey 5-Year Estimates, summarized at data.census.gov table C18130. In the tables presented here, statistics for people with disabilities (disability status or disability type) are based on having responded ‘yes’ to a series of questions within the American Community Survey (see the Disability Statistics Compendium Glossary for more details).")),
                          downloadButton("Downloader3", "Download XLSX"),
                          h2(strong(textOutput("mTitle3"))),
                          fluidRow(
                            htmlOutput("TStable3")  %>% withSpinner(color="#0D4D8C")
                          ),
                          p(textOutput("Citation3")),
                          p(paste("Disclaimer:",disclaimer))
                        )
                      )
             ),
             tabPanel(strong("Poverty"), fluid = TRUE,
                      h3("Source: American Community Survey (ACS) 5-year"),
                      sidebarLayout(
                        sidebarPanel(
                          h2("Customize this table:"),
                          p(actionLink("Link2Disclaimer3","Disclaimer",style = "text-decoration: underline;")),
                          hr(),
                          selectInput("Disability6",h3("Disability"),
                                      choices = c("Disability"#,
                                                  #"Hearing Disability",
                                                  #"Seeing Disability",
                                                  #"Cognitive Disability",
                                                  #"Ambulatory Disability",
                                                  #"Self-Care Disability",
                                                  #"Independent Living Disability"
                                      )),
                          selectInput("State6",h3("State"),
                                      choices = c("All",uniques$state)),
                          selectInput("Agegroup6",h3("Age Group"),
                                      choices=c("All",uniques$agegroup6)),
                          hr(),
                          selectInput("Year6",h3("Year"),
                                      choices=data_years$acs,
                                      selected = 1)
                        ),
                        mainPanel(
                          fluidRow(
                            p("This section presents statistics on poverty of the United States civilian population by disability status, within national, state, and county locations. The principal source of these data is the U.S. Census Bureau, specifically the American Community Survey 5-Year Estimates, summarized at data.census.gov table C18130. In the tables presented here,  statistics for people with disabilities (disability status or disability type) are based on having responded ‘yes’ to a series of questions within the American Community Survey (see the Disability Statistics Compendium Glossary for more details).")),
                          downloadButton("Downloader6", "Download XLSX"),
                          h2(strong(textOutput("mTitle6"))),
                          fluidRow(
                            htmlOutput("TStable6")  %>% withSpinner(color="#0D4D8C")
                          ),
                          p(textOutput("Citation6")),
                          p(paste("Disclaimer:",disclaimer))
                        )
                      )
             ),
             tabPanel(strong("Insurance"), fluid = TRUE,
                      h3("Source: American Community Survey (ACS) 5-year"),
                      sidebarLayout(
                        sidebarPanel(
                          h2("Customize this table:"),
                          p(actionLink("Link2Disclaimer4","Disclaimer",style = "text-decoration: underline;")),
                          hr(),
                          selectInput("Measure9",h3("Measure"),
                                      choices = c("With Health Insurance" = "INSURANCE",
                                                  "With Private Health Insurance" = "INSURANCE1",
                                                  "With Public Health Insurance" = "INSURANCE2")),
                          selectInput("Disability9",h3("Disability"),
                                      choices = c("Disability"#,
                                                  #"Hearing Disability",
                                                  #"Seeing Disability",
                                                  #"Cognitive Disability",
                                                  #"Ambulatory Disability",
                                                  #"Self-Care Disability",
                                                  #"Independent Living Disability"
                                      )),
                          selectInput("State9",h3("State"),
                                      choices = c("All",uniques$state)),
                          selectInput("Agegroup9",h3("Age Group"),
                                      choices=c("All",uniques$agegroup9)),
                          hr(),
                          selectInput("Year9",h3("Year"),
                                      choices=data_years$acs,
                                      selected = 1)
                        ),
                        mainPanel(
                          fluidRow(
                            p("This section presents statistics on health insurance of the United States civilian population by disability status, within national, state, and county locations. The principal source of these data is the U.S. Census Bureau, specifically the American Community Survey 5-Year Estimates, summarized at data.census.gov table B18135. In the tables presented here,  statistics for people with disabilities (disability status or disability type) are based on having responded ‘yes’ to a series of questions within the American Community Survey (see the Disability Statistics Compendium Glossary for more details).")),
                          downloadButton("Downloader9", "Download XLSX"),
                          h2(strong(textOutput("mTitle9"))),
                          fluidRow(
                            htmlOutput("TStable9")  %>% withSpinner(color="#0D4D8C")
                          ),
                          p(textOutput("Citation9")),
                          p(paste("Disclaimer:",disclaimer))
                        )
                      )
             ),
             tabPanel(strong("Education"), fluid = TRUE,
                      h3("Source: American Community Survey (ACS) 5-year"),
                      sidebarLayout(
                        sidebarPanel(
                          h2("Customize this table:"),
                          p(actionLink("Link2Disclaimer5","Disclaimer",style = "text-decoration: underline;")),
                          hr(),
                          selectInput("Disability13",h3("Disability"),
                                      choices = c("Disability"#,
                                                  #"Hearing Disability",
                                                  #"Seeing Disability",
                                                  #"Cognitive Disability",
                                                  #"Ambulatory Disability",
                                                  #"Self-Care Disability",
                                                  #"Independent Living Disability"
                                      )),
                          selectInput("State13",h3("State"),
                                      choices = c("All",uniques$state)),
                          hr(),
                          selectInput("Year13",h3("Year"),
                                      choices=data_years$acs,
                                      selected = 1)
                        ),
                        mainPanel(
                          fluidRow(
                            p("This section presents statistics on education of the United States civilian population by disability status, within national, state, and county locations. The principal source of these data is the U.S. Census Bureau, specifically the American Community Survey 5-Year Estimates, summarized at data.census.gov table S1811. In the tables presented here,  statistics for people with disabilities (disability status or disability type) are based on having responded ‘yes’ to a series of questions within the American Community Survey (see the Disability Statistics Compendium Glossary for more details).")),
                          downloadButton("Downloader13", "Download XLSX"),
                          h2(strong(textOutput("mTitle13"))),
                          fluidRow(
                            htmlOutput("TStable13")  %>% withSpinner(color="#0D4D8C")
                          ),
                          p(textOutput("Citation13")),
                          p(paste("Disclaimer:",disclaimer))
                        )
                      )
             ),
             tabPanel(title = strong("Disclaimer"),
                      value = "Disclaimer",
                      p(disclaimer))
             
  )
)
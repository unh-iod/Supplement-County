#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(writexl)
library(xml2)
library(rvest)
library(shinylogs)
library(DBI)
library(lubridate)

load(".RData")

cloud_prep()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  track_usage(storage_mode = store_custom(sendlog,
                                          db = "logs",
                                          idsession=as.character(format(as.double(floor(runif(1)*1e30)), scientific = F)),
                                          ip = session$request$REMOTE_ADDR,
                                          name = "BYOS-ACS5Y"),
              what = c("session", "input", "output", "error"),
              exclude_input_regex = NULL,
              exclude_input_id = NULL,
              on_unload = FALSE,
              app_name = NULL,
              exclude_users = NULL,
              get_user = NULL,
              dependencies = TRUE,
              session = getDefaultReactiveDomain())
  
  observeEvent(input$top_button,{shinyjs::runjs("window.scrollTo(0, 50)")})

  ################
  # Prevalence and Population
  
  ################
  # Load in inputs
  dis_dummy <- reactive({
    if(input$Disability1=="Disability"){
      "disability"
    }
    else if(input$Disability1=="Deaf or serious difficulty hearing"){
      "hearing"
    }
    else if(input$Disability1=="Blind or serious difficulty seeing"){
      "seeing"
    }
    else if(input$Disability1=="Serious difficulty concentrating, remembering, or making decisions"){
      "remembering"
    }
    else if(input$Disability1=="Serious difficulty walking or climbing stairs"){
      "mobility"
    }
    else if(input$Disability1=="Difficulty dressing or bathing"){
      "selfcare"
    }
    else if(input$Disability1=="Difficulty doing errands alone"){
      "independentliving"
    }
  })
  gen_dummy <- reactive({
    if(!input$Gender1=="All"){
      "Any"
    }
    else if(input$Gender1=="All"){
      "All"
    }
  })
  agegroup_dummy <- reactive({
    if(!input$Agegroup1=="All"){
      "Any"
    }
    else if(input$Agegroup1=="All"){
      "All"
    }
  })
  state_dummy <- reactive({
    if(!input$State1=="All"){
      "Any"
    }
    else if(input$State1=="All"){
      "All"
    }
  })

  ################
  # Data from GCS
  Dat <- reactive({
    index <- which(
      filterer$gender == gen_dummy() &
      filterer$agegroup == agegroup_dummy())
    filename <- paste("acs",input$Year1,"_",index,"_",dis_dummy(),"_PREV_COUNTY.rds",sep="")
    noprint <- cloud_prep() #
    tmp <- get_object(filename,"compendium_project_storage",type="actuals")
    tmp <- tmp[-c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),] %>%
      rbind(tmp[c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),]) %>%
      select(-Geography,-contains("dagger_")) %>%
      rename(Geography = Geographic.Area.Name)
    # Filter based on input selection
    if(state_dummy() == "All"){
      tmp <- tmp %>%
        filter(!str_detect(Geography,","))
    } else {
      tmp <- tmp %>%
        filter(str_detect(Geography,"United States") | str_detect(Geography,input$State1))
    }
    if(gen_dummy() == "Any"){
      tmp <- tmp %>%
        filter(gender == input$Gender1) %>%
        select(-gender)
    }
    if(agegroup_dummy() == "Any"){
      tmp <- tmp %>%
        filter(agegroup1 == input$Agegroup1) %>%
        select(-agegroup1)
    }

    tmp
  })
  ################
  
  ################
  # Table Title Info
  output$mTitle1 <- renderText({
    title1()
  })

  title1 <- reactive({
    txt <- "Custom Table: "
    if(gen_dummy() == "Any"){
      txt <- paste(txt,input$Gender1," ",sep="")
    }
    txt <-paste(txt,"Civilians ",sep="")
    ######### Age group choices ###################
    ######### Age group choices ###################
    if(agegroup_dummy() == "Any"){ # if any group is selected
      if(!input$Disability1=="Difficulty doing errands alone"){ # and if not difficulty doing errands alone
        txt <- paste(txt,"Ages ",input$Agegroup1," ",sep="") # then it use the selected group
      } else { # otherwise if difficulty doing errands alone
        if (input$Agegroup1 == "5 to 17 Years"){ # and its the young age group
          txt <- paste(txt,"Ages 15 to 17 ",sep="") # update the group to only include 15 to 17
        } else{
          txt <- paste(txt,"Ages ",input$Agegroup1," ",sep="") # then it use the selected group
        }
      }
    } else {
      if(input$Disability1=="Difficulty dressing or bathing" |
         input$Disability1=="Serious difficulty concentrating, remembering, or making decisions" |
         input$Disability1=="Serious difficulty walking or climbing stairs"){
        txt <- paste(txt,"Age 5 Years and Over ",sep="")
      }
      if(input$Disability1=="Difficulty doing errands alone"){
        txt <- paste(txt,"Age 18 Years and Over ",sep="")
      }
    }
    txt <-paste(txt,"Living in the Community ",sep="")
    if(state_dummy() == "Any"){ # if any group is selected
      txt <- paste(txt,"in ",input$State1,sep="") # then it use the selected group
    } else {
      txt <- paste(txt,"for the United States and States",sep="")
    }
    paste(txt,", by Disability Status",": ",as.numeric(input$Year1)-4,"-",input$Year1,sep="")
  })
  
  dis1 <- reactive({
    if(input$Disability1=="Disability"){
      "Disability"
    }
    else if(input$Disability1=="Deaf or serious difficulty hearing"){
      "Hearing Disability"
    }
    else if(input$Disability1=="Blind or serious difficulty seeing"){
      "Vision Disability"
    }
    else if(input$Disability1=="Serious difficulty concentrating, remembering, or making decisions"){
      "Cognitive Disability"
    }
    else if(input$Disability1=="Serious difficulty walking or climbing stairs"){
      "Ambulatory Disability"
    }
    else if(input$Disability1=="Difficulty dressing or bathing"){
      "Self-Care Disability"
    }
    else if(input$Disability1=="Difficulty doing errands alone"){
      "Independent Living Disability"
    }
  })
  
  output$Citation1 <- renderText({
    txt <- paste("Citation: ",authors,"(",year(Sys.Date()),"). Annual Disability Statistics Compendium: ",input$Year1," (Custom Prevalence and Population Table). Durham, NH: University of New Hampshire, Institute on Disability. Source: U.S. Census Bureau, ",as.numeric(input$Year1) - 4," - ",input$Year1," American Community Survey 5-year estimates. https://data.census.gov. Based on a sample and subject to sampling variability.",sep="")
    txt
  })
  
  tgen <- reactive({
    tgentmp1(Dat(),"PopandPrev",col_spanners = c("Total",dis1(),paste("No",dis1())))
  })
  
  output$TStable <- function(){
    rt <- rando_text()
    tgen() %>%
      as_xml_document() %>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- includeHTML(paste(rt,'.html',sep=""))
    invisible(file.remove(paste(rt,'.html',sep="")))
    return(tmp)
  }
  ################
  ################
  # Render ouptuts
  
  download_prepare <- reactive({
    rt <- rando_text()
    tgen6()%>%as_xml_document()%>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- read_html(paste(rt,'.html',sep="")) %>%
      html_table()
    invisible(file.remove(paste(rt,'.html',sep="")))
    tmp
  })
  
  output$Downloader <- downloadHandler(
    filename = function(){
      paste(title1(),'download.xlsx',sep="")},
    content = function(file){write_xlsx(download_prepare(),file)}
  )

  #######################################################################
  #######################################################################
  #######################################################################
  
  ################
  # Employment 
  
  ################
  # Load in inputs
  dis_dummy3 <- reactive({
    #if(input$Disability1=="Disability"){
    "disability"
    #}
    # else if(input$Disability1=="Deaf or serious difficulty hearing"){
    #   "hearing"
    # }
    # else if(input$Disability1=="Blind or serious difficulty seeing"){
    #   "seeing"
    # }
    # else if(input$Disability1=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "remembering"
    # }
    # else if(input$Disability1=="Serious difficulty walking or climbing stairs"){
    #   "mobility"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "selfcare"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "independentliving"
    # }
  })
  # gen_dummy <- reactive({
  #   if(!input$Gender1=="All"){
  #     "Any"
  #   }
  #   else if(input$Gender1=="All"){
  #    "All"
  #   }
  # })
  # agegroup_dummy3 <- reactive({
  #   if(!input$Agegroup3=="All"){
  #     "Any"
  #   }
  #   else if(input$Agegroup3=="All"){
  #     "All"
  #   }
  # })
  state_dummy3 <- reactive({
    if(!input$State3=="All"){
      "Any"
    }
    else if(input$State3=="All"){
      "All"
    }
  })
  measure_dummy3 <- reactive({
    if(input$Measure3 == "E2PR"){
      "Employment to Population Ratio"
    }
    else if(input$Measure3 == "LFP"){
      "Labor Force Participation"
    }
    else if(input$Measure3 == "UNEMP"){
      "Unemployment Rate"
    }
  })
  
  ################
  # Data from GCS
  Dat3 <- reactive({
    index <- which(1==1)
    filename <- paste("acs",input$Year3,"_",index,"_",dis_dummy3(),"_",input$Measure3,"_COUNTY.rds",sep="")
    noprint <- cloud_prep() #
    tmp <- get_object(filename,"compendium_project_storage",type="actuals")
    tmp <- tmp[-c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),] %>%
      rbind(tmp[c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),]) %>%
      select(-Geography,-contains("dagger_")) %>%
      rename(Geography = Geographic.Area.Name)
    # Filter based on input selection
    if(state_dummy3() == "All"){
      tmp <- tmp %>%
        filter(!str_detect(Geography,","))
    } else {
      tmp <- tmp %>%
        filter(str_detect(Geography,"United States") | str_detect(Geography,input$State3))
    }
    # if(gen_dummy() == "Any"){
    #   tmp <- tmp %>%
    #     filter(gender == input$Gender1) %>%
    #     select(-gender)
    # }
    # if(agegroup_dummy3() == "Any"){
    #   tmp <- tmp %>%
    #     filter(agegroup1 == input$Agegroup3) %>%
    #     select(-agegroup1)
    # }
    tmp
  })
  ################
  
  ################
  # Table Title Info
  output$mTitle3 <- renderText({
    title3()
  })
  
  title3 <- reactive({
    txt <- "Custom Table: "
    # if(gen_dummy() == "Any"){
    #   txt <- paste(txt,input$Gender1," ",sep="")
    # }
    txt <-paste(txt,measure_dummy3()," of Civilians Age 18 to 64 Living in the Community ",sep="")
    if(state_dummy3() == "Any"){ # if any group is selected
      txt <- paste(txt,"in ",input$State3,sep="") # then it use the selected group
    } else {
      txt <- paste(txt,"for the United States and States",sep="")
    }
    paste(txt,", by Disability Status",": ",as.numeric(input$Year3)-4,"-",input$Year3,sep="")
  })
  
  dis3 <- reactive({
    if(input$Disability3=="Disability"){
      "Disability"
    }
    # else if(input$Disability3=="Deaf or serious difficulty hearing"){
    #   "Hearing Disability"
    # }
    # else if(input$Disability3=="Blind or serious difficulty seeing"){
    #   "Vision Disability"
    # }
    # else if(input$Disability3=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "Cognitive Disability"
    # }
    # else if(input$Disability3=="Serious difficulty walking or climbing stairs"){
    #   "Ambulatory Disability"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "Self-Care Disability"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "Independent Living Disability"
    # }
  })
  
  output$Citation3 <- renderText({
    txt <- paste("Citation: ",authors,"(",year(Sys.Date()),"). Annual Disability Statistics Compendium: ",input$Year3," (Custom Employment Table). Durham, NH: University of New Hampshire, Institute on Disability. Source: U.S. Census Bureau, ",as.numeric(input$Year3) - 4," - ",input$Year3," American Community Survey 5-year estimates. https://data.census.gov. Based on a sample and subject to sampling variability.",sep="")
    txt
  })
  
  tgen3 <- reactive({
    tgentmp3(Dat3(),"Employed",col_spanners = c(dis3(),paste("No",dis3()),"test"))
  })
  
  output$TStable3 <- function(){
    rt <- rando_text()
    tgen3() %>%
      as_xml_document() %>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- includeHTML(paste(rt,'.html',sep=""))
    invisible(file.remove(paste(rt,'.html',sep="")))
    return(tmp)
  }
  
  ################
  ################
  # Render ouptuts
  
  download_prepare3 <- reactive({
    rt <- rando_text()
    tgen3()%>%as_xml_document()%>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- read_html(paste(rt,'.html',sep="")) %>%
      html_table()
    invisible(file.remove(paste(rt,'.html',sep="")))
    tmp
  })
  
  output$Downloader3 <- downloadHandler(
    filename = function(){
      paste(title3(),'download.xlsx',sep="")},
    content = function(file){write_xlsx(download_prepare(),file)}
  )  
  
#######################################################################
#######################################################################
#######################################################################
  
  ################
  # Poverty 
  
  ################
  # Load in inputs
  dis_dummy6 <- reactive({
    #if(input$Disability1=="Disability"){
      "disability"
    #}
    # else if(input$Disability1=="Deaf or serious difficulty hearing"){
    #   "hearing"
    # }
    # else if(input$Disability1=="Blind or serious difficulty seeing"){
    #   "seeing"
    # }
    # else if(input$Disability1=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "remembering"
    # }
    # else if(input$Disability1=="Serious difficulty walking or climbing stairs"){
    #   "mobility"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "selfcare"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "independentliving"
    # }
  })
  # gen_dummy <- reactive({
  #   if(!input$Gender1=="All"){
  #     "Any"
  #   }
  #   else if(input$Gender1=="All"){
  #    "All"
  #   }
  # })
  agegroup_dummy6 <- reactive({
    if(!input$Agegroup6=="All"){
      "Any"
    }
    else if(input$Agegroup6=="All"){
      "All"
    }
  })
  state_dummy6 <- reactive({
    if(!input$State6=="All"){
      "Any"
    }
    else if(input$State6=="All"){
      "All"
    }
  })
  
  ################
  # Data from GCS
  Dat6 <- reactive({
    index <- which(
      filterer6$agegroup == agegroup_dummy6())
    filename <- paste("acs",input$Year6,"_",index,"_",dis_dummy6(),"_POVERTY_COUNTY.rds",sep="")
    noprint <- cloud_prep() #
    tmp <- get_object(filename,"compendium_project_storage",type="actuals")
    tmp <- tmp[-c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),] %>%
      rbind(tmp[c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),]) %>%
      select(-Geography,-contains("dagger_")) %>%
      rename(Geography = Geographic.Area.Name)
    # Filter based on input selection
    if(state_dummy6() == "All"){
      tmp <- tmp %>%
        filter(!str_detect(Geography,","))
    } else {
      tmp <- tmp %>%
        filter(str_detect(Geography,"United States") | str_detect(Geography,input$State6))
    }
    # if(gen_dummy() == "Any"){
    #   tmp <- tmp %>%
    #     filter(gender == input$Gender1) %>%
    #     select(-gender)
    # }
    if(agegroup_dummy6() == "Any"){
      tmp <- tmp %>%
        filter(agegroup1 == input$Agegroup6) %>%
        select(-agegroup1)
    }
    tmp
  })
  ################
  
  ################
  # Table Title Info
  output$mTitle6 <- renderText({
    title6()
  })
  
  title6 <- reactive({
    txt <- "Custom Table: Poverty of "
    # if(gen_dummy() == "Any"){
    #   txt <- paste(txt,input$Gender1," ",sep="")
    # }
    txt <-paste(txt,"Civilians ",sep="")
    ######### Age group choices ###################
    if(agegroup_dummy6() == "Any"){ # if any group is selected
      txt <- paste(txt,input$Agegroup6," ",sep="") # then it use the selected group
    } else{
      txt <- paste(txt,"Ages 18 and Over ",sep="") # then it use the selected group
    }
    txt <-paste(txt,"Living in the Community ",sep="")
    if(state_dummy6() == "Any"){ # if any group is selected
      txt <- paste(txt,"in ",input$State6,sep="") # then it use the selected group
    } else {
      txt <- paste(txt,"for the United States and States",sep="")
    }
    paste(txt,", by Disability Status",": ",as.numeric(input$Year6)-4,"-",input$Year6,sep="")
  })
  
  dis6 <- reactive({
    if(input$Disability6=="Disability"){
      "Disability"
    }
    # else if(input$Disability6=="Deaf or serious difficulty hearing"){
    #   "Hearing Disability"
    # }
    # else if(input$Disability6=="Blind or serious difficulty seeing"){
    #   "Vision Disability"
    # }
    # else if(input$Disability6=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "Cognitive Disability"
    # }
    # else if(input$Disability6=="Serious difficulty walking or climbing stairs"){
    #   "Ambulatory Disability"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "Self-Care Disability"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "Independent Living Disability"
    # }
  })
  
  output$Citation6 <- renderText({
    txt <- paste("Citation: ",authors,"(",year(Sys.Date()),"). Annual Disability Statistics Compendium: ",input$Year6," (Custom Poverty Table). Durham, NH: University of New Hampshire, Institute on Disability. Source: U.S. Census Bureau, ",as.numeric(input$Year6) - 4," - ",input$Year6," American Community Survey 5-year estimates. https://data.census.gov. Based on a sample and subject to sampling variability.",sep="")
    txt
  })
  
  tgen6 <- reactive({
    tgentmp3(Dat6(),"Poverty",col_spanners = c(dis6(),paste("No",dis6()),"test"))
  })
  
  output$TStable6 <- function(){
    rt <- rando_text()
    tgen6() %>%
      as_xml_document() %>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- includeHTML(paste(rt,'.html',sep=""))
    invisible(file.remove(paste(rt,'.html',sep="")))
    return(tmp)
  }

  ################
  ################
  # Render ouptuts
  
  download_prepare6 <- reactive({
    rt <- rando_text()
    tgen6()%>%as_xml_document()%>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- read_html(paste(rt,'.html',sep="")) %>%
      html_table()
    invisible(file.remove(paste(rt,'.html',sep="")))
    tmp
  })
  
  output$Downloader6 <- downloadHandler(
    filename = function(){
      paste(title6(),'download.xlsx',sep="")},
    content = function(file){write_xlsx(download_prepare(),file)}
  )
  
#######################################################################
#######################################################################
#######################################################################
  
  ################
  # Insurance 
  
  ################
  # Load in inputs
  dis_dummy9 <- reactive({
    #if(input$Disability1=="Disability"){
      "disability"
    #}
    # else if(input$Disability1=="Deaf or serious difficulty hearing"){
    #   "hearing"
    # }
    # else if(input$Disability1=="Blind or serious difficulty seeing"){
    #   "seeing"
    # }
    # else if(input$Disability1=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "remembering"
    # }
    # else if(input$Disability1=="Serious difficulty walking or climbing stairs"){
    #   "mobility"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "selfcare"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "independentliving"
    # }
  })
  # gen_dummy <- reactive({
  #   if(!input$Gender1=="All"){
  #     "Any"
  #   }
  #   else if(input$Gender1=="All"){
  #    "All"
  #   }
  # })
  agegroup_dummy9 <- reactive({
    if(!input$Agegroup9=="All"){
      "Any"
    }
    else if(input$Agegroup9=="All"){
      "All"
    }
  })
  state_dummy9 <- reactive({
    if(!input$State9=="All"){
      "Any"
    }
    else if(input$State9=="All"){
      "All"
    }
  })
  measure_dummy9 <- reactive({
    if(input$Measure9 == "INSURANCE"){
      "With Health Insurance"
    }
    else if(input$Measure9 == "INSURANCE1"){
      "With Private Health Insurance"
    }
    else if(input$Measure9 == "INSURANCE2"){
      "With Public Health Insurance"
    }
  })
  
  denom_dummy9 <- reactive({
    if(input$Measure9 == "INSURANCE"){
      ""
    }
    else if(input$Measure9 == "INSURANCE1"){
      "With Health Coverage"
    }
    else if(input$Measure9 == "INSURANCE2"){
      "With Health Coverage"
    }
  })
  
  ################
  # Data from GCS
  Dat9 <- reactive({
    index <- which(
      filterer6$agegroup == agegroup_dummy9())
    filename <- paste("acs",input$Year9,"_",index,"_",dis_dummy9(),"_",input$Measure9,"_COUNTY.rds",sep="")
    noprint <- cloud_prep() #
    tmp <- get_object(filename,"compendium_project_storage",type="actuals")
    tmp <- tmp[-c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),] %>%
      rbind(tmp[c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),]) %>%
      select(-Geography,-contains("dagger_")) %>%
      rename(Geography = Geographic.Area.Name)
    # Filter based on input selection
    if(state_dummy9() == "All"){
      tmp <- tmp %>%
        filter(!str_detect(Geography,","))
    } else {
      tmp <- tmp %>%
        filter(str_detect(Geography,"United States") | str_detect(Geography,input$State9))
    }
    # if(gen_dummy() == "Any"){
    #   tmp <- tmp %>%
    #     filter(gender == input$Gender1) %>%
    #     select(-gender)
    # }
    if(agegroup_dummy9() == "Any"){
      tmp <- tmp %>%
        filter(agegroup1 == input$Agegroup9) %>%
        select(-agegroup1)
    }
    tmp
  })
  ################
  
  ################
  # Table Title Info
  output$mTitle9 <- renderText({
    title9()
  })
  
  title9 <- reactive({
    txt <- "Custom Table: "
    # if(gen_dummy() == "Any"){
    #   txt <- paste(txt,input$Gender1," ",sep="")
    # }
    txt <-paste(txt,"Civilians ",measure_dummy9()," ",sep="")
    ######### Age group choices ###################
    if(agegroup_dummy9() == "Any"){ # if any group is selected
      txt <- paste(txt,input$Agegroup9," ",sep="") # then it use the selected group
    } 
    txt <-paste(txt,"Living in the Community ",sep="")
    if(state_dummy9() == "Any"){ # if any group is selected
      txt <- paste(txt,"in ",input$State9,sep="") # then it use the selected group
    } else {
      txt <- paste(txt,"for the United States and States",sep="")
    }
    paste(txt,", by Disability Status",": ",as.numeric(input$Year9)-4,"-",input$Year9,sep="")
  })
  
  dis9 <- reactive({
    if(input$Disability9=="Disability"){
      "Disability"
    }
    # else if(input$Disability9=="Deaf or serious difficulty hearing"){
    #   "Hearing Disability"
    # }
    # else if(input$Disability9=="Blind or serious difficulty seeing"){
    #   "Vision Disability"
    # }
    # else if(input$Disability9=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "Cognitive Disability"
    # }
    # else if(input$Disability9=="Serious difficulty walking or climbing stairs"){
    #   "Ambulatory Disability"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "Self-Care Disability"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "Independent Living Disability"
    # }
  })
  
  output$Citation9 <- renderText({
    txt <- paste("Citation: ",authors,"(",year(Sys.Date()),"). Annual Disability Statistics Compendium: ",input$Year9," (Custom Insurance Table). Durham, NH: University of New Hampshire, Institute on Disability. Source: U.S. Census Bureau, ",as.numeric(input$Year9) - 4," - ",input$Year9," American Community Survey 5-year estimates. https://data.census.gov. Based on a sample and subject to sampling variability.",sep="")
    txt
  })
  
  tgen9 <- reactive({
    tgentmp3(Dat9(),measure_dummy9() %>% str_replace_all(" ",intToUtf8(95)),
             col_spanners = c(dis9(),paste("No",dis9()),"test"),denom_dummy9())
  })
  
  output$TStable9 <- function(){
    rt <- rando_text()
    tgen9() %>%
      as_xml_document() %>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- includeHTML(paste(rt,'.html',sep=""))
    invisible(file.remove(paste(rt,'.html',sep="")))
    return(tmp)
  }

  ################
  ################
  # Render ouptuts
  
  download_prepare9 <- reactive({
    rt <- rando_text()
    tgen9()%>%as_xml_document()%>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- read_html(paste(rt,'.html',sep="")) %>%
      html_table()
    invisible(file.remove(paste(rt,'.html',sep="")))
    tmp
  })
  
  output$Downloader9 <- downloadHandler(
    filename = function(){
      paste(title9(),'download.xlsx',sep="")},
    content = function(file){write_xlsx(download_prepare(),file)}
  )
  
  #######################################################################
  #######################################################################
  #######################################################################
  
  ################
  # Education 
  
  ################
  # Load in inputs
  dis_dummy13 <- reactive({
    #if(input$Disability1=="Disability"){
    "disability"
    #}
    # else if(input$Disability1=="Deaf or serious difficulty hearing"){
    #   "hearing"
    # }
    # else if(input$Disability1=="Blind or serious difficulty seeing"){
    #   "seeing"
    # }
    # else if(input$Disability1=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "remembering"
    # }
    # else if(input$Disability1=="Serious difficulty walking or climbing stairs"){
    #   "mobility"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "selfcare"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "independentliving"
    # }
  })
  # gen_dummy <- reactive({
  #   if(!input$Gender1=="All"){
  #     "Any"
  #   }
  #   else if(input$Gender1=="All"){
  #    "All"
  #   }
  # })
  # agegroup_dummy13 <- reactive({
  #   if(!input$Agegroup13=="All"){
  #     "Any"
  #   }
  #   else if(input$Agegroup13=="All"){
  #     "All"
  #   }
  # })
  state_dummy13 <- reactive({
    if(!input$State13=="All"){
      "Any"
    }
    else if(input$State13=="All"){
      "All"
    }
  })
  
  ################
  # Data from GCS
  Dat13 <- reactive({
    index <- which(1==1)
    filename <- paste("acs",input$Year13,"_",index,"_",dis_dummy13(),"_EDUC_COUNTY.rds",sep="")
    noprint <- cloud_prep() #
    tmp <- get_object(filename,"compendium_project_storage",type="actuals")
    tmp <- tmp[-c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),] %>%
      rbind(tmp[c(which(str_detect(tmp$Geographic.Area.Name,"Puerto Rico"))),]) %>%
      select(-Geography) %>%
      rename(Geography = Geographic.Area.Name)
    # Filter based on input selection
    if(state_dummy13() == "All"){
      tmp <- tmp %>%
        filter(!str_detect(Geography,","))
    } else {
      tmp <- tmp %>%
        filter(str_detect(Geography,"United States") | str_detect(Geography,input$State13))
    }
    # if(gen_dummy() == "Any"){
    #   tmp <- tmp %>%
    #     filter(gender == input$Gender1) %>%
    #     select(-gender)
    # }
    # if(agegroup_dummy13() == "Any"){
    #   tmp <- tmp %>%
    #     filter(agegroup1 == input$Agegroup13) %>%
    #     select(-agegroup1)
    # }
    tmp <- tmp %>%
      select(-agegroup1)
    tmp
  })
  ################
  
  ################
  # Table Title Info
  output$mTitle13 <- renderText({
    title13()
  })
  
  title13 <- reactive({
    txt <- "Custom Table: Education Level of "
    # if(gen_dummy() == "Any"){
    #   txt <- paste(txt,input$Gender1," ",sep="")
    # }
    txt <-paste(txt,"Civilians 25 Years and Over",sep="")
    ######### Age group choices ###################
    # if(agegroup_dummy13() == "Any"){ # if any group is selected
    #   if(!input$Disability13=="Difficulty doing errands alone"){ # and if not difficulty doing errands alone
    #     txt <- paste(txt,"Ages ",input$Agegroup13," ",sep="") # then it use the selected group
    #   } else { # otherwise if difficulty doing errands alone
    #     if (input$Agegroup13 == "5 to 17 Years"){ # and its the young age group
    #       txt <- paste(txt,"Ages 15 to 17 ",sep="") # update the group to only include 15 to 17
    #     } else{
    #       txt <- paste(txt,"Ages ",input$Agegroup13," ",sep="") # then it use the selected group
    #     }
    #   }
    # } else {
    #   if(input$Disability13=="Difficulty dressing or bathing" |
    #      input$Disability13=="Serious difficulty concentrating, remembering, or making decisions" |
    #      input$Disability13=="Serious difficulty walking or climbing stairs"){
    #     txt <- paste(txt,"Age 5 Years and Over ",sep="")
    #   }
    #   if(input$Disability13=="Difficulty doing errands alone"){
    #     txt <- paste(txt,"Age 18 Years and Over ",sep="")
    #   }
    # }
    txt <-paste(txt,"Living in the Community ",sep="")
    if(state_dummy13() == "Any"){ # if any group is selected
      txt <- paste(txt,"in ",input$State13,sep="") # then it use the selected group
    } else {
      txt <- paste(txt,"for the United States and States",sep="")
    }
    paste(txt,", by Disability Status",": ",as.numeric(input$Year13)-4,"-",input$Year13,sep="")
  })
  
  dis13 <- reactive({
    if(input$Disability13=="Disability"){
      "Disability"
    }
    # else if(input$Disability13=="Deaf or serious difficulty hearing"){
    #   "Hearing Disability"
    # }
    # else if(input$Disability13=="Blind or serious difficulty seeing"){
    #   "Vision Disability"
    # }
    # else if(input$Disability13=="Serious difficulty concentrating, remembering, or making decisions"){
    #   "Cognitive Disability"
    # }
    # else if(input$Disability13=="Serious difficulty walking or climbing stairs"){
    #   "Ambulatory Disability"
    # }
    # else if(input$Disability1=="Difficulty dressing or bathing"){
    #   "Self-Care Disability"
    # }
    # else if(input$Disability1=="Difficulty doing errands alone"){
    #   "Independent Living Disability"
    # }
  })
  
  output$Citation13 <- renderText({
    txt <- paste("Citation: ",authors,"(",year(Sys.Date()),"). Annual Disability Statistics Compendium: ",input$Year13," (Custom Education Table). Durham, NH: University of New Hampshire, Institute on Disability. Source: U.S. Census Bureau, ",as.numeric(input$Year13) - 4," - ",input$Year13," American Community Survey 5-year estimates. https://data.census.gov. Based on a sample and subject to sampling variability.",sep="")
    txt
  })
  
  tgen13 <- reactive({
    tgentmp13(Dat13(),"educ",col_spanners = c("Total",dis13(),paste("No",dis13())))
  })
  
  output$TStable13 <- function(){
    rt <- rando_text()
    tgen13() %>%
      as_xml_document() %>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- includeHTML(paste(rt,'.html',sep=""))
    invisible(file.remove(paste(rt,'.html',sep="")))
    return(tmp)
  }
  
  ################
  ################
  # Render ouptuts
  
  download_prepare13 <- reactive({
    rt <- rando_text()
    tgen13()%>%as_xml_document()%>%
      write_html(paste(rt,'.html',sep=""))
    tmp <- read_html(paste(rt,'.html',sep="")) %>%
      html_table()
    invisible(file.remove(paste(rt,'.html',sep="")))
    tmp
  })
  
  output$Downloader13 <- downloadHandler(
    filename = function(){
      paste(title13(),'download.xlsx',sep="")},
    content = function(file){write_xlsx(download_prepare(),file)}
  )
    
###########################################################################

    #Observe input URL parameter to open to specific tab...
  #links look like http://127.0.0.1:3344/?tab=Employment&a=b
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(any("tab" == names(query))){
      updateNavbarPage(
        session = getDefaultReactiveDomain(),
        inputId = "container",
        selected = query$tab
      )
    }
    if(any("year" == names(query))){
      updateSelectInput(session = getDefaultReactiveDomain(), 
                        "Year1",
                        choices = c(data_years$acs[which(data_years$acs == query$year)],data_years$acs[which(data_years$acs != query$year)]))
      updateSelectInput(session = getDefaultReactiveDomain(), 
                        "Year3",
                        choices = c(data_years$acs[which(data_years$acs == query$year)],data_years$acs[which(data_years$acs != query$year)]))
      updateSelectInput(session = getDefaultReactiveDomain(), 
                        "Year6",
                        choices = c(data_years$acs[which(data_years$acs == query$year)],data_years$acs[which(data_years$acs != query$year)]))
      updateSelectInput(session = getDefaultReactiveDomain(), 
                        "Year9",
                        choices = c(data_years$acs[which(data_years$acs == query$year)],data_years$acs[which(data_years$acs != query$year)]))
      updateSelectInput(session = getDefaultReactiveDomain(), 
                        "Year13",
                        choices = c(data_years$acs[which(data_years$acs == query$year)],data_years$acs[which(data_years$acs != query$year)]))
    }
  })
  
  ################
  
  #Disclaimer navigation
  observeEvent(input$Link2Disclaimer1, {
    newvalue <- "Disclaimer"
    updateNavbarPage(session, "container", newvalue)
  })
  observeEvent(input$Link2Disclaimer2, {
    newvalue <- "Disclaimer"
    updateNavbarPage(session, "container", newvalue)
  })
  observeEvent(input$Link2Disclaimer3, {
    newvalue <- "Disclaimer"
    updateNavbarPage(session, "container", newvalue)
  })
  observeEvent(input$Link2Disclaimer4, {
    newvalue <- "Disclaimer"
    updateNavbarPage(session, "container", newvalue)
  })
  observeEvent(input$Link2Disclaimer5, {
    newvalue <- "Disclaimer"
    updateNavbarPage(session, "container", newvalue)
  })
  
})

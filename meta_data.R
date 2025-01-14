# Sets data stored locally in app

library(shiny)
library(shinyjs)
library(tidyverse)
library(writexl)
library(xml2)
library(rvest)
library(shinylogs)
library(DBI)
library(lubridate)
#setwd("Supplement-County/")
getwd()

f <- function(x) {
  x <- gsub(">", "", gsub("<U\\+", "\\\\u", x))
  stringi::stri_unescape_unicode(x)
}

data_years <- list("acs" = c(2022),
                   "cps" = c())
uniques <- list()
uniques$gender <- c("Female","Male")
uniques$agegroup <- c("Under 5 Years",
                      "5 to 17 Years",
                      "Under 18 Years",
                      "18 to 64 Years",
                      "18 to 34 Years",
                      "35 to 64 Years",
                      "65 Years and Over",
                      "65 to 74 Years",
                      "75 Years and Over")
uniques$agegroup6 <- c("18 to 64 Years",
                      "65 Years and Over")
uniques$agegroup9 <- c("Under 19 Years",
                       "19 to 64 Years",
                       "65 Years and Over")
uniques$state <- readRDS(paste("../../Compendium2.000/Supplement/GoogleUploads-County/acs2022_1_disability_PREV_COUNTY.rds",sep="")) %>%
  select("Geographic.Area.Name") %>%
  filter(!str_detect(Geographic.Area.Name,",") & !str_detect(Geographic.Area.Name,"United States")) %>% .$`Geographic.Area.Name` 

disclaimer <- "Data users are responsible for the interpretation of statistics presented. Please feel free to contact us at disability.statistics@unh.edu for questions or technical assistance. 

Funding for this publication is made possible by: The Rehabilitation Research and Training Center on Disability Statistics and Demographics (StatsRRTC), funded by the U.S. Department of Health and Human Services Administration for Community Living National Institute on Disability, Independent Living, and Rehabilitation Research (NIDILRR), grant number 90RTGE0001. The information developed by the StatsRRTC  does not necessarily represent the policies of the Department of Health and Human Services, and you should not assume endorsement by the Federal Government.

"

filterer <- data.frame(
  gender = c("All","Any"),
  agegroup =  c("All","Any")) %>%
  tidyr::expand(gender,agegroup)

filterer6 <- data.frame(
  agegroup =  c("All","Any")) %>%
  tidyr::expand(agegroup)
authors = "Thomas, N., Bach, S., & Houtenville, A."
###########
#Support Functions
rando_text <- function(n = 1) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

id_correction_fn <- function(tab_html){
  mod <- tab_html %>% 
    as_raw_html()%>%
    as.character()
  tmp1 <- data.frame(c = names(tab_html$`_data`),
                     n = nchar(names(tab_html$`_data`))) %>%
    .[-1,] %>% 
    arrange(desc(n))
  for (i in 1:length(tmp1$n)){
    mod <- mod %>% 
      str_replace_all(paste('\\Q',tmp1$c[i],'\\E','"',sep=""),
                      paste("col_id_",i,'"',sep="")) 
  }
  tmp2 <- data.frame(s = tab_html$`_spanners`$spanner_label %>% unlist(),
                     n = nchar(tab_html$`_spanners`$spanner_label)) %>%
    arrange(desc(n))
  if(length(tab_html$`_spanners`$spanner_label)>0){
    for (y in 1:length(tab_html$`_spanners`$spanner_label)){
      mod <- mod %>% 
        str_replace_all(paste('\\Q',tmp2$s[y],'\\E','"',sep=""),
                        paste("col_spanner_id_",y,'"',sep=""))
    }
  }
  
  ##########################
  mod <- mod %>% 
    str_replace_all('\\Q<table class="gt_table"\\E', '<table class="gt_table" tabindex="0"')
  ##########################
  
  return(mod)
}
tgentmp1 <- function(d,measure,col_spanners){
  names(d) <- c("Geography",
                "#","ME#",
                "# ","ME# ",
                "%","ME%",
                "#  ","ME#  ",
                "% ","ME% ")
  if(nrow(d) == 0){
    tmp_html <- tibble("Geography" = "-",
                           "#"="-",
                           "ME#" = "-") %>% 
      gt(id = paste("tb1",measure,sep="")) |>
      tab_source_note("This selection is either not included in the survey or not computed due to combinatorial complexity constraints.")
    return(id_correction_fn(tmp_html))
  } 
  tab_html <- d %>%
    gt(id = paste("tb1",measure,sep=""),rowname_col = "Geography") |>
    sub_missing(missing_text = "*") |>
    tab_stubhead(label = "Geography") |>
    tab_source_note(c("Margins of error (ME) are based on 95% confidence intervals.",
                      "† the margin of error is estimated assuming independence due to the necessity of combining error estimtates when reporting aggregated measures.")) |>
    fmt_number(columns = c("#",
                           "# ",
                           "#  "),
               use_seps = TRUE,
               sep_mark = ",",
               decimals = 0) |>
    fmt_number(columns = c("%",
                           "% "),
               decimals = 2) |>
    tab_spanner(label = col_spanners[1],
                columns = c("#","ME#")) |>
    tab_spanner(label = col_spanners[2],
                columns = c("# ","ME# ",
                            "%","ME%")) |>
    tab_spanner(label = col_spanners[3],
                columns = c("#  ","ME#  ",
                            "% ","ME% ")) |>
    opt_table_font(
      font = list("Helvetica")) |>
    tab_options(
      container.height = "500px",
      table.font.size = 18, footnotes.font.size = 18, source_notes.font.size = 18) |>
    tab_style(style = "vertical-align:bottom", cells_column_labels()) |>
    tab_style(style = cell_borders(sides = c("right"),  weight = px(1), color="#CCCCCC"),
              locations = cells_body(columns = c(c(1:dim(d)[2])))) |>
    tab_style(style = cell_borders(sides = c("bottom"),  weight = px(1), color="#CCCCCC"),
              locations = cells_body(rows = c(c(1:dim(d)[1])))) #|>
    # opt_css(
    #   css = paste("
    #               .gt_stub {
    #               position: sticky;
    #               left: 0;
    #               }
    #               
    #               .gt_left {
    #               position: sticky;
    #               left: 0;
    #               }
    #               
    #               thead th:first-child {
    #               left: 0;
    #               z-index: 2;
    #               }
    #       
    #               ",
    #               sep="")
    # )
  id_correction_fn(tab_html)
}

tgentmp3 <- function(d,measure,col_spanners,total=""){
  names(d) <- c("Geography",
                "#","ME#", #
                "# ","ME# ",#
                "%","ME%",#
                "#  ","ME#  ",#
                "#   ","ME#   ",#
                "% ","ME% ",
                "Estimate", "Lower One-Sided 95% Confidence Interval",  "Upper One-Sided 95% Confidence Interval")
  if(nrow(d) == 0){
    tmp_html <- tibble("Geography" = "-",
                       "#"="-",
                       "ME#" = "-") %>% 
      gt(id = paste("tb1",measure,sep="")) |>
      tab_source_note("This selection is either not included in the survey or not computed due to combinatorial complexity constraints.")
    return(id_correction_fn(tmp_html))
  } 
  tab_html <- d %>%
    gt(id = paste("tb1",measure,sep=""),rowname_col = "Geography") |>
    sub_missing(missing_text = "*") |>
    tab_stubhead(label = "Geography") |>
    tab_source_note(c("Margins of error (ME) are based on 95% confidence intervals.",
                      "† the margin of error is estimated assuming independence due to the necessity of combining error estimtates when reporting aggregated measures.")) |>
    fmt_number(columns = c("#",
                           "# ",
                           "#  ",
                           "#   "),
               use_seps = TRUE,
               sep_mark = ",",
               decimals = 0) |>
    fmt_number(columns = c("%",
                           "% "),
               decimals = 2) |>
    tab_spanner(label = paste(col_spanners[1],"- Total",total),
                columns = c("#","ME#")) |>
    tab_spanner(label = paste(col_spanners[1],"-",measure),
                columns = c("# ","ME# ",
                            "%","ME%")) |>
    tab_spanner(label = paste(col_spanners[2],"- Total",total),
                columns = c("#  ","ME#  ")) |>
    tab_spanner(label = paste(col_spanners[2],"-",measure),
                columns = c("#   ","ME#   ",
                            "% ","ME% ")) |>
    tab_spanner(label = "Relative Ratio",
                columns = c("Estimate", "Lower One-Sided 95% Confidence Interval",  "Upper One-Sided 95% Confidence Interval")) |>
    
    opt_table_font(
      font = list("Helvetica")) |>
    tab_options(
      container.height = "500px",
      table.font.size = 18, footnotes.font.size = 18, source_notes.font.size = 18) |>
    tab_style(style = "vertical-align:bottom", cells_column_labels()) |>
    tab_style(style = cell_borders(sides = c("right"),  weight = px(1), color="#CCCCCC"),
              locations = cells_body(columns = c(c(1:dim(d)[2])))) |>
    tab_style(style = cell_borders(sides = c("bottom"),  weight = px(1), color="#CCCCCC"),
              locations = cells_body(rows = c(c(1:dim(d)[1])))) #|>
    # opt_css(
    #   css = paste("
    #               .gt_stub {
    #               position: sticky;
    #               left: 0;
    #               }
    #               
    #               .gt_left {
    #               position: sticky;
    #               left: 0;
    #               }
    #               
    #               thead th:first-child {
    #               left: 0;
    #               z-index: 2;
    #               }
    #       
    #               ",
    #               sep="")
    #)
  id_correction_fn(tab_html)
}

tgentmp13 <- function(d,measure,col_spanners){
  names(d) <- c("Geography",
                "Education Level",
                "#","ME#", #
                "%","ME%",#
                "# ","ME# ",#
                "% ","ME% ",
                "#  ","ME#  ",
                "%  ","ME%  ",
                "Estimate",
                "Lower One-Sided 95% Confidence Interval",
                "Upper One-Sided 95% Confidence Interval")
  if(nrow(d) == 0){
    tmp_html <- tibble("Geography" = "-",
                       "#"="-",
                       "ME#" = "-") %>% 
      gt(id = paste("tb1",measure,sep="")) |>
      tab_source_note("This selection is either not included in the survey or not computed due to combinatorial complexity constraints.")
    return(id_correction_fn(tmp_html))
  } 
  tab_html <- d %>%
    gt(id = paste("tb1",measure,sep=""),rowname_col = "Geography") |>
    sub_missing(missing_text = "*") |>
    tab_stubhead(label = "Geography") |>
    fmt_number(columns = c("#","ME#",
                           "# ","ME# ",
                           "#  ","ME#  "),
               use_seps = TRUE,
               sep_mark = ",",
               decimals = 0) |>
    tab_source_note(c("Margins of error (ME) are based on 95% confidence intervals.",
                      "*estimate is unavailable.")) |>
    tab_spanner(label = col_spanners[1],
                columns = c("#","ME#",
                            "%","ME%")) |>
    tab_spanner(label = col_spanners[2],
                columns = c("# ","ME# ",
                            "% ","ME% ")) |>
    tab_spanner(label = col_spanners[3],
                columns = c("#  ","ME#  ",
                            "%  ","ME%  ")) |>
    tab_spanner(label = "Relative Ratio",
                columns = c("Estimate",
                            "Lower One-Sided 95% Confidence Interval",
                            "Upper One-Sided 95% Confidence Interval"
                            )) |>
    opt_table_font(
      font = list("Helvetica")) |>
    tab_options(
      container.height = "500px",
      table.font.size = 18, footnotes.font.size = 18, source_notes.font.size = 18) |>
    tab_style(style = "vertical-align:bottom", cells_column_labels()) |>
    tab_style(style = cell_borders(sides = c("right"),  weight = px(1), color="#CCCCCC"),
              locations = cells_body(columns = c(c(1:dim(d)[2])))) |>
    tab_style(style = cell_borders(sides = c("bottom"),  weight = px(1), color="#CCCCCC"),
              locations = cells_body(rows = c(c(1:dim(d)[1])))) #%>%
  #   opt_css(
  #     css = paste("
  #                 .gt_stub {
  #                 position: sticky;
  #                 left: 0;
  #                 }
  #                 
  #                 .gt_left {
  #                 position: sticky;
  #                 left: 0;
  #                 }
  #                 
  #                 thead th:first-child {
  #                 left: 0;
  #                 z-index: 2;
  #                 }
  #         
  #                 ",
  #                 sep="")
  #   )
  id_correction_fn(tab_html)
}

source("gcs_interface.R")
save.image(".RData")

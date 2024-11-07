cloud_prep <- function(){
  library(googleAuthR)
  #gar_auth()
  if(Sys.getenv("COMPUTERNAME") == "MSI"){
    Sys.setenv("GCS_DEFAULT_BUCKET" = "compendium-402512",
               "GCS_AUTH_FILE" = paste(str_replace_all(getwd(),"/","\\\\"),"\\compendium-402512-a06b2e21a7d4.json",sep=""))
  }
  if(Sys.getenv("COMPUTERNAME") != "MSI"){
    Sys.setenv("GCS_DEFAULT_BUCKET" = "compendium-402512",
               "GCS_AUTH_FILE" = paste(getwd(),"/","compendium-402512-d16ee0969e4a.json",sep=""))
  }
  library(googleCloudStorageR)
  buckets <- gcs_list_buckets("compendium-402512")
  bucket <- "compendium_project_storage"
  objects <- gcs_list_objects(bucket)
  return(list(bucket = bucket,objects = objects))
}

get_object <- function(objname,bucket,type){
  if(type == "actuals"){
    raw = tibble(ST_text="",
                 gender="",
                 hisprace="",
                 agegroup="")
    try(
    raw_download <- gcs_get_object(objname,
                                   bucket = bucket,
                                   saveToDisk = objname,
                                   overwrite = TRUE))
    try(raw <- readRDS(objname) %>%
      select(-contains("_LCI"),-contains("_UCI"),-contains("RR_MoE")) %>%
      cbind(readRDS(objname) %>%
              select(contains("RR_LCI"),contains("RR_UCI"))))
    unlink(objname)
    fnames <- list.files(pattern = "\\.rds$") 
    unlink(fnames)
    raw
  }
}

getSqlConnection <- function(db){
  conn <-
    RMySQL::dbConnect(
      RMySQL::MySQL(),
      username = 'compendium-logs-user',
      password = '>np[n8evZzf=1;vZ',
      host = '34.56.118.167',
      dbname = db,
      port = 3306
    ) 
  return(conn)
}

table_write <- function(db,name, df, key_str, overwrite, append, row.names){
  conn <- getSqlConnection(db)
  res <- dbListTables(conn)
  res
  try(for(j in 1:(dim(df)[1])){
    dbWriteTable(conn,name = name, value = df[j,], overwrite = overwrite, append = append, row.names = row.names)
  })
  try(if (length(key_str)>0){
    for (i in 1: length(key_str)){
      DBI::dbSendQuery(conn,key_str[i])
    }
  })
  dbDisconnect(conn)
  return()
}

sendlog <- function(logs,db,idsession,ip,name){
  library(lubridate)
  # Prepare user inputs for storage in Google SQL Connect MySQL db
  
  # Initialize expected INPUT structure
  inputs <- data.frame(name = NULL, timestamp = NULL, value = NULL, type = NULL, binding = NULL)
  # Convert standard JSONish into tabular format
  for (i in 1: length(logs$inputs)){
    inputs <- inputs %>%
      rbind(logs$inputs[[i]])
  }
  # Clean up inputs dataframe to send it...
  inputs <- inputs %>%
    mutate(timestamp = timestamp %>% as_datetime(),
           idsession = idsession) %>%
    rename(values = value)
  
  # Initialize expected OUTPUT structure
  outputs <- data.frame(name = NULL, timestamp = NULL, binding = NULL)
  # Convert standard JSONish into tabular format
  for (i in 1: length(logs$outputs)){
    outputs <- outputs %>%
      rbind(logs$outputs[[i]])
  }
  # Clean up inputs dataframe to send it...
  outputs <- outputs %>%
    mutate(timestamp =  timestamp %>% as_datetime(),
           idsession = idsession) 
  
  # # Initialize expected ERRORS structure
  # try(errors <- data.frame(name = NULL, timestamp = NULL, error = NULL))
  # # Convert standard JSONish into tabular format
  # try(for (i in 1: length(logs$errors)){
  #   errors <- errors %>%
  #     rbind(logs$errors[[i]])
  # })
  # # Clean up inputs dataframe to send it...
  # try(errors <- errors %>%
  #   mutate(timestamp = timestamp %>% lubridate::as_datetime(),
  #          idsession = idsession))
  # 
  # try(print(errors))
  
  session <- cbind(idsession,name,ip,logs$session) %>%
    mutate(server_connected =  server_connected %>% as_datetime() ,
           server_disconnected = server_disconnected %>% as_datetime(),
           browser_connected = browser_connected %>% as_datetime())
  
  ############################################################################
  # Send to Tracking DB
  table_write(db = db,
              name = 'inputs',
              df = inputs,
              key_str = c(),
              overwrite = FALSE,
              append = TRUE,
              row.names = FALSE)
  
  table_write(db = db,
              name = 'outputs',
              df = outputs,
              key_str = c(),
              overwrite = FALSE,
              append = TRUE,
              row.names = FALSE)
  
  # try(table_write(db = db,
  #             name = 'errors',
  #             df = errors,
  #             key_str = c(), 
  #             overwrite = FALSE,
  #             append = TRUE,
  #             row.names = FALSE))
  
  table_write(db = db,
              name = 'session',
              df = session,
              key_str = c(), 
              overwrite = FALSE,
              append = TRUE,
              row.names = FALSE)
}


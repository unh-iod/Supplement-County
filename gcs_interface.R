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

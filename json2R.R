if (!require(jsonlite)) install.packages("jsonlite", dependencies = TRUE);
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE);
library(jsonlite)
library(dplyr)

# set working directory to the the path of the current open file in RStudio
if (!require(rstudioapi)) install.packages("rstudioapi", dependencies = TRUE);
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

json <- fromJSON("../addons.json") # takes about 18 seconds to load
addons <- data.frame(
  id = json$id, 
  guid = json$guid, 
  weekly_downloads = json$weekly_downloads, 
  name_en_us = json$name[["en-US"]], 
  description_en_us = json$description[["en-US"]], 
  homepage_en_us = json$homepage[["en-US"]], 
  icon_url = json$icon_url)
addons <- addons %>% distinct(guid, .keep_all = TRUE) # removes duplicates in relation to the guid column
saveRDS(addons, file = "./addons.rds")

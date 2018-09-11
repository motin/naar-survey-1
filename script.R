# set working directory to the the path of the current open file in RStudio
if (!require(rstudioapi)) install.packages("rstudioapi", dependencies = TRUE);
library(rstudioapi)
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# include data and common functions
source("tools.R")

# settings for plot exports
defaultWidth <- 1000
defaultHeight <- 1000
defaultPointSize <- 40
pdf.options(encoding='ISOLatin2.enc')

# === related to Part 1 ===

# load add-on data
addons <- loadAddonsData()

# select the ones relevant for this run
addonGuids <- c(
  "{c45c406e-ab73-11d8-be73-000a95be3b12}", 
  "DoesAmazonShipTo@usefulhelper.com", 
  "{a949831f-d9c0-45ae-8c60-91c2a86fbfb6}", 
  "jid0-1goESrxnxB5zVAoHJkD3TYqF6lB@jetpack",
  "{5f27f390-2663-4c7d-addb-a924ba085966}"
)
selectionOfAddons <- getAddonsThatMatchesAddonGuids(addons, addonGuids)

# === related to Part 2 ===
# debug visualization of the individual contribution, showing the specified motivations as needs on an odi landscape

#responses
#  clientid1_
#  motivation1_
#    importance
#    satisfactionWithoutAddon
#    satisfactionWithAddon
#  motivation2_
#    importance
#    satisfactionWithoutAddon
#    satisfactionWithAddon

# === main analysis ===
# mapping of specified motivations to a common needs map, then performing odi analysis with all the respondents' ratings

odiLandscapePlots <- generateOdiLandscapePlots()

png("plot1a_empty_odi_landspace_continous.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
odiLandscapePlots$odiLandscapePlotContinuous
dev.off()

png("plot1b_empty_odi_landspace_discrete.png",
    width=defaultWidth,
    height=defaultHeight,
    pointsize=defaultPointSize,
)
odiLandscapePlots$odiLandscapePlotDiscrete
dev.off()

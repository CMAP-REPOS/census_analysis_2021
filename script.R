library(tidyverse)
library(tidycensus)
library(cmapplot)
library(censusapi)



# Run every session
Sys.setenv(CENSUS_KEY="ea4079a757dc7fa173162ba144f2e6abb92b1112")
# readRenviron("./.Renviron")
Sys.getenv("CENSUS_KEY")

# # Daniel's Key:
# census_api_key("ea4079a757dc7fa173162ba144f2e6abb92b1112", install=TRUE)
#

# CMAP area, county FIPS codes
cmap_counties <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")




# ## View all available APIs
# listCensusApis() %>%
#   View()
#
# # View variables of response rate API
# listCensusMetadata(name = "acs/acs5",
#                    vintage = "2019") %>%
#   View()

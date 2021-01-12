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



#### FIGURE 1

# Import population estimates (downloaded from the Census Bureau)
state_populations <-
  read.csv("S:/Projects_FY21/Policy Development and Analysis/Census Response/2021 analysis/population_estimates.csv")

# Create list of states (for factor levels)
state_factors <-
  state_populations[which(state_populations$geog != "Illinois"),]$geog

# Make Illinois the last in the list, so that it is drawn last
state_factors <- c(state_factors,"Illinois")

# Calculate normalized population figures
state_populations_normalized <-
  state_populations %>%
  # Divide all population figures by 2010 Census count
  mutate(across(c("census":"X2020"),~./census)) %>%
  # Make data longer
  pivot_longer(cols = c("census":"X2020")) %>%
  # Keep relevant records
  filter(!(name %in% c("estimates_base","X2010"))) %>%
  # Rename "census" to "2010"
  mutate(name = case_when(
    name == "census" ~ "2010",
    TRUE ~ name)) %>%
  # Remove "X" from year names
  mutate(year = as.integer(sub("X","",name))) %>%
  # Add factor levels to states
  mutate(geog = factor(geog,levels = state_factors))



# Create Figure 1
figure1 <-
  state_populations_normalized %>%
  filter(geog != "District of Columbia") %>%
  ggplot(aes(x = year, y = value, color = geog)) +
  geom_line() +
  theme_cmap(legend.position = "none") +
  cmap_color_highlight(state_populations_normalized$geog,"Illinois") +
  scale_x_continuous(breaks = c(2010,2012,2014,2016,2018,2020)) +
  scale_y_continuous(label = scales::label_percent(accuracy = 1),limits = c(.95,1.20))

finalize_plot(figure1,
              title = "State-by-state population change since 2010 (highlighting Illinois).",
              caption = "Note: 2010 population figures are Census counts as of
              April 1, 2010. Other years are population estimates as of July 1
              of the respective year, normalized against 2010 population totals.
              Excludes Washington, D.C. as well as Puerto Rico and other U.S. territories.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of 2020
              U.S. Census Bureau Population Estimates.",
              caption_valign = "t",
              filename = "figure1",
              mode = "png",
              overwrite = TRUE)



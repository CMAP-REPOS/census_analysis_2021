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


####### MSA-level population figures (cited in text)


state_fips = unique(fips_codes$state_code)[1:51]

# Pull data at the county level across the US

pop_counties_USA_2014 <-
  map_dfr(
    state_fips,
    ~get_acs(geography = "county",
             state = .,
             variables = c("B01001_001","B01001I_001"),
             cache_table = TRUE,
             year = 2014,
             survey = "acs5",
             output = "wide"))

pop_counties_USA_2014 <-
  pop_counties_USA_2014 %>%
  select(GEOID,
         county = NAME,
         pop14 = B01001_001E,
         hispa14 = B01001I_001E)

pop_counties_USA_2019 <-
  map_dfr(
    state_fips,~get_acs(geography = "county",
                    state = .,
                    variables = c("B01001_001","B01001I_001"),
                    cache_table = TRUE,
                    year = 2019,
                    survey = "acs5",
                    output = "wide"))

pop_counties_USA_2019 <-
  pop_counties_USA_2019 %>%
  select(GEOID,
         county = NAME,
         pop19 = B01001_001E,
         hispa19 = B01001I_001E)

pop_counties_USA <- inner_join(pop_counties_USA_2014,
                               pop_counties_USA_2019 %>% select(-county),
                               by = c("GEOID"))

## Import crosswalk file for counties to MSAs
county_msa_crosswalk <- read.csv("S:/Projects_FY21/Policy Development and Analysis/Census Response/2021 analysis/County_MSA_Crosswalk.csv") %>%
  # Add leading spaces for states with FIPS codes that start with 0
  mutate(County_GEOID = sprintf("%05s",as.character(County_GEOID))) %>%
  # Replace leading spaces with 0
  mutate(County_GEOID = gsub('^[ ]{1,}','0',County_GEOID)) %>%
  # Remove " (Metropolitan Statistical Area)"
  mutate(MSA = gsub("\\s*\\([^\\)]+\\)","",MSA)) %>%
  select(MSA_GEOID,MSA,County_GEOID,County)


# Subset data from relevant metropolitan area peers
pop_counties_MSAs <- pop_counties_USA %>%
  # Join data with the county / MSA crosswalk file to assign data to MSAs.
  # Use inner join to remove counties not in an MSA.
  inner_join(.,county_msa_crosswalk, by = c("GEOID"="County_GEOID")) %>%
  # Adjust Chicago MSA to instead be the CMAP seven-county region
  ## Remove counties in the Chicago MSA that are not in the seven counties
  filter(!(MSA_GEOID == 16980 & !(GEOID %in% cmap_counties))) %>%
  ## Rename the MSA field for the seven counties to be "CMAP region"
  mutate(MSA = case_when(
    MSA_GEOID == 16980 ~ "CMAP region",
    TRUE ~ MSA)) %>%
  # Summarize by MSA
  group_by(MSA) %>%
  # Sum population variables
  summarize(across(pop14:hispa19,sum)) %>%
  # Sort by 2019 population
  arrange(-pop19) %>%
  # Keep top 50 by population
  slice_head(n = 50)


## Join the two years
msa_pop <-
  pop_counties_MSAs %>%
  mutate(pct_change = round(100*((pop19 - pop14) / pop14),2),
         pop_change = pop19 - pop14) %>%
  arrange(-pct_change) %>%
  mutate(Rank = row_number()) %>%
  mutate(pct_change = paste0(pct_change,"%")) %>%
  select(Rank,
         MSA,
         "2014 Population" = pop14,
         "2019 Population" = pop19,
         "Change (population)" = pop_change,
         "Change (percent)" = pct_change)

# Keep the top 10 and bottom 10 MSAs
msa_pop_export <- msa_pop[c(1:10,(nrow(msa_pop)-9):nrow(msa_pop)),]


# Export results

write.csv(msa_pop_export,"msa_pop_export.csv")



### Examine Hispanic population

## Join the two years
msa_hispa_pop <-
  pop_counties_MSAs %>%
  mutate(hispa_pct_change = round(100*((hispa19 - hispa14) / hispa14),2),
         hispa_pop_change = hispa19 - hispa14) %>%
  mutate(pct_change = round(100*((pop19 - pop14) / pop14),2)) %>%
  arrange(-hispa_pct_change) %>%
  mutate(Rank = row_number()) %>%
  mutate(pct_change = paste0(pct_change,"%"),
         hispa_pct_change = paste0(hispa_pct_change,"%")) %>%
  select(Rank,
         MSA,
         "Hispanic pop. (2010-14)" = hispa14,
         "Hispanic pop. (2015-2019)" = hispa19,
         "Diff." = hispa_pop_change,
         "Pct. change (Hispanic)" = hispa_pct_change,
         "Pct. change (total)" = pct_change)

# Keep the top 5 and bottom 5 MSAs
msa_hispa_pop_export <- msa_hispa_pop[c(1:5,(nrow(msa_hispa_pop)-4):nrow(msa_hispa_pop)),]


# Export results

write.csv(msa_hispa_pop_export,"msa_hispa_pop_export.csv")


########### Figure 2 and intra-Illinois comparisons


## Pull data on Illinois counties
county_pop_2019 <- get_acs(geography = "county",
                           state = "17",
                           variables = c("B01001_001"),
                           cache_table = TRUE,
                           year = 2019,
                           survey = "acs5",
                           output = "wide") %>%
  # Sort by population (descending)
  arrange(-B01001_001E) %>%
  # Rename variables and select
  select(NAME,
         pop19 = B01001_001E,
         GEOID)

## Pull same data for 2014
county_pop_2014 <- get_acs(geography = "county",
                           state = "17",
                           variables = c("B01001_001"),
                           cache_table = TRUE,
                           year = 2014,
                           survey = "acs5",
                           output = "wide") %>%
  # Sort by population (descending)
  arrange(-B01001_001E) %>%
  # Rename variables and select
  select(NAME,
         pop14 = B01001_001E,
         GEOID)



## Pull data on Chicago
place_pop_2019 <- get_acs(geography = "place",
                          state = "17",
                          variables = c("B01001_001"),
                          cache_table = TRUE,
                          year = 2019,
                          survey = "acs5",
                          output = "wide") %>%
  # Keep only Chicago
  filter(GEOID == 1714000) %>%
  # Rename variables and select
  select(NAME,
         pop19 = B01001_001E,
         GEOID)

## Pull same data for 2014
place_pop_2014 <- get_acs(geography = "place",
                          state = "17",
                          variables = c("B01001_001"),
                          cache_table = TRUE,
                          year = 2014,
                          survey = "acs5",
                          output = "wide") %>%
  # Keep only Chicago
  filter(GEOID == 1714000) %>%
  # Rename variables and select
  select(NAME,
         pop14 = B01001_001E,
         GEOID)

chicago_pop <-
  place_pop_2014 %>%
  inner_join(place_pop_2019 %>% select(GEOID,pop19), by = "GEOID") %>%
  mutate(NAME = "Chicago")

## Join the two years
county_pop <-
  county_pop_2014 %>%
  inner_join(county_pop_2019 %>% select(GEOID,pop19),by = "GEOID") %>%
  mutate(pct_change = round(100*((pop19 - pop14) / pop14),2),
         pop_change = pop19 - pop14) %>%
  arrange(-pct_change) %>%
  mutate(Rank = row_number()) %>%
  select(Rank,
         County = NAME,
         pop14,
         pop19,
         pop_change,
         pct_change,
         GEOID)

chicago_suburban_cook <-
  county_pop_2014 %>%
  inner_join(county_pop_2019 %>% select(GEOID,pop19),by = "GEOID") %>%
  filter(NAME == "Cook County, Illinois") %>%
  mutate(NAME = "Suburban Cook County") %>%
  mutate(
    pop19 = pop19 - chicago_pop$pop19,
    pop14 = pop14 - chicago_pop$pop14) %>%
  rbind(chicago_pop) %>%
  mutate(pct_change = round(100*((pop19 - pop14) / pop14),2),
         pop_change = pop19 - pop14,
         Rank = "") %>%
  select(Rank,
         County = NAME,
         pop14,
         pop19,
         pop_change,
         pct_change,
         GEOID)

# Keep the top 10 and bottom 10 counties, plus any not included from the CMAP region
county_pop_table <- county_pop[c(1:10,(nrow(county_pop)-9):nrow(county_pop)),] %>%
  rbind(county_pop %>% filter(GEOID %in% cmap_counties)) %>%
  rbind(chicago_suburban_cook) %>%
  distinct() %>%
  arrange(-pct_change) %>%
  mutate(pct_change = paste0(pct_change,"%")) %>%
  select(-GEOID) %>%
  mutate(County = sub(" County, Illinois","",County))

# Export just the changes and GEOIDs for the map
county_pop_map <-
  county_pop_2014 %>%
  inner_join(county_pop_2019 %>% select(GEOID,pop19),by = "GEOID") %>%
  mutate(pct_change = round(((pop19 - pop14) / pop14),4),
         pop_change = pop19 - pop14)

county_pop_map <-
  county_pop_map %>%
  select(NAME,
         pop14,
         pop19,
         pct_change,
         pop_change,
         GEOID) %>%
  filter(GEOID != 17031) %>%
  rbind(., chicago_suburban_cook %>%
          select(-Rank, NAME = County) %>%
          mutate(pct_change = pct_change/100)) %>%
  mutate(county_fips = sub("17","",GEOID))


# Export results

write.csv(county_pop_table,"county_pop_table.csv")
write.csv(county_pop_map,"county_pop_map.csv")


##### CMAP region vs. rest of IL

regional_comparison <-
  county_pop_map %>%
  mutate(CMAP = case_when(
    GEOID %in% cmap_counties ~ "CMAP",
    TRUE ~ "Rest of IL"
  )) %>%
  group_by(CMAP) %>%
  summarize(pop14 = sum(pop14),
            pop19 = sum(pop19)) %>%
  mutate(pct_change = round((100*(pop19 - pop14) / pop14),2),
         pop_change = pop19 - pop14)  %>%
  mutate(pct_change = paste0(pct_change,"%")) %>%
  select(Region = CMAP,
         "2014 Pop." = pop14,
         "2019 Pop." = pop19,
         "Change (Pop.)" = pop_change,
         "Change (Pct.)" = pct_change)


write.csv(regional_comparison,"regional_comparison.csv")


###### Figure 3

international_migration <-
  tibble(year = as.character(2010:2019),
         foreign = c(59735,
                     69354,
                     63654,
                     67422,
                     66537,
                     64896,
                     64093,
                     69590,
                     64608,
                     56785),
         error = c(4897,
                   6616,
                   6337,
                   6071,
                   6184,
                   5755,
                   4955,
                   5780,
                   6311,
                   6138),
         state = "IL")

international_migration %>%
  ggplot(aes(x = year, y = foreign, fill = state, label = paste0(round(foreign/1000,1),"K"))) +
  geom_col() +
  geom_errorbar(aes(ymin = foreign - error, ymax = foreign + error),width = .2, alpha = .5) +
  theme_cmap(legend.position = "none") +
  geom_text(vjust = -.2) +
  scale_y_continuous(label = scales::label_comma()) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(title = "In-migrants to Illinois from foreign countries, 2010-19.",
              caption = "Note: Black lines represent margins of error based on a
              90% confidence interval. The confidence interval for 2019 does not
              overlap the interval for two other years: 2011 and 2017, which had
              the highest levels of foreign in-migration over the past ten years.
              However, these figures should be taken as approximate.
              <br><br>
              Source: CMAP analysis of 1-Year State-to-State Migration Flows, 2010-19.",
              caption_valign = "t",
              title_width = 2,
              mode = "png",
              filename = "figure2",
              overwrite = T)



migration_county_18 <- getCensus(
  region = "county:*",
  regionin = "state:17",
  name = "acs/flows",
  vintage = 2018,
  vars = c("FROMABROAD","FROMABROAD_M"))

migration_totals_18 <- migration_county_18 %>%
  distinct() %>%
  summarize(total18 = sum(as.integer(FROMABROAD)),
            moe18 = sqrt(sum(as.integer(FROMABROAD_M)^2)))

migration_county_14 <- getCensus(
  region = "county:*",
  regionin = "state:17",
  name = "acs/flows",
  vintage = 2014,
  vars = c("FROMABROAD","FROMABROAD_M"))

migration_totals_14 <- migration_county_14 %>%
  distinct() %>%
  summarize(total14 = sum(as.integer(FROMABROAD)),
            moe14 = sqrt(sum(as.integer(FROMABROAD_M)^2)))

##### Figure 4

migration_vars19 <- listCensusMetadata(name = "acs/acs5",vintage = "2019")
migration_vars14 <- listCensusMetadata(name = "acs/acs5",vintage = "2014")
migration_vars09 <- listCensusMetadata(name = "acs/acs5",vintage = "2009")




table_country_of_origin_19 <-
  get_acs(geography = "county",
          state = 17,
          county = sub("17","",cmap_counties),
          table = "B05006",
          cache_table = TRUE,
          year = 2019,
          survey = "acs5",
  )

cmap_country_of_origin_19 <-
  table_country_of_origin_19 %>%
  group_by(variable) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2))) %>%
  ungroup() %>%
  mutate(percent = round(estimate / .[which(variable == "B05006_001"),]$estimate,4)) %>%
  mutate(name = paste0(variable,"E")) %>%
  left_join(migration_vars19 %>% select(name, label), by = "name") %>%
  select(-name)


table_country_of_origin_14 <-
  get_acs(geography = "county",
          state = 17,
          county = sub("17","",cmap_counties),
          table = "B05006",
          cache_table = TRUE,
          year = 2014,
          survey = "acs5",
  )

cmap_country_of_origin_14 <-
  table_country_of_origin_14 %>%
  group_by(variable) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2))) %>%
  ungroup() %>%
  mutate(percent = round(estimate / .[which(variable == "B05006_001"),]$estimate,4)) %>%
  mutate(name = paste0(variable,"E")) %>%
  left_join(migration_vars14 %>% select(name, label), by = "name") %>%
  select(-name)



table_country_of_origin_09 <-
  get_acs(geography = "county",
          state = 17,
          county = sub("17","",cmap_counties),
          table = "B05006",
          cache_table = TRUE,
          year = 2009,
          survey = "acs5",
  )

cmap_country_of_origin_09 <-
  table_country_of_origin_09 %>%
  group_by(variable) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2))) %>%
  ungroup() %>%
  mutate(percent = round(estimate / .[which(variable == "B05006_001"),]$estimate,4)) %>%
  mutate(name = paste0(variable,"E")) %>%
  left_join(migration_vars09 %>% select(name, label), by = "name") %>%
  select(-name)


cmap_country_of_origin <-
  cmap_country_of_origin_19 %>%
  filter(variable %in% c("B05006_001", # Total
                         "B05006_150", # Mexico
                         "B05006_059", # India
                         "B05006_040", # Poland
                         "B05006_073", # Philippines
                         "B05006_050", # China (excluding HK and Taiwan)
                         "B05006_054", # Korea
                         "B05006_044", # Ukraine
                         "B05006_148", # Guatemala
                         "B05006_131", # Caribbean
                         "B05006_076") # Vietnam
  ) %>%
  mutate(year = 2019) %>%
  mutate(country = recode(variable,
                          "B05006_001" = "Total",
                          "B05006_150" = "Mexico",
                          "B05006_059" = "India",
                          "B05006_040" = "Poland",
                          "B05006_073" = "Philippines",
                          "B05006_050" = "China (excluding HK and Taiwan)",
                          "B05006_054" = "Korea",
                          "B05006_044" = "Ukraine",
                          "B05006_148" = "Guatemala",
                          "B05006_131" = "Caribbean",
                          "B05006_076" = "Vietnam")) %>%
  rbind(cmap_country_of_origin_14 %>%
          filter(variable %in% c(
            "B05006_001", # Total
            "B05006_138", # Mexico
            "B05006_059", # India
            "B05006_039", # Poland
            "B05006_073", # Philippines
            "B05006_050", # China (excluding HK and Taiwan)
            "B05006_054", # Korea
            "B05006_042", # Ukraine
            "B05006_142", # Guatemala
            "B05006_124", # Caribbean
            "B05006_076") # Vietnam
          ) %>%
          mutate(year = 2014) %>%
          mutate(country = recode(variable,
                                  "B05006_001" = "Total",
                                  "B05006_138" = "Mexico",
                                  "B05006_059" = "India",
                                  "B05006_039" = "Poland",
                                  "B05006_073" = "Philippines",
                                  "B05006_050" = "China (excluding HK and Taiwan)",
                                  "B05006_054" = "Korea",
                                  "B05006_042" = "Ukraine",
                                  "B05006_142" = "Guatemala",
                                  "B05006_124" = "Caribbean",
                                  "B05006_076" = "Vietnam"))) %>%
  rbind(cmap_country_of_origin_09 %>%
          filter(variable %in% c(
            "B05006_001", # Total
            "B05006_138", # Mexico
            "B05006_059", # India
            "B05006_038", # Poland
            "B05006_073", # Philippines
            "B05006_050", # China (excluding HK and Taiwan)
            "B05006_054", # Korea
            "B05006_041", # Ukraine
            "B05006_142", # Guatemala
            "B05006_124", # Caribbean
            "B05006_076") # Vietnam
          ) %>%
          mutate(year = 2009) %>%
          mutate(country = recode(variable,
                                  "B05006_001" = "Total",
                                  "B05006_138" = "Mexico",
                                  "B05006_059" = "India",
                                  "B05006_038" = "Poland",
                                  "B05006_073" = "Philippines",
                                  "B05006_050" = "China (excluding HK and Taiwan)",
                                  "B05006_054" = "Korea",
                                  "B05006_041" = "Ukraine",
                                  "B05006_142" = "Guatemala",
                                  "B05006_124" = "Caribbean",
                                  "B05006_076" = "Vietnam"))) %>%
  select(-variable,-label,-moe)


top_four <- c("Mexico","India","Poland","Philippines")


cmap_country_of_origin_top_four <-
  cmap_country_of_origin %>%
  filter(country %in% c(top_four)) %>%
  group_by(year) %>%
  summarize(total = sum(estimate)) %>%
  left_join(cmap_country_of_origin %>% filter(country == "Total"), by = "year") %>%
  mutate(other_total = estimate - total) %>%
  mutate(other_pct = round(other_total / estimate,4))

cmap_country_of_origin_chart <-
  cmap_country_of_origin %>%
  filter(country %in% c(top_four)) %>%
  rbind(tibble(
    year = c(2009,2014,2019),
    country = "Other countries",
    estimate = c(cmap_country_of_origin_top_four$other_total),
    percent = c(cmap_country_of_origin_top_four$other_pct))) %>%
  rbind(cmap_country_of_origin %>%
          filter(country == "Total"))


figure4 <-
  cmap_country_of_origin_chart %>%
  filter(country != "Total") %>%
  mutate(country = factor(country, levels = c("Mexico","India","Poland","Philippines","Other countries"))) %>%
  mutate(label = paste0(round(100*percent,1),"%")) %>%
  ggplot(aes(x = year, y = estimate, fill = country, label = label)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  theme_cmap() +
  scale_y_continuous(label = scales::label_comma()) +
  scale_x_continuous(breaks = c(2009,2014,2019)) +
  geom_text(position = position_stack(reverse = TRUE), vjust = 1.1, color = "white") +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(figure4,
              title = "Foreign-born population in the CMAP region by country of birth.",
              caption = "Note: Includes the top four countries by origin in 2019. Estimates are for the seven counties that make up the CMAP region in northeastern Illinois.
              <br><br>
              Source: CMAP analysis of 2005-09, 2010-14, and 2015-19 American Community Survey data.",
              filename = "figure3",
              mode = "png",
              caption_valign = "t",
              overwrite = T)


### Proportion of population that is foreign-born


cmap_pop <-
  map_dfr(c(2009,2014,2019),
          ~get_acs(geography = "county",
                   state = 17,
                   variables = c("B01001_001"),
                   cache_table = TRUE,
                   year = .,
                   survey = "acs5") %>%
            filter(GEOID %in% cmap_counties) %>%
            summarize(pop = sum(estimate))) %>%
  cbind(tibble(year = c(2009,2014,2019)))

cmap_foreign_born <-
  cmap_country_of_origin %>%
  filter(country == "Total") %>%
  left_join(cmap_pop, by = "year") %>%
  mutate(pct_foreign_born = estimate / pop)



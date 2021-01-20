library(tidyverse)
library(tidycensus)
library(cmapplot)
library(censusapi)


# # Run every session
# Sys.setenv(CENSUS_KEY="YOUR KEY HERE")
# readRenviron("./.Renviron")
# Sys.getenv("CENSUS_KEY")

# # Run once per machine
# census_api_key("YOUR KEY HERE", install=TRUE)

# CMAP area, county FIPS codes
cmap_counties <- c("17031", "17043", "17089", "17093", "17097", "17111", "17197")

# # View all available APIs
# listCensusApis() %>%
#   View()

# # View variables of response rate API
# listCensusMetadata(name = "acs/acs5",
#                    vintage = "2019") %>%
#   View()

#### FIGURE 1

# Import population estimates
state_populations <-
  read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/national/totals/nst-est2020.csv") %>%
  # Remove regional summaries
  filter(STATE != 0)

# Calculate normalized population figures
state_populations_normalized <-
  state_populations %>%
  # Remove DC
  filter(NAME != "District of Columbia") %>%
  # Divide all population figures by 2010 Census count
  mutate(across(c("CENSUS2010POP":"POPESTIMATE2020"),~./CENSUS2010POP)) %>%
  # Make data longer
  pivot_longer(cols = c("CENSUS2010POP":"POPESTIMATE2020"),names_to = "survey_year") %>%
  # Keep relevant records
  filter(!(survey_year %in% c("ESTIMATESBASE2010","POPESTIMATE2010"))) %>%
  # Rename "census" to "2010"
  mutate(survey_year = case_when(
    survey_year == "CENSUS2010POP" ~ "2010",
    TRUE ~ survey_year)) %>%
  # Remove "X" from year names
  mutate(year = as.integer(sub("POPESTIMATE","",survey_year))) %>%
  # Add column for on-graph labels for a subset of states
  mutate(label = case_when(
    NAME %in% c("California","Illinois","North Dakota",
                "New York","Pennsylvania","Utah",
                "West Virginia") ~ NAME,
    TRUE ~ "")) %>%
  # Add value for labels
  mutate(label_y = case_when(
    NAME %in% c("California","Illinois","North Dakota",
                "New York","Pennsylvania","Utah",
                "West Virginia") & year == 2020 ~ value
  )) %>%
  # Add factor levels to states
  mutate(NAME = fct_relevel(NAME,
                            c("California","North Dakota",
                              "New York","Pennsylvania","Utah",
                              "West Virginia","Illinois"),
                            after = Inf))



# Create Figure 1
figure1 <-
  state_populations_normalized %>%
  # Set aesthetics
  ggplot(aes(x = year, y = value, color = NAME)) +
  # Add line graph
  geom_line() +
  # Add CMAP design standards and remove legend
  theme_cmap(legend.position = "none") +
  # Highlight Illinois in blue and make other states gray
  cmap_color_highlight(state_populations_normalized$NAME,
                       value = c("Illinois","California","North Dakota",
                                 "New York","Pennsylvania","Utah",
                                 "West Virginia"),
                       color_value = c("#00b0f0",rep("#475c66",6))
                       ) +
  # Modify breaks
  scale_x_continuous(breaks = c(2010,2012,2014,2016,2018,2020),
                     limits = c(2010,2022)) +
  # Reformat y-axis labels
  scale_y_continuous(label = scales::label_percent(accuracy = 1),
                     limits = c(.95,1.20)) +
  # Add labels
  geom_text(aes(label = label, y = label_y, x = 2020.1),hjust = 0)

# Export Figure 1
finalize_plot(figure1,
              title = "State-by-state population change since 2010 (highlighting
              Illinois and select states).",
              caption = "Note: 2010 population figures are census counts as of
              April 1, 2010. Other years are population estimates as of July 1
              of the respective year, normalized against 2010 population totals.
              All figures are derived from estimates based on the 2010 census
              and do not rely on data from the yet-to-be-released 2020 census.
              Excludes Washington, D.C., as well as Puerto Rico and other U.S.
              territories.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of 2020
              U.S. Census Bureau Population Estimates.",
              caption_valign = "t",
              filename = "figure1",
              mode = c("svg","png","pdf"),
              overwrite = TRUE)


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

## Join both years of Chicago's population
chicago_pop <-
  place_pop_2014 %>%
  inner_join(place_pop_2019 %>% select(GEOID,pop19), by = "GEOID") %>%
  mutate(NAME = "Chicago")

## Join the two years for the county
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
county_pop_table <-
  county_pop[c(1:10,(nrow(county_pop)-9):nrow(county_pop)),] %>%
  rbind(county_pop %>% filter(GEOID %in% cmap_counties)) %>%
  rbind(chicago_suburban_cook) %>%
  distinct() %>%
  arrange(-pct_change) %>%
  mutate(pct_change = paste0(pct_change,"%")) %>%
  select(-GEOID) %>%
  mutate(County = sub(" County, Illinois","",County))

# Export just the changes and GEOIDs for the map
county_pop_map_wip <-
  county_pop_2014 %>%
  inner_join(county_pop_2019 %>% select(GEOID,pop19),by = "GEOID") %>%
  mutate(pct_change = round(((pop19 - pop14) / pop14),4),
         pop_change = pop19 - pop14)

# Add Chicago and suburban Cook County as jurisdictions
county_pop_map <-
  county_pop_map_wip %>%
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

write.csv(county_pop_table,"outputs/county_pop_table.csv")
write.csv(county_pop_map,"outputs/county_pop_map.csv")


##### CMAP region vs. rest of IL

# Join county-level populations from 2014 and 2019
regional_comparison_wip <-
  county_pop_2014 %>%
  inner_join(county_pop_2019 %>% select(GEOID,pop19),by = "GEOID")

# Format data and add total row
regional_comparison <-
  regional_comparison_wip %>%
  # Sort into CMAP region vs. rest of IL
  mutate(CMAP = case_when(
    GEOID %in% cmap_counties ~ "CMAP",
    TRUE ~ "Rest of IL"
  )) %>%
  # Add all data for a total row
  rbind(regional_comparison_wip %>% mutate(CMAP = "Total")) %>%
  group_by(CMAP) %>%
  # Calculate and format statistics of interest
  summarize(pop14 = sum(pop14),
            pop19 = sum(pop19)) %>%
  mutate(pct_change = round((100*(pop19 - pop14) / pop14),2),
         pop_change = pop19 - pop14)  %>%
  mutate(pct_change = paste0(pct_change,"%")) %>%
  select(Region = CMAP,
         "2010-14 Pop." = pop14,
         "2015-19 Pop." = pop19,
         "Change (pop.)" = pop_change,
         "Change (pct.)" = pct_change)


write.csv(regional_comparison,"outputs/regional_comparison.csv")


####### MSA-level population figures (cited in text)


state_fips = unique(fips_codes$state_code)[1:51]

# Pull data at the county level across the US

# Pull data for 2010-14 ACS
pop_counties_USA_2014 <-
  # Use mop_dfr to combine multiple data pulls
  map_dfr(
    state_fips,
    ~get_acs(geography = "county",
             state = .,
             variables = c("B01001_001","B01001I_001"),
             cache_table = TRUE,
             year = 2014,
             survey = "acs5",
             output = "wide"))

# Reformat and only keep relevant columns
pop_counties_USA_2014 <-
  pop_counties_USA_2014 %>%
  select(GEOID,
         county = NAME,
         pop14 = B01001_001E,
         hispa14 = B01001I_001E)

# Use same logic for 2015-19 ACS
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

# Combine the above data pulls
pop_counties_USA <-
  pop_counties_USA_2014 %>%
  inner_join(pop_counties_USA_2019 %>% select(-county), by = "GEOID")

## Import crosswalk file for counties to MSAs
county_msa_crosswalk <- read.csv("sources/county_msa_crosswalk.csv") %>%
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
  inner_join(county_msa_crosswalk, by = c("GEOID"="County_GEOID")) %>%
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
  # Sort by 2015-19 population
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
         "2010-14 Pop." = pop14,
         "2015-19 Pop." = pop19,
         "Change (pop.)" = pop_change,
         "Change (pct.)" = pct_change)

# Keep the top 10 and bottom 10 MSAs
msa_pop_export <- msa_pop[c(1:10,(nrow(msa_pop)-9):nrow(msa_pop)),]


# Export results

write.csv(msa_pop_export,"outputs/msa_pop_export.csv")



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
         "Hispanic pop. (2015-19)" = hispa19,
         "Diff." = hispa_pop_change,
         "Pct. change (Hispanic)" = hispa_pct_change,
         "Pct. change (total)" = pct_change)

# Keep the top 5 and bottom 5 MSAs
msa_hispa_pop_export <- msa_hispa_pop[c(1:5,(nrow(msa_hispa_pop)-4):nrow(msa_hispa_pop)),]


# Export results

write.csv(msa_hispa_pop_export,"outputs/msa_hispa_pop_export.csv")


#############################
# Figure 3

# Create Figure 3
figure3 <-
  msa_pop_chart %>%
  ggplot(aes(x = reorder(MSA,desc(-Rank)), fill = MSA, y = pct_change)) +
  geom_col() +
  theme_cmap(legend.position = "none",
             gridlines = "h",
             axis.text.x=element_blank(),
             hline = 0) +
  # Highlight Illinois in blue and make other states gray
  cmap_fill_highlight(msa_pop_chart$MSA,
                       value = c("CMAP region",
                                 "New York-Newark-Jersey City, NY-NJ-PA",
                                 "Los Angeles-Long Beach-Anaheim, CA",
                                 "Houston-The Woodlands-Sugar Land, TX",
                                 "Austin-Round Rock-Georgetown, TX",
                                 "Washington-Arlington-Alexandria, DC-VA-MD-WV",
                                 "Pittsburgh, PA"),
                       color_value = c("#00b0f0",rep("#475c66",6))) +
  scale_y_continuous(label = scales::label_percent(accuracy = 1),
                     limits = c(-.02,.16)) +
  scale_x_discrete(expand = c(0,4.3)) +
  geom_text(aes(label = label),
            hjust = ifelse(msa_pop_chart$pct_change > 0, 0.04, .96),
            vjust = ifelse(msa_pop_chart$pct_change > 0, -.1, 1.1))

# Export Figure 3
finalize_plot(figure3,
              title = "Regional population change, 2010-14 vs. 2015-19
              (highlighting the CMAP region and select regions).",
              caption = "Note: CMAP region is highlighted in blue. All figures
              are calculated based on five-year American Community Survey county
              population totals, using Metropolitan Statistical Area boundaries
              as of March 2020. CMAP region totals are calculated using the
              seven county area and not the larger Metropolitan Statistical Area.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of
              2010-14 and 2015-19 American Community Survey data.",
              caption_valign = "t",
              filename = "figure3",
              height = 7,
              mode = c("svg","png","pdf"),
              overwrite = TRUE)


#############################
# State-to-state migration

# 2019 State-to-state migration totals downloaded and analyzed from the Bureau:
# https://www.census.gov/data/tables/time-series/demo/geographic-mobility/state-to-state-migration.html


############################
# Figure 4

# Load variable names for relevant data sets
migration_vars19 <- listCensusMetadata(name = "acs/acs5",vintage = "2019")
migration_vars14 <- listCensusMetadata(name = "acs/acs5",vintage = "2014")
migration_vars09 <- listCensusMetadata(name = "acs/acs5",vintage = "2009")



# Load country of origin data for 2015-19
table_country_of_origin_19 <-
  get_acs(geography = "county",
          state = 17,
          county = sub("17","",cmap_counties),
          table = "B05006",
          cache_table = TRUE,
          year = 2019,
          survey = "acs5",
  )

# Summarize seven-county data into regional totals
cmap_country_of_origin_19 <-
  table_country_of_origin_19 %>%
  group_by(variable) %>%
  # Generate totals and MOEs
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2))) %>%
  ungroup() %>%
  # Calculate percent out of total foreign-born population
  mutate(percent = round(estimate / .[which(variable == "B05006_001"),]$estimate,4)) %>%
  # Make variable names consistent for joining purposes
  mutate(name = paste0(variable,"E")) %>%
  # Join to variable names
  left_join(migration_vars19 %>% select(name, label), by = "name") %>%
  select(-name)

# Repeat logic for 2010-14
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


# Repeat logic for 2005-09
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


# Select relevant variables for the top 10 countries of birth for foreign-born
# residents from each dataset, and recode into country names. Uses the top 10
# list from 2019 for all datasets. Note that variable codes are not consistent
# between surveys, requiring manual assignment in the code below.
cmap_country_of_origin <-
  cmap_country_of_origin_19 %>%
  filter(variable %in%
           c("B05006_001", # Total
             "B05006_150", # Mexico
             "B05006_059", # India
             "B05006_040", # Poland
             "B05006_073", # Philippines
             "B05006_050", # China (excluding Hong Kong and Taiwan)
             "B05006_054", # Korea
             "B05006_044", # Ukraine
             "B05006_148", # Guatemala
             "B05006_131", # Caribbean
             "B05006_076") # Vietnam
  ) %>%
  mutate(year = "2015-19") %>%
  mutate(country =
           recode(variable,
                  "B05006_001" = "Total",
                  "B05006_150" = "Mexico",
                  "B05006_059" = "India",
                  "B05006_040" = "Poland",
                  "B05006_073" = "Philippines",
                  "B05006_050" = "China (excluding Hong Kong and Taiwan)",
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
            "B05006_050", # China (excluding Hong Kong and Taiwan)
            "B05006_054", # Korea
            "B05006_042", # Ukraine
            "B05006_142", # Guatemala
            "B05006_124", # Caribbean
            "B05006_076") # Vietnam
          ) %>%
          mutate(year = "2010-14") %>%
          mutate(country =
                   recode(variable,
                          "B05006_001" = "Total",
                          "B05006_138" = "Mexico",
                          "B05006_059" = "India",
                          "B05006_039" = "Poland",
                          "B05006_073" = "Philippines",
                          "B05006_050" = "China (excluding Hong Kong and Taiwan)",
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
            "B05006_050", # China (excluding Hong Kong and Taiwan)
            "B05006_054", # Korea
            "B05006_041", # Ukraine
            "B05006_142", # Guatemala
            "B05006_124", # Caribbean
            "B05006_076") # Vietnam
          ) %>%
          mutate(year = "2005-09") %>%
          mutate(country =
                   recode(variable,
                          "B05006_001" = "Total",
                          "B05006_138" = "Mexico",
                          "B05006_059" = "India",
                          "B05006_038" = "Poland",
                          "B05006_073" = "Philippines",
                          "B05006_050" = "China (excluding Hong Kong and Taiwan)",
                          "B05006_054" = "Korea",
                          "B05006_041" = "Ukraine",
                          "B05006_142" = "Guatemala",
                          "B05006_124" = "Caribbean",
                          "B05006_076" = "Vietnam"))) %>%
  select(-variable,-label,-moe)

# Make helper list for top four countries of birth.
top_four <- c("Mexico","India","Poland","Philippines")

# Filter data to identify population of foreign-born residents from other countries
cmap_country_of_origin_top_four <-
  cmap_country_of_origin %>%
  filter(country %in% c(top_four)) %>%
  group_by(year) %>%
  # Calculate total of residents born in top four countries
  summarize(total = sum(estimate)) %>%
  # Join with totals
  left_join(cmap_country_of_origin %>% filter(country == "Total"), by = "year") %>%
  # Calculate total and percentage for those born in other countries
  mutate(other_total = estimate - total) %>%
  mutate(other_pct = round(other_total / estimate,4))

# Create base data
cmap_country_of_origin_chart <-
  cmap_country_of_origin %>%
  # Keep top four data
  filter(country %in% c(top_four)) %>%
  # Add row for other countries with calculated data
  rbind(tibble(
    year = c("2005-09","2010-14","2015-19"),
    country = "Other countries",
    estimate = c(cmap_country_of_origin_top_four$other_total),
    percent = c(cmap_country_of_origin_top_four$other_pct))) %>%
  rbind(cmap_country_of_origin %>%
          filter(country == "Total"))

# Create chart
figure4 <-
  # Load data
  cmap_country_of_origin_chart %>%
  # Remove totals
  filter(country != "Total") %>%
  # Set factors in desired order
  mutate(country = factor(country,
                          levels = c("Mexico","India","Poland",
                                     "Philippines","Other countries"))) %>%
  # Create a percentage label
  mutate(label = paste0(format(round(100*percent,1),nsmall = 1),"%")) %>%
  # Initiate ggplot aesthetic
  ggplot(aes(x = year, y = estimate, fill = country, label = label)) +
  # Add column graph
  geom_col(position = position_stack(reverse = TRUE)) +
  # Call CMAP design standards
  theme_cmap() +
  # Change y-axis text formatting
  scale_y_continuous(label = scales::label_comma()) +
  # Add percentage labels
  geom_text(position = position_stack(reverse = TRUE),
            vjust = 1,
            color = "white") +
  # Manually set color palette (using CMAP color hex codes)
  scale_fill_discrete(type = c("#00becc", "#003f8c", "#67ac00", "#6d8692", "#00665c"))

# Export Figure 4
finalize_plot(figure4,
              title = "Foreign-born population in the CMAP region by country of birth.",
              caption = "Note: Includes the top four countries by origin in 2015-19.
              Estimates are for the seven counties that make up the CMAP region in
              northeastern Illinois.
              <br><br>
              Source: CMAP analysis of 2005-09, 2010-14, and 2015-19 American
              Community Survey data.",
              filename = "figure4",
              mode = c("svg","png","pdf"),
              caption_valign = "t",
              overwrite = T)


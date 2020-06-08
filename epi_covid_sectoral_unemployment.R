library(tidyverse)
library(readxl)

# https://economic.github.io/ui_state_detailed/
epi <- read_csv("state_ui_industry_recoded.csv")

# add sector-to-supersector crosswalk from QCEW
# https://www.bls.gov/cew/classifications/industry/industry-supersectors.htm
high_level_crosswalk <- read_excel("high_level_crosswalk.xlsx", sheet = "List")
epi <- inner_join(epi, high_level_crosswalk, by = c("NAICS" = "naics")) %>%
  select(-c(total, domain, naics_sector))


# I included in my first writeup:
# bar charts:
# - stacked claims by supersector by week, faceted by big six states
# - stacked total claims of top five sectors by state (six big states)
# - filled (ie, percentages) total claims by supersector (all states)
# tables:
# - sectors sorted by total unemployment (across all states)
# - sectors sorted by total unemployment (TX only)
# dot plots:
# - all sectors/states, unemp on Y, LQ on X
# - same for TX only with labeled/colored points

# maybe have separate variables for
# weekly claims by state/sector/supersector
# total claims by state/sector/supersector


###############################################################
# All states weekly by supersector
###############################################################

# all states together by number
ggplot(epi, aes(`Week ending`, `Initial claims (IC)`, fill=super_sector)) +
  geom_bar(position="stack", stat="identity")
# facets by state by number
ggplot(epi, aes(`Week ending`, `Initial claims (IC)`, fill=super_sector)) +
  geom_bar(position="stack", stat="identity") + facet_wrap(~ State)
# USE THIS: facets by state by percentage
ggplot(epi, aes(`Week ending`, `Initial claims (IC)`, fill=super_sector)) +
  geom_bar(position="fill", stat="identity") + facet_wrap(~ State)

###############################################################
# The six biggest states - ADD CA
###############################################################

# create new var bigstates
thebigs <- c("CA","GA","MA","MI","NY","TX","WA")
bigstates <- filter(epi, State %in% thebigs)

# add big-6 sector unemployment rates and LQs from JobsEQ
empl_six_states <- read_excel("empl six states.xlsx")
empl_six_states[empl_six_states$NAICS == 31, "NAICS"] <- "31-33"
empl_six_states[empl_six_states$NAICS == 44, "NAICS"] <- "44-45"
empl_six_states[empl_six_states$NAICS == 48, "NAICS"] <- "48-49"
bigstates <- inner_join(bigstates, empl_six_states) %>% select(-`Industry`)


# USE THIS: facets of big 6, weekly by supersector
# the problem with this is, CA screws up the Y axis because the numbers
# are a LOT bigger. leave it out for now
bigstates %>% filter(State != "CA") %>%
  ggplot(aes(`Week ending`, `Initial claims (IC)`, fill=super_sector)) +
  geom_bar(position="stack", stat="identity") + facet_wrap(~ State)


# could calculate and graph a weekly unemployment rate here, before
# creating a variable for totals

# it'd be especially interesting to know which sectors have seen the
# highest rise since this started. start with the first week in this
# data or even with pre-crisis sectoral unemployment from jobseq


#######################################################
# Analysis of totals by number and rate
#######################################################


# new vars for state/sector totals for all states and the big ones
# what columns need to be carried over?
sector_totals <-
  group_by(epi, State, `Sector name`, NAICS, super_sector) %>%
  summarize(total_ic = sum(`Initial claims (IC)`)) %>%
  ungroup()
# big state totals, add total sector employment and unemp rate
bigstate_totals <- filter(sector_totals, State %in% thebigs) %>%
  inner_join(., empl_six_states) %>%
  select(-`Industry`) %>%
  mutate(sector_unemp = total_ic / Empl)



###########################################################
# Scatterplots of unemployment rate by sector and LQ
###########################################################

# (facets on states, colors on industries)
ggplot(filter(bigstate_totals, State=="TX"),
       aes(LQ, sector_unemp, color=`Sector name`)) + geom_point()
# LQ for mining is such an outlier (>4), let's exclude it
# add labels and delete legend
# USE THIS:
ggplot(filter(bigstate_totals, State=="TX", NAICS!=21),
       aes(LQ, sector_unemp, color=`Sector name`)) +
  geom_point() + geom_text(aes(label=`Sector name`)) + 
  theme(legend.position="none")

# Leave aside sector, and just look across all states and sectors
# for a relationship between unemployment and LQ and fit a line
# USE THIS:
ggplot(filter(bigstate_totals, LQ < 1.5 & LQ > 0.5), aes(LQ, sector_unemp)) +
  geom_point() + geom_smooth()



# Calculate supersector unemployment by state
# (really only useful if you want to graph it)
# bigstates <-
#   group_by(bigstates, super_sector, State) %>%
#   mutate(super_sector_unemp = sum(total_ic) / sum(Empl)) %>%
#   ungroup()

################################################
# Tables
###############################################

# USE THIS: Table of unemployment rate by sector ACROSS the big-6 states
bigstate_totals %>%
  group_by(`Sector name`) %>%
  summarise(sector_unemp = sum(total_ic) / sum(Empl)) %>%
  arrange(desc(sector_unemp)) %>%
  select(`Sector name`, sector_unemp)

# USE THIS: and Texas
bigstate_totals %>%
  filter(State == "TX") %>%
  group_by(`Sector name`) %>%
  summarise(sector_unemp = sum(total_ic) / sum(Empl)) %>%
  arrange(desc(sector_unemp)) %>%
  select(`Sector name`, sector_unemp) %>% write.table(., "clipboard", sep="\t")


#############################################################
# Further analysis and visualization of totals
#############################################################

# USE THIS: top 5 (2-digit) sectors by total job losses, for each of the big 6 states
bigstate_totals %>%
  group_by(State) %>%
  filter(!(NAICS %in% c("72","44-45","62"))) %>%
  top_n(5, total_ic) %>%
  ggplot(aes(State, total_ic, fill=`Sector name`)) +
  geom_bar(position="stack", stat="identity")

# ADD THIS: smiilar, but top 5 sectors by unemp rate
bigstate_totals %>%
  group_by(State) %>%
  filter(!(NAICS %in% c("72","44-45","62"))) %>% # filter out biggest-hit sectors
  top_n(5, sector_unemp) %>% # could change to top 3
  ggplot(aes(State, sector_unemp, fill=`Sector name`)) +
  geom_bar(position="dodge", stat="identity") + coord_flip()

# all states in the data, top 5 sectors by number, and coord_flipped
# could also do top 3 for a less busy graph
# or filter out the Accomm. and Retail sectors, among the top 3 for pretty
# much all states, to get the top 3 for each state other than those two.
# more revealing of differences among the states.
sector_totals %>%
  group_by(State) %>%
  filter(!(NAICS %in% c("72","44-45","62"))) %>%
  top_n(3, total_ic) %>%
  ggplot(aes(State, total_ic, fill=`Sector name`)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip()


library(here); library(tidyverse); library(tidycensus); library(sf); library(magrittr)

# a = Total employed persons
# b = Pedestrian commuters
# c = Pedestrian commute percentage
# d = Work at home observations
# e = Work at home pedestrian commuters
# f = Students ages 5-14
# g = Estimated school pedestrian share (see nhts.R)
# h = School pedestrian commuters
# i = College students
# j = College pedestrian commuters
# k* = Daily commuter subtotal
# l* = Daily commute trip subtotal
# m* = Yearly commute trip subtotal
# n* = Percentage of commute trips in all walking trips
# o* = Total yearly pedestrian trips
# p* = Average daily pedestrian trips
# q = Last-mile pedestrian commuters
# * denotes that addition of q has affected result

# a - j
# Get ACS estimates
# https://api.census.gov/data/2016/acs/acs5/variables.html
acs <- get_acs(geography = "tract",
                   state = c(42),
                   geometry = TRUE,
                   output = "wide",
                   variables = c(
                     ml_ft_blpov = "B17004_004E", # start a
                     ml_pt_blpov = "B17004_005E",
                     ml_unemp_blpov = "B17004_006E",
                     fm_ft_blpov = "B17004_008E",
                     fm_pt_blpov = "B17004_009E",
                     fm_unemp_blpov = "B17004_010E",
                     ml_ft_abpov = "B17004_013E",
                     ml_pt_abpov = "B17004_014E",
                     ml_unemp_abpov = "B17004_015E",
                     fm_ft_abpov = "B17004_017E",
                     fm_pt_abpov = "B17004_018E",
                     fm_unemp_abpov = "B17004_019E", # end a
                     walkComm = "B08301_019E", # b
                     transComm = "B08301_010E", # q
                     walkUniverse = "B08301_001E", # c
                     workHome = "B08301_021E", # d
                     age5 = "B09001_005E", # start f
                     age6 = "B09001_006E",
                     age9 = "B09001_007E",
                     age12 = "B09001_008E", # end f
                     undergrad = "B14001_008E", # g
                     grad = "B14001_009E" # g
                   ))
# Clean up fields
acs <- acs[, -( grep("\\M$" , colnames(acs), perl = TRUE))] %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  filter(stcty %in% c("42017", "42029", "42045", "42091", "42101"))

# g n
# Get NHTS data, compute student pedestrian mode share and pct walk trips with commute purpose
# Uncomment the two lines below to download 2017 NHTS
# url <- "https://nhts.ornl.gov/assets/2016/download/Csv.zip"
# download.file(url, here("proprietary", "nhts.zip"), mode = "wb")
# unzip(here("proprietary", "nhts.zip"), exdir = here("proprietary"))
nhts <- read.csv(here("proprietary", "trippub.csv"))
school_peds <- nhts %>%
  filter(WHYTO == 8 | WHYFROM == 8) %>% # People going to school
  filter(URBAN != 4) # Remove rural areas
school_pct <- length(school_peds$TRPTRANS[school_peds$TRPTRANS == 1]) /
  length(school_peds$PERSONID) # g
trip_purpose <- nhts %>%
  filter(HHSTATE == "PA" & TRPTRANS == 1 & URBAN != 4)
commute_purpose <- length(trip_purpose$WHYTRP1S[trip_purpose$WHYTRP1S == 10]) /
  length(trip_purpose$PERSONID) # n

# Set up ACS data
acs %<>% mutate(emp_ft = ml_ft_blpov + ml_ft_abpov + fm_ft_abpov + fm_ft_blpov, # a
                emp_pt = ml_pt_blpov + ml_pt_abpov + fm_pt_abpov + fm_pt_blpov, # a
                pct_walk = walkComm / walkUniverse, # c
                walk_comm = (emp_ft + emp_pt) * pct_walk, # b
                pct_work_home = workHome / walkUniverse,
                work_home = (emp_ft + emp_pt) * pct_work_home, # d
                work_home_comm = work_home / 4, # e
                youth = age5 + age6 + age9 + age12, # f
                school_comm = youth * school_pct, # h
                college = undergrad + grad, # i
                college_comm = college * pct_walk, # j
                wlk_cm = walk_comm + work_home_comm + school_comm + college_comm, # k
                wlk_cm_d = wlk_cm * 2, # l
                wlk_cm_y = wlk_cm_d * 260, # m
                wlk_tr_y = wlk_cm_y / commute_purpose, # o
                wlk_tr_d = wlk_tr_y / 365) %>% # p
  select(GEOID, wlk_tr_d)
st_write(acs, here("outputs", "./est_peds.shp"), delete_dsn = TRUE)
acs %<>% st_set_geometry(NULL)
write.csv(acs, here("outputs", "est_peds.csv"), row.names = FALSE)

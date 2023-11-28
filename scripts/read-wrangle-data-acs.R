#Read and wrangle alternative datasets for covariates
#Oct 30, 2023
#Revised Nov 1, 2023: simplify
library(tidyverse)
library(tidycensus)
library(sf)
library(mapview)

#Read state-level 1-year ACS data-----
#Note geographies of 65,000 pop or greater are available in ACS 1-year
#https://walker-data.com/tidycensus/articles/basic-usage.html


#Look up variable codes
vars_acs_1_2019 = load_variables(2019, "acs1", cache=TRUE)
vars_acs_5_2019 = load_variables(2019, "acs5", cache=TRUE)
#View(vars_acs_1_2019)
#Sex by age: Men 45-64
#B01001_015: Estimate!!Total:!!Male:!!45 to 49 years
#B01001_016 Estimate!!Total:!!Male:!!50 to 54 years
#B01001_017 Estimate!!Total:!!Male:!!55 to 59 years
#B01001_018 Estimate!!Total:!!Male:!!60 and 61 years
#B01001_019 Estimate!!Total:!!Male:!!62 to 64 years

#Sex by age: Women 45-64
#B01001_039 Estimate!!Total:!!Female:!!45 to 49 years
#B01001_040 Estimate!!Total:!!Female:!!50 to 54 years
#B01001_041 Estimate!!Total:!!Female:!!55 to 59 years
#B01001_042 Estimate!!Total:!!Female:!!60 and 61 years
#B01001_043 Estimate!!Total:!!Female:!!62 to 64 years


## Gather data for one year - just get geometry--------
options(tigris_use_cache = TRUE)
#To simplify code below, just get geometry here
acs_1_year_2019_just_geo  = get_acs(
  geography = "state", 
  year=2019,
  cache_table = TRUE,
  keep_geo_vars = FALSE, 
  output = "wide",
  survey = "acs1", 
  geometry = TRUE, #keep geometry for this test run
  variables = c(
    pop_tot_ = "B01003_001"  #total pop
   )
) %>% 
  #rename the _M prefix to moe for margin of error
  #remove the _E prefix altogether
  rename_with(~gsub("_E", "", .x, fixed = TRUE)) %>% 
  rename_with(~gsub("_M", "_moe", .x, fixed = TRUE)) %>% 
  mutate(
    state_fips=as.numeric(GEOID),
    year=2019
  )

#Grab geometry from test above
names(acs_1_year_2019_just_geo)

#save this to disc so I can use it elsewhere
lookup_acs_state_geo=acs_1_year_2019_just_geo %>% 
  dplyr::select(GEOID, state_fips,geometry)
setwd(here("data","data-processed"))
save(lookup_acs_state_geo,file="lookup_acs_state_geo.RData")


## Create a function to gather data for a given year------
gather_wrangle_acs_by_year=function(year_val){
  acs_1_year_by_state   = get_acs(
    geography = "state", 
    year=year_val, 
    cache_table = TRUE,
    keep_geo_vars = FALSE, 
    output = "wide",
    survey = "acs1", 
    geometry = FALSE, #Already have this above. Turn geometry off.
    variables = c(
      
      ### Gather totals-------
      pop_tot_ = "B01003_001", #total pop
      
      #### Pop by sex by age---------
      #median age
      age_med_ = "B01002_001",
      
      #Men/women by age: 45-64
      #wom for women
      pop_men_45_49_="B01001_015",
      pop_men_50_54_="B01001_016",
      pop_men_55_59_="B01001_017",
      pop_men_60_61_="B01001_018",
      pop_men_62_64_="B01001_019",
      
      pop_wom_45_49_="B01001_039",
      pop_wom_50_54_="B01001_040",
      pop_wom_55_59_="B01001_041",
      pop_wom_60_61_="B01001_042",
      pop_wom_62_64_="B01001_043",
      
      #For totals below, will need 25 and older by sex/age
      
      #### Race by sex by age---------
      
      pop_race_tot_ = "B02001_001",
      pop_race_b_ = "B02001_003",#Black alone
      # pop_race_w_ = "B02001_002",#white alone
      pop_race_w_="B03002_003",#white alone; not hispanic or latino
      pop_race_h_tot_="B03002_001", #total pop for hispanic or latino question
      pop_race_h_ = "B03002_012", #Hispanic or Latino 
      
      #Do this within age group as they do
      #sex by age by race
      pop_men_45_54_race_b_="B01001B_011",
      pop_men_55_64_race_b_="B01001B_013",
      pop_wom_45_54_race_b_="B01001B_027",
      pop_wom_55_64_race_b_="B01001B_028",
      
      pop_men_45_54_race_w_="B01001H_012",
      pop_men_55_64_race_w_="B01001H_013",
      pop_wom_45_54_race_w_="B01001H_027",
      pop_wom_55_64_race_w_="B01001H_028",
      
      pop_men_45_54_race_h_="B01001I_012",
      pop_men_55_64_race_h_="B01001I_013",
      pop_wom_45_54_race_h_="B01001I_027",
      pop_wom_55_64_race_h_="B01001I_028",
      
      #### Poverty-------
      #Estimate!!Total:!!Income in the past 12 months below poverty level:
      #POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
      pop_pov_="B17001_002", #pov for income below poverty line
      pop_pov_tot_ ="B17001_001", #total pop for the proportion
      
      #Poverty by gender by age - overall
      pop_men_45_54_pov_="B17001_013",
      pop_men_55_64_pov_="B17001_014",
      pop_wom_45_54_pov_="B17001_027",
      pop_wom_55_64_pov_="B17001_028",
      
      # Poverty by gender by age by race
      pop_men_45_54_race_b_pov_ ="B17001B_013", 
      pop_men_55_64_race_b_pov_="B17001B_014",
      pop_wom_45_54_race_b_pov_ ="B17001B_027",
      pop_wom_55_64_race_b_pov_ = "B17001B_028",
      
      #white - not hispanic/latino
      pop_men_45_54_race_w_pov_ ="B17001H_013", 
      pop_men_55_64_race_w_pov_="B17001H_014",
      pop_wom_45_54_race_w_pov_ ="B17001H_027",
      pop_wom_55_64_race_w_pov_ = "B17001H_028",
      
      #hispanic
      pop_men_45_54_race_h_pov_ ="B17001I_013", 
      pop_men_55_64_race_h_pov_="B17001I_014",
      pop_wom_45_54_race_h_pov_ ="B17001I_027",
      pop_wom_55_64_race_h_pov_ = "B17001I_028",
      
      
      #### Employment -----------
      #by sex by age
      #gathering total in labor force and unemployed for each category
      pop_men_45_54_in_labor_force_ ="B23001_046",
      pop_men_45_54_unemployed_="B23001_050",
      pop_men_55_59_in_labor_force_= "B23001_053",
      pop_men_55_59_unemployed_="B23001_057",
      pop_men_60_61_in_labor_force_="B23001_060",
      pop_men_60_61_unemployed_="B23001_064" ,
      pop_men_62_64_in_labor_force_="B23001_067",
      pop_men_62_64_unemployed_="B23001_071",
      
      pop_wom_45_54_in_labor_force_="B23001_132",
      pop_wom_45_54_unemployed_="B23001_136",
      pop_wom_55_59_in_labor_force_= "B23001_139",
      pop_wom_55_59_unemployed_="B23001_143",
      pop_wom_60_61_in_labor_force_="B23001_146",
      pop_wom_60_61_unemployed_="B23001_150" ,
      pop_wom_62_64_in_labor_force_="B23001_153",
      pop_wom_62_64_unemployed_="B23001_157" ,
      
      #by race - only 25-54 and 55-64 area available categories
      #black
      pop_men_25_54_race_b_in_labor_force_ ="B23002B_018",
      pop_men_25_54_race_b_unemployed_="B23002B_022",
      pop_men_55_64_race_b_in_labor_force_ ="B23002B_025",
      pop_men_55_64_race_b_unemployed_="B23002B_029",
      
      pop_wom_25_54_race_b_in_labor_force_ ="B23002B_059",
      pop_wom_25_54_race_b_unemployed_="B23002B_061",
      pop_wom_55_64_race_b_in_labor_force_ ="B23002B_064",
      pop_wom_55_64_race_b_unemployed_="B23002B_068",
      
      #white    
      pop_men_25_54_race_w_in_labor_force_ ="B23002H_018",
      pop_men_25_54_race_w_unemployed_="B23002H_022",
      pop_men_55_64_race_w_in_labor_force_ ="B23002H_025",
      pop_men_55_64_race_w_unemployed_="B23002H_029",
      
      pop_wom_25_54_race_w_in_labor_force_ ="B23002H_057",
      pop_wom_25_54_race_w_unemployed_="B23002H_061",
      pop_wom_55_64_race_w_in_labor_force_ ="B23002H_064",
      pop_wom_55_64_race_w_unemployed_="B23002H_068",
      
      #hispanic
      pop_men_25_54_race_h_in_labor_force_ ="B23002I_018",
      pop_men_25_54_race_h_unemployed_="B23002I_022",
      pop_men_55_64_race_h_in_labor_force_ ="B23002I_025",
      pop_men_55_64_race_h_unemployed_="B23002I_029",
      
      pop_wom_25_54_race_h_in_labor_force_ ="B23002I_057",
      pop_wom_25_54_race_h_unemployed_="B23002I_061",
      pop_wom_55_64_race_h_in_labor_force_ ="B23002I_064",
      pop_wom_55_64_race_h_unemployed_="B23002I_068",
      
      #### Education--------
      #Note among pop. 25 and older by race and sex
      #Not available to my knowledge with finer stratification by age within race category
      
      #all races
      #Only include lower than high school and then sum and then take proportion
      pop_men_45_64_educ_lt_9th_="B15001_028",
      pop_men_45_64_educ_9th_12th_="B15001_029",#no diploma
      
      pop_wom_45_64_educ_lt_9th_="B15001_069",
      pop_wom_45_64_educ_9th_12th_="B15001_070",#no diploma
      
      
      #by race: among pop 25 and older
      #To get total, I should include complement as well. Easier than adding up age groups.
      
      #black
      pop_men_25_plus_race_b_educ_lt_hs_="C15002B_003",
      pop_men_25_plus_race_b_educ_hs_grad_="C15002B_004",
      pop_wom_25_plus_race_b_educ_lt_hs_="C15002B_008",    
      pop_wom_25_plus_race_b_educ_hs_grad_="C15002B_009",
      
      #white - not hispanic
      pop_men_25_plus_race_w_educ_lt_hs_="C15002H_003",
      pop_men_25_plus_race_w_educ_hs_grad_="C15002H_004",
      pop_wom_25_plus_race_w_educ_lt_hs_="C15002H_008",    
      pop_wom_25_plus_race_w_educ_hs_grad_="C15002H_009",
      
      #hispanic
      pop_men_25_plus_race_h_educ_lt_hs_="C15002I_003",
      pop_men_25_plus_race_h_educ_hs_grad_="C15002I_004",
      pop_wom_25_plus_race_h_educ_lt_hs_="C15002I_008",    
      pop_wom_25_plus_race_h_educ_hs_grad_="C15002I_009" ,
      
      #### Married-------
      #Looks like marital status is available in ACS by age/sex overall,
      #but not by race
      #married - spouse present-Estimate!!Total:!!Male:!!Now married:!!Married, spouse present:
      pop_men_45_49_married_now_="B12002_027",
      pop_men_50_54_married_now_="B12002_028",
      pop_men_55_59_married_now_="B12002_029",
      pop_men_60_64_married_now_="B12002_030",
      
      pop_wom_45_49_married_now_="B12002_120",
      pop_wom_50_54_married_now_="B12002_121",
      pop_wom_55_59_married_now_="B12002_122",
      pop_wom_60_64_married_now_="B12002_123",
      
      #by race - 15 and older
      #hopefully still capture state-by-state variation even if not accurate in age cat.
      #also use never married so I can more easily calculate proportions
      #black
      #note never and now are not necessarily complements. 
      #assume they are for these purposes; error the same across cats; not same age group anyway
      pop_men_15_plus_race_b_married_never_="B12002B_003",
      pop_men_15_plus_race_b_married_now_="B12002B_004",
      pop_wom_15_plus_race_b_married_never_="B12002B_009",
      pop_wom_15_plus_race_b_married_now_="B12002B_010",
      
      #white
      pop_men_15_plus_race_w_married_never_="B12002H_003",
      pop_men_15_plus_race_w_married_now_="B12002H_004",
      pop_wom_15_plus_race_w_married_never_="B12002H_009",
      pop_wom_15_plus_race_w_married_now_="B12002H_010",
      
      #hispanic
      pop_men_15_plus_race_h_married_never_="B12002I_003",
      pop_men_15_plus_race_h_married_now_="B12002I_004",
      pop_wom_15_plus_race_h_married_never_="B12002I_009",
      pop_wom_15_plus_race_h_married_now_="B12002I_010"
      
    )
  ) %>% 
    #rename the _M prefix to moe for margin of error
    #remove the _E prefix altogether
    rename_with(~gsub("_E", "", .x, fixed = TRUE)) %>% 
    rename_with(~gsub("_M", "_moe", .x, fixed = TRUE)) %>% 
    
    
    mutate(
      ### Calculate proportions-------  
      #### pop by sex by age-------
      pop_men_45_64=pop_men_45_49+
        pop_men_50_54+
        pop_men_55_59+
        pop_men_60_61+
        pop_men_62_64,
      
      pop_wom_45_64=pop_wom_45_49+
        pop_wom_50_54+
        pop_wom_55_59+
        pop_wom_60_61+
        pop_wom_62_64,
      
      #below I use "bot" to indicate sum of both genders
      pop_bot_45_64=pop_men_45_64+pop_wom_45_64,
      
      #proportion of adults 45-64 who are men
      prop_men_45_64=pop_men_45_64/pop_bot_45_64,
      
      #proportion of total population aged 45-64
      prop_45_64_of_tot=pop_bot_45_64/pop_tot,
      
      #### race-----------
      prop_race_b=pop_race_b/pop_race_tot,
      prop_race_w=pop_race_w/pop_race_tot,
      prop_race_h=pop_race_h/pop_race_h_tot,#should be same as pop_race_tot
      prop_race_non_w=1-prop_race_w,
      
      #race by sex
      pop_men_45_64_race_b=pop_men_45_54_race_b+pop_men_55_64_race_b,
      pop_wom_45_64_race_b=pop_wom_45_54_race_b+pop_wom_55_64_race_b,
      pop_men_45_64_race_w=pop_men_45_54_race_w+pop_men_55_64_race_w,
      pop_wom_45_64_race_w=pop_wom_45_54_race_w+pop_wom_55_64_race_w,
      pop_men_45_64_race_h=pop_men_45_54_race_h+pop_men_55_64_race_h,
      pop_wom_45_64_race_h=pop_wom_45_54_race_h+pop_wom_55_64_race_h,
      
      #race totals
      pop_bot_45_64_race_b=pop_men_45_64_race_b+pop_wom_45_64_race_b,
      pop_bot_45_64_race_w=pop_men_45_64_race_w+pop_wom_45_64_race_w,
      pop_bot_45_64_race_h=pop_men_45_64_race_h+pop_wom_45_64_race_h,
      
      #proportion men in racial group
      prop_men_45_64_race_b=pop_men_45_64_race_b/pop_bot_45_64_race_b,
      prop_men_45_64_race_w=pop_men_45_64_race_w/pop_bot_45_64_race_w,
      prop_men_45_64_race_h=pop_men_45_64_race_h/pop_bot_45_64_race_h,
      
      #proportion of race in 45_64 group
      #bot to indicate both genders
      prop_race_b_men_45_64=pop_men_45_64_race_b/pop_men_45_64,
      prop_race_w_men_45_64=pop_men_45_64_race_w/pop_men_45_64,
      prop_race_h_men_45_64=pop_men_45_64_race_h/pop_men_45_64,
      prop_race_non_w_men_45_64=1-prop_race_w_men_45_64,#proportion non white 
      
      prop_race_b_wom_45_64=pop_wom_45_64_race_b/pop_wom_45_64,
      prop_race_w_wom_45_64=pop_wom_45_64_race_w/pop_wom_45_64,
      prop_race_h_wom_45_64=pop_wom_45_64_race_h/pop_wom_45_64,
      prop_race_non_w_wom_45_64=1-prop_race_w_wom_45_64,#proportion non white 
      
      prop_race_b_bot_45_64=pop_bot_45_64_race_b/pop_bot_45_64,
      prop_race_w_bot_45_64=pop_bot_45_64_race_w/pop_bot_45_64,
      prop_race_h_bot_45_64=pop_bot_45_64_race_h/pop_bot_45_64,
      prop_race_non_w_bot_45_64=1-prop_race_w_bot_45_64,#proportion non white, overall
      
      
      #### Poverty---------
      #proportion in poverty, overall and by age/sex/race
      prop_pov=pop_pov/pop_pov_tot,
      
      #overall in 45-64 - not race stratified
      pop_men_45_64_pov=pop_men_45_54_pov+pop_men_55_64_pov,
      pop_wom_45_64_pov=pop_wom_45_54_pov+pop_wom_55_64_pov,
      prop_men_45_64_pov=pop_men_45_64_pov/pop_men_45_64,#proportion
      prop_wom_45_64_pov=pop_wom_45_64_pov/pop_wom_45_64,#proportion
      pop_bot_45_64_pov=pop_men_45_64_pov+pop_wom_45_64_pov,
      prop_bot_45_64_pov=pop_bot_45_64_pov/pop_bot_45_64,
      
      #by race
      pop_men_45_64_race_b_pov=pop_men_45_54_race_b_pov+pop_men_55_64_race_b_pov,
      pop_wom_45_64_race_b_pov=pop_wom_45_54_race_b_pov+pop_wom_55_64_race_b_pov,
      prop_men_45_64_race_b_pov=pop_men_45_64_race_b_pov/pop_men_45_64_race_b,#proportion
      prop_wom_45_64_race_b_pov=pop_wom_45_64_race_b_pov/pop_wom_45_64_race_b,#proportion
      pop_bot_45_64_race_b_pov=pop_men_45_64_race_b_pov+pop_wom_45_64_race_b_pov,
      prop_bot_45_64_race_b_pov=pop_bot_45_64_race_b_pov/pop_bot_45_64_race_b,
      
      pop_men_45_64_race_w_pov=pop_men_45_54_race_w_pov+pop_men_55_64_race_w_pov,
      pop_wom_45_64_race_w_pov=pop_wom_45_54_race_w_pov+pop_wom_55_64_race_w_pov,
      prop_men_45_64_race_w_pov=pop_men_45_64_race_w_pov/pop_men_45_64_race_w,#proportion
      prop_wom_45_64_race_w_pov=pop_wom_45_64_race_w_pov/pop_wom_45_64_race_w,#proportion
      pop_bot_45_64_race_w_pov=pop_men_45_64_race_w_pov+pop_wom_45_64_race_w_pov,
      prop_bot_45_64_race_w_pov=pop_bot_45_64_race_w_pov/pop_bot_45_64_race_w,
      
      pop_men_45_64_race_h_pov=pop_men_45_54_race_h_pov+pop_men_55_64_race_h_pov,
      pop_wom_45_64_race_h_pov=pop_wom_45_54_race_h_pov+pop_wom_55_64_race_h_pov,
      prop_men_45_64_race_h_pov=pop_men_45_64_race_h_pov/pop_men_45_64_race_h,#proportion
      prop_wom_45_64_race_h_pov=pop_wom_45_64_race_h_pov/pop_wom_45_64_race_h,#proportion
      pop_bot_45_64_race_h_pov=pop_men_45_64_race_h_pov+pop_wom_45_64_race_h_pov,
      prop_bot_45_64_race_h_pov=pop_bot_45_64_race_h_pov/pop_bot_45_64_race_h,
      
      
      #### Employment---------
      pop_men_45_64_in_labor_force= pop_men_45_54_in_labor_force+
        pop_men_55_59_in_labor_force+
        pop_men_60_61_in_labor_force+
        pop_men_62_64_in_labor_force,
      
      pop_wom_45_64_in_labor_force= pop_wom_45_54_in_labor_force+
        pop_wom_55_59_in_labor_force+
        pop_wom_60_61_in_labor_force+
        pop_wom_62_64_in_labor_force,
      
      pop_men_45_64_unemployed= pop_men_45_54_unemployed+
        pop_men_55_59_unemployed+
        pop_men_60_61_unemployed+
        pop_men_62_64_unemployed,
      
      pop_wom_45_64_unemployed= pop_wom_45_54_unemployed+
        pop_wom_55_59_unemployed+
        pop_wom_60_61_unemployed+
        pop_wom_62_64_unemployed,
      
      #pop_bot for both genders
      pop_bot_45_64_in_labor_force=pop_men_45_64_in_labor_force+pop_wom_45_64_in_labor_force, 
      pop_bot_45_64_unemployed=pop_men_45_64_unemployed+pop_wom_45_64_unemployed, 
      
      #proportion unemployed
      prop_men_45_64_unemployed=pop_men_45_64_unemployed/pop_men_45_64_in_labor_force,
      prop_wom_45_64_unemployed=pop_wom_45_64_unemployed/pop_wom_45_64_in_labor_force,
      prop_bot_45_64_unemployed=pop_bot_45_64_unemployed/pop_bot_45_64_in_labor_force,
      
      #employment by race - note 25-64
      #black
      pop_men_25_64_race_b_in_labor_force=pop_men_25_54_race_b_in_labor_force+pop_men_55_64_race_b_in_labor_force,
      pop_men_25_64_race_b_unemployed=pop_men_25_54_race_b_unemployed+pop_men_55_64_race_b_unemployed,
      prop_men_25_64_race_b_unemployed=pop_men_25_64_race_b_unemployed/pop_men_25_64_race_b_in_labor_force,
      
      pop_wom_25_64_race_b_in_labor_force=pop_wom_25_54_race_b_in_labor_force+pop_wom_55_64_race_b_in_labor_force,
      pop_wom_25_64_race_b_unemployed=pop_wom_25_54_race_b_unemployed+pop_wom_55_64_race_b_unemployed,
      prop_wom_25_64_race_b_unemployed=pop_wom_25_64_race_b_unemployed/pop_wom_25_64_race_b_in_labor_force,
      
      pop_bot_25_64_race_b_in_labor_force=pop_men_25_64_race_b_in_labor_force+pop_wom_25_64_race_b_in_labor_force,
      pop_bot_25_64_race_b_unemployed=pop_men_25_64_race_b_unemployed+pop_wom_25_64_race_b_unemployed,
      prop_bot_25_64_race_b_unemployed=pop_bot_25_64_race_b_unemployed/pop_bot_25_64_race_b_in_labor_force,
      
      #white
      pop_men_25_64_race_w_in_labor_force=pop_men_25_54_race_w_in_labor_force+pop_men_55_64_race_w_in_labor_force,
      pop_men_25_64_race_w_unemployed=pop_men_25_54_race_w_unemployed+pop_men_55_64_race_w_unemployed,
      prop_men_25_64_race_w_unemployed=pop_men_25_64_race_w_unemployed/pop_men_25_64_race_w_in_labor_force,
      
      pop_wom_25_64_race_w_in_labor_force=pop_wom_25_54_race_w_in_labor_force+pop_wom_55_64_race_w_in_labor_force,
      pop_wom_25_64_race_w_unemployed=pop_wom_25_54_race_w_unemployed+pop_wom_55_64_race_w_unemployed,
      prop_wom_25_64_race_w_unemployed=pop_wom_25_64_race_w_unemployed/pop_wom_25_64_race_w_in_labor_force,
      
      pop_bot_25_64_race_w_in_labor_force=pop_men_25_64_race_w_in_labor_force+pop_wom_25_64_race_w_in_labor_force,
      pop_bot_25_64_race_w_unemployed=pop_men_25_64_race_w_unemployed+pop_wom_25_64_race_w_unemployed,
      prop_bot_25_64_race_w_unemployed=pop_bot_25_64_race_w_unemployed/pop_bot_25_64_race_w_in_labor_force,
      
      #hispanic
      pop_men_25_64_race_h_in_labor_force=pop_men_25_54_race_h_in_labor_force+pop_men_55_64_race_h_in_labor_force,
      pop_men_25_64_race_h_unemployed=pop_men_25_54_race_h_unemployed+pop_men_55_64_race_h_unemployed,
      prop_men_25_64_race_h_unemployed=pop_men_25_64_race_h_unemployed/pop_men_25_64_race_h_in_labor_force,
      
      pop_wom_25_64_race_h_in_labor_force=pop_wom_25_54_race_h_in_labor_force+pop_wom_55_64_race_h_in_labor_force,
      pop_wom_25_64_race_h_unemployed=pop_wom_25_54_race_h_unemployed+pop_wom_55_64_race_h_unemployed,
      prop_wom_25_64_race_h_unemployed=pop_wom_25_64_race_h_unemployed/pop_wom_25_64_race_h_in_labor_force,
      
      pop_bot_25_64_race_h_in_labor_force=pop_men_25_64_race_h_in_labor_force+pop_wom_25_64_race_h_in_labor_force,
      pop_bot_25_64_race_h_unemployed=pop_men_25_64_race_h_unemployed+pop_wom_25_64_race_h_unemployed,
      prop_bot_25_64_race_h_unemployed=pop_bot_25_64_race_h_unemployed/pop_bot_25_64_race_h_in_labor_force,
      
      
      #### Education--------
      #Overall proportion, 45-64
      #Less than high school graduate
      pop_men_45_64_educ_lt_hs=pop_men_45_64_educ_lt_9th+pop_men_45_64_educ_9th_12th,
      pop_wom_45_64_educ_lt_hs=pop_wom_45_64_educ_lt_9th+pop_wom_45_64_educ_9th_12th,
      pop_bot_45_64_educ_lt_hs=pop_men_45_64_educ_lt_hs+pop_wom_45_64_educ_lt_hs,
      
      prop_men_45_64_educ_lt_hs=pop_men_45_64_educ_lt_hs/pop_men_45_64,
      prop_wom_45_64_educ_lt_hs=pop_wom_45_64_educ_lt_hs/pop_wom_45_64,
      prop_bot_45_64_educ_lt_hs=pop_bot_45_64_educ_lt_hs/pop_bot_45_64,

      
      #in racial groups, 25+
      pop_men_25_plus_race_b_educ_tot=pop_men_25_plus_race_b_educ_lt_hs+pop_men_25_plus_race_b_educ_hs_grad,
      pop_wom_25_plus_race_b_educ_tot=pop_wom_25_plus_race_b_educ_lt_hs+pop_wom_25_plus_race_b_educ_hs_grad,
      prop_men_25_plus_race_b_educ_lt_hs=pop_men_25_plus_race_b_educ_lt_hs/pop_men_25_plus_race_b_educ_tot,#proportion
      prop_wom_25_plus_race_b_educ_lt_hs=pop_wom_25_plus_race_b_educ_lt_hs/pop_wom_25_plus_race_b_educ_tot,#proportion
      pop_bot_25_plus_race_b_educ_lt_hs=pop_men_25_plus_race_b_educ_lt_hs+pop_wom_25_plus_race_b_educ_lt_hs,
      pop_bot_25_plus_race_b_educ_tot=pop_men_25_plus_race_b_educ_tot+pop_wom_25_plus_race_b_educ_tot,
      prop_bot_25_plus_race_b_educ_lt_hs=pop_bot_25_plus_race_b_educ_lt_hs/pop_bot_25_plus_race_b_educ_tot,#proportion
      
      pop_men_25_plus_race_w_educ_tot=pop_men_25_plus_race_w_educ_lt_hs+pop_men_25_plus_race_w_educ_hs_grad,
      pop_wom_25_plus_race_w_educ_tot=pop_wom_25_plus_race_w_educ_lt_hs+pop_wom_25_plus_race_w_educ_hs_grad,
      prop_men_25_plus_race_w_educ_lt_hs=pop_men_25_plus_race_w_educ_lt_hs/pop_men_25_plus_race_w_educ_tot,#proportion
      prop_wom_25_plus_race_w_educ_lt_hs=pop_wom_25_plus_race_w_educ_lt_hs/pop_wom_25_plus_race_w_educ_tot,#proportion
      pop_bot_25_plus_race_w_educ_lt_hs=pop_men_25_plus_race_w_educ_lt_hs+pop_wom_25_plus_race_w_educ_lt_hs,
      pop_bot_25_plus_race_w_educ_tot=pop_men_25_plus_race_w_educ_tot+pop_wom_25_plus_race_w_educ_tot,
      prop_bot_25_plus_race_w_educ_lt_hs=pop_bot_25_plus_race_w_educ_lt_hs/pop_bot_25_plus_race_w_educ_tot,#proportion
      
      pop_men_25_plus_race_h_educ_tot=pop_men_25_plus_race_h_educ_lt_hs+pop_men_25_plus_race_h_educ_hs_grad,
      pop_wom_25_plus_race_h_educ_tot=pop_wom_25_plus_race_h_educ_lt_hs+pop_wom_25_plus_race_h_educ_hs_grad,
      prop_men_25_plus_race_h_educ_lt_hs=pop_men_25_plus_race_h_educ_lt_hs/pop_men_25_plus_race_h_educ_tot,#proportion
      prop_wom_25_plus_race_h_educ_lt_hs=pop_wom_25_plus_race_h_educ_lt_hs/pop_wom_25_plus_race_h_educ_tot,#proportion
      pop_bot_25_plus_race_h_educ_lt_hs=pop_men_25_plus_race_h_educ_lt_hs+pop_wom_25_plus_race_h_educ_lt_hs,
      pop_bot_25_plus_race_h_educ_tot=pop_men_25_plus_race_h_educ_tot+pop_wom_25_plus_race_h_educ_tot,
      prop_bot_25_plus_race_h_educ_lt_hs=pop_bot_25_plus_race_h_educ_lt_hs/pop_bot_25_plus_race_h_educ_tot,#proportion
      
      #### Married-------
      #Overall in 45-64 group
      pop_men_45_64_married_now= pop_men_45_49_married_now+
        pop_men_50_54_married_now+
        pop_men_55_59_married_now+
        pop_men_60_64_married_now,
      
      pop_wom_45_64_married_now= pop_wom_45_49_married_now+
        pop_wom_50_54_married_now+
        pop_wom_55_59_married_now+
        pop_wom_60_64_married_now,
      
      pop_bot_45_64_married_now=pop_men_45_64_married_now+pop_wom_45_64_married_now,
      
      prop_men_45_64_married_now= pop_men_45_64_married_now/pop_men_45_64,
      prop_wom_45_64_married_now= pop_wom_45_64_married_now/pop_wom_45_64,
      prop_bot_45_64_married_now=pop_bot_45_64_married_now/pop_bot_45_64,
      
      #in racial groups, 15+ ; proportion married now
      pop_men_15_plus_race_b_married_cat_tot=pop_men_15_plus_race_b_married_never+pop_men_15_plus_race_b_married_now,
      pop_wom_15_plus_race_b_married_cat_tot=pop_wom_15_plus_race_b_married_never+pop_wom_15_plus_race_b_married_now,
      prop_men_15_plus_race_b_married_now=pop_men_15_plus_race_b_married_now/pop_men_15_plus_race_b_married_cat_tot,#proportion
      prop_wom_15_plus_race_b_married_now=pop_wom_15_plus_race_b_married_now/pop_wom_15_plus_race_b_married_cat_tot,#proportion
      pop_bot_15_plus_race_b_married_now=pop_men_15_plus_race_b_married_now+pop_wom_15_plus_race_b_married_now,
      pop_bot_15_plus_race_b_married_cat_tot=pop_men_15_plus_race_b_married_cat_tot+pop_wom_15_plus_race_b_married_cat_tot,
      prop_bot_15_plus_race_b_married_now=pop_bot_15_plus_race_b_married_now/pop_bot_15_plus_race_b_married_cat_tot,#proportion
      
      pop_men_15_plus_race_w_married_cat_tot=pop_men_15_plus_race_w_married_never+pop_men_15_plus_race_w_married_now,
      pop_wom_15_plus_race_w_married_cat_tot=pop_wom_15_plus_race_w_married_never+pop_wom_15_plus_race_w_married_now,
      prop_men_15_plus_race_w_married_now=pop_men_15_plus_race_w_married_now/pop_men_15_plus_race_w_married_cat_tot,#proportion
      prop_wom_15_plus_race_w_married_now=pop_wom_15_plus_race_w_married_now/pop_wom_15_plus_race_w_married_cat_tot,#proportion
      pop_bot_15_plus_race_w_married_now=pop_men_15_plus_race_w_married_now+pop_wom_15_plus_race_w_married_now,
      pop_bot_15_plus_race_w_married_cat_tot=pop_men_15_plus_race_w_married_cat_tot+pop_wom_15_plus_race_w_married_cat_tot,
      prop_bot_15_plus_race_w_married_now=pop_bot_15_plus_race_w_married_now/pop_bot_15_plus_race_w_married_cat_tot,#proportion
      
      pop_men_15_plus_race_h_married_cat_tot=pop_men_15_plus_race_h_married_never+pop_men_15_plus_race_h_married_now,
      pop_wom_15_plus_race_h_married_cat_tot=pop_wom_15_plus_race_h_married_never+pop_wom_15_plus_race_h_married_now,
      prop_men_15_plus_race_h_married_now=pop_men_15_plus_race_h_married_now/pop_men_15_plus_race_h_married_cat_tot,#proportion
      prop_wom_15_plus_race_h_married_now=pop_wom_15_plus_race_h_married_now/pop_wom_15_plus_race_h_married_cat_tot,#proportion
      pop_bot_15_plus_race_h_married_now=pop_men_15_plus_race_h_married_now+pop_wom_15_plus_race_h_married_now,
      pop_bot_15_plus_race_h_married_cat_tot=pop_men_15_plus_race_h_married_cat_tot+pop_wom_15_plus_race_h_married_cat_tot,
      prop_bot_15_plus_race_h_married_now=pop_bot_15_plus_race_h_married_now/pop_bot_15_plus_race_h_married_cat_tot,#proportion
      
      ### other--------
      #rename GEOID to state_fips for easier linking below
      state_fips=as.numeric(GEOID),
      year=year_val
    )

  return(acs_1_year_by_state)
}

## Test function with 2019------

acs_1_year_by_state_2019=gather_wrangle_acs_by_year(2019)
names(acs_1_year_by_state_2019)
acs_1_year_by_state_2019_geo=acs_1_year_by_state_2019 %>% 
  dplyr::select(-starts_with("state_fips")) %>% #to avoid merge conflict
  left_join(lookup_acs_state_geo,by="GEOID") %>% 
  st_as_sf()

#race
acs_1_year_by_state_2019_geo %>% mapview(zcol="pop_race_h")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_men_45_64")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_race_b")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_race_h")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_race_w")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_race_non_w")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_45_64_of_tot")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_race_non_w_bot_45_64")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_race_b_bot_45_64")


#poverty
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_pov")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_45_64_pov")

#employment and education
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_45_64_unemployed")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_men_25_plus_race_w_educ_lt_hs")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_wom_25_plus_race_w_educ_lt_hs")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_45_64_educ_lt_hs")#error
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_45_64_unemployed")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_25_plus_race_b_educ_lt_hs")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_25_plus_race_w_educ_lt_hs")

#married
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_45_64_married_now")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_15_plus_race_b_married_now")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_15_plus_race_w_married_now")
acs_1_year_by_state_2019_geo %>% mapview(zcol="prop_bot_15_plus_race_h_married_now")

# acs_1_year_by_state_2006=gather_wrangle_acs_by_year(2006)
# acs_1_year_by_state_2007=gather_wrangle_acs_by_year(2007)
#acs_1_year_by_state_2005=gather_wrangle_acs_by_year(2005)#issue with C15002B_003 with 2005

#1-year ACS support begins with 2005, so could impute pre-2005
#Having an issue with 2005 for some vars, so start with 2006
years_for_function=2006:2019
years_for_function

## Run the function for each year -------
#and stack results on top of one another
acs_1_year_by_state_not_imputed=years_for_function %>% 
  map_dfr(gather_wrangle_acs_by_year) %>% 
  dplyr::select(year,state_fips,GEOID,everything())

names(acs_1_year_by_state_not_imputed)

#Limit to one year and map
library(sf)
acs_1_year_by_state_not_imputed %>% 
  dplyr::select(-starts_with("state_fips")) %>% #to avoid merge conflict
  filter(year==2015) %>% 
  left_join(lookup_acs_state_geo,by="GEOID") %>% 
  st_as_sf() %>% 
  mapview(zcol="prop_race_b")

## Impute data before 2005 for this exercise------
#Note the 5-year ACS is not available in tidycensus before 2005 either
acs_1_year_by_state_not_imputed %>% 
  dplyr::select(year,state_fips,GEOID,everything())

state_year_blank_data_imputed_years=acs_1_year_by_state_not_imputed %>% 
  group_by(state_fips) %>% 
  summarise(n=n()) %>% 
  dplyr::select(-n) %>% 
  mutate(n_years_to_impute=6) %>% 
  uncount(n_years_to_impute) %>% 
  group_by(state_fips) %>% 
  mutate(
    year=1999+row_number(),
    acs_data_imputed="yes")

#Now append to this other data
#Also,  I might need a convenience lookup for the state-state-abbreviation
us_state_fips_url = "https://raw.githubusercontent.com/michaeldgarber/lookups-county-state/main/us-state-fips.csv"
us_state_fips_lookup = us_state_fips_url %>% 
  url() %>% 
  read_csv() %>% 
  mutate(state_fips=as.numeric(state_fips))
acs_1_year_by_state_2000_2019=acs_1_year_by_state_not_imputed %>% 
  bind_rows(state_year_blank_data_imputed_years) %>% 
  dplyr::select(year,state_fips,GEOID,contains("imputed"),everything()) %>% 
  arrange(state_fips,year) %>% 
  group_by(state_fips) %>% 
  fill(
    #all except for these
    -starts_with("year"),
    -starts_with("state_fips"), 
    -starts_with("imputed"),
    
       .direction="up") %>% 
  ungroup() %>% 
  arrange(state_fips,year) %>% 
  #Drop margin of error  to simplify and NAME
  dplyr::select(-ends_with("moe"),-NAME) 

#checks

#Save this to disc so I don't have to run this every time I open it
library(here)
setwd(here("data","data-processed"))
save(acs_1_year_by_state_2000_2019,file="acs_1_year_by_state_2000_2019.RData")

# acs_1_year_by_state_2000_2019 %>%
#   filter(year<2008) %>%
#   View()
names(acs_1_year_by_state_2000_2019)

us_state_fips_lookup
## Table of pop. size in 2019----
# acs_1_year_by_state_2000_2019 %>% 
#   filter(year==2019) %>% 
#   dplyr::select(state_fips,pop_bot_45_64) %>% 
#   left_join(us_state_fips_lookup, by="state_fips") %>% 
#   arrange(pop_bot_45_64) %>% 
#   View()
  


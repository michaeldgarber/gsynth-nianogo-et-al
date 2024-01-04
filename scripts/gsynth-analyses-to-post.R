#Run generalized synthetic control analyses
#Nov 7, 2023: paring down; exclude the no-covariate model
#Throughout, I'm using 2,000 bootstrap reps
#Note analyses are replicated with the exact number of bootstrap reps here:
#gsynth-replicate-nianogo-et-al-main-text.R
#Using trimmed variables to avoid outlier influence in modeling
#Nov 15: changing name of gsynth obj from "est_" to "gsynth_out_"
#Nov 28th: simplifying before posting

#Read in data if needed
library(here)
library(tidyverse)
library(gsynth)

#source(here("scripts", "read-wrangle-data-acs.R"))
#source(here("scripts", "read-wrangle-data-pres-election.R"))

#all that's needed for this script:
source(here("scripts", "read-wrangle-data-nianogo-et-al.R"))

packageVersion("gsynth")#check version


# Nianogo analyses - replicate (Scenario 1, Table)-------

## overall--------
#The below code is adapted from the script posted by Nianogo et al.:
#Medicaid_cvd_disparities_05_13_23_final.Rmd
#summary(overall$population)

#Set the number of bootstrap reps as an object to make sure I'm consistent throughout
nboots_val=2000

gsynth_out_overall_authors <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate + 
    cardio_rate + population +
    low_educ + employed_for_wages + party + 
    low_income + married + male + race_nonwhite,
  data = overall,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val,#for more stability, try raising this
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = T #turn on parallel computing to speed up bootstrapping
)

#Plot following their code
plot(gsynth_out_overall_authors, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "", #ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

#save gsynth output for use in RMarkdown
setwd(here("data","data-processed"))
save(gsynth_out_overall_authors,file="gsynth_out_overall_authors.RData")

### Examine elements of output-----
#load("gsynth_out_overall_authors.RData")
gsynth_out_overall_authors$est.avg#replicated their estimate and CI
gsynth_out_overall_authors$est.avg[1]+gsynth_out_overall_authors$est.avg[2]*1.96
gsynth_out_overall_authors$est.avg[1]-gsynth_out_overall_authors$est.avg[2]*1.96
gsynth_out_overall_authors$Y.tr #actual outcome in treated
gsynth_out_overall_authors$Y.ct #predicted counterfactual outcome in treated
gsynth_out_overall_authors$D.tr #treatment status by year for each state

gsynth_out_overall_authors$eff.boot #a bootstrap effect estimate for every unit for every bootstrap
gsynth_out_overall_authors$att.avg
gsynth_out_overall_authors$att
gsynth_out_overall_authors$eff

### Data wrangling of output--------
#### Bootstrapped effect estimates-------
#Wrangle the data into a tibble so it's easier to work with
summary(covars_alt$year)
tib_eff_boot_overall=gsynth_out_overall_authors$eff.boot %>% 
  #converting to tibble makes the data such that there
  #are 20 rows and 6,800 columns, each with information
  #containing the bootstrapped effect estimates.
  #We need to organize the data such that each row
  #represents a unit-month-bootstrap replicate
  as_tibble() %>% 
  #Note each row represents the time variable.
  #The observation period begins in 2000, so 
  #we should add 1999 to the row number to get the first obs
  #to be 2000
  mutate(year=row_number()+1999) %>% 
  #make year the left-most variable to make it easier to see
  dplyr::select(year,everything()) %>% 
  #now make the data long form
  pivot_longer(cols=-year) %>% 
  #we now have three variables: year, name, and value.
  #the "name" variable contains the state id and the bootstrap
  #replicate. We need to separate these two values into separate variables
  #We can use separate_wider_delim() for this from tidyr
  separate_wider_delim(name,names=c("state_id","boot_rep"), ".") %>% 
  #make the state_id and boot_rep (boot replication) variables numeric
  mutate(
    state_id=as.numeric(state_id),
    boot_rep_char=str_sub(boot_rep, 5,11),#5 to 11 in case lots of digits
    boot_rep=as.numeric(boot_rep_char)
  ) %>% 
  #drop vars not needed
  dplyr::select(-boot_rep_char) %>% 
  #note the difference-based effect estimate is stored in "value"
  #let's call it diff_boot
  rename(diff_boot =value)


tib_eff_boot_overall

#Do similar data wrangling for predicted counterfactuals,
#actual outcomes, treatment status, etc., to facilitate downstream
#calculations

gsynth_out_overall_authors$est.beta

#### Actual outcome in treated------
tib_y_tr_overall=gsynth_out_overall_authors$Y.tr %>% 
  as_tibble() %>%
  mutate(year=row_number()+1999) %>% 
  dplyr::select(year,everything()) %>% 
  pivot_longer(cols=-year) %>% 
  rename(
    state_id = name,
    y_tr_pt = value #observed treated value, pt for point estimate
    ) %>% 
  mutate(state_id =as.numeric(state_id))

tib_y_tr_overall

#### Counterfactual outcome in treated----
tib_y_ct_overall=gsynth_out_overall_authors$Y.ct %>% 
  as_tibble() %>%
  mutate(year=row_number()+1999) %>% 
  dplyr::select(year,everything()) %>% 
  pivot_longer(cols=-year) %>% 
  rename(
    state_id = name,
    y_ct_pt = value#estimated counterfactual value, pt for point estimate
    ) %>% 
  mutate(state_id =as.numeric(state_id))

tib_y_ct_overall



#### Treatment status in state-year-------
gsynth_out_overall_authors$eff
gsynth_out_overall_authors$D.tr
tib_treatedpost_overall=gsynth_out_overall_authors$D.tr %>% 
  as_tibble() %>%
  mutate(year=row_number()+1999) %>% 
  dplyr::select(year,everything()) %>% 
  pivot_longer(cols=-year) %>% 
  rename(
    state_id = name,
    treatedpost = value) %>% 
  mutate(state_id=as.numeric(state_id))

#tib_treatedpost_overall %>% View()

#number of treated years by state
tib_treatedpost_overall %>% 
  filter(treatedpost==1) %>% 
  group_by(state_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  arrange(desc(n))
#### Number of never-treated states-------
#also keep track of number of controls (never-treated units)
n_never_treated_overall=gsynth_out_overall_authors$Nco %>% 
  as_tibble() %>%
  rename(n_never_treated = value) 

n_never_treated_overall
## Join them together and calculate bootstrapped counterfactual
#in each bootstrapped replicate
names(state_wts_by_treated_year_overall)
names(state_year_wts_overall)
summary(state_wts_by_treated_year_overall$prop_pop_by_state)
summary(state_year_wts_overall$prop_of_tot_pop_year)
state_year_wts_overall
gsynth_out_tib_overall=tib_eff_boot_overall %>% 
  left_join(tib_y_tr_overall,by=c("year","state_id")) %>% 
  left_join(tib_y_ct_overall,by=c("year","state_id")) %>% 
  left_join(tib_treatedpost_overall,by=c("year","state_id")) %>% 
  
  #add weights
  left_join(state_wts_by_treated_year_overall,by=c("year","state_id")) %>% 
  left_join(state_year_wts_overall,by=c("year","state_id")) %>% 
  bind_cols(n_never_treated_overall) %>% 
  mutate(
    diff_pt=y_tr_pt-y_ct_pt,#difference effect (point estimate)
    ratio_pt=y_tr_pt/y_ct_pt, #Ratio effect (point estimate)
    #bootstrap values
    #effect estimate:diff= y_tr-y_ct, so
    #to calculate bootstrapped counterfactuals:
    #y_ct=y_tr-diff
    y_ct_boot=y_tr_pt-diff_boot, #counterfactual estimate - bootstrap
    ratio_boot=y_tr_pt/y_ct_boot,#ratio bootstrap if needed
    
    #Calculate measures of fit:
    #Consider root mean square error and mean absolute error
    #absolute value first
    #simply the absolute value of the truth minus estimate
    diff_pt_abs=abs(diff_pt),
    #squared prediction error
    diff_pt_squared=diff_pt**2#which will then be square-rooted later
  ) %>% 
  dplyr::select(
    year, state_id,
    contains("boot_rep"),
                treatedpost,
                contains("y_tr"),
                contains("y_ct"),
                contains("pt"),contains("boot"),
                everything())

#gsynth_out_tib_overall %>%    View()

### Test weighted calcs-------
#simple weighted mean of diff_pt
#weight by state and then summarize over year (i.e., equal weight to each year)
summary(gsynth_out_tib_overall$prop_of_tot_pop_year)
summary(gsynth_out_tib_overall$prop_pop_by_state)
gsynth_out_tib_overall %>% 
  filter(treatedpost==1) %>% 
  group_by(year) %>% 
  summarise(
    diff_pt_mean_unweighted=mean(diff_pt,na.rm=T),
    diff_pt_mean_weighted=weighted.mean(
      x=diff_pt,
      w=prop_pop_by_state,
      na.rm=T)
    ) %>% 
  ungroup() %>% 
  mutate(overall=1) %>% 
  group_by(overall) %>% 
  summarise(
    diff_pt_mean_unweighted=mean(diff_pt_mean_unweighted,na.rm=T),
    diff_pt_mean_weighted=mean(diff_pt_mean_weighted,na.rm=T)
    )

#weight by state-year
gsynth_out_tib_overall %>% 
  filter(treatedpost==1) %>% 
  #now simply group by treatedpost and summarise
  group_by(treatedpost) %>% 
  summarise(
    diff_pt_mean_unweighted=mean(diff_pt,na.rm=T),
    diff_pt_mean_weighted=weighted.mean(
      x=diff_pt,
      w=prop_of_tot_pop_year,
      na.rm=T)) %>% 
  ungroup() 



## Summarize bootstrapped effect estimates----
#Make this a function as I use this code in several places below
#Separate into two functions to allow flexibility of grouping variables
names(n_never_treated_overall)

summarise_by_boot=function(df){
  df %>% 
    #Note this expects a group_by() statement before it
    #In each bootstrap replicate, calculate the average treatment
    #effect in the treated
    summarise(
      #mean of point estimate difference effect. this should be the 
      #same across replicates
      #Note no weighting needed. states with longer follow-up time
      #will implicitly have more weight, as they have more treated
      #observations.
      diff_pt_mean=mean(diff_pt, na.rm=TRUE),
      
      #Nov 16 2023: add weighted mean
      diff_pt_mean_wt=weighted.mean(
        x=diff_pt,
        w=prop_of_tot_pop_year, #proportion of total population-years
        na.rm=T),
      
      #mean of bootstrapped difference effect. 
      #This will vary between  replicates
      diff_boot_mean=mean(diff_boot,na.rm=TRUE),
      
      #weighted version
      diff_boot_mean_wt=weighted.mean(
        x=diff_boot,
        w=prop_of_tot_pop_year,
        na.rm=T
      ),
      
      #summarize mean absolute error and mean square error
      #Constant within replicate
      diff_pt_abs_mean=mean(diff_pt_abs,na.rm=T),
      diff_pt_squared_mean=mean(diff_pt_squared,na.rm=T),
      
      #For ratios, it's useful to also summarize
      #the average observed treated value
      #and the average estimated counterfactual
      y_tr_pt_mean=mean(y_tr_pt,na.rm=T),#observed treated value
      y_ct_pt_mean=mean(y_ct_pt,na.rm=T),#counterfactual 
      
      #and weighted versions
      y_tr_pt_mean_wt=weighted.mean(
        x=y_tr_pt,
        w=prop_of_tot_pop_year, #proportion of total population-years
        na.rm=T),
      
      y_ct_pt_mean_wt=weighted.mean(
        x=y_ct_pt,
        w=prop_of_tot_pop_year, #proportion of total population-years
        na.rm=T),

      #mean of the counterfactual estimate in the bootstrap rep
      y_ct_boot_mean=mean(y_ct_boot,na.rm=T),
      y_ct_boot_mean_wt=weighted.mean(
        x=y_ct_boot,
        w=prop_of_tot_pop_year,
        na.rm=T),
      
      
      #and keep track of number of never-treated states throughout
      #doesn't vary throughout so can take mean (or median)
      n_never_treated=mean(n_never_treated,na.rm=T),
      
      #Nov 15 2023:
      #Add weighted means
    ) %>% 
    ungroup() %>% 
    mutate(
      ratio_pt_mean=y_tr_pt_mean/y_ct_pt_mean,      #ratio effect, point estimate
      #ratio effect, bootstrap. note the ratio is not calculated 
      #in the summarise statement to avoid taking the arithmetic mean of a ratio
      ratio_boot_mean=y_tr_pt_mean/y_ct_boot_mean, 
      diff_boot_mean_alt_calc=y_tr_pt_mean-y_ct_boot_mean,
      
      #weighted calcs.
      ratio_pt_mean_wt=y_tr_pt_mean_wt/y_ct_pt_mean_wt,
      ratio_boot_mean_wt=y_tr_pt_mean_wt/y_ct_boot_mean_wt,
      diff_boot_mean_wt_alt_calc=y_tr_pt_mean_wt-y_ct_boot_mean_wt
    )
}
  
summarise_over_boot=function(df){
  df %>% 
    summarise(
      #summarize the treated and counterfactual values
      y_tr_pt_mean=mean(y_tr_pt_mean,na.rm=T),#mean of the means from above
      y_ct_pt_mean=mean(y_ct_pt_mean,na.rm=T),#mean of the means from above
      
      diff_pt_mean=mean(diff_pt_mean,na.rm=TRUE),
      ratio_pt_mean=mean(ratio_pt_mean,na.rm=TRUE),
      
      #weighted versions
      #Note I can now take the simple mean, as this is merely varying
      #over bootstrap replication
      y_tr_pt_mean_wt=mean(y_tr_pt_mean_wt,na.rm=T),
      y_ct_pt_mean_wt=mean(y_ct_pt_mean_wt,na.rm=T),
      
      #this should be "wt_mean" for consistency with confidence interval syntax below
      diff_pt_wt_mean=mean(diff_pt_mean_wt,na.rm=TRUE),
      ratio_pt_wt_mean=mean(ratio_pt_mean_wt,na.rm=TRUE),

      #Now return the 95% confidence intervals of the mean bootstrapped 
      #difference effect over replicates
      ratio_pt_ll=quantile(ratio_boot_mean, probs=0.025, na.rm=TRUE),
      ratio_pt_ul=quantile(ratio_boot_mean,probs=.975,na.rm=TRUE),
      
      diff_pt_ll=quantile(diff_boot_mean, probs=0.025, na.rm=TRUE),
      diff_pt_ul=quantile(diff_boot_mean,probs=.975,na.rm=TRUE),
      
      #should be exact same by linearity of expectation but check just in case
      diff_pt_alt_calc_ll=quantile(diff_boot_mean_alt_calc, probs=0.025, na.rm=TRUE),
      diff_pt_alt_calc_ul=quantile(diff_boot_mean_alt_calc, probs=0.975, na.rm=TRUE),
      
      #weighted versions
      ratio_pt_wt_ll=quantile(ratio_boot_mean_wt, probs=0.025, na.rm=TRUE),
      ratio_pt_wt_ul=quantile(ratio_boot_mean_wt,probs=.975,na.rm=TRUE),
      
      diff_pt_wt_ll=quantile(diff_boot_mean_wt, probs=0.025, na.rm=TRUE),
      diff_pt_wt_ul=quantile(diff_boot_mean_wt,probs=.975,na.rm=TRUE),
      
      diff_pt_wt_alt_calc_ll=quantile(diff_boot_mean_wt_alt_calc, probs=0.025, na.rm=TRUE),
      diff_pt_wt_alt_calc_ul=quantile(diff_boot_mean_wt_alt_calc, probs=0.975, na.rm=TRUE),

      
      #simply repeat this, as won't vary between bootstrap reps
      diff_pt_abs_mean=mean(diff_pt_abs_mean,na.rm=T),
      diff_pt_squared_mean=mean(diff_pt_squared_mean,na.rm=T),
      
      #and keep track of number of never-treated states throughout
      #doesn't vary throughout so can take mean (or median)
      n_never_treated=mean(n_never_treated,na.rm=T)
      

    ) %>% 
    ungroup() %>% 
    mutate(
      diff_pt_ci_width=diff_pt_ul-diff_pt_ll,#keep track of width of CI
      #square root the square error
      diff_pt_root_mean_square=sqrt(diff_pt_squared_mean),
      
      #width of weighted difference as well
      diff_pt_wt_ci_width=diff_pt_wt_ul-diff_pt_wt_ll

    )
}


gsynth_out_tib_overall %>% 
  filter(treatedpost==1) %>% 
  ggplot(aes(x=diff_boot))+
  geom_histogram()

gsynth_out_tib_overall %>% 
  filter(treatedpost==1) %>% 
  summary(diff_boot)
  
  
results_overall=gsynth_out_tib_overall %>% 
  group_by(treatedpost,boot_rep) %>% 
  summarise_by_boot() %>% 
  group_by(treatedpost) %>% 
  summarise_over_boot()


#results_overall %>% filter(treatedpost==1) %>%  View()
gsynth_out_overall_authors$MSPE
sqrt(gsynth_out_overall_authors$MSPE)

#Point estimate is the exact same. 
#Confidence intervals are close but not exactly the same.
#Note I can get more precise CIs by running more bootstrap reps.

#These should be the same. Try with more reps.
results_overall %>% 
  filter(treatedpost==1)
gsynth_out_overall_authors$att
gsynth_out_overall_authors$att.avg
gsynth_out_overall_authors$est.avg
-9.67-1.1
#This must be based on a 1.96 estimate assuming normal
-4.2825609+1.96*2.750655
-4.2825609-1.96*2.750655
gsynth_out_overall_authors$att.avg.boot
gsynth_out_overall_authors$att.boot


## Subgroups------
### black----
#Run gsynth for each subgroup.
#Note the authors use the "complete" (imputed) data if the data have missing values.
gsynth_out_black_authors <- gsynth(cvd_death_rate ~ treatedpost + primarycare_rate + 
                      cardio_rate + population + 
                      low_educ + employed_for_wages + party +  
                      low_income + married + male,
                    data = black_complete, 
                    EM = F, 
                    index = c("state_id","year"),
                    inference = "parametric", se = TRUE, 
                    r = c(0, 5), 
                    seed = 123,
                    nboots = nboots_val, 
                    CV = TRUE, 
                    force = "two-way", 
                    parallel = TRUE)

gsynth_out_black_authors$est.avg
gsynth_out_black_authors$est.beta
gsynth_out_black_authors$MSPE #could simply report this

### hispanic-----
gsynth_out_hispanic_authors <- gsynth(
  cvd_death_rate ~ treatedpost + primarycare_rate + 
         cardio_rate + population + 
         low_educ + employed_for_wages + party +  
         low_income + married + male,
       data = hispanic_complete,
       EM = F,
       index = c("state_id","year"),
       inference = "parametric", se = TRUE,
       r = c(0, 5),
       seed = 123,
       nboots = nboots_val, 
       CV = TRUE, 
       force = "two-way", 
       parallel = TRUE
  )
plot(gsynth_out_hispanic_authors, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

gsynth_out_hispanic_authors$est.avg


### white---------
gsynth_out_white_authors <- gsynth(cvd_death_rate ~ treatedpost + primarycare_rate + 
                      cardio_rate + population + 
                      low_educ + employed_for_wages + party +  
                      low_income + married + male,
                    data = white,  
                    EM = F, 
                    index = c("state_id","year"),
                    inference = "parametric", se = TRUE, 
                    r = c(0, 5), 
                    seed = 123,
                    nboots = nboots_val, 
                    CV = TRUE, 
                    force = "two-way", 
                    parallel = TRUE)

plot(gsynth_out_white_authors, type = "counterfactual", raw = "none", theme.bw = TRUE, 
     main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### men--------
summary(men$primarycare_rate)
summary(overall$primarycare_rate)
summary(men$population)
summary(women$population)
gsynth_out_men_authors <- gsynth(cvd_death_rate ~ treatedpost + primarycare_rate + 
                    cardio_rate + population + 
                    low_educ + employed_for_wages + party +  
                    low_income + married + race_nonwhite,
                  data = men,  
                  EM = F, 
                  index = c("state_id","year"),
                  inference = "parametric", se = TRUE, 
                  r = c(0, 5), 
                  seed = 123,
                  nboots = nboots_val, 
                  CV = TRUE, 
                  force = "two-way", 
                  parallel = TRUE)

gsynth_out_men_authors$est.avg
plot(gsynth_out_men_authors, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### women------
gsynth_out_women_authors <- gsynth(cvd_death_rate ~ treatedpost + primarycare_rate + 
                      cardio_rate + population + 
                      low_educ + employed_for_wages + party +  
                      low_income + married + race_nonwhite,
                    data = women,  
                    EM = F, 
                    index = c("state_id","year"),
                    inference = "parametric", se = TRUE, 
                    r = c(0, 5), 
                    seed = 123,
                    nboots = nboots_val, 
                    CV = TRUE, 
                    force = "two-way", 
                    parallel = TRUE)

plot(gsynth_out_women_authors, type = "counterfactual", raw = "none", theme.bw = TRUE, 
     main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

## Wrangle gsynth output---------
### Write function-----
#Modify code above for the overall estimate
wrangle_gsynth_output=function(gsynth_obj_val){
  
  #bootstrap estimates
  tib_eff_boot =gsynth_obj_val$eff.boot %>% 
    as_tibble() %>% 
    mutate(year=row_number()+1999) %>% 
    dplyr::select(year,everything()) %>% 
    pivot_longer(cols=-year) %>% 
    separate_wider_delim(name,names=c("state_id","boot_rep"), ".") %>% 
    mutate(
      state_id=as.numeric(state_id),
      boot_rep_char=str_sub(boot_rep, 5,11),
      boot_rep=as.numeric(boot_rep_char)
    ) %>% 
    dplyr::select(-boot_rep_char) %>% 
    rename(diff_boot =value)
  
  #actual outcome in treated
  tib_y_tr =gsynth_obj_val$Y.tr %>% 
    as_tibble() %>%
    mutate(year=row_number()+1999) %>% 
    dplyr::select(year,everything()) %>% 
    pivot_longer(cols=-year) %>% 
    rename(
      state_id = name,
      y_tr_pt = value #observed treated value, pt for point estimate
    ) %>% 
    mutate(state_id =as.numeric(state_id))
  
  
  # Counterfactual outcome in treated
  tib_y_ct =gsynth_obj_val$Y.ct %>% 
    as_tibble() %>%
    mutate(year=row_number()+1999) %>% 
    dplyr::select(year,everything()) %>% 
    pivot_longer(cols=-year) %>% 
    rename(
      state_id = name,
      y_ct_pt = value#estimated counterfactual value, pt for point estimate
    ) %>% 
    mutate(state_id =as.numeric(state_id))
  
  
  #Treatment status in state-year
  tib_treatedpost =gsynth_obj_val$D.tr %>% 
    as_tibble() %>%
    mutate(year=row_number()+1999) %>% 
    dplyr::select(year,everything()) %>% 
    pivot_longer(cols=-year) %>% 
    rename(
      state_id = name,
      treatedpost = value) %>% 
    mutate(state_id=as.numeric(state_id))
  
  #keep track of number of never-treated states
  n_never_treated=gsynth_obj_val$Nco %>% 
    as_tibble() %>%
    rename(n_never_treated = value) 
  
  ## Join them together
  gsynth_out_tib=tib_eff_boot %>% 
    left_join(tib_y_tr,by=c("year","state_id")) %>% 
    left_join(tib_y_ct,by=c("year","state_id")) %>% 
    left_join(tib_treatedpost,by=c("year","state_id")) %>% 
    bind_cols(n_never_treated) %>% #number of never-treated states (controls)
    mutate(
      diff_pt=y_tr_pt-y_ct_pt,#difference effect (point estimate)
      ratio_pt=y_tr_pt/y_ct_pt, #Ratio effect (point estimate)
      #bootstrap values
      #effect estimate:diff= y_tr-y_ct, so
      #to calculate bootstrapped counterfactuals:
      #y_ct=y_tr-diff
      y_ct_boot=y_tr_pt-diff_boot, #counterfactual estimate - bootstrap
      ratio_boot=y_tr_pt/y_ct_boot,#ratio bootstrap if needed
      
      #Calculate measures of fit:
      #Consider root mean square error and mean absolute error
      #absolute value first
      #simply the absolute value of the truth minus estimate
      diff_pt_abs=abs(diff_pt),
      #squared prediction error if it's before treatment
      diff_pt_squared=diff_pt**2#which will then be square-rooted later
    ) %>% 
    dplyr::select(
      year, state_id,
      contains("boot_rep"),
      treatedpost,
      contains("y_tr"),
      contains("y_ct"),
      contains("pt"),contains("boot"),
      everything())
  
  #tibble of gysnth output 
  return(gsynth_out_tib)
}

### Apply function----
#Use function to wrangle each gsynth output
#For completeness, add overall here as well, even though it's done above.

gsynth_out_tib_overall_authors=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_overall_authors
  ) %>% 
  #add an indicator to keep track of subgroup
  mutate(subgroup="overall") %>% 
  #add the state-year proportions for weighting effect estimates
  left_join(state_wts_by_treated_year_overall,by=c("year","state_id")) %>% 
  left_join(state_year_wts_overall,by=c("year","state_id")) 

#gsynth_out_tib_overall_authors %>% View()

gsynth_out_tib_black_authors=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_black_authors
) %>% 
  #add an indicator to keep track of subgroup
  mutate(subgroup="black") %>% 
  #add the state-year proportions for weighting effect estimates
  left_join(state_wts_by_treated_year_race_b,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_b,by=c("year","state_id")) 


gsynth_out_tib_hispanic_authors=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_hispanic_authors
) %>% 
  mutate(subgroup="hispanic") %>% 
  left_join(state_wts_by_treated_year_race_h,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_h,by=c("year","state_id")) 

gsynth_out_tib_hispanic_authors

gsynth_out_tib_white_authors=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_white_authors
  ) %>% 
  mutate(subgroup="white") %>% 
  left_join(state_wts_by_treated_year_race_w,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_w,by=c("year","state_id")) 

gsynth_out_tib_white_authors

gsynth_out_tib_men_authors=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_men_authors
) %>% 
  mutate(subgroup="men") %>% 
  left_join(state_wts_by_treated_year_men,by=c("year","state_id")) %>% 
  left_join(state_year_wts_men,by=c("year","state_id")) 

gsynth_out_tib_men_authors

gsynth_out_tib_women_authors=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_women_authors) %>% 
  mutate(subgroup="women")%>% 
  left_join(state_wts_by_treated_year_wom,by=c("year","state_id")) %>% 
  left_join(state_year_wts_wom,by=c("year","state_id")) 

gsynth_out_tib_women_authors



### Summarize results by subgroup------
#Stack sub-group datasets together and summarize
gsynth_out_tib_all_groups_authors=gsynth_out_tib_overall_authors %>% 
  bind_rows(
    gsynth_out_tib_black_authors,
    gsynth_out_tib_hispanic_authors,
    gsynth_out_tib_white_authors,
    gsynth_out_tib_men_authors,
    gsynth_out_tib_women_authors
  ) %>% 
  mutate(method="authors")  #keep track of this


#gsynth_out_tib_all_groups_authors %>% View()

#Sort the subgroups for easy copy/paste to match Nianogo order
subgroup_order=gsynth_out_tib_all_groups_authors %>% 
  distinct(subgroup) %>% 
  mutate(subgroup_order=case_when(
    subgroup=="overall"~1,
    subgroup=="white"~2,
    subgroup=="black"~3,
    subgroup=="hispanic"~4,
    subgroup=="men"~5,
    subgroup=="women"~6
  ))

subgroup_order
results_all_groups_authors=gsynth_out_tib_all_groups_authors %>% 
  group_by(treatedpost,subgroup,boot_rep) %>% #group by subgroup as well here
  summarise_by_boot() %>% 
  group_by(treatedpost,subgroup) %>% #summarize by subgroup and treatment status
  summarise_over_boot() %>% 
  left_join(subgroup_order,by="subgroup") %>% 
  arrange(subgroup_order) %>% 
  mutate(method="authors",#keep track for later summary
        method_order=1)


#point estimates are identical to main paper, 
#but confidence intervals are a little different, presumably
#because the main paper uses a normal approximation (1.96*SE), and I'm 
#using the empirical distribution of the bootstrap replicates here.

#results_all_groups_authors %>% View()
results_all_groups_authors %>% 
  filter(treatedpost==1) %>% #  View()





# Alternative gsynth models-------
## Alternative covariates (Scenario 2)-----

### overall--------
#Alternative covariates for political environment defined here:
#scripts/read-wrangle-data-nianogo-et-al.R

n_distinct(overall_covars_alt$year)
n_distinct(overall_covars_alt$state_id)
nrow(overall_covars_alt)
summary(overall_covars_alt$prop_race_non_w_bot_45_64)
summary(overall_covars_alt$prop_bot_45_64_pov)
summary(overall_covars_alt$male_overall)
#naming convention: the models and output will be called sub_pol for sub politics
gsynth_out_overall_sub_pol=gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_overall + #use BRFSS b/c identical w/ ACSs
    low_educ_overall + 
    married_overall+ #married, overall, 15+
    employed_for_wages_overall + #brfss question. use BRFSS
    low_income_overall + 
    male_overall + 
    race_nonwhite_overall+
    vote_share_dem #popular vote data
  ,
  data = overall_covars_alt,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_overall_sub_pol$est.avg
gsynth_out_overall_sub_pol$est.beta
plot(gsynth_out_overall_sub_pol, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### black--------

names(black_complete)

names(black_complete_covars_alt)
table(black_complete_covars_alt$vote_share_dem_cat_6)
summary(black_complete_covars_alt$prop_men_45_64_race_b)
summary(black_complete_covars_alt$prop_men_45_64_race_b_pov)
summary(black_complete_covars_alt$prop_bot_45_64_race_b_pov)
summary(black_complete_covars_alt$prop_bot_45_64_married_now)
summary(black_complete_covars_alt$prop_bot_15_plus_race_b_married_now)
summary(black_complete_covars_alt$prop_men_25_plus_race_b_educ_lt_hs)
summary(black_complete_covars_alt$prop_men_25_64_race_b_unemployed)
black_complete_covars_alt %>% dplyr::select(contains("trim"))
gsynth_out_black_sub_pol <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_black +  
    low_educ_trim_black +  
    married_trim_black+  
    employed_for_wages_trim_black +     
    low_income_trim_black +
    male_black+
    #    vote_share_dem #popular vote data
    vote_share_dem_cat_4_4+
    vote_share_dem_cat_4_3+
    vote_share_dem_cat_4_2
  ,
  data = black_complete_covars_alt,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_black_sub_pol$est.avg
gsynth_out_black_sub_pol$est.beta
plot(gsynth_out_black_sub_pol, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


setwd(here("data","data-processed"))#save for use elsewhere
save(gsynth_out_black_sub_pol,file="gsynth_out_black_sub_pol.RData")

### hispanic-------
names(hispanic_complete_covars_alt)

hispanic_complete_covars_alt
summary(hispanic_complete_covars_alt$prop_bot_45_64_race_h_pov)
summary(hispanic_complete_covars_alt$prop_men_45_64_race_h)

gsynth_out_hispanic_sub_pol <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_hispanic +  
    low_educ_trim_hispanic +  
    married_trim_hispanic+  
    employed_for_wages_trim_hispanic +     
    low_income_trim_hispanic +
    male_hispanic+
    vote_share_dem #popular vote data
  ,
  data = hispanic_complete_covars_alt,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_hispanic_sub_pol$est.avg
gsynth_out_hispanic_sub_pol$est.beta
plot(gsynth_out_hispanic_sub_pol, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### white-------
names(white)

white_covars_alt
summary(white_covars_alt$prop_bot_45_64_race_h_pov)
summary(white_covars_alt$prop_men_45_64_race_h)

gsynth_out_white_sub_pol <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_white +  
    low_educ_trim_white +  
    married_trim_white+  
    employed_for_wages_trim_white +     
    low_income_trim_white +
    male_white+
    vote_share_dem #popular vote data
  ,
  data = white_covars_alt,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  #The authors use 200. It might be worth trying to increase number of reps
  #if CPU allows
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_white_sub_pol$est.avg
gsynth_out_white_sub_pol$est.beta
plot(gsynth_out_white_sub_pol, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### men--------
summary(men_covars_alt$prop_men_45_64_pov)
summary(men_covars_alt$prop_race_non_w_men_45_64)
gsynth_out_men_sub_pol <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_men + 
    low_educ_men + 
    married_men+ 
    employed_for_wages_men +  
    low_income_men + 
    race_nonwhite_men+
    vote_share_dem #popular vote data
  ,
  data = men_covars_alt,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_men_sub_pol$est.avg
gsynth_out_men_sub_pol$est.beta
plot(gsynth_out_men_sub_pol, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### women--------
summary(men_covars_alt$prop_wom_45_64_pov)
summary(men_covars_alt$prop_race_non_w_wom_45_64)

gsynth_out_women_sub_pol <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_women + #use BRFSS b/c identical w/ ACSs
    low_educ_women + 
    married_women+ #married, white, 15+
    employed_for_wages_women + 
    low_income_overall + 
    male_overall + 
    race_nonwhite_overall+
    vote_share_dem #popular vote data
  ,
  data = women_covars_alt,
  EM = F, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  #The authors use 200. It might be worth trying to increase number of reps
  #if CPU allows
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_women_sub_pol$est.avg
gsynth_out_women_sub_pol$est.beta
plot(gsynth_out_women_sub_pol, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### Wrangle output of each-----

gsynth_out_tib_overall_sub_pol=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_overall_sub_pol
) %>% 
  #add an indicator to keep track of subgroup
  mutate(subgroup="overall") %>% 
  #add the state-year proportions for weighting effect estimates
  left_join(state_wts_by_treated_year_overall,by=c("year","state_id")) %>% 
  left_join(state_year_wts_overall,by=c("year","state_id")) 

gsynth_out_tib_overall_sub_pol

gsynth_out_tib_black_sub_pol=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_black_sub_pol
) %>% 
  mutate(subgroup="black") %>% 
  left_join(state_wts_by_treated_year_race_b,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_b,by=c("year","state_id")) 

gsynth_out_tib_black_sub_pol
summary(gsynth_out_tib_black_sub_pol$diff_boot)
gsynth_out_tib_hispanic_sub_pol=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_hispanic_sub_pol
) %>% 
  mutate(subgroup="hispanic") %>% 
  left_join(state_wts_by_treated_year_race_h,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_h,by=c("year","state_id")) 

gsynth_out_tib_hispanic_sub_pol
summary(gsynth_out_tib_hispanic_sub_pol$diff_boot)
summary(gsynth_out_tib_hispanic_sub_pol$y_ct_boot)#some are negative; cite Bonander
#save this for use in RMarkdown to support claim of some negative predicted
#counterfactual outcomes
setwd(here("data","data-processed"))
save(gsynth_out_tib_hispanic_sub_pol,file="gsynth_out_tib_hispanic_sub_pol.RData")


gsynth_out_tib_white_sub_pol=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_white_sub_pol
  ) %>% 
  mutate(subgroup="white") %>% 
  left_join(state_wts_by_treated_year_race_w,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_w,by=c("year","state_id")) 
        
gsynth_out_tib_white_sub_pol

gsynth_out_tib_men_sub_pol=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_men_sub_pol
  ) %>% 
  mutate(subgroup="men") %>% 
  left_join(state_wts_by_treated_year_men,by=c("year","state_id")) %>% 
  left_join(state_year_wts_men,by=c("year","state_id")) 

gsynth_out_tib_men_sub_pol

gsynth_out_tib_women_sub_pol=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_women_sub_pol) %>% 
  mutate(subgroup="women") %>% 
  left_join(state_wts_by_treated_year_wom,by=c("year","state_id")) %>% 
  left_join(state_year_wts_wom,by=c("year","state_id")) 



gsynth_out_tib_all_groups_sub_pol=gsynth_out_tib_overall_sub_pol %>% 
  bind_rows(
    gsynth_out_tib_black_sub_pol,
    gsynth_out_tib_hispanic_sub_pol,
    gsynth_out_tib_white_sub_pol,
    gsynth_out_tib_men_sub_pol,
    gsynth_out_tib_women_sub_pol
  ) %>% 
  mutate(method="sub_pol")



gsynth_out_tib_all_groups_sub_pol %>% 
  filter(subgroup=="hispanic") %>% 
  group_by(treatedpost) %>% 
  summarise(y_ct_boot_min=min(y_ct_boot,na.rm=T))

summary(gsynth_out_tib_all_groups_sub_pol$y_ct_boot)
summary(gsynth_out_tib_all_groups_sub_pol$y_ct_boot)



results_all_groups_sub_pol=gsynth_out_tib_all_groups_sub_pol %>% 
  group_by(treatedpost,subgroup,boot_rep) %>% #group by subgroup as well here
  summarise_by_boot() %>% 
  group_by(treatedpost,subgroup) %>% #summarize by subgroup and treatment status
  summarise_over_boot() %>% 
  left_join(subgroup_order,by="subgroup") %>% 
  arrange(subgroup_order) %>% 
  mutate(method="sub_pol",#keep track for later summary
         method_order=2)

# results_all_groups_sub_pol %>% 
#   filter(treatedpost==1) %>% 
#   View()

results_all_groups_sub_pol %>% 
  filter(treatedpost==0) %>% 
  View()


## EM method (Scenario 3)------

#https://yiqingxu.org/packages/gsynth/articles/tutorial.html#em-method
#From that page:
#The EM algorithm proposed by Gobillon and Magnac (2016) takes advantage 
#of the treatment group information in the pre-treatment period. 
#We implement this method. The estimation takes more time, 
#but the results are very similar to that from the original method – 
#the coefficients will be slightly more precisely estimated.

#Use same covars as scenario 2 above


### overall--------
#As decided here, using the following alternative covariates
#gsynth-alt-models-scratch.R
n_distinct(overall_covars_alt$year)
n_distinct(overall_covars_alt$state_id)
nrow(overall_covars_alt)
summary(overall_covars_alt$prop_race_non_w_bot_45_64)
summary(overall_covars_alt$prop_bot_45_64_pov)
summary(overall_covars_alt$male_overall)
#naming convention: the models and output will be called sub_pol for sub politics
gsynth_out_overall_sub_pol_em=gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_overall + 
    low_educ_overall + 
    married_overall+ 
    employed_for_wages_overall + 
    low_income_overall + 
    male_overall + 
    race_nonwhite_overall+
    vote_share_dem #popular vote data
  ,
  data = overall_covars_alt,
  EM = T, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_overall_sub_pol_em$est.avg
gsynth_out_overall_sub_pol_em$est.beta
plot(gsynth_out_overall_sub_pol_em, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### black--------

names(black_complete)
names(black_complete_covars_alt)
table(black_complete_covars_alt$vote_share_dem_cat_6)
summary(black_complete_covars_alt$prop_men_45_64_race_b)
summary(black_complete_covars_alt$prop_men_45_64_race_b_pov)
summary(black_complete_covars_alt$prop_bot_45_64_race_b_pov)
summary(black_complete_covars_alt$prop_bot_45_64_married_now)
summary(black_complete_covars_alt$prop_bot_15_plus_race_b_married_now)
summary(black_complete_covars_alt$prop_men_25_plus_race_b_educ_lt_hs)
summary(black_complete_covars_alt$prop_men_25_64_race_b_unemployed)
black_complete_covars_alt %>% dplyr::select(contains("trim"))
gsynth_out_black_sub_pol_em <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_black +  
    low_educ_trim_black +  
    married_trim_black+  
    employed_for_wages_trim_black +     
    low_income_trim_black +
    male_black+
    vote_share_dem #popular vote data
  ,
  data = black_complete_covars_alt,
  EM = T, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_black_sub_pol_em$est.avg
gsynth_out_black_sub_pol_em$est.beta
plot(gsynth_out_black_sub_pol_em, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))



### hispanic-------
names(hispanic_complete_covars_alt)

hispanic_complete_covars_alt
summary(hispanic_complete_covars_alt$prop_bot_45_64_race_h_pov)
summary(hispanic_complete_covars_alt$prop_men_45_64_race_h)

gsynth_out_hispanic_sub_pol_em <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_hispanic +  
    low_educ_trim_hispanic +  
    married_trim_hispanic+  
    employed_for_wages_trim_hispanic +     
    low_income_trim_hispanic +
    male_hispanic+
    vote_share_dem #popular vote data
  ,
  data = hispanic_complete_covars_alt,
  EM = T, 
  index = c("state_id","year"),
  inference = "parametric", 
  #  inference="jackknife",
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  #The authors use 200. It might be worth trying to increase number of reps
  #if CPU allows
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_hispanic_sub_pol_em$est.avg
gsynth_out_hispanic_sub_pol_em$est.beta
plot(gsynth_out_hispanic_sub_pol_em, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### white-------
names(white)

white_covars_alt
summary(white_covars_alt$prop_bot_45_64_race_h_pov)
summary(white_covars_alt$prop_men_45_64_race_h)

gsynth_out_white_sub_pol_em <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_white +  
    low_educ_trim_white +  
    married_trim_white+  
    employed_for_wages_trim_white +     
    low_income_trim_white +
    male_white+
    vote_share_dem #popular vote data
  ,
  data = white_covars_alt,
  EM = T, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  #The authors use 200. It might be worth trying to increase number of reps
  #if CPU allows
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_white_sub_pol_em$est.avg
gsynth_out_white_sub_pol_em$est.beta
plot(gsynth_out_white_sub_pol_em, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### men--------
summary(men_covars_alt$prop_men_45_64_pov)
summary(men_covars_alt$prop_race_non_w_men_45_64)
gsynth_out_men_sub_pol_em <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_men + 
    low_educ_men + 
    married_men+ 
    employed_for_wages_men +  
    low_income_men + 
    race_nonwhite_men+
    vote_share_dem #popular vote data
  ,
  data = men_covars_alt,
  EM = T, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_men_sub_pol_em$est.avg
gsynth_out_men_sub_pol_em$est.beta
plot(gsynth_out_men_sub_pol_em, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### women--------
summary(men_covars_alt$prop_wom_45_64_pov)
summary(men_covars_alt$prop_race_non_w_wom_45_64)

gsynth_out_women_sub_pol_em <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_women + #use BRFSS b/c identical w/ ACSs
    low_educ_women + 
    married_women+ #married, white, 15+
    employed_for_wages_women + 
    low_income_overall + 
    male_overall + 
    race_nonwhite_overall+
    vote_share_dem #popular vote data
  ,
  data = women_covars_alt,
  EM = T, 
  index = c("state_id","year"),
  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #same seed as authors
  #The authors use 200. It might be worth trying to increase number of reps
  #if CPU allows
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

gsynth_out_women_sub_pol_em$est.avg
gsynth_out_women_sub_pol_em$est.beta
plot(gsynth_out_women_sub_pol_em, type = "counterfactual", raw = "none", 
     theme.bw = TRUE, main = "",#ylim = c(220, 475),  
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### Wrangle output of each-----
gsynth_out_tib_overall_sub_pol_em=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_overall_sub_pol_em
  ) %>% 
  mutate(subgroup="overall") %>% 
  left_join(state_wts_by_treated_year_overall,by=c("year","state_id")) %>% 
  left_join(state_year_wts_overall,by=c("year","state_id")) 

gsynth_out_tib_overall_sub_pol_em

gsynth_out_tib_black_sub_pol_em=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_black_sub_pol_em
  ) %>% 
  mutate(subgroup="black") %>% 
  left_join(state_wts_by_treated_year_race_b,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_b,by=c("year","state_id")) 

gsynth_out_tib_black_sub_pol_em
summary(gsynth_out_tib_black_sub_pol_em$diff_boot)

gsynth_out_tib_hispanic_sub_pol_em=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_hispanic_sub_pol_em
  ) %>% 
  mutate(subgroup="hispanic") %>% 
  left_join(state_wts_by_treated_year_race_h,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_h,by=c("year","state_id")) 

gsynth_out_tib_hispanic_sub_pol_em
summary(gsynth_out_tib_hispanic_sub_pol_em$diff_boot)
summary(gsynth_out_tib_hispanic_sub_pol_em$y_ct_boot)



gsynth_out_tib_white_sub_pol_em=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_white_sub_pol_em
  ) %>% 
  mutate(subgroup="white") %>% 
  left_join(state_wts_by_treated_year_race_w,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_w,by=c("year","state_id")) 

gsynth_out_tib_white_sub_pol_em

gsynth_out_tib_men_sub_pol_em=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_men_sub_pol_em
  ) %>% 
  mutate(subgroup="men") %>% 
  left_join(state_wts_by_treated_year_men,by=c("year","state_id")) %>% 
  left_join(state_year_wts_men,by=c("year","state_id")) 

gsynth_out_tib_men_sub_pol_em

gsynth_out_tib_women_sub_pol_em=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_women_sub_pol_em) %>% 
  mutate(subgroup="women") %>% 
  left_join(state_wts_by_treated_year_wom,by=c("year","state_id")) %>% 
  left_join(state_year_wts_wom,by=c("year","state_id")) 


gsynth_out_tib_all_groups_sub_pol_em=gsynth_out_tib_overall_sub_pol_em %>% 
  bind_rows(
    gsynth_out_tib_black_sub_pol_em,
    gsynth_out_tib_hispanic_sub_pol_em,
    gsynth_out_tib_white_sub_pol_em,
    gsynth_out_tib_men_sub_pol_em,
    gsynth_out_tib_women_sub_pol_em
  ) %>% 
  mutate(method="sub_pol_em")

gsynth_out_tib_all_groups_sub_pol_em


results_all_groups_sub_pol_em=gsynth_out_tib_all_groups_sub_pol_em %>% 
  group_by(treatedpost,subgroup,boot_rep) %>% #group by subgroup as well here
  summarise_by_boot() %>% 
  group_by(treatedpost,subgroup) %>% #summarize by subgroup and treatment status
  summarise_over_boot() %>% 
  left_join(subgroup_order,by="subgroup") %>% 
  arrange(subgroup_order) %>% 
  mutate(method="sub_pol_em",#keep track for later summary
         method_order=4)

results_all_groups_sub_pol_em %>% 
  filter(treatedpost==1) %>% 
  View()

results_all_groups_sub_pol_em %>% 
  filter(treatedpost==0) %>% 
  View()

## Athey MC-NNM method (Scenario 4)-------
### Overall-------
gsynth_out_overall_sub_pol_mc_nnm=gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_overall + #use BRFSS b/c identical w/ ACSs
    low_educ_overall + 
    married_overall+ #married, overall, 15+
    employed_for_wages_overall + #brfss question. use BRFSS
    vote_share_dem+#popular vote data
    low_income_overall + 
    male_overall + 
    race_nonwhite_overall
  ,
  data = overall_covars_alt,
  estimator = "mc",
  #      EM = F, 
  index = c("state_id","year"),
  #      inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #arbitrary seed so same results
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)

#save for use in RMarkdown
setwd(here("data","data-processed"))
save(gsynth_out_overall_sub_pol_mc_nnm,
     file="gsynth_out_overall_sub_pol_mc_nnm.RData")

gsynth_out_overall_sub_pol_mc_nnm$est.avg
gsynth_out_overall_sub_pol_mc_nnm$est.beta
plot(gsynth_out_overall_sub_pol_mc_nnm, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### black--------
gsynth_out_black_sub_pol_mc_nnm <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_black + #use BRFSS b/c identical w/ ACSs
    low_educ_black + 
    married_black+ #married, black, 15+
    employed_for_wages_black + #brfss question. use BRFSS
    vote_share_dem+#popular vote data - continuous
    low_income_black + 
    male_black
  ,
  data = black_complete_covars_alt,
  estimator = "mc",
  index = c("state_id","year"),
  #  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #arbitrary seed so same results
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_black_sub_pol_mc_nnm$est.avg
gsynth_out_black_sub_pol_mc_nnm$est.beta
plot(gsynth_out_black_sub_pol_mc_nnm, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### hispanic--------
gsynth_out_hispanic_sub_pol_mc_nnm <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_hispanic + 
    low_educ_hispanic + 
    married_hispanic+ 
    employed_for_wages_hispanic + 
    low_income_hispanic + 
    male_hispanic+
    vote_share_dem #popular vote data - continuous
  ,
  data = hispanic_complete_covars_alt,
  estimator = "mc",
  index = c("state_id","year"),
  #  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #arbitrary seed so same results
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_hispanic_sub_pol_mc_nnm$est.avg
gsynth_out_hispanic_sub_pol_mc_nnm$est.beta
plot(gsynth_out_hispanic_sub_pol_mc_nnm, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### white--------
gsynth_out_white_sub_pol_mc_nnm <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_white + #use BRFSS b/c identical w/ ACSs
    low_educ_white + 
    married_white+ #married, white, 15+
    employed_for_wages_white + #brfss question. use BRFSS
    low_income_white + 
    male_white+
    vote_share_dem#popular vote data - continuous
  ,
  data = white_covars_alt,
  estimator = "mc",
  index = c("state_id","year"),
  #  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #arbitrary seed so same results
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_white_sub_pol_mc_nnm$est.avg
gsynth_out_white_sub_pol_mc_nnm$est.beta
plot(gsynth_out_white_sub_pol_mc_nnm, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### men--------
gsynth_out_men_sub_pol_mc_nnm <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_men + 
    low_educ_men + 
    married_men+ 
    employed_for_wages_men + 
    low_income_men + 
    race_nonwhite_men+
    vote_share_dem #popular vote data
  ,
  data = men_covars_alt,
  estimator = "mc",
  index = c("state_id","year"),
  #  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #arbitrary seed so same results
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_men_sub_pol_mc_nnm$est.avg
gsynth_out_men_sub_pol_mc_nnm$est.beta
plot(gsynth_out_men_sub_pol_mc_nnm, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))

### women--------
gsynth_out_women_sub_pol_mc_nnm <- gsynth(
  cvd_death_rate ~ treatedpost + 
    primarycare_rate +
    cardio_rate + 
    population_women + 
    low_educ_women + 
    married_women+ 
    employed_for_wages_women + 
    low_income_women + 
    race_nonwhite_women+
    vote_share_dem #popular vote data
  ,
  data = women_covars_alt,
  estimator = "mc",
  index = c("state_id","year"),
  #  inference = "parametric", 
  se = TRUE, 
  r = c(0, 5), 
  seed = 123, #arbitrary seed so same results
  nboots = nboots_val, #number of bootstrap reps
  CV = TRUE, #perform a cross-validation procedure
  force = "two-way", 
  parallel = TRUE
)
gsynth_out_women_sub_pol_mc_nnm$est.avg
gsynth_out_women_sub_pol_mc_nnm$est.beta
plot(gsynth_out_women_sub_pol_mc_nnm, type = "counterfactual", 
     raw="none",
     theme.bw = TRUE, main = "", 
     #     ylim = c(120, 200), 
     legendOff = TRUE, xlab = "", ylab = "") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5),
        plot.margin = margin(10, 10, 50, 10))


### Wrangle output of each-----

gsynth_out_tib_overall_sub_pol_mc_nnm=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_overall_sub_pol_mc_nnm) %>% 
  mutate(subgroup="overall")%>% #keep track of subgroup
  left_join(state_wts_by_treated_year_overall,by=c("year","state_id")) %>% 
  left_join(state_year_wts_overall,by=c("year","state_id")) 

gsynth_out_tib_overall_sub_pol_mc_nnm

gsynth_out_tib_black_sub_pol_mc_nnm=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_black_sub_pol_mc_nnm) %>% 
  mutate(subgroup="black") %>% 
  left_join(state_wts_by_treated_year_race_b,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_b,by=c("year","state_id")) 

gsynth_out_tib_black_sub_pol_mc_nnm

gsynth_out_tib_hispanic_sub_pol_mc_nnm=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_hispanic_sub_pol_mc_nnm) %>% 
  mutate(subgroup="hispanic")%>% 
  left_join(state_wts_by_treated_year_race_h,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_h,by=c("year","state_id")) 

gsynth_out_tib_hispanic_sub_pol_mc_nnm

gsynth_out_tib_white_sub_pol_mc_nnm=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_white_sub_pol_mc_nnm
) %>% 
  mutate(subgroup="white")%>% 
  left_join(state_wts_by_treated_year_race_w,by=c("year","state_id")) %>% 
  left_join(state_year_wts_race_w,by=c("year","state_id")) 

gsynth_out_tib_white_sub_pol_mc_nnm

gsynth_out_tib_men_sub_pol_mc_nnm=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_men_sub_pol_mc_nnm
  ) %>% 
  mutate(subgroup="men")%>% 
  left_join(state_wts_by_treated_year_men,by=c("year","state_id")) %>% 
  left_join(state_year_wts_men,by=c("year","state_id")) 

gsynth_out_tib_men_sub_pol_mc_nnm

gsynth_out_tib_women_sub_pol_mc_nnm=wrangle_gsynth_output(
  gsynth_obj_val=gsynth_out_women_sub_pol_mc_nnm) %>% 
  mutate(subgroup="women")%>% 
  left_join(state_wts_by_treated_year_wom,by=c("year","state_id")) %>% 
  left_join(state_year_wts_wom,by=c("year","state_id")) 

gsynth_out_tib_women_sub_pol_mc_nnm

gsynth_out_tib_all_groups_sub_pol_mc_nnm=gsynth_out_tib_overall_sub_pol_mc_nnm %>% 
  bind_rows(
    gsynth_out_tib_black_sub_pol_mc_nnm,
    gsynth_out_tib_hispanic_sub_pol_mc_nnm,
    gsynth_out_tib_white_sub_pol_mc_nnm,
    gsynth_out_tib_men_sub_pol_mc_nnm,
    gsynth_out_tib_women_sub_pol_mc_nnm
  ) %>% 
  mutate(method="sub_pol_mc_nnm")

gsynth_out_tib_all_groups_sub_pol_mc_nnm

results_all_groups_sub_pol_mc_nnm=gsynth_out_tib_all_groups_sub_pol_mc_nnm %>% 
  group_by(treatedpost,subgroup,boot_rep) %>% #group by subgroup as well here
  summarise_by_boot() %>% 
  group_by(treatedpost,subgroup) %>% #summarize by subgroup and treatment status
  summarise_over_boot()  %>% 
  left_join(subgroup_order,by="subgroup") %>% 
  arrange(subgroup_order) %>% 
  mutate(method="sub_pol_mc_nnm",#keep track for later summary
         method_order=5)

results_all_groups_sub_pol_mc_nnm %>% 
  filter(treatedpost==1) %>% 
  View()


names(gsynth_out_tib_all_groups_sub_pol_mc_nnm)


# Compare effects between subgroups------
## Between subgroup heterogeneity-----

#To calculate a confidence interval around the difference in mean difference,
#calculate a difference in mean difference in each replicate, 
#and then find the 2.5th and 97.5th percentile
#over replicates

names(gsynth_out_tib_all_groups_authors)
nrow(gsynth_out_tib_all_groups_authors)
#Steps: 
#1. find the mean difference in each bootstrap replication for each subgroup for each method
#2. For each boot rep, find difference in mean difference between relevant subgroups
#3. Summarize over boot reps.
exp(mean(log(c(.5,2))))#mean is 1, as it should be if .5 and 2 are ratios

table(gsynth_out_tib_all_groups_sub_pol$method)
nrow(gsynth_out_tib_all_groups_sub_pol)

#I use this syntax twice, so make a function out of it
mutate_diff_diff_ratio_ratio=function(df){
  df %>% 
    mutate(
    #Now can calculate difference in differences and ratio of
    #ratios for each bootstrap replicate
    #Note these are means over time and over state. 
    diff_diff_pt_mean=diff_pt_mean-diff_pt_mean_ref,
    diff_diff_boot_mean=diff_boot_mean-diff_boot_mean_ref,
    
    #and weighted results here as well
    diff_diff_pt_mean_wt=diff_pt_mean_wt-diff_pt_mean_wt_ref,
    diff_diff_boot_mean_wt=diff_boot_mean_wt-diff_boot_mean_wt_ref,

    ratio_ratio_pt_mean=ratio_pt_mean/ratio_pt_mean_ref,
    ratio_ratio_boot_mean=ratio_boot_mean/ratio_boot_mean_ref,
    
    ratio_ratio_pt_mean_wt=ratio_pt_mean_wt/ratio_pt_mean_wt_ref,
    ratio_ratio_boot_mean_wt=ratio_boot_mean_wt/ratio_boot_mean_wt_ref,
    
    #Now to summarize the ratios over bootstrap replicates,
    #I need to log them so that I can then calculate geometric mean
    #exp(mean(log))
    ratio_ratio_pt_mean_log=log(ratio_ratio_pt_mean),
    ratio_ratio_boot_mean_log=log(ratio_ratio_boot_mean),
    
    ratio_ratio_pt_mean_wt_log=log(ratio_ratio_pt_mean_wt),
    ratio_ratio_boot_mean_wt_log=log(ratio_ratio_boot_mean_wt)
    )
}

summarise_diff_diff_ratio_ratio=function(df){
  df %>% 
    summarise(
      #difference measures
      diff_diff_mean=mean(diff_diff_pt_mean,na.rm=T),
      diff_diff_ll=quantile(diff_diff_boot_mean,probs=0.025,na.rm=T),
      diff_diff_ul=quantile(diff_diff_boot_mean,probs=0.975,na.rm=T),
      
      #ratios
      #I'll then need to exponentiate these in a subsequent step
      ratio_ratio_mean_log_mean=mean(ratio_ratio_pt_mean_log,na.rm=T),
      ratio_ratio_mean_log_ll=quantile(ratio_ratio_boot_mean_log, probs=0.025, na.rm=TRUE),
      ratio_ratio_mean_log_ul=quantile(ratio_ratio_boot_mean_log, probs=0.975, na.rm=TRUE),
      
      #weighted results for this as well
      #note the "mean" is intentionally on the other side of the "wt" here
      #because it's the mean over weighted means
      diff_diff_wt_mean=mean(diff_diff_pt_mean_wt,na.rm=T),
      diff_diff_wt_ll=quantile(diff_diff_boot_mean_wt,probs=0.025,na.rm=T),
      diff_diff_wt_ul=quantile(diff_diff_boot_mean_wt,probs=0.975,na.rm=T),
      
      ratio_ratio_mean_wt_log_mean=mean(ratio_ratio_pt_mean_wt_log,na.rm=T),
      ratio_ratio_mean_wt_log_ll=quantile(ratio_ratio_boot_mean_wt_log, probs=0.025, na.rm=TRUE),
      ratio_ratio_mean_wt_log_ul=quantile(ratio_ratio_boot_mean_wt_log, probs=0.975, na.rm=TRUE),
      
    ) %>% 
    mutate(
      ratio_ratio_mean=exp(ratio_ratio_mean_log_mean),
      ratio_ratio_ll=exp(ratio_ratio_mean_log_ll),
      ratio_ratio_ul=exp(ratio_ratio_mean_log_ul),
      
      #"wt" intentionally before "mean" here
      ratio_ratio_wt_mean=exp(ratio_ratio_mean_wt_log_mean),
      ratio_ratio_wt_ll=exp(ratio_ratio_mean_wt_log_ll),
      ratio_ratio_wt_ul=exp(ratio_ratio_mean_wt_log_ul),
      
    )  
}

results_compare_subgroups=gsynth_out_tib_all_groups_authors %>% 
  bind_rows(
    gsynth_out_tib_all_groups_sub_pol,
    gsynth_out_tib_all_groups_sub_pol_em,
    gsynth_out_tib_all_groups_sub_pol_mc_nnm
  ) %>% 
  filter(treatedpost==1) %>% #filter to post-treatment values for each subgroup
  group_by(method,#i.e., overall vs alt. covariates, etc.
           subgroup,
           boot_rep) %>% 
  summarise_by_boot() %>%
  ungroup() %>% 
  left_join(subgroup_order,by="subgroup") %>% 
  #arrange by subgroup order but note that it's still grouped by
  #boot rep
  group_by(method,boot_rep) %>% 
  arrange(subgroup_order) %>% 
  mutate(
    #get the referent value for each boot rep
    #it will depend on the group
    diff_pt_mean_ref=case_when(
      subgroup=="black"~lag(diff_pt_mean,n=1),#ref: white
      subgroup=="hispanic"~lag(diff_pt_mean,n=2),#ref: white  
      subgroup=="women"~lag(diff_pt_mean,n=1)#ref: men
      #else missing
    ),
    
    diff_boot_mean_ref=case_when(
      subgroup=="black"~lag(diff_boot_mean,n=1),#ref: white
      subgroup=="hispanic"~lag(diff_boot_mean,n=2),  #ref: white
      subgroup=="women"~lag(diff_boot_mean,n=1)#ref: men
    ),
    
    #weighted versions
    diff_pt_mean_wt_ref=case_when(
      subgroup=="black"~lag(diff_pt_mean_wt,n=1),#ref: white
      subgroup=="hispanic"~lag(diff_pt_mean_wt,n=2),#ref: white  
      subgroup=="women"~lag(diff_pt_mean_wt,n=1)#ref: men
    ),
    
    diff_boot_mean_wt_ref=case_when(
      subgroup=="black"~lag(diff_boot_mean_wt,n=1),#ref: white
      subgroup=="hispanic"~lag(diff_boot_mean_wt,n=2),  #ref: white
      subgroup=="women"~lag(diff_boot_mean_wt,n=1)#ref: men
    ),

    ratio_pt_mean_ref=case_when(
      subgroup=="black"~lag(ratio_pt_mean,n=1),#ref: white
      subgroup=="hispanic"~lag(ratio_pt_mean,n=2),#ref: white  
      subgroup=="women"~lag(ratio_pt_mean,n=1)#ref: men
      #else missing
    ),
    ratio_boot_mean_ref=case_when(
      subgroup=="black"~lag(ratio_boot_mean,n=1),#ref: white
      subgroup=="hispanic"~lag(ratio_boot_mean,n=2),  #ref: white
      subgroup=="women"~lag(ratio_boot_mean,n=1)#ref: men
    ),
    
    #weighted versions - ratio
    ratio_pt_mean_wt_ref=case_when(
      subgroup=="black"~lag(ratio_pt_mean_wt,n=1),#ref: white
      subgroup=="hispanic"~lag(ratio_pt_mean_wt,n=2),#ref: white  
      subgroup=="women"~lag(ratio_pt_mean_wt,n=1)#ref: men
      #else missing
    ),
    ratio_boot_mean_wt_ref=case_when(
      subgroup=="black"~lag(ratio_boot_mean_wt,n=1),#ref: white
      subgroup=="hispanic"~lag(ratio_boot_mean_wt,n=2),  #ref: white
      subgroup=="women"~lag(ratio_boot_mean_wt,n=1)#ref: men
    )

  ) %>% 
  mutate_diff_diff_ratio_ratio() %>% 
  #Cool, now summarize over bootstrap rep
  group_by(method,subgroup) %>% 
  summarise_diff_diff_ratio_ratio() %>% #see above
  ungroup() 

# results_compare_subgroups %>% 
#   left_join(subgroup_order,by="subgroup") %>% 
#   arrange(method,subgroup_order) %>% 
#   View()




## Combine comparison measures in one table-------
names(results_compare_subgroups)

results_comparison_measures=results_compare_subgroups

# Summarize all together for table------
names(results_all_groups_authors)
names(results_all_groups_sub_pol)
names(results_compare_subgroups)
#Nov 28, 2023: renaming this with _post 
table_gsynth_results_post=results_all_groups_authors %>% 
  bind_rows(
    results_all_groups_sub_pol,
    results_all_groups_sub_pol_em,
    results_all_groups_sub_pol_mc_nnm
        
  ) %>% 
  #reshape the pre-treatment MAE so that it's included in both rows
  #this is dependent on being sorted by method,subgroup,treated post
  #so make sure
  arrange(method_order,subgroup_order,treatedpost) %>% 
  mutate(
    diff_pt_abs_mean_pre_tx=case_when(
      treatedpost==1~lag(diff_pt_abs_mean)
  )) %>% 
  filter(treatedpost==1) %>%   #now restrict to treatedpost
  #add the between-group comparison measures
  left_join(results_comparison_measures,by=c("method","subgroup")) %>%
  dplyr::select(method, method_order, starts_with("subgroup"),
                starts_with("n_never_treated"),
                starts_with("diff_pt_abs_mean"),
                treatedpost,
                starts_with("diff_pt"),
                starts_with("ratio_pt"),
                starts_with("diff_diff"),
                starts_with("ratio_ratio")) %>% 
  #remove the squared differences
  dplyr::select(-contains("square"))  %>% 
  mutate(#keep track of number of bootstrap reps as well
    nboots=nboots_val,
    
    #create a variable for including or not in
    #the main text table
    method_include=case_when(
      method=="authors"~1,
      method=="sub_pol"~1,
      method=="sub_pol_em"~1,
      method=="sub_pol_mc_nnm"~1,
      TRUE ~0
    ),
    
    subgroup_title=str_to_title(subgroup),
    #make the subgroup a factor to change the order
    subgroup_fac=fct_reorder(as.factor(subgroup_title),subgroup_order),
    method_labels=case_when(
      method=="authors"~"1: authors",
      method=="sub_pol"~"2: alt. covars",
      method=="sub_pol_em"~"3: GM",
      method=="sub_pol_mc_nnm"~"4: MC-NNM",
    )
    
    )
  
#table_gsynth_results_post %>% View()
table(table_gsynth_results_post$method_include)
table(table_gsynth_results_post$subgroup_fac)
table(table_gsynth_results_post$method_labels)
table(table_gsynth_results_post$subgroup_title)
names(table_gsynth_results_post)
#Save these results so I don't have to run the code every time
setwd(here("data","data-processed"))
save(table_gsynth_results_post,file="table_gsynth_results_post.RData")

table_gsynth_results_post
## Summary plots
load("table_gsynth_results_post.RData")
table_gsynth_results_post %>% 
  filter(method_include==1) %>% 
  ggplot(aes(x=diff_pt_ci_width,y=diff_pt_mean,color=subgroup_fac))+
  geom_point()+
  scale_color_brewer(
    name="Group",
    type="diverging",palette="Dark2")+
  labs(
    x="CI width, difference effect estimate",
    y="Difference effect estimate"
  )+
  facet_grid(rows="method_labels")+
  theme_bw()

table_gsynth_results_post %>% 
  filter(method_include==1) %>% 
  ggplot(aes(x=diff_pt_ci_width,y=diff_pt_abs_mean_pre_tx,color=subgroup))+
  geom_point()+
  facet_grid(rows="method")+
  theme_bw()

names(table_gsynth_results_post)

# examine weighted results
table_gsynth_results_post %>% 
  dplyr::select(method, starts_with("subgroup"), contains("wt")) %>% 
  View()

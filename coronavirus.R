# simulagte Frederik D. Weber, 2020-3-14

if (!require(tidyverse)) {install.packages('tidyverse')}
library(tidyverse)

N = 82000000 # the population of the country
N0 = 10 # the starting population infected people of the country (the first spreaders)
ICU_capacity = 28000 # the countris full ICU limit
COVID_ICU_priority = 0.2 # the number of useable ICU for COVID
Hospitalization_rate = 0.18 # how many need to go to hospital (includes death rate)
Critical_ICU_rate = 0.047 # how many get critical and need ICUs (includes death rate)
death_rate_with_ICU = 0.02 # the rate at which people eventually die if they get infected having ICUs available
death_rate_without_ICU = 0.03 # the rate at which people eventually die if they get infected with no ICUs left for

COVID_ICU_capacity = ICU_capacity * COVID_ICU_priority # the real capacity in ICUs

# delays 
days_to_infectious = 2 # when after infection do people get infectious
days_to_symptoms = 5 # average time to show symptoms
days_to_recovery = days_to_symptoms + 21 # time to full recovery
days_to_hospitalization = days_to_symptoms + 7 # average time to need hospitalization if it is needed
days_in_icu = 7 # how long is the ICU occupied by people on average (includes death rate)
days_to_death = days_to_hospitalization + 7 # when after infection will you die if you die on average
days_infectious = days_to_symptoms + 3 # how long after the infection since infection are you infectious (not considering days_to_infectious)

E_natural = 4 # Exposure to people by infected in natural conditions
p_natural = 0.33 # probability of infection by exposure in natural condition
p_cautious = 0.07 # probability of infection by exposure when people are carful

E_desocialization = 0.6 # Exposure to people by infected with social precaution
E_lockdown = 0.2 # Exposure to people by infected after a lockdown

Time_range = 180

init_reaction_delay = 14 # some time to recognize it also reached the country in  question

df.scenarios.long = data.frame()
df.long.scenarios = list()
reaction_times = c()

nScenario = 80
for (scenario in 1:nScenario) {
  reaction_time = scenario * 1
  reaction_times = c(reaction_times,reaction_time)
  cautious_day = init_reaction_delay # the day after people get more cautious and use more handwashing and distance
  desocialization_day = cautious_day + reaction_time # the day people get cautious to a degree of avoiding contact to unnecessary people
  lockdown_day = desocialization_day + reaction_time # the day of lockdown, you meet the people in your home, the cashier, some people in the waiting line and the police to guide you if you are not on what to medical treatment or system critical work
  
  cases = c()
  infection_rate = c()
  imunization = c()
  deaths = c()
  death_rates = c()
  hospitalized = c()
  dehospitalized = c()
  recovered = c()
  ICUed = c()
  inICU = c()
  
  
  cases[1:Time_range] = 0
  infection_rate[1:Time_range] = 0
  imunization[1:Time_range] = 0
  deaths[1:Time_range] = 0
  death_rates[1:Time_range] = 0
  hospitalized[1:Time_range] = 0
  dehospitalized[1:Time_range] = 0
  recovered[1:Time_range] = 0
  ICUed[1:Time_range] = 0
  inICU[1:Time_range] = 0
  
  
  
  noMoreICU = F
  N_immune = 0
  N_infected = 0
  cases[1] = N0
  for (d in 1:Time_range) {
    
    N_infectable = N - sum(cases[1:d])
    i = N_infectable/N # infestation proportion
    
    if (d>=desocialization_day) {
      E = E_desocialization
    } else if (d>=lockdown_day) {
      E = E_lockdown
    } else {
      E = E_natural
    }
    
    if (d>=cautious_day) {
      p = p_cautious
    } else {
      p = p_natural
    }
    
    infectiousness = E*i*p
    
    if (d>days_to_infectious) {
      currently_infectious_cases = cases[max(1,d-days_to_recovery+days_to_infectious):(d-days_to_infectious)]
      currently_infectious = sum(currently_infectious_cases)
      
      currently_infectious_cases_hospitalized = currently_infectious_cases[0:max(0,(length(currently_infectious_cases)-days_to_hospitalization))]*Hospitalization_rate
      currently_potentially_infectious_but_hospitalized = sum(currently_infectious_cases_hospitalized)
      currently_infectious_cases_died = currently_infectious_cases[0:max(0,(length(currently_infectious_cases)-days_to_death))]*death_rate
      currently_potentially_infectious_but_died = sum(currently_infectious_cases_died)
      
      #cases[d] =  (currently_infectious - currently_potentially_infectious_but_hospitalized - currently_potentially_infectious_but_died) * infectiousness
      cases[d] =  currently_infectious * infectiousness
      
      if (cases[d] < 1){
        cases[d] = 0
      }
    }
    
    #Nd_next = N_infected*infectiousness
    #Nd_next = infectiousness^d * N0
    
    infection_rate[d] = infectiousness
    
    if (d>days_infectious) {
      N_immune = N_immune + cases[d-days_infectious]
    }
    
    if (d>days_to_death) {
      
      
      if (d>1 & inICU[d-days_to_death] > COVID_ICU_capacity) {
        death_rate = death_rate_without_ICU
      } else {
        death_rate = death_rate_with_ICU
      }
      death_rates[d] = death_rate
      deaths[d] = cases[d-days_to_death]*death_rate
      
    }
    
    if (d>days_to_hospitalization) {
      hospitalized[d] = cases[d-days_to_hospitalization]*Hospitalization_rate
      ICUed[d] = cases[d-days_to_hospitalization]*Critical_ICU_rate
      inICU[d] = sum(ICUed[(d-days_in_icu):d])
    } else {
      ICUed[d] = 0
    }
    
    if (d>days_to_recovery) {
      recovered[d] = cases[d-days_to_recovery]*(1- death_rates[d-days_to_recovery])
    }
    
    imunization[d] = N_immune
    
    N_infected = sum(cases[0:d])
    
    
  }
  
  
  
  df = data.frame(scenario = scenario, day = 1:Time_range, daily_cases = cases, infection_rate = infection_rate, imunization = imunization, daily_deaths = deaths, died = cumsum(deaths), ICUed = ICUed, in_ICU = inICU)
  df.long = df %>% select(c('scenario', 'day', 'died', 'daily_cases', 'daily_deaths', 'in_ICU')) %>% gather("feature", "value", -scenario, -day)
  df.long.scenarios[[scenario]] = df.long
  
  df.scenarios.long = rbind(df.scenarios.long,df.long)
  
  
}

if (!require(ggplot2)) {install.packages('ggplot2')}
library(ggplot2)

yRange = c(1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000)

#for (scenario in 1:nScenario) {
plot_scenario = function(scenario){
  reaction_time = reaction_times[scenario]
  cautious_day = init_reaction_delay # the day after people get more cautious and use more handwashing and distance
  desocialization_day = cautious_day + reaction_time # the day people get cautious to a degree of avoiding contact to unnecessary people
  lockdown_day = desocialization_day + reaction_time # the day of lockdown, you meet the people in your home, the cashier, some people in the waiting line and the police to guide you if you are not on what to medical treatment or system critical work
  
  final_deaths = floor(max(df.long.scenarios[[scenario]]$value[df.long.scenarios[[scenario]]$feature == 'died']))
  
  max_in_ICU = floor(max(df.long.scenarios[[scenario]]$value[df.long.scenarios[[scenario]]$feature == 'in_ICU']))
  
  fail = ''
  if (max_in_ICU >COVID_ICU_capacity) {
    fail = 'HEALTH CARE FAIL!'
  }
  
  p = ggplot(df.long.scenarios[[scenario]], aes(x = day, y = value, fill = feature, color = feature)) +
    geom_area(stat ="identity", position = "identity", alpha=0.2) +
    geom_line(stat ="identity", position = "identity") +
    
    ggtitle(label = paste0('population: ', as.character(N), ' reaction time: ', as.character(reaction_time), ' days', ', deaths after ', as.character(Time_range), ' days: ', as.character(final_deaths))) + 
    
    xlab("Days since first infectors entered country") + 
    ylab("People") + 
    
    
    geom_hline(yintercept = COVID_ICU_capacity, linetype="dashed", color='blue', size = 1) + 
    annotate("text", x = 1, y = COVID_ICU_capacity+1, label = "ICU capacity for COVID-19", color='blue',vjust = 0, hjust = 0) + 
    
    geom_vline(xintercept = cautious_day, linetype="dashed", color='green') + 
    annotate("text", x = cautious_day+1, y = 10, label = "caution", color='green',hjust = 0) + 
    
    geom_vline(xintercept = desocialization_day, linetype="dashed", color='yellow') + 
    annotate("text", x = desocialization_day+1, y = 100, label = "desocialization", color='yellow',hjust = 0) + 
    
    geom_vline(xintercept = lockdown_day, linetype="dashed", color='red') + 
    annotate("text", x = lockdown_day+1, y = 1000, label = "lockdown", color='red',hjust = 0) + 
    
    theme_classic() + 
    scale_y_continuous(breaks = yRange, labels = as.character(yRange), limits = c(1,1E7), expand = c(0, 0) ,trans='log10') + 
    scale_x_continuous(breaks = seq(0,Time_range,10)) + 
    
    annotate("text", x = 1, y = 1E6, label = fail, color='red', size = 20, vjust = 0, hjust = 0) + 
    
    
    #facet_grid(. ~ scenario) + 
    NULL
  show(p)
  
}

gif <- function() {
  lapply(seq(1,nScenario,1), function(i) {
    plot_scenario(i)
  })
}
if (!require(animation)) {install.packages('animation')}
library(animation)
saveGIF(gif(), interval = .25, ani.width=700, ani.height=400, movie.name="COVID19_Germany.gif")


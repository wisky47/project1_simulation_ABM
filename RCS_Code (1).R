#####
#####Import Data

#Location of the file
file <- "C:/Users/wi1user/Desktop/COVID-19-Fälle Heinsberg.xlsx"

#Install and import readxl
#install.packages("readxl")
library("readxl")

#Read Anzahl COVID-19-Fälle (NUmber of COVID-19 Cases)
rki_cases <- read_excel(file, sheet = 1, range = cell_limits(c(1,2), c(276,2))) 

#Calculate percentage of COVID-19 cases over population of Heinsberg
rki_cases_per <- unlist(rki_cases) / 42236 * 100

rki_cases
plot(unlist(rki_cases))
rki_cases_per
plot(unlist(rki_cases_per))

plot(unlist(rki_cases_per), type="l", col="blue", ylab="Percentage", xlab="Days")


#####
#####Start NetLogo

#install.packages("RNetLogo")
#install.packages("rJava")
Sys.setenv(CLASSPATH="C:/Program Files/NetLogo 6.0.4/app/netlogo-6.0.4.jar") 
Sys.getenv("CLASSPATH")
library("rJava")
library("RNetLogo")
nl.path <- "C:/Program Files/NetLogo 6.0.4/app"
nl.jarname <- "netlogo-6.0.4.jar"
NLStart(nl.path, nl.jarname=nl.jarname, gui=TRUE)
model.path <- "C:/Users/wi1user/Desktop/rcs/SoSAD-COVID-19_detailed_restart_compatible.nlogo"
NLLoadModel(model.path)




#####
#####Simulation Function (Train)

#Create simulation function with parameters, name of parameters, number of simulaiton and real data. 
#Simulation function return MSE (between real data and average of simulation)

covid_simulation_train <- function(param.set, parameter.names,no.repeated.sim, real.data){
  
  covid_cases_sum <- list(rep(0, length(real.data)))
  
  for (i in 1:no.repeated.sim){
    NLCommand("set-scenario-covid-uncontrolled")
    lapply(seq(1:length(parameter.names)), function(x) {NLCommand("set ",parameter.names[x], param.set[x])})
    
    NLCommand("set population-size 1000")
    NLCommand("set ill-people-stay-at-home true")
    NLCommand("set restrict-contacts-after 16")
    NLCommand("set reallow-contacts-after 51")
    NLCommand("set close-schools-after 5")
    NLCommand("set reopen-schools-after 35")
    NLCommand("set school-reopening-rate 95")
    NLCommand("set home-office-after 16")
    NLCommand("set home-office-rate 27")
    NLCommand("set back-to-office-after 270")
    
    NLCommand("setup")
    covid_cases <- NLDoReport(length(real.data), "go",  "count people with [days > 0 or infected?]") 

    covid_cases_sum <- Map("+", unlist(covid_cases_sum), unlist(covid_cases))
  }
  covid_cases_mean <- unlist(covid_cases_sum) / no.repeated.sim
  covid_cases_percentage <- unlist(covid_cases_mean) / NLReport("population-size") * 100
  print(unlist(covid_cases_percentage))
  plot(unlist(covid_cases_percentage)) 
  return (mean((unlist(real.data) - unlist(covid_cases_percentage))^2))
}


#####
#####Simulation Function (Test)

#Create simulation function with parameters, name of parameters, number of simulaiton and real data. 
#Simulation function return MSE (between real data and average of simulation)

covid_simulation_test <- function(param.set, parameter.names,no.repeated.sim, real.data){
  
  covid_cases_sum <- list(rep(0, length(real.data)))
  
  for (i in 1:no.repeated.sim){
    NLCommand("set-scenario-covid-uncontrolled")
    lapply(seq(1:length(parameter.names)), function(x) {NLCommand("set ",parameter.names[x], param.set[x])})
    
    NLCommand("set population-size 1000")
    NLCommand("set ill-people-stay-at-home true")
    NLCommand("set restrict-contacts-after 16")
    NLCommand("set reallow-contacts-after 51")
    NLCommand("set close-schools-after 5")
    NLCommand("set reopen-schools-after 35")
    NLCommand("set school-reopening-rate 95")
    NLCommand("set home-office-after 16")
    NLCommand("set home-office-rate 27")
    NLCommand("set back-to-office-after 270")
    
    NLCommand("setup")
    covid_cases <- NLDoReport(length(real.data), "go",  "count people with [days > 0 or infected?]") 
    
    covid_cases_sum <- Map("+", unlist(covid_cases_sum), unlist(covid_cases))
  }
  covid_cases_mean <- unlist(covid_cases_sum) / no.repeated.sim
  covid_cases_percentage <- unlist(covid_cases_mean) / NLReport("population-size") * 100
  print(unlist(covid_cases_percentage))
  plot(unlist(covid_cases_percentage))
  return(covid_cases_percentage) 
}




#####
##### Control Simulation Functions (Train and Test)

#Control train function 

#Set parameters of the function
parameter.values <- list("work-contacts" = 3,
                         "school-contacts" = 3,
                         "freetime-contacts-children" = 3,
                         "freetime-contacts-adults" = 3,
                         "max-contacts" = 3)
parameter.names <- names(parameter.values)
real.data <- rki_cases_per
no.repeated.sim <- 30

#Test the function
covid_simulation_train(param.set = c(3, 3, 3, 3, 3),
                       parameter.names = parameter.names,
                       no.repeated.sim = no.repeated.sim,
                       real.data = real.data)


#Control test function 

#Set parameters of the function
parameter.values <- list("work-contacts" = 3,
                         "school-contacts" = 3,
                         "freetime-contacts-children" = 3,
                         "freetime-contacts-adults" = 3,
                         "max-contacts" = 3)
parameter.names <- names(parameter.values)
real.data <- rki_cases_per
no.repeated.sim <- 30

#Test the function
sim_results_control <- covid_simulation_test(param.set = c(3, 3, 3, 3, 3),
                                             parameter.names = parameter.names,
                                             no.repeated.sim = no.repeated.sim,
                                             real.data = real.data)

#Test confidence interval
#Standard deviation of the data
std <- sd(sim_results_control)

#Interval for 95% confidence interval
interval <- 1.96 * (std/sqrt(length(sim_results_control)))

#Upper and lower bounds of the data
lower <- sim_results_control - interval
upper <- sim_results_control + interval

#The data and its upper/lower bounds

plot(unlist(upper), type="l", lty=2, ylab="Percentage", xlab="Days (After Train)")
lines(unlist(rki_cases_per), type="l", col="blue")
lines(unlist(lower), type="l", lty=2)

for (i in 1:length(sim_results_control)){
  if(rki_cases_per[i] < lower[i]){
    print("Lower limit exceeded")
    print(i)
  }
  if(rki_cases_per[i] > upper[i]){
    print("Upper limit exceeded")
    print(i)
  }
}
  


#####
#####Calibration with calibrar (with training function)

#Install and import calibrar
#install.packages("calibrar")
library("calibrar")

#Set parameters of the function
parameter.values <- list("work-contacts" = 3,
                         "school-contacts" = 3,
                         "freetime-contacts-children" = 3,
                         "freetime-contacts-adults" = 3,
                         "max-contacts" = 3)
parameter.names <- names(parameter.values)
real.data <- rki_cases_per[1:49]
no.repeated.sim <- 30

#Calibration process for cmaes
cmaes <- calibrate (par=c(3, 3, 3, 3, 3),
                  fn=covid_simulation_train,
                  parameter.names = parameter.names,
                  no.repeated.sim = no.repeated.sim,
                  real.data = real.data,
                  lower=c(1, 1, 1, 1, 1), upper=c(10, 10, 10, 10, 4),
                  method= "cmaes")

print("The values of the parameters for cmaes method")
print(cmaes)

#Calibration process for lbfgsb
lbfgsb <- calibrate (par=c(3, 3, 3, 3, 3),
                    fn=covid_simulation_train,
                    parameter.names = parameter.names,
                    no.repeated.sim = no.repeated.sim,
                    real.data = real.data,
                    lower=c(1, 1, 1, 1, 1), upper=c(10, 10, 10, 10, 4),
                    method= "L-BFGS-B")

print("The values of the parameters for lbfgsb method")
print(lbfgsb)

#Calibration process for ahr
ahr <- calibrate (par=c(3, 3, 3, 3, 3),
                  fn=covid_simulation_train,
                  parameter.names = parameter.names,
                  no.repeated.sim = no.repeated.sim,
                  real.data = real.data,
                  lower=c(1, 1, 1, 1, 1), upper=c(10, 10, 10, 10, 4))

print("The values of the parameters for ahr method")
print(ahr)




#####
#####Evaluation and Results

parameter.values <- list("work-contacts" = 4.22,
                         "school-contacts" = 5.70,
                         "freetime-contacts-children" = 2.95,
                         "freetime-contacts-adults" = 3.16,
                         "max-contacts" = 1.58)
parameter.names <- names(parameter.values)
real.data <- rki_cases_per[1:112]
no.repeated.sim <- 30

sim_results <- covid_simulation_test(param.set = c(3.14, 3.12, 1.04, 4.41, 2.7),
                                     parameter.names = parameter.names,
                                     no.repeated.sim = no.repeated.sim,
                                     real.data = real.data)

#####Create Confidence Interval

#Standard deviation of the data
std <- sd(sim_results)

#Interval for 95% confidence interval
interval <- 1.96 * (std/sqrt(length(sim_results)))

#Upper and lower bounds of the data
lower <- sim_results - interval
upper <- sim_results + interval

#The data and its upper/lower bounds

plot(unlist(upper[50:112]), type="l", lty=2, ylab="Percentage", xlab="Days (After First Outlier)")
lines(unlist(rki_cases_per[50:112]), type="l", col="blue")
lines(unlist(lower[50:112]), type="l", lty=2)

for (i in 22:length(sim_results)){
  if(rki_cases_per[i] < lower[i]){
    print("Lower limit exceeded")
    print(i)
  }
  if(rki_cases_per[i] > upper[i]){
    print("Upper limit exceeded")
    print(i)
  }
}

NLQuit()

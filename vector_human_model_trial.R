
# install.packages("tidyverse")
# install.packages("deSolve")
library(tidyverse)
library(deSolve)


#define the model equation

vector_human <- function(t, x, parms) {
  
  with(as.list(c(parms, x)), {
    
       
       #Total populations
       M = Sm+Im
       H = S+I
       
       #Vector equations
       dSm= mu_m*M - beta*I/H*Sm-mu_m*Sm
       dIm= beta*I/H*Sm -mu_m*Im
       
       #Human Equations
       
       dS= -alpha*Im/H*S + gamma*I
       dI= alpha*Im/H*S - gamma*I
       
       output <-c(dSm,dIm,dS,dI)
       list(output)
       
  } )
  
}

#DeSolve package-Initialise the compartments
       
       
       #The initial populations in the compartments
       start <- c(
         Sm=40000,
         Im=3000,
         S=5000,
         I=1000
       )
       
       #DeSolve package:Define parameters to be used
           
           parms <- c(
             mu_m = 1/15,
             alpha = 0.12,
             beta = 0.25,
             gamma = 1/20
             
           )
             
             #The time interval
             
             times <- seq(0, 365)
           
       
       # solve the equations
       
       vector_model <- ode(
         times = times,
         parms = parms,
         func = vector_human,
         y = start
       )
       
#Plot the results
       #make data plotable
       vmplot<-as_tibble(as.data.frame(vector_model)) %>%
         pivot_longer(names_to = "variable", cols = !1)
       
       vmplot %>%
         group_by(variable) %>%
         ggplot()+
         geom_line(aes(x = time, y =value, colour = variable))+
         theme_minimal() +
         labs(title = "vector-human compartments" , y =("population") , colour = "species") +
         facet_wrap( ~variable)
       
       
       ## scenario analysis two:simulate the numbers of students in each compartment over 365 days
       
    ## define the model
       
       sis_model <- function(t,x parms) {
         
         with(as .list(c(parms, x)),
       }
       

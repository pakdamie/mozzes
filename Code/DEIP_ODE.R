

###PURE LINEAR CHAIN TRICK MODEL; if it's too slow I might write it in C 

DEIP_ODE <- function(t, state, param) {
       
        
        S<- state[1]
        E <- state[2:(n_E + 1)] 
        D <- state[(n_E + 2):(n_E+n_D+1)] 
        I <- state[(n_E+n_D +2)] 
        DI <- state[(n_E+n_D+3)] 
        
        
         with(
                as.list(c(state, param)),
                {
                        
        b = param['b'] #birth rate
        beta = param['beta'] # infection rate
        mu_S = param['mu_S'] #natural mortality rate of the susceptible mosquito
        bite = param['bite'] #as of right now let's do a constant
        alpha_E = param['alpha_E'] #the development rate of the single fed exposed class
        mu_E = param['mu_E'] # the background mortality rate of the exposed class
        alpha_I = param['alpha_I'] #the senescent rate 
        n_E = param['n_E'] #the shape parameter for the exposed class
        n_D = param['n_D'] #the shape parameter for the double fed class
        alpha_DI = param['alpha_DI'] #the development rate of the double infected (infected)
        alpha_D = param['alpha_D'] #the development rate of the double infected
        mu_D = param['mu_D'] #the mortality rate of the double fed
        VH = param['VH']
 
 
###Susceptible mosquitos
 dS = b - (beta * S * VH) - (mu_S * S)
 
###Exposed mosquitos class
 
 dE = rep(0, n_E)
 
 dE[1]  <- (beta*S* VH) - (n_E * alpha_E * E[1]) - (bite*E[1]) - (mu_E * E[1])
 
 if (n_E > 1) {
         for (i in 2:n_E) {
                 dE[i] <- n_E * alpha_E * E[i-1] - n_E * alpha_E * E[i] -
                         (bite * E[i]) - (mu_E * E[i])
         }
 }
 

###Double fed exposed mosquito class
 
dD <- rep(0, n_D)
 
dD[1] = (bite * E[1]) - n_D * alpha_D * D[1] - mu_D * D[1]
 
if (n_D > 1) {
        for (i in 2:n_D) {
                dD[i] <- n_D * alpha_D * D[i-1] - n_D * alpha_D * D[i] +
                        (bite * D[i]) - (mu_D * D[i])
        }
}

###Single- fed infected mosquito class

 dI = n_E * alpha_E * E[n_E] - alpha_I * I 
 

###Double-fed infected mosquito class
 dDI = n_D * alpha_D * D[n_D] - alpha_DI * DI
 
    
 return(list(c(dS, dE, dD, dI, dDI)))
                }
        )
}


 
###BAD PARAM VALUES 
parameters_n <- c(
        b = 0.02 , #birth rate
        beta = 1e-3,# infection rate
        mu_S = 1/42 , #mosquito mortality rate  
        bite = 1e-6, #as of right now let's do a constant
        alpha_E = 1/11,  #the development rate of the single fed exposed class
        mu_E = 1/42, # the background mortality rate of the exposed class 
        alpha_I = 1/42, #the senescent rate 
        n_E = 10, #the shape parameter for the exposed class
        n_D = 10, #the shape parameter for the double fed class
        alpha_DI = 1/20, #the development rate of the double infected (infected)
        alpha_D = 1/11 * (2), #the development rate of the double infected
        mu_D = 1/20,
        VH = 50) 

inits_n <- c(S = 850, 
             E = rep(0,parameters_n['n_E']),
             D = rep(0,parameters_n['n_D']),
             I  =0 ,
             DI =0)

times <- seq(0, 30, by = 1)


out_DDE <- data.frame(ode(y = inits_n, times = times, func = DEIP_ODE ,
               parms = parameters_n))



###
susceptible <- out_DDE[,2]
exposed <- rowSums(out_DDE[,(3:(n_E + 2))])
double_fed_exposed <- rowSums(out_DDE[,(n_E+3):(n_E+ n_D+2)])
infected <- out_DDE[,(n_E + n_D +3 )]
double_infected <- out_DDE[,(n_E + n_D +4 )]

par(mfrow=c(3,2))
plot(susceptible, type = 'l', main = 'susceptible')
plot(exposed, type = 'l', main = 'exposed')
plot(double_fed_exposed, type = 'l', main = 'double_fed_Exposed')
plot(infected, type = 'l', main = 'infected')
plot(double_infected, type = 'l', main = 'double_infected')


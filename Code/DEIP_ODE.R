
### Vectorizing in progress 

DEIP_ODE <- function(t,y, params){
        
 b = params['b'] #birth rate
 beta = params['beta'] # infection rate
 mu_S = params['mu_S'] #natural mortality rate of the susceptible mosquitio
 bite = params['bite'] #as of right now let's do a really simple bite rate
 alpha_E = params['alphaE'] #the development rate of the single fed exposed class
 mu_E = params['mu_E'] # the background mortality rate of the exposed class
 mu_I = params['mu_I'] #the background mortality rate of the infectious mosquito
 alpha_I = params['alphaI'] #the senescent rate 
 n_E = parmas['nE'] #the shape parameter for the exposed class
 n_D = params['nD'] #the shape parameter for the double fed class
 n_I = params['nI'] # the shape parameter for the infected class
 n_DI = params['nDI'] #the shape parameter for the double infected class 
 alpha_DI = params['alphaDI'] #the development rate of the double infected (infected)
 alpha_D = params['alphaD'] #the development rate of the double infected
 mu_D = params['muD'] #the mortality rate of the double fed
 mu_DI = params['muDI'] #the mortality rate of the double fed infected
 
 dS = b - beta * S * VH - mu_S * S
 
 dE_0  = (beta * S*VH) - (n_E * alpha*E * E[1]) - (bite*E[1]) - (mu_E * E[1])
 
 dE =  
 for (i in seq(2, n_E)){

         dE[n] = (n_E * alpha_E * E[n-1]) + (n_E * alpha_E * E[n]) - (bite * E[n]) - (mu_E *E[n])
         
 }
 
 dD_0 = bite *E[1] - n_D * alpha_D * D[1] - mu_D * D[1]
 
 for (i in seq(2, n_D)){
         
         dD[n] = (n_D * alpha_D * D[n-1]) + (n_D * alpha_D * D[n]) + (bite * D[n]) - (mu_D *D[n])
 }
 
 dI[1] = n_E * alpha_E * E[n] - n_I * alpha_I * I[1] - mu_I * I[1]
 
 for (k in seq(2, n_D)){
         dI[k] = (n_I * alpha_I * I[n-1]) + (n_I * alpha_I * I[n])  - (mu_I *I[n])
         
         
 }
 
 dDI[1] = n_D * alpha_D * D[n] - n_DI * alpha_DI * DI[1] - mu_DI * DI[1]
 
 for (k in seq(2, n_DI)){
         dDI[k] = (n_DI * alpha_DI * DI[n-1]) + (n_DI * alpha_DI * DI[n])  - (mu_DI *DI[n])
         
 }
 
 }
 
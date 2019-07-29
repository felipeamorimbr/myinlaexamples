library(INLA)
library(brinla)

#Creating 

usair.formula1 <- SO2 ~ negtemp + manuf + wind + precip + days #Model formula

####Testing model using lm function
#Default priors: beta ~ N(0,10^6) , log(tau) ~ loggamma(1,10^-5)
#tau = precission 
usair.lm1 <- lm(usair.formula1, data = usair)
#Residual standard error 15,79

####Testing model using INLA
usair.inla1 <- inla(usair.formula1, data = usair, control.compute = list(dic = TRUE, cpo = TRUE))


###Modelo INLA mudando a priori
usair.inla2 <- inla(usair.formula1, data = usair, control.compute = list(dic = TRUE, cpo = TRUE),
                    control.fixed = list(mean.intercept = 100, prec.intercept = 10^-2, mean = list(
                      negtemp = 2, wind = -3, default = 0), prec = 1), control.family = list(hyper = list(prec = list (prior = "gaussian",
                                                                                                                      param = c(0,1)))))
data(cars)

#Regressao linear
cars.lm <- lm(dist ~ speed, data = cars)
summary(cars.lm)

#INLA
cars.inla <- inla(dist ~ speed, data = cars, control.compute = list(dic = TRUE, cpo = TRUE))
summary(cars.inla)

cars.inla2 <- inla(dist ~ speed, data = cars,
                   control.compute = list(dic = TRUE, cpo = TRUE),
                   control.fixed = list(mean = list(speed = 10), prec = 1),
                   control.family = list (hyper = list(prec = list(prior = "gaussian", param = c(0,1)))))
summary(cars.inla2)

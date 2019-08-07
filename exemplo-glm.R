#Exemplo GLM
require(brinla)
require(INLA)
require(MASS)
#Ver modelos disponiveis
names(inla.models()$likelihood)

#Bernoulli
data("lowbwt", package = "brinla")
lowbwt.glm1 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + HT + UI + FTV, data = lowbwt, family = binomial(link = "logit")) #Default link function
lowbwt.inla1 <- inla(LOW ~ AGE + LWT + RACE + SMOKE + HT + UI + FTV, data = lowbwt, family = "binomial", #default link function
                     Ntrials = 1, control.family = list(link = "logit"),
                     control.compute = list(dic = TRUE, cpo = TRUE))

lowbwt.glm2 <- glm(LOW ~ AGE + LWT + RACE + SMOKE + HT + UI + FTV, data = lowbwt, family = binomial(link = "probit"))
lowbwt.inla2 <- inla(LOW ~ AGE + LWT + RACE + SMOKE + HT + UI + FTV, data = lowbwt, family = "binomial", 
                     Ntrials = 1, control.family = list(link = "probit"),
                     control.compute = list(dic = TRUE, cpo = TRUE))
#Poisson
data("AIDS", package = "brinla")
aids.f <- DEATHS ~ TIME
aids.glm1 <- glm(aids.f, data = AIDS, family = poisson())
aids.inla1 <- inla(formula = aids.f, family = "poisson", data = AIDS, control.compute = list(dic = T, cpo = T))
summary(aids.glm1)
summary(aids.inla1)

#Binomial Negativa
data("crab", package = "brinla")
crabs.f <- SATELLITES ~ COLOR + SPINE + WEIGHT + WIDTH
crabs.glm1 <- glm.nb(crabs.f, data = crab)
crabs.inla1 <- inla(crabs.f , data = crab, family = "nbinomial", control.compute = list(dic = T, cpo =T))
summary(crabs.glm1)
summary(crabs.inla1)

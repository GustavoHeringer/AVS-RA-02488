data <- read.table("data_AVS-RA-02488.txt", h=T)

library(gof)#
library (MuMIn) #

# Shape index
data$SI <- data$mus_perimetro / (2*sqrt(3.14159265*data$mus_tamanho))

# Correlation
cor(data[,c(2:58, 68:83, 85)], method = "spearman") 

# Scale transformation (landscape permeability metrics)
out_l1_r1_1km_sc <- scale(data$out_l1_r1_1km)
out_l2_r1_1km_sc <- scale(data$out_l2_r1_1km)
out_l2_r2_1km_sc <- scale(data$out_l2_r2_1km)
out_l2_r3_1km_sc <- scale(data$out_l2_r3_1km)
out_l2_r4_1km_sc <- scale(data$out_l2_r4_1km)
out_l3_r2_1km_sc <- scale(data$out_l3_r2_1km)
out_l4_r2_1km_sc <- scale(data$out_l4_r2_1km)
out_l5_r3_1km_sc <- scale(data$out_l5_r3_1km)
out_l8_r3_1km_sc <- scale(data$out_l8_r3_1km)
out_l9_r3_1km_sc <- scale(data$out_l9_r3_1km)
out_l5_r4_1km_sc <- scale(data$out_l5_r4_1km)
out_l6_r4_1km_sc <- scale(data$out_l6_r4_1km)
out_l7_r4_1km_sc <- scale(data$out_l7_r4_1km)
out_l8_r8_1km_sc <- scale(data$out_l8_r8_1km)
out_l8_r9_1km_sc <- scale(data$out_l8_r9_1km)
out_l9_r8_1km_sc <- scale(data$out_l9_r8_1km)
out_l9_r9_1km_sc <- scale(data$out_l9_r9_1km)
out_l10_r3_1km_sc <- scale(data$out_l10_r3_1km)
out_l11_r3_1km_sc <- scale(data$out_l11_r3_1km)
out_l1_r1_2km_sc <- scale(data$out_l1_r1_2km)
out_l2_r1_2km_sc <- scale(data$out_l2_r1_2km)
out_l2_r2_2km_sc <- scale(data$out_l2_r2_2km)
out_l2_r3_2km_sc <- scale(data$out_l2_r3_2km)
out_l2_r4_2km_sc <- scale(data$out_l2_r4_2km)
out_l3_r2_2km_sc <- scale(data$out_l3_r2_2km)
out_l4_r2_2km_sc <- scale(data$out_l4_r2_2km)
out_l5_r3_2km_sc <- scale(data$out_l5_r3_2km)
out_l8_r3_2km_sc <- scale(data$out_l8_r3_2km)
out_l9_r3_2km_sc <- scale(data$out_l9_r3_2km)
out_l5_r4_2km_sc <- scale(data$out_l5_r4_2km)
out_l6_r4_2km_sc <- scale(data$out_l6_r4_2km)
out_l7_r4_2km_sc <- scale(data$out_l7_r4_2km)
out_l8_r8_2km_sc <- scale(data$out_l8_r8_2km)
out_l8_r9_2km_sc <- scale(data$out_l8_r9_2km)
out_l9_r8_2km_sc <- scale(data$out_l9_r8_2km)
out_l9_r9_2km_sc <- scale(data$out_l9_r9_2km)
out_l10_r3_2km_sc <- scale(data$out_l10_r3_2km)
out_l11_r3_2km_sc <- scale(data$out_l11_r3_2km)
out_l1_r1_05km_sc <- scale(data$out_l1_r1_05km)
out_l2_r1_05km_sc <- scale(data$out_l2_r1_05km)
out_l2_r2_05km_sc <- scale(data$out_l2_r2_05km)
out_l2_r3_05km_sc <- scale(data$out_l2_r3_05km)
out_l2_r4_05km_sc <- scale(data$out_l2_r4_05km)
out_l3_r2_05km_sc <- scale(data$out_l3_r2_05km)
out_l4_r2_05km_sc <- scale(data$out_l4_r2_05km)
out_l5_r3_05km_sc <- scale(data$out_l5_r3_05km)
out_l8_r3_05km_sc <- scale(data$out_l8_r3_05km)
out_l9_r3_05km_sc <- scale(data$out_l9_r3_05km)
out_l5_r4_05km_sc <- scale(data$out_l5_r4_05km)
out_l6_r4_05km_sc <- scale(data$out_l6_r4_05km)
out_l7_r4_05km_sc <- scale(data$out_l7_r4_05km)
out_l8_r8_05km_sc <- scale(data$out_l8_r8_05km)
out_l8_r9_05km_sc <- scale(data$out_l8_r9_05km)
out_l9_r8_05km_sc <- scale(data$out_l9_r8_05km)
out_l9_r9_05km_sc <- scale(data$out_l9_r9_05km)
out_l10_r3_05km_sc <- scale(data$out_l10_r3_05km)
out_l11_r3_05km_sc <- scale(data$out_l11_r3_05km)

# Models (landscape permeability metrics)
m.c.1.1.05 <- glm(Ac_pres2 ~ out_l1_r1_05km_sc, family="binomial", data=data)
m.c.1.1.1 <- glm(Ac_pres2 ~ out_l1_r1_1km_sc, family="binomial", data=data)
m.c.1.1.2 <- glm(Ac_pres2 ~ out_l1_r1_2km_sc, family="binomial", data=data)
m.c.2.1.05 <- glm(Ac_pres2 ~ out_l2_r1_05km_sc, family="binomial", data=data)
m.c.2.1.1 <- glm(Ac_pres2 ~ out_l2_r1_1km_sc, family="binomial", data=data)
m.c.2.1.2 <- glm(Ac_pres2 ~ out_l2_r1_2km_sc, family="binomial", data=data)
m.c.2.2.05 <- glm(Ac_pres2 ~ out_l2_r2_05km_sc, family="binomial", data=data)
m.c.2.2.1 <- glm(Ac_pres2 ~ out_l2_r2_1km_sc, family="binomial", data=data)
m.c.2.2.2 <- glm(Ac_pres2 ~ out_l2_r2_2km_sc, family="binomial", data=data)
m.c.2.3.05 <- glm(Ac_pres2 ~ out_l2_r3_05km_sc, family="binomial", data=data)
m.c.2.3.1 <- glm(Ac_pres2 ~ out_l2_r3_1km_sc, family="binomial", data=data)
m.c.2.3.2 <- glm(Ac_pres2 ~ out_l2_r3_2km_sc, family="binomial", data=data)
m.c.2.4.05 <- glm(Ac_pres2 ~ out_l2_r4_05km_sc, family="binomial", data=data)
m.c.2.4.1 <- glm(Ac_pres2 ~ out_l2_r4_1km_sc, family="binomial", data=data)
m.c.2.4.2 <- glm(Ac_pres2 ~ out_l2_r4_2km_sc, family="binomial", data=data)
m.c.3.2.05 <- glm(Ac_pres2 ~ out_l3_r2_05km_sc, family="binomial", data=data)
m.c.3.2.1 <- glm(Ac_pres2 ~ out_l3_r2_1km_sc, family="binomial", data=data)
m.c.3.2.2 <- glm(Ac_pres2 ~ out_l3_r2_2km_sc, family="binomial", data=data)
m.c.4.2.05 <- glm(Ac_pres2 ~ out_l4_r2_05km_sc, family="binomial", data=data)
m.c.4.2.1 <- glm(Ac_pres2 ~ out_l4_r2_1km_sc, family="binomial", data=data)
m.c.4.2.2 <- glm(Ac_pres2 ~ out_l4_r2_2km_sc, family="binomial", data=data)
m.c.5.3.05 <- glm(Ac_pres2 ~ out_l5_r3_05km_sc, family="binomial", data=data)
m.c.5.3.1 <- glm(Ac_pres2 ~ out_l5_r3_1km_sc, family="binomial", data=data)
m.c.5.3.2 <- glm(Ac_pres2 ~ out_l5_r3_2km_sc, family="binomial", data=data)
m.c.8.3.05 <- glm(Ac_pres2 ~ out_l8_r3_05km_sc, family="binomial", data=data)
m.c.8.3.1 <- glm(Ac_pres2 ~ out_l8_r3_1km_sc, family="binomial", data=data)
m.c.8.3.2 <- glm(Ac_pres2 ~ out_l8_r3_2km_sc, family="binomial", data=data)
m.c.9.3.05 <- glm(Ac_pres2 ~ out_l9_r3_05km_sc, family="binomial", data=data)
m.c.9.3.1 <- glm(Ac_pres2 ~ out_l9_r3_1km_sc, family="binomial", data=data)
m.c.9.3.2 <- glm(Ac_pres2 ~ out_l9_r3_2km_sc, family="binomial", data=data)
m.c.10.3.05 <- glm(Ac_pres2 ~ out_l10_r3_05km_sc, family="binomial", data=data)
m.c.10.3.1 <- glm(Ac_pres2 ~ out_l10_r3_1km_sc, family="binomial", data=data)
m.c.10.3.2 <- glm(Ac_pres2 ~ out_l10_r3_2km_sc, family="binomial", data=data)
m.c.11.3.05 <- glm(Ac_pres2 ~ out_l11_r3_05km_sc, family="binomial", data=data)
m.c.11.3.1 <- glm(Ac_pres2 ~ out_l11_r3_1km_sc, family="binomial", data=data)
m.c.11.3.2 <- glm(Ac_pres2 ~ out_l11_r3_2km_sc, family="binomial", data=data)
m.c.8.8.05 <- glm(Ac_pres2 ~ out_l8_r8_05km_sc, family="binomial", data=data)
m.c.8.8.1 <- glm(Ac_pres2 ~ out_l8_r8_1km_sc, family="binomial", data=data)
m.c.8.8.2 <- glm(Ac_pres2 ~ out_l8_r8_2km_sc, family="binomial", data=data)
m.c.8.9.05 <- glm(Ac_pres2 ~ out_l8_r9_05km_sc, family="binomial", data=data)
m.c.8.9.1 <- glm(Ac_pres2 ~ out_l8_r9_1km_sc, family="binomial", data=data)
m.c.8.9.2 <- glm(Ac_pres2 ~ out_l8_r9_2km_sc, family="binomial", data=data)
m.c.9.9.05 <- glm(Ac_pres2 ~ out_l9_r9_05km_sc, family="binomial", data=data)
m.c.9.9.1 <- glm(Ac_pres2 ~ out_l9_r9_1km_sc, family="binomial", data=data)
m.c.9.9.2 <- glm(Ac_pres2 ~ out_l9_r9_2km_sc, family="binomial", data=data)
m.c.9.8.05 <- glm(Ac_pres2 ~ out_l9_r8_05km_sc, family="binomial", data=data)
m.c.9.8.1 <- glm(Ac_pres2 ~ out_l9_r8_1km_sc, family="binomial", data=data)
m.c.9.8.2 <- glm(Ac_pres2 ~ out_l9_r8_2km_sc, family="binomial", data=data)

# Model selection
m.sel.circ <- model.sel(m.c.1.1.05, m.c.1.1.1, m.c.1.1.2, m.c.2.1.05, m.c.2.1.1, m.c.2.1.2, m.c.2.2.05, m.c.2.2.1, m.c.2.2.2, m.c.2.3.05, m.c.2.3.1, m.c.2.3.2, m.c.2.4.05, m.c.2.4.1, m.c.2.4.2, m.c.3.2.05, m.c.3.2.1, m.c.3.2.2, m.c.4.2.05, m.c.4.2.1, m.c.4.2.2, m.c.5.3.05, m.c.5.3.1, m.c.5.3.2, m.c.8.3.05, m.c.8.3.1, m.c.8.3.2, m.c.9.3.05, m.c.9.3.1, m.c.9.3.2, m.c.10.3.05, m.c.10.3.1, m.c.10.3.2, m.c.11.3.05, m.c.11.3.1, m.c.11.3.2, m.c.8.8.05, m.c.8.8.1, m.c.8.8.2, m.c.8.9.05, m.c.8.9.1, m.c.8.9.2, m.c.9.9.05, m.c.9.9.1, m.c.9.9.2, m.c.9.8.05, m.c.9.8.1, m.c.9.8.2)

subset(m.sel.circ, delta < 2)
get.models(m.sel.circ, 1)[[1]]

# Transforming to km scale (structural metrics)
data$for_area1_km <- (data$for_area1)*0.000001
data$for_area2_km <- (data$for_area2)*0.000001
data$for_area05_km <- (data$for_area05)*0.000001
data$mus_area1_km <- data$mus_area1*0.000001
data$mus_area2_km <- data$mus_area2*0.000001
data$mus_area05_km <- data$mus_area05*0.000001
data$mus_tamanho_km <- data$mus_tamanho*0.000001
data$big_distance_km <- data$big_distance*0.001
data$sm_distance_km <- data$sm_distance*0.001
data$big_leng1_km <- data$big_leng1*0.001
data$big_leng2_km <- data$big_leng2*0.001
data$big_leng05_km <- data$big_leng05*0.001
data$sm_leng1_km <- data$sm_leng1*0.001
data$sm_leng2_km <- data$sm_leng2*0.001
data$sm_leng05_km <- data$sm_leng05*0.001
data$mus_perimetro_km <- data$mus_perimetro*0.001

# Scale transformation (structural metrics)
for_area1_km_sc <- scale(data$for_area1_km)
for_area2_km_sc <- scale(data$for_area2_km)
for_area05_km_sc <- scale(data$for_area05_km)
mus_area1_km_sc <- scale(data$mus_area1_km)
mus_area2_km_sc <- scale(data$mus_area2_km)
mus_area05_km_sc <- scale(data$mus_area05_km)
mus_tamanho_km_sc <- scale(data$mus_tamanho_km)
mus_perimetro_km_sc <- scale(data$mus_perimetro_km)
sm_leng1_km_sc <- scale(data$sm_leng1_km)
sm_leng2_km_sc <- scale(data$sm_leng2_km)
SI_sc <- scale(data$SI)
big_leng1_km_sc <- scale(data$big_leng1_km) 
big_leng2_km_sc <- scale(data$big_leng2_km) 
big_leng05_km_sc <- scale(data$big_leng05_km) 
sm_leng05_km_sc <- scale(data$sm_leng05_km)
big_distance_km_sc <- scale(data$big_distance_km)
sm_distance_km_sc <- scale(data$sm_distance_km)

# Global model
m.glob.full2 <- glm (Ac_pres2 ~ out_l11_r3_1km_sc + for_area1_km_sc + for_area2_km_sc + for_area05_km_sc + mus_area1_km_sc + mus_area2_km_sc + mus_area05_km_sc + mus_tamanho_km_sc + mus_perimetro_km_sc + big_distance_km_sc + sm_distance_km_sc + big_leng1_km_sc + big_leng2_km_sc + big_leng05_km_sc + sm_leng1_km_sc + sm_leng2_km_sc + sm_leng05_km_sc + SI_sc, family = "binomial", data = data)

options(na.action = "na.fail")
selection.full2 <- dredge(m.glob.full2, subset= !(for_area2_km_sc && for_area1_km_sc) & !(for_area2_km_sc && for_area05_km_sc) & !(for_area1_km_sc && for_area05_km_sc) & !(mus_area2_km_sc && mus_area1_km_sc) & !(mus_area2_km_sc && mus_area05_km_sc) & !(mus_area1_km_sc && mus_area05_km_sc) & !(big_leng2_km_sc && big_leng1_km_sc) & !(big_leng1_km_sc && big_leng05_km_sc) & !(big_leng2_km_sc && big_leng05_km_sc) & !(sm_leng2_km_sc && sm_leng1_km_sc) & !(sm_leng1_km_sc && sm_leng05_km_sc) & !(sm_leng2_km_sc && sm_leng05_km_sc) & !(out_l11_r3_1km_sc && for_area1_km_sc) & !(out_l11_r3_1km_sc && for_area05_km_sc) & !(mus_area2_km_sc && mus_tamanho_km_sc) & !(mus_area2_km_sc && mus_perimetro_km_sc) & !(mus_area1_km_sc && mus_tamanho_km_sc) & !(mus_perimetro_km_sc && mus_area1_km_sc) & !(mus_perimetro_km_sc && mus_tamanho_km_sc) & !(SI_sc && mus_perimetro_km_sc) & !(big_leng2_km_sc && big_distance_km_sc) & !(big_leng1_km_sc && big_distance_km_sc))

m.medio2 <- model.avg(selection.full2, subset = delta < 2, revised.var = TRUE)
subset(selection.full2, delta < 2)

# Testing linearity
mod1 <- cumres(get.models(selection.full2, 1)[[1]])
mod2 <- cumres(get.models(selection.full2, 2)[[1]])
mod3 <- cumres(get.models(selection.full2, 3)[[1]])
mod4 <- cumres(get.models(selection.full2, 4)[[1]])
mod5 <- cumres(get.models(selection.full2, 5)[[1]])
mod6 <- cumres(get.models(selection.full2, 6)[[1]])
mod7 <- cumres(get.models(selection.full2, 7)[[1]])

plot(mod1)
plot(mod2)
plot(mod3)
plot(mod4)
plot(mod5)
plot(mod6)
plot(mod7)

# Results
summary(m.medio2)
confint(m.medio2)
fric_2.T <- summary(m.medio2)
as.data.frame(fric_2.T$coefficients)
as.data.frame(fric_2.T$sw)

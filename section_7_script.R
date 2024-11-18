library(tidyverse)
library(readr)
library(estimatr)
library(fixest)
library(AER)

# データのインポート
data713 <- read_csv("ipehd_qje2009_master.csv")
glimpse(data713)


# 各モデル別に推定を実施
# iv_regを使用し推定。係数・SE等の推定は一致。ただし、タイガーストック検定、J検定は同じ値は得られず。（微妙に異なる）

model1 <- lm_robust(f_rw ~ f_prot, data = data713)
summary(model1)


model2 <- AER::ivreg(f_rw ~ f_prot | kmwittenberg, data = data713)
summary(model2, vcov. = sandwich,
        diagnostics = TRUE)


model3 <- lm_robust(f_rw ~ f_prot
                    + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                    + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss,
                    data = data713)
summary(model3)


model4 <- AER::ivreg(f_rw ~ 
                     + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                     + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                     + f_prot |
                     + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                     + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                     + kmwittenberg,
                     data = data713)
summary(model4, vcov. = sandwich,
        diagnostics = TRUE)


model5 <- lm_robust(f_prot ~ 
                    + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                    + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                    + kmwittenberg,
                    data = data713)
summary(model5)
model5$statistic[14]^2


model6 <- AER::ivreg(f_rw ~ 
                    + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                    + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                    + f_prot |
                    + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                    + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                    + kmwittenberg + kmwittenberg:lnpop + kmwittenberg:gpop,
                    data = data713)
summary(model6, vcov. = sandwich,
        diagnostics = TRUE)

model6j <- lm_robust(model6$residuals ~ 
                     + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                     + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                     + kmwittenberg + kmwittenberg:lnpop + kmwittenberg:gpop,
                     data = data713)
summary(model6j, vcov. = sandwich)



# 























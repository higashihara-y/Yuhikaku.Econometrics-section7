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


# ワルド検定統計量による外生性の検定
model6j <- lm_robust(model6$residuals ~ 
                     + f_young + f_jew + f_fem + f_ortsgeb + f_pruss + hhsize
                     + lnpop + gpop + f_blind + f_deaf + f_dumb + f_miss
                     + kmwittenberg + kmwittenberg:lnpop + kmwittenberg:gpop,
                     data = data713)
summary(model6j)

hypothesis <- c("kmwittenberg", "f_young", "f_jew", "f_fem", "f_ortsgeb",
                "f_pruss", "hhsize", "lnpop", "gpop", "f_blind", "f_deaf",
                "f_dumb", "f_miss")
linearHypothesis(model6j, hypothesis, rep(0, length(hypothesis)), "Chisq")




#estimater::iv_robustを使用した推定を行い、表形式で出力
# ST統計値、J統計値、P値の表への出力が未完了
models_73 <- list("(1)" = lm_robust(f_rw ~ f_prot, se_type = "stata",
                                    data = data713),
                  "(2)" = iv_robust(f_rw ~ f_prot | kmwittenberg, 
                                    se_type = "stata", data = data713,
                                    diagnostics = TRUE),
                  "(3)" = lm_robust(f_rw ~ f_prot
                                    + f_young + f_jew + f_fem + f_ortsgeb
                                    + f_pruss + hhsize
                                    + lnpop + gpop + f_blind + f_deaf
                                    + f_dumb + f_miss,
                                    se_type = "stata", data = data713),
                  "(4)" = iv_robust(f_rw ~ 
                                      + f_young + f_jew + f_fem + f_ortsgeb
                                    + f_pruss + hhsize + lnpop + gpop
                                    + f_blind + f_deaf + f_dumb + f_miss
                                    + f_prot |
                                    + f_young + f_jew + f_fem + f_ortsgeb
                                    + f_pruss + hhsize
                                    + lnpop + gpop + f_blind + f_deaf
                                    + f_dumb + f_miss
                                    + kmwittenberg,
                                    se_type = "stata", data = data713,
                                    diagnostics = TRUE),
                  "(5)" = lm_robust(f_prot ~ 
                                    + f_young + f_jew + f_fem + f_ortsgeb
                                    + f_pruss + hhsize + lnpop + gpop
                                    + f_blind + f_deaf + f_dumb + f_miss
                                    + kmwittenberg,
                                    se_type = "stata", data = data713),
                  "(6)" = iv_robust(f_rw ~ 
                                    + f_young + f_jew + f_fem + f_ortsgeb
                                    + f_pruss + hhsize + lnpop + gpop
                                    + f_blind + f_deaf + f_dumb + f_miss
                                    + f_prot |
                                    + f_young + f_jew + f_fem + f_ortsgeb
                                    + f_pruss + hhsize + lnpop + gpop
                                    + f_blind + f_deaf + f_dumb + f_miss
                                    + kmwittenberg + kmwittenberg:lnpop
                                    + kmwittenberg:gpop,
                                    se_type = "stata", data = data713,
                                    diagnostics = TRUE))

cm <- c(
  "f_rw" = "識字率",
  "f_prot" = "新教徒率",
  "kmwittenberg" = "距離",
  "f_young" = "子供率",
  "f_jew" = "ユダヤ率",
  "f_fem" = "女性率",
  "f_ortsgeb" = "出身者率",
  "f_pruss" = "普人率",
  "hhsize" = "平均家計人数",
  "lnpop" = "対数人口",
  "gpop" = "人口成長率",
  "f_blind" = "視覚障害率",
  "f_deaf" = "聴覚障害率",
  "f_dumb" = "知的・精神障害率",
  "f_miss" = "欠落率",
  "(Intercept)" = "定数項"
)

# 各統計量を推定するモデルを指定
attr(models_73$`(2)`, "ST") <- TRUE
attr(models_73$`(4)`, "ST") <- TRUE
attr(models_73$`(6)`, "ST") <- TRUE
attr(models_73$`(6)`, "J") <- TRUE

# glance.iv_robust <- function(x) {
#   if(!isTRUE(attr(x, "ST"))) return(NULL)
#   ST <- summary(x)$diagnostic_first_stage_fstatistic
#   out <- tibble("ST_test" = ST["value"],
#                 "adj.r.squared" = "")
#   
#   if(isTRUE(attr(x, "J"))) {
#     j <- summary(x)$diagnostic_overid_test
#     out <- out |> 
#       mutate("J_test" = j["value"],
#              "p_value" = sprintf("(%.3f)", j["p.value"]))
#   }
#   return(out)
# }

glance.iv_robust <- function(x) {
  if(isTRUE(attr(x, "ST"))) {
    ST <- summary(x)$diagnostic_first_stage_fstatistic
    x[["ST_test"]] <- ST["value"]
    x[["adj.r.squared"]] <- "-"
  } else {
    x[["ST_test"]] <- "-"
  }
  
  if(isTRUE(attr(x, "J"))) {
    j <- summary(x)$diagnostic_overid_test
    x[["j_test"]] <- j["value"]
    x[["p_value"]] <- sprintf("(%.3f)", j["p.value"])
  } else {
    x[["j_test"]] <- "-"
  }
  out <- tibble(
    "staiger_stock_test" = x[["ST_test"]],
    "adj.r.squared" = x[["adj.r.squared"]],
    "j_test" = x[["j_test"]],
    "p_value" = x[["p_value"]])
  return(out)
}

for(nm in names(models_73)) {
  models_73[[nm]]["staiger_stock_test"] <- 
    glance.iv_robust(models_73[[nm]])["staiger_stock_test"]
  models_73[[nm]]["adj.r.squared"] <- 
    glance.iv_robust(models_73[[nm]])["adj.r.squared"]
  models_73[[nm]]["j_test"] <- 
    glance.iv_robust(models_73[[nm]])["j_test"]
  # models_73[[nm]]["p_value"] <- 
  #   glance.iv_robust(models_73[[nm]])["p_value"]
}

gm <- tribble(~raw, ~clean, ~fmt,
              "staiger_stock_test", "ST検定統計量", 2,
              "j_test", "$J$検定統計量", 3,
              # "p_value", " ", 3,
              "adj.r.squared", "$\\bar{R}^2$", 3,
              "nobs", "サンプルサイズ", 0)

rows <- tribble(~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`, 
               "被説明変数", "識字率", "識字率", "識字率", "識字率",
               "新教徒率", "識字率",
               "推定法", "OLS" , "2SLS" , "OLS" , "2SLS" , "OLS" , "2SLS")

attr(rows, 'position') <- c(1, 2)

modelsummary::modelsummary(
  models_73,
  stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
  gof_map = gm,
  coef_map = cm,
  add_rows = rows,
  estimate = "{estimate}{stars}",
  output = "kableExtra",
  notes = "* p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001"
) |> 
  kableExtra::row_spec(c(0, 2, 31), 
                       extra_css = "border-bottom: 1.5px solid") |> 
  kableExtra::row_spec(32, extra_css = "border-bottom: 1.5px solid")

modelsummary::gof_map

# ST・J検定統計量自体は、正しい数値を得られていることを確認
summary( iv_robust(f_rw ~ f_prot | kmwittenberg, 
                   se_type = "stata", data = data713,
                   diagnostics = TRUE))

summary(iv_robust(f_rw ~ 
                    + f_young + f_jew + f_fem + f_ortsgeb
                  + f_pruss + hhsize + lnpop + gpop
                  + f_blind + f_deaf + f_dumb + f_miss
                  + f_prot |
                    + f_young + f_jew + f_fem + f_ortsgeb
                  + f_pruss + hhsize
                  + lnpop + gpop + f_blind + f_deaf
                  + f_dumb + f_miss
                  + kmwittenberg,
                  se_type = "stata", data = data713,
                  diagnostics = TRUE))

summary(iv_robust(f_rw ~ 
                    + f_young + f_jew + f_fem + f_ortsgeb
                  + f_pruss + hhsize + lnpop + gpop
                  + f_blind + f_deaf + f_dumb + f_miss
                  + f_prot |
                    + f_young + f_jew + f_fem + f_ortsgeb
                  + f_pruss + hhsize + lnpop + gpop
                  + f_blind + f_deaf + f_dumb + f_miss
                  + kmwittenberg + kmwittenberg:lnpop
                  + kmwittenberg:gpop,
                  se_type = "stata", data = data713,
                  diagnostics = TRUE))



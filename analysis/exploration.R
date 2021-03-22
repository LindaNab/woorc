#############################################################
## ANALYSIS SCRIPTS USED FOR THE SCIENTIFIC RESEARCH
##
## This script contains code that explores the output of the sim
## study summarised in data(summary)
## lindanab4@gmail.com - 20210322
#############################################################

##############################
# 0 - Load librairies --------
##############################
library(dplyr)
data(summary) # summary of sim study

##############################
# 1 - Explore results --------
##############################
# Percentage bias
with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, perc_bias, ylim = c(-40, 20)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, perc_bias, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, perc_bias, col = "blue"))
legend("topright",
       legend = c("uncorrected", "mecor", "simex"),
       col = c("black", "red", "blue"),
       pch = 1)

# Mean squared error
with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, mse, ylim = c(0, 0.02)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, mse, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, mse, col = "blue"))
legend("topright",
       legend = c("uncorrected", "mecor", "simex"),
       col = c("black", "red", "blue"),
       pch = 1)

# Coverage
with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, cover, ylim = c(0, 1.1)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, cover, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, cover, col = "blue"))
legend("bottomright",
       legend = c("uncorrected", "mecor", "simex"),
       col = c("black", "red", "blue"),
       pch = 1)

# ModelSE and EmpSe
with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, modelse, ylim = c(0, 0.15)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, modelse, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, modelse, col = "blue"))
with(summary %>% dplyr::filter(method == "uncor"),
     points(scen_no, empse, col = "black", pch = 3))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, empse, col = "red", pch = 3))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, empse, col = "blue", pch = 3))
legend("topleft",
       legend = c("modelse uncor", "modelse mecor", "modelse simex",
                  "empse uncor", "empse mecor", "empse simex"),
       col = c("black", "red", "blue"),
       pch = c(1, 1, 1, 3, 3, 3))

apply(scen_no1[ , c(1, 5, 9)], 2, mean)
apply(scen_no1[ , c(2, 6, 10)], 2, mean)
apply(scen_no1[ , c(1, 5, 9)], 2, sd)
sum(scen_no1[, 3] < 0.2 & scen_no1[, 4] > 0.2) / 1000
sum(scen_no1[, 7] < 0.2 & scen_no1[, 8] > 0.2) / 1000
sum(scen_no1[, 11] < 0.2 & scen_no1[, 12] > 0.2) / 1000

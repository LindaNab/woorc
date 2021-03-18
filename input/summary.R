process_scen_nos(1:14)
summary <- summarise_sim(1:14)
save(summary,
     file = "./data/summary.rda")
data(summary)
summary_uncor <- summary %>% dplyr::filter(method == "uncor")
with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, perc_bias, ylim = c(-40, 100)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, perc_bias, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, perc_bias, col = "blue"))
legend("topright",
       legend = c("uncorrected", "mecor", "simex"),
       col = c("black", "red", "blue"),
       pch = 1)

with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, mse, ylim = c(0, 0.1)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, mse, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, mse, col = "blue"))
legend("topright",
       legend = c("uncorrected", "mecor", "simex"),
       col = c("black", "red", "blue"),
       pch = 1)

with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, cover, ylim = c(0, 1.1)))
with(summary %>% dplyr::filter(method == "mecor"),
     points(scen_no, cover, col = "red"))
with(summary %>% dplyr::filter(method == "simex"),
     points(scen_no, cover, col = "blue"))
legend("topright",
       legend = c("uncorrected", "mecor", "simex"),
       col = c("black", "red", "blue"),
       pch = 1)

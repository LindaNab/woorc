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
data("summary_lowrel") # summary of sim study
summary <- summary_lowrel

##############################
# 1 - Explore results --------
##############################


# ModelSE and EmpSe
with(summary %>% dplyr::filter(method == "uncor"),
     plot(scen_no, modelse, ylim = c(0, 100)))
with(summary %>% dplyr::filter(method == "mecor.delta"),
     points(scen_no, modelse, col = "red"))
with(summary %>% dplyr::filter(method == "mecor.btstrp"),
     points(scen_no, modelse, col = "blue"))
with(summary %>% dplyr::filter(method == "uncor"),
     points(scen_no, empse, col = "black", pch = 3))
with(summary %>% dplyr::filter(method == "mecor.delta"),
     points(scen_no, empse, col = "red", pch = 3))
with(summary %>% dplyr::filter(method == "mecor.btstrp"),
     points(scen_no, empse, col = "blue", pch = 3))
legend("topleft",
       legend = c("modelse uncor", "modelse mecor", "modelse simex",
                  "empse uncor", "empse mecor", "empse simex"),
       col = c("black", "red", "blue"),
       pch = c(1, 1, 1, 3, 3, 3))


##############################
# 2 - Percentage bias --------
##############################
# perc bias
plot_percbias <- function(filename,
                          panelname,
                          title,
                          selection_of_summary,
                          legend = FALSE){
  pdf(filename,
      width = 2.1, height = 2.1,
      pointsize = 8/0.75)

  par(
    mar = c(4.25, 5, 2, 1.25),
    xpd = NA,
    cex = 0.75
  )
  plot(   0,
          type = "n",
          xaxt = "n",
          yaxt = "n",
          yaxs = "i",
          xaxs = "i",
          frame.plot = F,
          ann = F,
          xlim = c(0, 1),
          ylim = c(-100, 100),
          asp = 1 / 200
  )

  selection_of_summary %>%
    filter(method == "uncor") %>%
    with(., lines(reliability,
                  perc_bias,
                  type = "b",
                  pch = 19,
                  col = "darkgray"))
  selection_of_summary %>%
    filter(method == "mecor.delta") %>%
    with(., lines(reliability,
                  perc_bias,
                  type = "b",
                  pch = 19))
  axis(
    1,
    at = seq(from = 0, to = 1, by = 0.2),
    labels = seq(from = 0, to = 1, by = 0.2),
    las = 1
  )
  mtext("Reliability",
        side = 1,
        line = 3,
        cex = 0.75)
  axis(
    2,
    at = seq(from = -100, to = 100, by = 50),
    labels = c(
      expression(-100),
      expression(-50),
      expression(0),
      expression(50),
      expression(100)
    ),
    las = 1
  )
  mtext("Percentage Bias, %",
        side = 2,
        line = 3,
        cex = 0.75)
  mtext(title,
        side = 3,
        line = 0,
        cex = 0.75)
  if (legend == TRUE){
    legend("topright",
           legend = c("OLS", "RC"),
           lty = 1,
           pch = c(19, 19),
           col = c("darkgray", "black"),
           title = expression(underline(Estimator)))
  }
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from = "in", to = "user")
  y <- grconvertY(c(0, di[2]), from = "in", to = "user")
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  x <- x[1] + strwidth(panelname)
  y <- y[2] - strheight(panelname) * 1.5
  text(x, y, panelname, cex = 1)
  dev.off()
}

##############################
# 3 - Mean Squared Error ------
##############################
# perc bias
plot_mse <- function(filename,
                     panelname,
                     title,
                     selection_of_summary,
                     legend = FALSE) {
  pdf(filename,
      width = 2.1, height = 2.1,
      pointsize = 8/0.75)

  par(
    mar = c(4.25, 5, 2, 1.25),
    xpd = NA,
    cex = 0.75
  )
  plot(   0,
          type = "n",
          xaxt = "n",
          yaxt = "n",
          yaxs = "i",
          xaxs = "i",
          frame.plot = F,
          ann = F,
          xlim = c(0, 1),
          ylim = c(0, 10),
          asp = 1 / 10
  )

  selection_of_summary %>%
    filter(method == "uncor") %>%
    with(., lines(reliability,
                  mse,
                  type = "b",
                  pch = 19,
                  col = "darkgray"))
  selection_of_summary %>%
    filter(method == "mecor.delta") %>%
    with(., lines(reliability,
                  mse,
                  type = "b",
                  pch = 19))
  axis(
    1,
    at = seq(from = 0, to = 1, by = 0.2),
    labels = seq(from = 0, to = 1, by = 0.2),
    las = 1
  )
  mtext("Reliability",
        side = 1,
        line = 3,
        cex = 0.75)
  axis(
    2,
    at = seq(from = 0, to = 10, by = 2),
    labels = seq(from = 0, to = 10, by = 2),
    las = 1
  )
  mtext("Mean Squared Error",
        side = 2,
        line = 3,
        cex = 0.75)
  mtext(title,
        side = 3,
        line = 0,
        cex = 0.75)
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from = "in", to = "user")
  y <- grconvertY(c(0, di[2]), from = "in", to = "user")
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  x <- x[1] + strwidth(panelname)
  y <- y[2] - strheight(panelname) * 1.5
  text(x, y, panelname, cex = 1)
  dev.off()
}

##############################
# 4 - Coverage ---------------
##############################
# coverage
plot_cov <- function(filename,
                     panelname,
                     title,
                     selection_of_summary,
                     legend = FALSE) {
  pdf(filename,
      width = 2.1, height = 2.1,
      pointsize = 8/0.75)

  par(
    mar = c(4.25, 5, 2, 1.25),
    xpd = NA,
    cex = 0.75
  )
  plot(   0,
          type = "n",
          xaxt = "n",
          yaxt = "n",
          yaxs = "i",
          xaxs = "i",
          frame.plot = F,
          ann = F,
          xlim = c(0, 1),
          ylim = c(0, 1.1),
          asp = 1 / 1.1
  )
  segments(x0 = 0, y0 = 0.95, x1 = 1, y1 = 0.95, col = "gray", lty = 3)
  selection_of_summary %>%
    filter(method == "uncor") %>%
    with(., lines(reliability,
                  cover,
                  type = "b",
                  pch = 19,
                  col = "darkgray"))
  selection_of_summary %>%
    filter(method == "mecor.delta") %>%
    with(., lines(reliability,
                  cover,
                  type = "b",
                  pch = 19))
  selection_of_summary %>%
    filter(method == "mecor.btstrp") %>%
    with(., lines(reliability,
                  cover,
                  type = "b",
                  pch = 17))
  axis(
    1,
    at = seq(from = 0, to = 1, by = 0.2),
    labels = seq(from = 0, to = 1, by = 0.2),
    las = 1
  )
  mtext("Reliability",
        side = 1,
        line = 3,
        cex = 0.75)
  axis(
    2,
    at = c(seq(from = 0, to = 1, by = 0.2), 1.1),
    labels = c(seq(from = 0, to = 1, by = 0.2), ""),
    las = 1
  )
  mtext("Coverage",
        side = 2,
        line = 3,
        cex = 0.75)
  mtext(title,
        side = 3,
        line = 0,
        cex = 0.75)
  if (legend == TRUE){
    legend(x = 0.05, y = 0.8,
           legend = c("Delta", "Bootstrap", "Wald"),
           pch = c(19, 17),
           col = c("black", "black", "darkgray"),
           title = expression(underline(Method)))
  }
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from = "in", to = "user")
  y <- grconvertY(c(0, di[2]), from = "in", to = "user")
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  x <- x[1] + strwidth(panelname)
  y <- y[2] - strheight(panelname) * 1.5
  text(x, y, panelname, cex = 1)
  dev.off()
}

##############################
# 5 - EmpSe + ModSE ----------
##############################
plot_se <- function(filename,
                    panelname,
                    title,
                    selection_of_summary,
                    legend1 = FALSE,
                    legend2 = FALSE) {
  pdf(filename,
      width = 2.1, height = 2.1,
      pointsize = 8/0.75)

  par(
    mar = c(4.25, 5, 2, 1.25),
    xpd = NA,
    cex = 0.75
  )
  plot(   0,
          type = "n",
          xaxt = "n",
          yaxt = "n",
          yaxs = "i",
          xaxs = "i",
          frame.plot = F,
          ann = F,
          xlim = c(0.5, 1),
          ylim = c(0, 2),
          asp = 0.5 / 2
  )
  #empse
  selection_of_summary <-
    selection_of_summary %>%
    filter(between(reliability, 0.5, 1))
  selection_of_summary %>%
    filter(method == "uncor") %>%
    with(., points(reliability,
                  empse,
                  type = "b",
                  pch = 21,
                  bg = "darkgray",
                  col = "darkgray"))
  selection_of_summary %>%
    filter(method == "mecor.delta") %>%
    with(., points(reliability,
                  empse,
                  type = "b",
                  pch = 21,
                  bg = "black"))
  #modse
  selection_of_summary %>%
    filter(method == "uncor") %>%
    with(., points(reliability,
                  modelse,
                  type = "b",
                  pch = 21,
                  col = "darkgray",
                  bg = rgb(0.255, 0.255, 0.255, maxColorValue = 1, alpha = 0),
                  lty = 3))
  selection_of_summary %>%
    filter(method == "mecor.delta") %>%
    with(., points(reliability,
                  modelse,
                  type = "b",
                  pch = 21,
                  bg = rgb(0.255, 0.255, 0.255, maxColorValue = 1, alpha = 0),
                  lty = 3))
  selection_of_summary %>%
    filter(method == "mecor.btstrp") %>%
    with(., points(reliability,
                  modelse,
                  type = "b",
                  pch = 24,
                  bg = rgb(0.255, 0.255, 0.255, maxColorValue = 1, alpha = 0),
                  lty = 3))
  axis(
    1,
    at = seq(from = 0.5, to = 1, by = 0.1),
    labels = seq(from = 0.5, to = 1, by = 0.1),
    las = 1
  )
  mtext("Reliability",
        side = 1,
        line = 3,
        cex = 0.75)
  axis(
    2,
    at = seq(from = 0, to = 2, by = 0.5),
    labels = seq(from = 0, to = 2, by = 0.5),
    las = 1
  )
  mtext("Standard Error",
        side = 2,
        line = 3,
        cex = 0.75)
  mtext(title,
        side = 3,
        line = 0,
        cex = 0.75)
  if (legend1 == TRUE){
    legend("topright",
           legend = c("EmpSE", "ModSE"),
           pch = c(21, 21),
           pt.bg = c("darkgray", "white"),
           lty = c(1, 3),
           col = c("darkgray", "darkgray"),
           title = expression(underline("OLS")))
  }
  if (legend2 == TRUE){
    legend("topright",
           legend = c("EmpSE", "ModSE (D)", "ModSE (B)"),
           pch = c(21, 21, 24),
           pt.bg = c("black", "white", "white"),
           lty = c(1, 3, 3),
           col = c("black", "black", "black"),
           title = expression(underline("RC")))
  }
  di <- dev.size("in")
  x <- grconvertX(c(0, di[1]), from = "in", to = "user")
  y <- grconvertY(c(0, di[2]), from = "in", to = "user")
  fig <- par("fig")
  x <- x[1] + (x[2] - x[1]) * fig[1:2]
  y <- y[1] + (y[2] - y[1]) * fig[3:4]
  x <- x[1] + strwidth(panelname)
  y <- y[2] - strheight(panelname) * 1.5
  text(x, y, panelname, cex = 1)
  dev.off()
}

##############################
# 5 - Plots ------------------
##############################
# value outside plot range
summary %>%
  filter(!between(perc_bias, -100, 100)) %>%
  select(perc_bias, bias_mcse)
# bias regcal ss 25, 50; rel 0.01, 0.05, 0.1
summary %>%
  filter(nobs %in% c(25, 50) & reliability %in% c(0.01, 0.05, 0.1)) %>%
  filter(method == "mecor.delta") %>%
  select(nobs, reliability, method, perc_bias, bias_mcse) %>%
  mutate(perc_bias = round(perc_bias, 1), bias_mcse = round(bias_mcse, 3)) %>%
  arrange(nobs, reliability)
plot_percbias(filename = "./results/percbias_ss600.pdf",
              panelname = "A)",
              title = "No. of observations = 600",
              selection_of_summary = summary %>%
                filter(nobs == 600 & between(perc_bias, -100, 100)),
              legend = TRUE)
plot_percbias(filename = "./results/percbias_ss300.pdf",
              panelname = "B)",
              title = "No. of observations = 300",
              selection_of_summary = summary %>%
                filter(nobs == 300 & between(perc_bias, -100, 100)),
              legend = FALSE)
plot_percbias(filename = "./results/percbias_ss150.pdf",
              panelname = "A)",
              title = "No. of observations = 150",
              selection_of_summary = summary %>%
                filter(nobs == 150 & between(perc_bias, -100, 100)),
              legend = TRUE)
plot_percbias(filename = "./results/percbias_ss50.pdf",
              panelname = "B)",
              title = "No. of observations = 50",
              selection_of_summary = summary %>%
                filter(nobs == 50 & between(perc_bias, -100, 100)),
              legend = FALSE)
plot_percbias(filename = "./results/percbias_ss25.pdf",
              panelname = "C)",
              title = "No. of observations = 25",
              selection_of_summary = summary %>%
                filter(nobs == 25 & between(perc_bias, -100, 100)),
              legend = FALSE)

# mean squared error
summary %>%
  filter(method == "mecor.delta" & !between(mse, 0, 10)) %>%
  select(nobs, reliability, mse, mse_mcse) %>%
  arrange(nobs, reliability) %>%
  mutate(mse_round = round(mse, 0), mse_mcse_round = round(mse_mcse, 0))
summary %>%
  filter(method == "mecor.delta") %>%
  slice_max(mse) %>%
  select(scen_no, method, nobs, reliability, mse, mse_mcse) %>%
  mutate(mse = round(mse, 0), mse_mcse = round(mse_mcse, 3))
summary %>%
  filter(method == "uncor" & scen_no == 26) %>%
  select(scen_no, method, nobs, reliability, mse, mse_mcse) %>%
  mutate(mse = round(mse, 0), mse_mcse = round(mse_mcse, 3))
summary %>%
  filter(method == "uncor")
plot_mse(filename = "./results/mse_ss600.pdf",
         panelname = "C)",
         title = "No. of observations = 600",
         selection_of_summary = summary %>%
           filter(nobs == 600 & between(mse, 0, 10)))
plot_mse(filename = "./results/mse_ss300.pdf",
         panelname = "D)",
         title = "No. of observations = 300",
         selection_of_summary = summary %>%
           filter(nobs == 300 & between(mse, 0, 10)))
plot_mse(filename = "./results/mse_ss150.pdf",
         panelname = "D)",
         title = "No. of observations = 150",
         selection_of_summary = summary %>%
           filter(nobs == 150 & between(mse, 0, 10)))
plot_mse(filename = "./results/mse_ss50.pdf",
         panelname = "E)",
         title = "No. of observations = 50",
         selection_of_summary = summary %>%
           filter(nobs == 50 & between(mse, 0, 10)))
plot_mse(filename = "./results/mse_ss25.pdf",
         panelname = "F)",
         title = "No. of observations = 25",
         selection_of_summary = summary %>%
           filter(nobs == 25 & between(mse, 0, 10)))

# coverage
# range of coverage (delta overcoverage)
summary %>%
  filter(method == "mecor.delta") %>%
  filter(between(reliability, 0.01, 0.8)) %>%
  pull(cover) %>%
  quantile()
summary %>%
  filter(method == "mecor.delta") %>%
  filter(between(reliability, 0.01, 0.8)) %>%
  pull(cover_mcse) %>% max() < 0.05
# range of coverage (uncor undercoverage)
summary %>%
  filter(method == "uncor") %>%
  pull(cover) %>%
  quantile()
summary %>%
  filter(method == "uncor") %>%
  pull(cover_mcse) %>% max() < 0.05
plot_cov(filename = "./results/cov_ss600.pdf",
         panelname = "E)",
         title = "No. of observations = 600",
         selection_of_summary = summary %>%
           filter(nobs == 600 & between(cover, 0, 100)),
         legend = TRUE)
plot_cov(filename = "./results/cov_ss300.pdf",
         panelname = "F)",
         title = "No. of observations = 300",
         selection_of_summary = summary %>%
           filter(nobs == 300 & between(cover, 0, 100)))
plot_cov(filename = "./results/cov_ss150.pdf",
         panelname = "G)",
         title = "No. of observations = 150",
         selection_of_summary = summary %>%
           filter(nobs == 150 & between(cover, 0, 100)),
         legend = TRUE)
plot_cov(filename = "./results/cov_ss50.pdf",
         panelname = "H)",
         title = "No. of observations = 50",
         selection_of_summary = summary %>%
           filter(nobs == 50 & between(cover, 0, 100)))
plot_cov(filename = "./results/cov_ss25.pdf",
         panelname = "I)",
         title = "No. of observations = 25",
         selection_of_summary = summary %>%
           filter(nobs == 25 & between(cover, 0, 100)))

# empse / model based se
summary %>%
  filter(between(reliability, 0.5, 1)) %>%
  filter(method == "mecor.btstrp" & !between(modelse, 0, 2)) %>%
  select(nobs, reliability, modelse, modelse_mcse) %>%
  arrange(-nobs, reliability) %>%
  mutate(modelse_round = round(modelse, 1), modelse_mcse_round = round(modelse_mcse, 3))
plot_se(filename = "./results/se_ss600.pdf",
        panelname = "A)",
        title = "No. of observations = 600",
        selection_of_summary = summary %>%
          filter(nobs == 600 & between(empse, 0, 2) & between(modelse, 0, 2)),
        legend1 = TRUE,
        legend2 = FALSE)
plot_se(filename = "./results/se_ss300.pdf",
        panelname = "B)",
        title = "No. of observations = 300",
        selection_of_summary = summary %>%
          filter(nobs == 300 & between(empse, 0, 2) & between(modelse, 0, 2)),
        legend2 = TRUE)
plot_se(filename = "./results/se_ss150.pdf",
        panelname = "A)",
        title = "No. of observations = 150",
        selection_of_summary = summary %>%
          filter(nobs == 150 & between(empse, 0, 2) & between(modelse, 0, 2)),
        legend1 = TRUE)
plot_se(filename = "./results/se_ss50.pdf",
        panelname = "B)",
        title = "No. of observations = 50",
        selection_of_summary = summary %>%
          filter(nobs == 50 & between(empse, 0, 2) & between(modelse, 0, 2)),
        legend2 = TRUE)
plot_se(filename = "./results/se_ss25.pdf",
        panelname = "C)",
        title = "No. of observations = 25",
        selection_of_summary = summary %>%
          filter(nobs == 25 & between(empse, 0, 2) & between(modelse, 0, 2)))


# table
# nobs, reliablity, ols (empse, model se), delta (empse, model se), bootstrap (empse, model se)
# nobs = c(600, 300, 150, 50, 25)
# reliability = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
table_se <-
  summary %>%
  filter(between(reliability, 0, 0.5)) %>%
  select(nobs,
         reliability,
         method,
         empse,
         empse_mcse,
         modelse,
         modelse_mcse) %>%
  mutate(empse = round(empse, 2),
         empse_mcse = round(empse_mcse, 3),
         modelse = round(modelse, 2),
         modelse_mcse = round(modelse_mcse, 3)) %>%
  arrange(method,-nobs, reliability)
table_se_uncor <- table_se[1:35, ]
table_se_delta <- table_se[36:70, ]
table_se_btstrp <- table_se[71:105, ]
table_se_wide <-
  table_se_delta %>%
  left_join(table_se_btstrp, by = c("nobs", "reliability"),
            suffix = c("_delta", "_btstrp")) %>%
  left_join(table_se_uncor, by = c("nobs", "reliability")) %>%
  select(-c("method_delta", "method_btstrp", "method")) %>%
  mutate(empse_regcal = paste0(empse_delta, " (", ifelse(empse_mcse_delta < 0.001, "<0.001", empse_mcse_delta), ")"),
         modelse_delta = paste0(modelse_delta, " (", ifelse(modelse_mcse_delta < 0.001, "<0.001", modelse_mcse_delta), ")"),
         modelse_btstrp = paste0(modelse_btstrp, " (", ifelse(modelse_mcse_btstrp < 0.001, "<0.001", modelse_mcse_btstrp), ")"),
         empse_uncor = paste0(empse, " (", ifelse(empse_mcse < 0.001, "<0.001", empse_mcse), ")"),
         modelse_uncor = paste0(modelse, " (", ifelse(modelse_mcse < 0.001, "<0.001", modelse_mcse), ")")) %>%
  select(-c(empse_mcse_delta, modelse_mcse_delta,
            empse_mcse_btstrp, modelse_mcse_btstrp,
            empse, modelse,
            empse_mcse, modelse_mcse))
write.csv(table_se_wide, "./results/table_se.csv")

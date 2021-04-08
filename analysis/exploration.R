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


##############################
# 2 - Reliability ------------
##############################
# perc bias
sum_rel <- summary %>% dplyr::filter(scen_no %in% 1:9)
pdf("./results/reliability_percbias.pdf",
    width = 2.5, height = 2.5,
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
        ylim = c(-100, 20),
        asp = 1 / 120
     )
cols <- c(rep("black", 27))
cols[4 + c(0, 9, 18)] <- "grey"
with(sum_rel, points(reliability,
                     perc_bias,
                     pch = c(rep(19, 9), rep(17, 9), rep(8, 9)),
                     col = cols))
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
        at = seq(from = -100, to = 20, by = 20),
        labels = c(
                expression(-100),
                expression(-80),
                expression(-60),
                expression(-40),
                expression(-20),
                expression(0),
                expression(20)
        ),
        las = 1
)
mtext("Percentage Bias, %",
      side = 2,
      line = 3,
      cex = 0.75)
legend("bottomright",
       legend = c("Uncorrected", "Mecor", "Simex"),
       pch = c(19, 17, 8),
       title = expression(underline(Method)))
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("A)")
y <- y[2] - strheight("A)") * 1.5
text(x, y, "A)", cex = 1)
dev.off()

# mse
pdf("./results/reliability_mse.pdf",
    width = 2.5, height = 2.5,
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
        ylim = c(0, 0.03),
        asp = 1 / 0.03
)
cols <- c(rep("black", 27))
cols[4 + c(0, 9, 18)] <- "grey"
with(sum_rel, points(reliability,
                     mse,
                     pch = c(rep(19, 9), rep(17, 9), rep(8, 9)),
                     col = cols))
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
        at = seq(from = 0, to = 0.03, by = 0.01),
        labels = c(
                expression(0),
                expression(0.01),
                expression(0.02),
                expression(0.03)
        ),
        las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("B)")
y <- y[2] - strheight("B)") * 1.5
text(x, y, "B)", cex = 1)
dev.off()

# coverage
pdf("./results/reliability_coverage.pdf",
    width = 2.5, height = 2.5,
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
        ylim = c(0, 1),
        asp = 1 / 1
)
cols <- c(rep("black", 27))
cols[4 + c(0, 9, 18)] <- "grey"
with(sum_rel, points(reliability,
                     cover,
                     pch = c(rep(19, 9), rep(17, 9), rep(8, 9)),
                     col = cols))
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
        at = seq(from = 0, to = 1, by = 0.2),
        labels = seq(from = 0, to = 1, by = 0.2),
        las = 1
)
mtext("Coverage",
      side = 2,
      line = 3,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("C)")
y <- y[2] - strheight("C)") * 1.5
text(x, y, "C)", cex = 1)
dev.off()

##############################
# 2 - Sample size ------------
##############################
# perc bias
sum_ss <- summary %>% dplyr::filter(scen_no %in% c(1, 13:15))
pdf("./results/samplesize_percbias.pdf",
    width = 2.5, height = 2.5,
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
        xlim = c(0, 1000),
        ylim = c(-100, 20),
        asp = 1000 / 120
)
cols <- c(rep("black", nrow(sum_ss)))
n_scen <- nrow(sum_ss) / 3
cols[seq(1, n_scen * 3, by = n_scen)] <- "grey"
with(sum_ss, points(nobs,
                    perc_bias,
                    pch = c(rep(19, n_scen), rep(17, n_scen), rep(8, n_scen)),
                    col = cols))
axis(
        1,
        at = seq(from = 0, to = 1000, by = 250),
        labels = seq(from = 0, to = 1000, by = 250),
        las = 1
)
mtext("Sample Size",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
        2,
        at = seq(from = -100, to = 20, by = 20),
        labels = c(
                expression(-100),
                expression(-80),
                expression(-60),
                expression(-40),
                expression(-20),
                expression(0),
                expression(20)
        ),
        las = 1
)
mtext("Percentage Bias, %",
      side = 2,
      line = 3,
      cex = 0.75)
legend("bottomright",
       legend = c("Uncorrected", "Mecor", "Simex"),
       pch = c(19, 17, 8),
       title = expression(underline(Method)))
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("A)")
y <- y[2] - strheight("A)") * 1.5
text(x, y, "A)", cex = 1)
dev.off()

# mse
pdf("./results/samplesize_mse.pdf",
    width = 2.5, height = 2.5,
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
        xlim = c(0, 1000),
        ylim = c(0, 0.03),
        asp = 1000 / 0.03
)
cols <- c(rep("black", nrow(sum_ss)))
n_scen <- nrow(sum_ss) / 3
cols[seq(1, n_scen * 3, by = n_scen)] <- "grey"
with(sum_ss, points(nobs,
                     mse,
                     pch = c(rep(19, n_scen), rep(17, n_scen), rep(8, n_scen)),
                     col = cols))
axis(
        1,
        at = seq(from = 0, to = 1000, by = 250),
        labels = seq(from = 0, to = 1000, by = 250),
        las = 1
)
mtext("Sample Size",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
        2,
        at = seq(from = 0, to = 0.03, by = 0.01),
        labels = c(
                expression(0),
                expression(0.01),
                expression(0.02),
                expression(0.03)
        ),
        las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("B)")
y <- y[2] - strheight("B)") * 1.5
text(x, y, "B)", cex = 1)
dev.off()

# coverage
pdf("./results/samplesize_coverage.pdf",
    width = 2.5, height = 2.5,
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
        xlim = c(0, 1000),
        ylim = c(0, 1),
        asp = 1000 / 1
)
cols <- c(rep("black", nrow(sum_ss)))
n_scen <- nrow(sum_ss) / 3
cols[seq(1, n_scen * 3, by = n_scen)] <- "grey"
with(sum_ss, points(nobs,
                     cover,
                     pch = c(rep(19, n_scen), rep(17, n_scen), rep(8, n_scen)),
                     col = cols))
axis(
        1,
        at = seq(from = 0, to = 1000, by = 250),
        labels = seq(from = 0, to = 1000, by = 250),
        las = 1
)
mtext("Sample Size",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
        2,
        at = seq(from = 0, to = 1, by = 0.2),
        labels = seq(from = 0, to = 1, by = 0.2),
        las = 1
)
mtext("Coverage",
      side = 2,
      line = 3,
      cex = 0.75)
di <- dev.size("in")
x <- grconvertX(c(0, di[1]), from = "in", to = "user")
y <- grconvertY(c(0, di[2]), from = "in", to = "user")
fig <- par("fig")
x <- x[1] + (x[2] - x[1]) * fig[1:2]
y <- y[1] + (y[2] - y[1]) * fig[3:4]
x <- x[1] + strwidth("C)")
y <- y[2] - strheight("C)") * 1.5
text(x, y, "C)", cex = 1)
dev.off()




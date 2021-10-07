#############################################################
## ANALYSIS SCRIPTS USED FOR THE SCIENTIFIC RESEARCH
##
## This script contains code that explores the output of the sim
## study PARTII (logreg) summarised in data(summary_logreg)
## lindanab4@gmail.com - 20210920
#############################################################

##############################
# 0 - Load librairies --------
##############################
library(dplyr)
data(summary_logreg) # summary of sim study

##############################
# 1 - ModelSE and EmpSE ------
##############################
with(summary_logreg %>% dplyr::filter(method == "uncor"),
     plot(scen_no, modelse, ylim = c(0, 0.15)))
with(summary_logreg %>% dplyr::filter(method == "mecor"),
     points(scen_no, modelse, col = "red"))
with(summary_logreg %>% dplyr::filter(method == "simex"),
     points(scen_no, modelse, col = "blue"))
with(summary_logreg %>% dplyr::filter(method == "uncor"),
     points(scen_no, empse, col = "black", pch = 3))
with(summary_logreg %>% dplyr::filter(method == "mecor"),
     points(scen_no, empse, col = "red", pch = 3))
with(summary_logreg %>% dplyr::filter(method == "simex"),
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
sum_rel <- summary_logreg %>% dplyr::filter(scen_no %in% 1:9)
sum_rel <- sum_rel[order(sum_rel$method, sum_rel$reliability),]
max(sum_rel$bias_mcse) < 0.02
max(sum_rel[(sum_rel$scen_no == 2 & sum_rel$method == "mecor") == FALSE, "mse_mcse"]) < 0.02
sum_rel[(sum_rel$scen_no == 2 & sum_rel$method == "mecor"), "mse_mcse"] # rel = 0.05, regcal
max(sum_rel$cover_mcse) < 0.02


pdf("./results/logreg_reliability_percbias.pdf",
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
cols <- c(rep("black", 9))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 9 + 1):(9 * (i + 1))
  with(sum_rel[rows,],
       points(
         reliability,
         perc_bias,
         pch = rep(pchs[i+1], 9),
         col = cols,
         type = "b"
       ))
}
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
sum_rel_mse <-
  sum_rel %>%
  filter(!(scen_no == 2 & method == "mecor"))
pdf("./results/logreg_reliability_mse.pdf",
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
        ylim = c(0, 0.05),
        asp = 1 / 0.05
)
cols <- c(rep("black", 9))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in c(0,2)){
  rows <- (i * 9 + 1 - i * 0.5 ):(9 * (i + 1) - i * 0.5)
  with(sum_rel_mse[rows,],
       points(
         reliability,
         mse,
         pch = rep(pchs[i+1], 9),
         col = cols,
         type = "b"
       ))
}
cols <- c(rep("black", 8))
cols[3] <- "grey"
rows <- 10:17 # only 8 scens
with(sum_rel_mse[rows,],
     points(
       reliability,
       mse,
       pch = rep(pchs[2], 9),
       col = cols,
       type = "b"
     ))
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
  at = seq(from = 0, to = 0.05, by = 0.01),
  labels = c(
    expression(0),
    expression(0.01),
    expression(0.02),
    expression(0.03),
    expression(0.04),
    expression(0.05)
  ),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
legend("topright",
       legend = c("Uncorrected", "RC", "Simex"),
       pch = c(19, 17, 8),
       title = expression(underline(Method)))
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
pdf("./results/logreg_reliability_coverage.pdf",
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
segments(x0 = 0, y0 = 0.95, x1 = 1, col = "grey")
cols <- c(rep("black", 9))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 9 + 1):(9 * (i + 1))
  with(sum_rel[rows,],
       points(
         reliability,
         cover,
         pch = rep(pchs[i+1], 9),
         col = cols,
         type = "b"
       ))
}
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
sum_ss <- summary_logreg %>% dplyr::filter(scen_no %in% c(1, 10:12))
sum_ss <- sum_ss[order(sum_ss$method, sum_ss$nobs),]
max(sum_ss$bias_mcse) < 0.01
max(sum_ss$mse_mcse) < 0.01
max(sum_ss$cover_mcse) < 0.01
sum_ss[(sum_ss$scen_no == 10 & sum_ss$method == "mecor"), "mse"]
sum_ss[(sum_ss$scen_no == 10 & sum_ss$method == "mecor"), "mse_mcse"]

pdf("./results/logreg_samplesize_percbias.pdf",
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
        xlim = c(0, 4000),
        ylim = c(-100, 20),
        asp = 4000 / 120
)
cols <- c(rep("black", 4))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_ss[rows,],
       points(
         nobs,
         perc_bias,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 4000, by = 500),
  labels = seq(from = 0, to = 4000, by = 500),
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
sum_ss_mse <-
  sum_ss %>%
  filter(!(scen_no == 10 & method == "mecor"))
pdf("./results/logreg_samplesize_mse.pdf",
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
        xlim = c(0, 4000),
        ylim = c(0, 0.05),
        asp = 4000 / 0.05
)
cols <- c(rep("black", 4))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in c(0,2)){
  rows <- (i * 4 + 1 - i * 0.5 ):(4 * (i + 1) - i * 0.5)
  with(sum_ss_mse[rows,],
       points(
         nobs,
         mse,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
rows <- 5:7 # only 8 scens
with(sum_ss_mse[rows,],
     points(
       nobs,
       mse,
       pch = rep(pchs[2], 4),
       col = c("black", "black", "grey"),
       type = "b"
     ))
axis(
  1,
  at = seq(from = 0, to = 4000, by = 500),
  labels = seq(from = 0, to = 4000, by = 500),
  las = 1
)
mtext("Sample Size",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 0.05, by = 0.01),
  labels = c(
    expression(0),
    expression(0.01),
    expression(0.02),
    expression(0.03),
    expression(0.04),
    expression(0.05)
  ),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
legend("topright",
       legend = c("Uncorrected", "RC", "Simex"),
       pch = c(19, 17, 8),
       title = expression(underline(Method)))
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
pdf("./results/logreg_samplesize_coverage.pdf",
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
        xlim = c(0, 4000),
        ylim = c(0, 1),
        asp = 4000 / 1
)
segments(x0 = 0, y0 = 0.95, x1 = 1000, col = "grey")
cols <- c(rep("black", 4))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_ss[rows,],
       points(
         nobs,
         cover,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 4000, by = 500),
  labels = seq(from = 0, to = 4000, by = 500),
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

##############################
# 3 - Number of replicates ---
##############################
# perc bias
sum_nrep <- summary_logreg %>% dplyr::filter(scen_no %in% c(1, 14:16))
sum_nrep <- sum_nrep[order(sum_nrep$method, sum_nrep$nrep),]
max(sum_nrep$bias_mcse) < 0.01
max(sum_nrep$mse_mcse) < 0.01
max(sum_nrep$cover_mcse) < 0.01


pdf("./results/logreg_nrep_percbias.pdf",
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
        xlim = c(0, 10),
        ylim = c(-100, 20),
        asp = 10 / 120
)
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_nrep[rows,],
       points(
         nrep,
         perc_bias,
         pch = rep(pchs[i+1], 4),
         col = "black",
         type = "b"
       ))
  with(sum_nrep[(i * 4 + 1),],
       points(
         nrep,
         perc_bias,
         pch = rep(pchs[i+1], 4),
         col = "grey",
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 10, by = 1),
  labels = seq(from = 0, to = 10, by = 1),
  las = 1
)
mtext("Number of replicates",
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
pdf("./results/logreg_nrep_mse.pdf",
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
        xlim = c(0, 10),
        ylim = c(0, 0.05),
        asp = 10 / 0.05
)
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_nrep[rows,],
       points(
         nrep,
         mse,
         pch = rep(pchs[i+1], 4),
         col = "black",
         type = "b"
       ))
  with(sum_nrep[(i * 4 + 1),],
       points(
         nrep,
         mse,
         pch = rep(pchs[i+1], 4),
         col = "grey",
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 10, by = 1),
  labels = seq(from = 0, to = 10, by = 1),
  las = 1
)
mtext("Number of replicates",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 0.05, by = 0.01),
  labels = c(
    expression(0),
    expression(0.01),
    expression(0.02),
    expression(0.03),
    expression(0.04),
    expression(0.05)
  ),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
legend("topright",
       legend = c("Uncorrected", "RC", "Simex"),
       pch = c(19, 17, 8),
       title = expression(underline(Method)))
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
pdf("./results/logreg_nrep_coverage.pdf",
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
        xlim = c(0, 10),
        ylim = c(0, 1),
        asp = 10 / 1
)
segments(x0 = 0, y0 = 0.95, x1 = 10, col = "grey")
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_nrep[rows,],
       points(
         nrep,
         cover,
         pch = rep(pchs[i+1], 4),
         col = "black",
         type = "b"
       ))
  with(sum_nrep[(i * 4 + 1),],
       points(
         nrep,
         cover,
         pch = rep(pchs[i+1], 4),
         col = "grey",
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 10, by = 2),
  labels = seq(from = 0, to = 10, by = 2),
  las = 1
)
mtext("Number of replicates",
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
# 3 - R-squared --------------
##############################
# perc bias
sum_rsq <- summary_logreg %>% dplyr::filter(scen_no %in% c(1, 17:19))
sum_rsq <- sum_rsq[order(sum_rsq$method, sum_rsq$r_squared_est),]
max(sum_rsq$bias_mcse) < 0.01
max(sum_rsq$mse_mcse) < 0.01
max(sum_rsq$cover_mcse) < 0.01

pdf("./results/logreg_rsq_percbias.pdf",
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
        xlim = c(0, 0.8),
        ylim = c(-100, 20),
        asp = 0.8 / 120
)
cols <- c(rep("black", 4))
cols[3] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_rsq[rows,],
       points(
         r_squared_est,
         perc_bias,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 0.8, by = 0.2),
  labels = seq(from = 0, to = 0.8, by = 0.2),
  las = 1
)
mtext("Pseudo R-Squared (Nagelkerke)",
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
pdf("./results/logreg_rsq_mse.pdf",
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
        xlim = c(0, 0.8),
        ylim = c(0, 0.05),
        asp = 0.8 / 0.05
)
cols <- c(rep("black", 4))
cols[3] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_rsq[rows,],
       points(
         r_squared_est,
         mse,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 0.8, by = 0.2),
  labels = seq(from = 0, to = 0.8, by = 0.2),
  las = 1
)
mtext("Pseudo R-Squared (Nagelkerke)",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = 0, to = 0.05, by = 0.01),
  labels = c(
    expression(0),
    expression(0.01),
    expression(0.02),
    expression(0.03),
    expression(0.04),
    expression(0.05)
  ),
  las = 1
)
mtext("Mean Squared Error",
      side = 2,
      line = 3,
      cex = 0.75)
legend("topright",
       legend = c("Uncorrected", "RC", "Simex"),
       pch = c(19, 17, 8),
       title = expression(underline(Method)))
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
pdf("./results/logreg_rsq_coverage.pdf",
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
        xlim = c(0, 0.8),
        ylim = c(0, 1),
        asp = 0.8 / 1
)
segments(x0 = 0, y0 = 0.95, x1 = 0.8, col = "grey")
cols <- c(rep("black", 4))
cols[3] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_rsq[rows,],
       points(
         r_squared_est,
         cover,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
axis(
  1,
  at = seq(from = 0, to = 0.8, by = 0.2),
  labels = seq(from = 0, to = 0.8, by = 0.2),
  las = 1
)
mtext("Pseudo R-Squared (Nagelkerke)",
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



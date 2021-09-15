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
sum_rel <- sum_rel[order(sum_rel$method, sum_rel$reliability),]
max(sum_rel$bias_mcse) < 0.01
max(sum_rel$mse_mcse) < 0.01
max(sum_rel$cover_mcse) < 0.01

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
        ylim = c(-80, 20),
        asp = 1 / 100
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
        at = seq(from = -80, to = 20, by = 20),
        labels = c(
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
        ylim = c(0, 0.05),
        asp = 1 / 0.05
)
cols <- c(rep("black", 9))
cols[4] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 9 + 1):(9 * (i + 1))
  with(sum_rel[rows,],
       points(
         reliability,
         mse,
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
sum_ss <- summary %>% dplyr::filter(scen_no %in% c(1, 13:15))
sum_ss <- sum_ss[order(sum_ss$method, sum_ss$nobs),]
max(sum_ss$bias_mcse) < 0.01
max(sum_ss$mse_mcse) < 0.01
max(sum_ss$cover_mcse) < 0.01

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
        ylim = c(-80, 20),
        asp = 1000 / 100
)
cols <- c(rep("black", 4))
cols[3] <- "grey"
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
        at = seq(from = -80, to = 20, by = 20),
        labels = c(
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
        ylim = c(0, 0.05),
        asp = 1000 / 0.05
)
cols <- c(rep("black", 4))
cols[3] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_ss[rows,],
       points(
         nobs,
         mse,
         pch = rep(pchs[i+1], 4),
         col = cols,
         type = "b"
       ))
}
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
segments(x0 = 0, y0 = 0.95, x1 = 1000, col = "grey")
cols <- c(rep("black", 4))
cols[3] <- "grey"
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

##############################
# 3 - Number of replicates ---
##############################
# perc bias
sum_nrep <- summary %>% dplyr::filter(scen_no %in% c(1, 20:22))
sum_nrep <- sum_nrep[order(sum_nrep$method, sum_nrep$nrep),]
max(sum_nrep$bias_mcse) < 0.005
max(sum_nrep$mse_mcse) < 0.005
max(sum_nrep$cover_mcse) < 0.01


pdf("./results/nrep_percbias.pdf",
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
        ylim = c(-80, 20),
        asp = 10 / 100
)
cols <- c(rep("black", 4))
cols[2] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_nrep[rows,],
       points(
         nrep,
         perc_bias,
         pch = rep(pchs[i+1], 4),
         col = cols,
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
  at = seq(from = -80, to = 20, by = 20),
  labels = c(
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
pdf("./results/nrep_mse.pdf",
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
cols <- c(rep("black", 4))
cols[2] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_nrep[rows,],
       points(
         nrep,
         mse,
         pch = rep(pchs[i+1], 4),
         col = cols,
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
pdf("./results/nrep_coverage.pdf",
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
cols <- c(rep("black", 4))
cols[2] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_nrep[rows,],
       points(
         nrep,
         cover,
         pch = rep(pchs[i+1], 4),
         col = cols,
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
sum_rsq <- summary %>% dplyr::filter(scen_no %in% c(1, 10:12))
sum_rsq <- sum_rsq[order(sum_rsq$method, sum_rsq$r_squared),]
max(sum_rsq$bias_mcse) < 0.01
max(sum_rsq$mse_mcse) < 0.01
max(sum_rsq$cover_mcse) < 0.01

pdf("./results/rsq_percbias.pdf",
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
        ylim = c(-80, 20),
        asp = 0.8 / 100
)
cols <- c(rep("black", 4))
cols[1] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_rsq[rows,],
       points(
         r_squared,
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
mtext("R-Squared",
      side = 1,
      line = 3,
      cex = 0.75)
axis(
  2,
  at = seq(from = -80, to = 20, by = 20),
  labels = c(
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
pdf("./results/rsq_mse.pdf",
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
cols[1] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_rsq[rows,],
       points(
         r_squared,
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
mtext("R-Squared",
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
pdf("./results/rsq_coverage.pdf",
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
cols[1] <- "grey"
pchs <- c(19, 17, 8)
for (i in 0:2){
  rows <- (i * 4 + 1):(4 * (i + 1))
  with(sum_rsq[rows,],
       points(
         r_squared,
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
mtext("R-Squared",
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



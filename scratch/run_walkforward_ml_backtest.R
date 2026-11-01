#!/usr/bin/env Rscript
# ==============================================================================
# scratch/run_walkforward_ml_backtest.R
# Walk-forward ElasticNet backtest (2006-10 -> 2026-10) using the production
# 27-feature pipeline, fed into the discrete event-driven portfolio engine.
#
# Anti-leakage design:
#   * Retrain every 6 months; rolling 10-year pooled training window.
#   * Training rows must have Date < retrain_date - 14 calendar days, so every
#     7-trading-day triple-barrier label is fully realized before the test period.
#   * cv.glmnet folds are contiguous time blocks (no random shuffling).
#   * Winsorization bounds come from the training set only.
#   * Prob at close(t) -> order at open(t+1) inside the engine.
# ==============================================================================
suppressPackageStartupMessages({ library(xts); library(glmnet) })
set.seed(42)

OUT <- "output"; ART <- "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c"
pz <- readRDS("scratch/wf_feature_panel.rds")
panel <- pz$panel; feats <- pz$feature_names
cat(sprintf("Panel: %d rows | %d symbols | %d features\n", nrow(panel), length(unique(panel$Symbol)), length(feats)))

X_all <- as.matrix(panel[, feats]); X_all[!is.finite(X_all)] <- NA
ok_feat <- complete.cases(X_all)

retrain_dates <- seq(as.Date("2006-07-01"), as.Date("2026-07-01"), by = "6 months")
TRAIN_YEARS <- 10; EMBARGO_DAYS <- 14; MAX_TRAIN <- 200000

panel$Prob <- NA_real_
diag_rows <- list(); coef_rows <- list()

auc_fn <- function(p, y) {
  ok <- !is.na(p) & !is.na(y); p <- p[ok]; y <- y[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0); if (n1 == 0 || n0 == 0) return(NA)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (as.numeric(n1) * as.numeric(n0))
}

for (i in seq_along(retrain_dates)) {
  R  <- retrain_dates[i]
  R2 <- if (i < length(retrain_dates)) retrain_dates[i + 1] else as.Date("2027-01-01")
  tr <- which(ok_feat & !is.na(panel$TargetBinary) &
              panel$Date <  R - EMBARGO_DAYS &
              panel$Date >= seq(R, by = sprintf("-%d years", TRAIN_YEARS), length.out = 2)[2])
  te <- which(ok_feat & panel$Date >= R & panel$Date < R2)
  if (length(tr) < 5000 || length(te) == 0) next
  if (length(tr) > MAX_TRAIN) tr <- sort(sample(tr, MAX_TRAIN))

  Xtr <- X_all[tr, , drop = FALSE]; ytr <- panel$TargetBinary[tr]
  lo <- apply(Xtr, 2, quantile, 0.01, na.rm = TRUE); hi <- apply(Xtr, 2, quantile, 0.99, na.rm = TRUE)
  clip <- function(X) { for (j in seq_len(ncol(X))) X[, j] <- pmin(pmax(X[, j], lo[j]), hi[j]); X }
  Xtr <- clip(Xtr); Xte <- clip(X_all[te, , drop = FALSE])

  # Contiguous time-block folds
  d_tr  <- panel$Date[tr]
  qs    <- quantile(as.numeric(d_tr), probs = seq(0, 1, length.out = 6))
  foldid <- pmin(5, findInterval(as.numeric(d_tr), qs, rightmost.closed = TRUE))
  foldid[foldid < 1] <- 1

  cvf <- cv.glmnet(Xtr, ytr, family = "binomial", alpha = 0.5, foldid = foldid, type.measure = "deviance")
  pr  <- as.numeric(predict(cvf, Xte, s = "lambda.min", type = "response"))
  panel$Prob[te] <- pr

  cf <- as.matrix(coef(cvf, s = "lambda.min"))[-1, 1]
  coef_rows[[i]] <- data.frame(Retrain = R, Feature = names(cf), Coef = as.numeric(cf))
  yte <- panel$TargetBinary[te]
  diag_rows[[i]] <- data.frame(
    Retrain = R, Train_N = length(tr), Test_N = length(te),
    Train_Base = mean(ytr), Test_Base = mean(yte, na.rm = TRUE),
    NonZero = sum(cf != 0), OOS_AUC = auc_fn(pr, yte),
    Mean_Prob = mean(pr), Pct_ge_058 = mean(pr >= 0.58) * 100, Pct_ge_065 = mean(pr >= 0.65) * 100,
    IC_Ret = suppressWarnings(cor(pr, panel$TargetRet[te], method = "spearman", use = "complete.obs")))
  cat(sprintf("[%s] train %6d | test %6d | nz %2d | OOS AUC %.3f | IC %+.3f | mean p %.3f | p>=.58 %4.1f%%\n",
              R, length(tr), length(te), sum(cf != 0), diag_rows[[i]]$OOS_AUC, diag_rows[[i]]$IC_Ret,
              mean(pr), mean(pr >= 0.58) * 100))
}
diag_df <- do.call(rbind, diag_rows); coef_df <- do.call(rbind, coef_rows)
write.csv(diag_df, file.path(OUT, "wf_ml_retrain_diagnostics.csv"), row.names = FALSE)
write.csv(coef_df, file.path(OUT, "wf_ml_coefficients.csv"), row.names = FALSE)

oos <- panel[!is.na(panel$Prob) & panel$Date >= as.Date("2006-10-02"), ]
cat(sprintf("\nPooled OOS: %d preds | AUC %.4f | Spearman IC vs 7d ret %+.4f\n", nrow(oos),
            auc_fn(oos$Prob, oos$TargetBinary),
            cor(oos$Prob, oos$TargetRet, method = "spearman", use = "complete.obs")))
oos$Decile <- cut(rank(oos$Prob, ties.method = "first"), 10, labels = FALSE)
calib <- aggregate(cbind(Prob, TargetBinary, TargetRet) ~ Decile, data = oos, FUN = mean)
calib$TargetRet <- calib$TargetRet * 100
colnames(calib) <- c("Decile", "Mean_Prob", "Hit_Rate", "Mean_7d_Ret_Pct")
cat("\nOOS calibration by probability decile:\n"); print(round(calib, 4), row.names = FALSE)
write.csv(calib, file.path(OUT, "wf_ml_calibration_deciles.csv"), row.names = FALSE)

# Feature retention over retrains
ret_tab <- aggregate(Coef ~ Feature, data = coef_df, FUN = function(x) c(Retained = mean(x != 0) * 100, MeanCoef = mean(x)))
ret_tab <- data.frame(Feature = ret_tab$Feature, Retained_Pct = ret_tab$Coef[, 1], Mean_Coef = ret_tab$Coef[, 2])
ret_tab <- ret_tab[order(-ret_tab$Retained_Pct, -abs(ret_tab$Mean_Coef)), ]
cat("\nFeature retention across retrains:\n"); print(ret_tab, row.names = FALSE, digits = 3)
write.csv(ret_tab, file.path(OUT, "wf_ml_feature_retention.csv"), row.names = FALSE)

# ==============================================================================
# PORTFOLIO SIMULATION
# ==============================================================================
cat("\n[Loading discrete engine...]\n")
source("scratch/engine_20yr_discrete.R")
b_rds <- readRDS("data/intraday_breadth_10d.rds")
m_b <- merge(xts(rep(NA_real_, length(eval_dates)), order.by = eval_dates), b_rds)
m_b <- na.locf(m_b, na.rm = FALSE)
breadth_eval <- as.numeric(m_b[eval_dates, 2]); breadth_eval[is.na(breadth_eval)] <- 0

proxy_list <- stocks_list
ml_list <- stocks_list
n_map <- 0
for (sym in names(ml_list)) {
  sub <- oos[oos$Symbol == sym, c("Date", "Prob")]
  pv <- rep(0.50, length(ml_list[[sym]]$dates))
  if (nrow(sub) > 0) {
    m <- match(ml_list[[sym]]$dates, sub$Date)
    pv[!is.na(m)] <- sub$Prob[m[!is.na(m)]]
    n_map <- n_map + sum(!is.na(m))
  }
  ml_list[[sym]]$prob <- pv
}
cat(sprintf("Mapped %d OOS model probabilities into %d engine stock profiles.\n", n_map, length(ml_list)))

runs <- list()
HURDLE_BULL <- 0.58; HURDLE_BEAR <- 0.65
cat("Run 1: Hand-tuned proxy (reference), 1.5x + hedge + breadth\n")
runs[["Proxy score (prior engine)"]] <- run_20yr_sim(proxy_list, eval_dates, max_positions = 5, use_bear_hedge = TRUE, use_breadth = TRUE)
cat("Run 2: ElasticNet WF, live hurdles 0.58/0.65, 1.5x + hedge + breadth\n")
runs[["ML WF, hurdles .58/.65, breadth"]] <- run_20yr_sim(ml_list, eval_dates, max_positions = 5, use_bear_hedge = TRUE, use_breadth = TRUE)
cat("Run 3: ElasticNet WF, live hurdles, NO breadth\n")
runs[["ML WF, hurdles .58/.65, no breadth"]] <- run_20yr_sim(ml_list, eval_dates, max_positions = 5, use_bear_hedge = TRUE, use_breadth = FALSE)
HURDLE_BULL <- 0.55; HURDLE_BEAR <- 0.60
cat("Run 4: ElasticNet WF, relaxed hurdles 0.55/0.60, breadth\n")
runs[["ML WF, hurdles .55/.60, breadth"]] <- run_20yr_sim(ml_list, eval_dates, max_positions = 5, use_bear_hedge = TRUE, use_breadth = TRUE)
HURDLE_BULL <- 0.58; HURDLE_BEAR <- 0.65
cat("Run 5: ElasticNet WF, live hurdles, 1.0x cash, breadth\n")
runs[["ML WF, 1.0x cash, breadth"]] <- run_20yr_sim(ml_list, eval_dates, max_positions = 5, use_bear_hedge = TRUE, use_breadth = TRUE, leverage = 1.0)

bm <- function(r) {
  c_r <- cumprod(1 + r); n <- length(r); dd <- c_r / cummax(c_r) - 1
  list(terminal = 10000 * tail(c_r, 1), cagr = (tail(c_r, 1)^(252 / n) - 1) * 100,
       max_dd = abs(min(dd)) * 100, vol = sd(r) * sqrt(252) * 100,
       sharpe = mean(r - rf_sub) / sd(r) * sqrt(252), equity = 10000 * c_r, dd = dd * 100)
}
st <- c(lapply(runs, function(x) bm(x$daily_returns)),
        list("QQQ Buy & Hold" = bm(qqq_sub), "SPY Buy & Hold" = bm(spy_sub)))
score <- data.frame(
  Strategy = names(st),
  Terminal = sapply(st, function(s) sprintf("$%s", format(round(s$terminal), big.mark = ","))),
  CAGR = sapply(st, function(s) sprintf("%+.2f%%", s$cagr)),
  MaxDD = sapply(st, function(s) sprintf("-%.2f%%", s$max_dd)),
  Vol = sapply(st, function(s) sprintf("%.1f%%", s$vol)),
  Sharpe = sapply(st, function(s) sprintf("%.2f", s$sharpe)),
  Trades = c(sapply(runs, `[[`, "total_trades"), NA, NA),
  WinRate = c(sprintf("%.1f%%", sapply(runs, `[[`, "win_rate")), "-", "-"),
  row.names = NULL)
cat("\n=== WALK-FORWARD ML 20-YEAR BACKTEST ===\n"); print(score, row.names = FALSE)
write.csv(score, file.path(OUT, "wf_ml_20yr_scorecard.csv"), row.names = FALSE)

years <- unique(format(eval_dates, "%Y"))
yr <- function(r) sapply(years, function(y) (prod(1 + r[format(eval_dates, "%Y") == y]) - 1) * 100)
yr_df <- data.frame(Year = years,
  Proxy = sprintf("%+.1f%%", yr(runs[[1]]$daily_returns)),
  ML_Live = sprintf("%+.1f%%", yr(runs[[2]]$daily_returns)),
  ML_Relaxed = sprintf("%+.1f%%", yr(runs[[4]]$daily_returns)),
  QQQ = sprintf("%+.1f%%", yr(qqq_sub)), SPY = sprintf("%+.1f%%", yr(spy_sub)))
cat("\n=== CALENDAR RETURNS ===\n"); print(yr_df, row.names = FALSE)
write.csv(yr_df, file.path(OUT, "wf_ml_20yr_calendar.csv"), row.names = FALSE)

d1 <- eval_dates <= as.Date("2016-10-02")
dec <- data.frame(Strategy = names(st),
  D1_CAGR = sapply(c(lapply(runs, `[[`, "daily_returns"), list(qqq_sub, spy_sub)), function(r) sprintf("%+.2f%%", bm(r[d1])$cagr)),
  D2_CAGR = sapply(c(lapply(runs, `[[`, "daily_returns"), list(qqq_sub, spy_sub)), function(r) sprintf("%+.2f%%", bm(r[!d1])$cagr)), row.names = NULL)
cat("\n=== DECADES ===\n"); print(dec, row.names = FALSE)
write.csv(dec, file.path(OUT, "wf_ml_20yr_decades.csv"), row.names = FALSE)

# ==============================================================================
# CHARTS
# ==============================================================================
chart <- file.path(OUT, "wf_ml_20yr_performance_chart.png")
png(chart, width = 1600, height = 1350, res = 140)
layout(matrix(c(1, 1, 2, 2, 3, 4), ncol = 2, byrow = TRUE), heights = c(1.4, 0.8, 0.95))
par(mar = c(2.8, 5, 2.8, 1.5))
cols <- c("#757575", "#ef6c00", "#fbc02d", "#0d47a1", "#2e7d32", "#c62828", "#000000")
ltys <- c(3, 1, 2, 1, 4, 1, 2); lwds <- c(1.5, 2.6, 1.6, 2.0, 1.5, 2.2, 1.4)
yl <- range(unlist(lapply(st, `[[`, "equity")))
plot(eval_dates, st[[1]]$equity, type = "n", log = "y", ylim = c(yl[1] * 0.9, yl[2] * 3),
     main = "Walk-Forward ElasticNet (27 features, 6-mo retrain, OOS only): Equity on $10,000 (log)",
     ylab = "Equity ($, log)", xlab = "")
grid(col = "gray88", lty = 1)
for (i in seq_along(st)) lines(eval_dates, st[[i]]$equity, col = cols[i], lty = ltys[i], lwd = lwds[i])
legend("topleft", bg = "white", cex = 0.74, col = cols, lty = ltys, lwd = lwds,
       legend = sprintf("%s: $%s | %+.2f%% CAGR | MaxDD -%.1f%% | Sharpe %.2f", names(st),
                        sapply(st, function(s) format(round(s$terminal), big.mark = ",")),
                        sapply(st, `[[`, "cagr"), sapply(st, `[[`, "max_dd"), sapply(st, `[[`, "sharpe")))
plot(eval_dates, st[[2]]$dd, type = "l", col = cols[2], lwd = 1.8, ylim = c(-80, 0),
     main = "Underwater Drawdown (%)", ylab = "Drawdown (%)", xlab = "")
grid(col = "gray88", lty = 1)
lines(eval_dates, st[[1]]$dd, col = cols[1], lwd = 1.3, lty = 3)
lines(eval_dates, st[["QQQ Buy & Hold"]]$dd, col = cols[6], lwd = 1.5)
legend("bottomleft", horiz = TRUE, bg = "white", cex = 0.8, col = cols[c(2, 1, 6)], lty = c(1, 3, 1),
       legend = c("ML WF (live hurdles)", "Proxy score", "QQQ"))
par(mar = c(4.2, 5, 2.8, 1.5))
plot(diag_df$Retrain, diag_df$OOS_AUC, type = "b", pch = 19, col = "#0d47a1", ylim = c(0.45, 0.62),
     main = "Out-of-Sample AUC per 6-Month Test Period", ylab = "AUC", xlab = "Retrain date")
abline(h = 0.5, col = "red", lty = 2); grid(col = "gray88", lty = 1)
barplot(calib$Hit_Rate * 100, names.arg = calib$Decile, col = "#ef6c00", border = NA,
        ylim = c(0, max(calib$Hit_Rate * 100) * 1.15),
        main = "OOS Hit Rate by Predicted-Probability Decile", ylab = "Realized label = 1 (%)", xlab = "Decile (1 = lowest prob)")
abline(h = mean(oos$TargetBinary, na.rm = TRUE) * 100, col = "red", lty = 2)
dev.off()
file.copy(chart, file.path(ART, "wf_ml_20yr_performance_chart.png"), overwrite = TRUE)
saveRDS(list(score = score, diag = diag_df, calib = calib, retention = ret_tab, yr = yr_df, dec = dec,
             returns = lapply(runs, `[[`, "daily_returns"), dates = eval_dates),
        file.path(OUT, "wf_ml_20yr_results.rds"))
cat("\nChart saved:", chart, "\n")

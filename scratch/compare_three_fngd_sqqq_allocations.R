suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/02_trend_lmMA.R")
source("R/08_metrics.R")

cat("========================================================================================\n")
cat(" 10-YEAR $10,000 BACKTEST: COMPARING 3 FNGD + SQQQ INVERSE SLEEVES\n")
cat(" (1) 75% FNGD + 25% SQQQ\n")
cat(" (2) 50% FNGD + 50% SQQQ\n")
cat(" (3) 25% FNGD + 75% SQQQ\n")
cat(" Rules: 15-Day Time Stop -> Rotate to SGOV | Circuit Breaker: -12.0%\n")
cat(" Period: 2,520 Trading Days (2016 - 2026)\n")
cat("========================================================================================\n\n")

symbols <- c("QQQ", "GLD", "BIL", "SGOV", "SQQQ", "FNGD")
raw_data <- list()
for (s in symbols) {
  raw_data[[s]] <- suppressWarnings(getSymbols(s, src="yahoo", from="2010-01-01", auto.assign=FALSE))
}

ret_data <- list()
for (s in symbols) {
  ad <- Ad(raw_data[[s]])
  ret_data[[s]] <- na.omit(ad / lag.xts(ad, 1) - 1)
}

sgov_spliced <- ret_data[["BIL"]]
sgov_common  <- intersect(index(ret_data[["SGOV"]]), index(sgov_spliced))
sgov_spliced[sgov_common] <- ret_data[["SGOV"]][sgov_common]

qqq_lm <- lmMA(Cl(raw_data[["QQQ"]]), n = 50)
gld_lm <- lmMA(Cl(raw_data[["GLD"]]), n = 50)

common_10y <- sort(intersect(intersect(intersect(
  index(ret_data[["QQQ"]]), index(ret_data[["GLD"]])), index(sgov_spliced)),
  index(ret_data[["SQQQ"]])))
common_10y <- sort(intersect(common_10y, index(na.omit(qqq_lm$fit))))
common_10y <- sort(intersect(common_10y, index(na.omit(gld_lm$fit))))

eval_dates <- tail(common_10y, 2520)
n_total <- length(eval_dates)

qqq_v  <- as.numeric(ret_data[["QQQ"]][eval_dates])
gld_v  <- as.numeric(ret_data[["GLD"]][eval_dates])
sgov_v <- as.numeric(sgov_spliced[eval_dates])
sqqq_v <- as.numeric(ret_data[["SQQQ"]][eval_dates])

fngd_ret <- ret_data[["FNGD"]]
fngd_v <- numeric(n_total)
for (i in 1:n_total) {
  d <- eval_dates[i]
  if (d %in% index(fngd_ret)) {
    fngd_v[i] <- as.numeric(fngd_ret[d])
  } else {
    fngd_v[i] <- sqqq_v[i]
  }
}

qqq_cl_v   <- as.numeric(Cl(raw_data[["QQQ"]])[eval_dates])
qqq_fit_v  <- as.numeric(qqq_lm$fit[eval_dates])
qqq_slp_v  <- as.numeric(qqq_lm$slope[eval_dates])
macro_bull <- (qqq_cl_v >= qqq_fit_v) & (qqq_slp_v > 0)

gld_cl_v   <- as.numeric(Cl(raw_data[["GLD"]])[eval_dates])
gld_fit_v  <- as.numeric(gld_lm$fit[eval_dates])
gld_slp_v  <- as.numeric(gld_lm$slope[eval_dates])
gld_bull   <- (gld_cl_v >= gld_fit_v) & (gld_slp_v > 0)

# Base allocations in Dual Bear
allocs <- list(
  "75% FNGD + 25% SQQQ" = 0.75 * fngd_v + 0.25 * sqqq_v,
  "50% FNGD + 50% SQQQ" = 0.50 * fngd_v + 0.50 * sqqq_v,
  "25% FNGD + 75% SQQQ" = 0.25 * fngd_v + 0.75 * sqqq_v
)

# Load swing trades walk-forward cache
CACHE_DIR_10Y <- "data/cache_10yr"
SECTOR_MAP <- if (file.exists("sector_map.json")) jsonlite::fromJSON("sector_map.json") else list()
get_sym_sector <- function(s) {
  if (!is.null(SECTOR_MAP[[s]])) return(SECTOR_MAP[[s]])
  return("General_Tech")
}
cache_files <- list.files(CACHE_DIR_10Y, pattern = "_10yr_wf[.]rds$", full.names = TRUE)
sim_data <- list()
for (cf in cache_files) {
  obj <- readRDS(cf)
  sim_data[[obj$symbol]] <- obj
}
avail_syms <- names(sim_data)
n_syms <- length(avail_syms)

mat_prob  <- matrix(0.5,  nrow = n_total, ncol = n_syms)
mat_rs    <- matrix(0.0,  nrow = n_total, ncol = n_syms)
mat_vol   <- matrix(0.25, nrow = n_total, ncol = n_syms)
mat_ret_b <- matrix(0.0,  nrow = n_total, ncol = n_syms)
mat_valid <- matrix(FALSE, nrow = n_total, ncol = n_syms)

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- sim_data[[s]]
  idx_p <- match(eval_dates, index(obj$pred_prob))
  v_p   <- !is.na(idx_p)
  if (any(v_p)) mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
  idx_rs <- match(eval_dates, index(obj$rs_spy))
  v_rs   <- !is.na(idx_rs)
  if (any(v_rs)) mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
  idx_v <- match(eval_dates, index(obj$garch_vol))
  v_v   <- !is.na(idx_v)
  if (any(v_v)) {
    raw_v <- as.numeric(obj$garch_vol)[idx_v[v_v]]
    raw_v[is.na(raw_v) | raw_v < 0.05] <- 0.25
    mat_vol[v_v, j] <- raw_v
  }
  idx_b <- match(eval_dates, index(obj$bracket_ret))
  v_b   <- !is.na(idx_b)
  if (any(v_b)) {
    mat_ret_b[v_b, j] <- as.numeric(obj$bracket_ret)[idx_b[v_b]]
    mat_valid[v_b, j] <- TRUE
  }
}
sym_sectors <- sapply(avail_syms, get_sym_sector)

# Timing function for cash sleeve:
# Can simulate:
# (a) pure 15-day time stop
# (b) 15-day time stop + -10% loss stop
get_cash_series <- function(base_fb, max_days = 15, max_loss = NULL) {
  cash_ret <- numeric(n_total)
  in_ep <- FALSE
  ep_d <- 0
  ep_cum_ret <- 1.0
  rot_active <- FALSE
  rot_days <- 0
  rot_spells <- 0
  
  for (t in 1:n_total) {
    if (macro_bull[t]) {
      cash_ret[t] <- qqq_v[t]
      in_ep <- FALSE
      ep_d <- 0
      ep_cum_ret <- 1.0
      rot_active <- FALSE
    } else if (gld_bull[t]) {
      cash_ret[t] <- gld_v[t]
      in_ep <- FALSE
      ep_d <- 0
      ep_cum_ret <- 1.0
      rot_active <- FALSE
    } else {
      # Dual Bear
      if (!in_ep) {
        in_ep <- TRUE
        ep_d <- 0
        ep_cum_ret <- 1.0
        rot_active <- FALSE
      }
      ep_d <- ep_d + 1
      
      if (rot_active) {
        cash_ret[t] <- sgov_v[t]
        rot_days <- rot_days + 1
      } else {
        day_r <- base_fb[t]
        ep_cum_ret <- ep_cum_ret * (1 + day_r)
        
        # Check triggers
        trigger <- FALSE
        if (!is.null(max_days) && ep_d >= max_days) trigger <- TRUE
        if (!is.null(max_loss) && (ep_cum_ret - 1.0) <= -max_loss) trigger <- TRUE
        
        cash_ret[t] <- day_r
        if (trigger) {
          rot_active <- TRUE
          rot_spells <- rot_spells + 1
        }
      }
    }
  }
  list(series = cash_ret, rot_days = rot_days, rot_spells = rot_spells)
}

# Portfolio simulation engine
sim_portfolio_engine <- function(cash_series, cb_thresh = 0.12) {
  port_ret <- numeric(n_total)
  peak_equity <- 1.0
  cur_equity <- 1.0
  cb_active_days <- 0
  
  for (t in 1:n_total) {
    dd <- (cur_equity - peak_equity) / peak_equity
    is_cb_active <- if (!is.null(cb_thresh)) (dd <= -cb_thresh) else FALSE
    if (is_cb_active) cb_active_days <- cb_active_days + 1
    
    cb_multiplier <- if (is_cb_active) 0.50 else 1.0
    
    is_bull <- macro_bull[t]
    eff_max_pos <- if (is_bull) 3 else 2
    eff_p_long  <- if (is_bull) 0.58 else 0.65
    
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    val_flg <- mat_valid[t, ]
    
    qualify <- which(val_flg & p_ups >= eff_p_long & rs_vals > 0)
    top_idx <- integer(0)
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      ranked <- qualify[order(-scores)]
      sec_cnts <- list()
      for (cand in ranked) {
        c_sec <- sym_sectors[cand]
        cnt <- if (is.null(sec_cnts[[c_sec]])) 0 else sec_cnts[[c_sec]]
        if (cnt < 3) {
          top_idx <- c(top_idx, cand)
          sec_cnts[[c_sec]] <- cnt + 1
          if (length(top_idx) >= eff_max_pos) break
        }
      }
    }
    
    n_sel <- length(top_idx)
    cash_day_ret <- cash_series[t]
    
    if (n_sel > 0) {
      inv_v <- 1 / vols[top_idx]
      weights <- inv_v / sum(inv_v)
      raw_swing_exp <- min(1.0, n_sel / 3.0)
      swing_exposure <- raw_swing_exp * cb_multiplier
      swing_ret <- sum(weights * rets_b[top_idx])
      port_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_day_ret
    } else {
      port_ret[t] <- cash_day_ret
    }
    
    cur_equity <- cur_equity * (1 + port_ret[t])
    if (cur_equity > peak_equity) peak_equity <- cur_equity
  }
  
  eq <- cumprod(1 + port_ret)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  ann_vol <- sd(port_ret) * sqrt(252)
  sharpe <- (mean(port_ret) * 252 - 0.04) / ann_vol
  final_10k <- 10000 * tail(eq, 1)
  calmar <- cagr / abs(mdd)
  
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe, AnnVol = ann_vol, 
       Calmar = calmar, CB_Days = cb_active_days, equity_curve = eq)
}

# Standalone Cash Sleeve Engine (No swings, 100% Cash / Hedge sleeve)
sim_standalone_engine <- function(cash_series) {
  eq <- cumprod(1 + cash_series)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  ann_vol <- sd(cash_series) * sqrt(252)
  sharpe <- (mean(cash_series) * 252 - 0.04) / ann_vol
  final_10k <- 10000 * tail(eq, 1)
  calmar <- cagr / abs(mdd)
  list(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe, AnnVol = ann_vol, 
       Calmar = calmar, equity_curve = eq)
}

# Run both modes: Pure 15-Day Time Stop & 15-Day Time Stop + -10% Loss Stop
modes <- list(
  "Pure 15-Day Time Stop" = list(max_days = 15, max_loss = NULL),
  "15-Day Time Stop + -10% Loss Stop" = list(max_days = 15, max_loss = 0.10)
)

port_results <- list()
stand_results <- list()
equity_curves <- list()

for (m_name in names(modes)) {
  m_cfg <- modes[[m_name]]
  for (a_name in names(allocs)) {
    fb_vec <- allocs[[a_name]]
    cs_obj <- get_cash_series(fb_vec, max_days = m_cfg$max_days, max_loss = m_cfg$max_loss)
    
    # Portfolio simulation
    p_sim <- sim_portfolio_engine(cs_obj$series, cb_thresh = 0.12)
    # Standalone simulation
    s_sim <- sim_standalone_engine(cs_obj$series)
    
    key <- sprintf("%s | %s", m_name, a_name)
    equity_curves[[key]] <- p_sim$equity_curve
    
    port_results[[key]] <- data.frame(
      Mode = m_name,
      Allocation = a_name,
      Final_10k = p_sim$Final_10k,
      CAGR = p_sim$CAGR * 100,
      MaxDD = p_sim$MaxDD * 100,
      Sharpe = p_sim$Sharpe,
      AnnVol = p_sim$AnnVol * 100,
      Calmar = p_sim$Calmar,
      CB_Days = p_sim$CB_Days,
      CB_Pct = p_sim$CB_Days / n_total * 100,
      Rot_Days = cs_obj$rot_days,
      stringsAsFactors = FALSE
    )
    
    stand_results[[key]] <- data.frame(
      Mode = m_name,
      Allocation = a_name,
      Final_10k = s_sim$Final_10k,
      CAGR = s_sim$CAGR * 100,
      MaxDD = s_sim$MaxDD * 100,
      Sharpe = s_sim$Sharpe,
      AnnVol = s_sim$AnnVol * 100,
      Calmar = s_sim$Calmar,
      stringsAsFactors = FALSE
    )
  }
}

df_port <- do.call(rbind, port_results)
df_stand <- do.call(rbind, stand_results)

# Save results to output
dir.create("output", showWarnings = FALSE)
write.csv(df_port, "output/compare_fngd_sqqq_portfolio.csv", row.names = FALSE)
write.csv(df_stand, "output/compare_fngd_sqqq_standalone.csv", row.names = FALSE)

cat("\n========================================================================================\n")
cat(" 1. FULL PORTFOLIO MODEL RESULTS (SWING TRADING + DYNAMIC DEFENSIVE SLEEVE)\n")
cat(" Starting Capital: $10,000 | 10 Years (2016 - 2026) | Circuit Breaker: -12.0%\n")
cat("========================================================================================\n\n")

print_df <- df_port
print_df$Final_10k_Str <- sprintf("$%.2fM", print_df$Final_10k / 1e6)
print_df$CAGR_Str <- sprintf("%+.2f%%", print_df$CAGR)
print_df$MaxDD_Str <- sprintf("%.2f%%", print_df$MaxDD)
print_df$Sharpe_Str <- sprintf("%.2f", print_df$Sharpe)
print_df$AnnVol_Str <- sprintf("%.1f%%", print_df$AnnVol)
print_df$Calmar_Str <- sprintf("%.2f", print_df$Calmar)
print_df$CB_Str <- sprintf("%d d (%.1f%%)", print_df$CB_Days, print_df$CB_Pct)

hdr <- sprintf("%-35s | %-22s | %14s | %10s | %10s | %8s | %8s | %8s | %15s",
               "Protection Mode", "Inverse Allocation", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Ann Vol", "Calmar", "CB Active Days")
cat(hdr, "\n", paste(rep("-", nchar(hdr)), collapse=""), "\n")

for (i in 1:nrow(print_df)) {
  cat(sprintf("%-35s | %-22s | %14s | %10s | %10s | %8s | %8s | %8s | %15s\n",
              print_df$Mode[i], print_df$Allocation[i], print_df$Final_10k_Str[i],
              print_df$CAGR_Str[i], print_df$MaxDD_Str[i], print_df$Sharpe_Str[i],
              print_df$AnnVol_Str[i], print_df$Calmar_Str[i], print_df$CB_Str[i]))
}

cat("\n\n========================================================================================\n")
cat(" 2. STANDALONE DEFENSIVE CASH ENGINE (0% SWINGS, 100% CASH/HEDGE SLEEVE)\n")
cat(" Starting Capital: $10,000 | 10 Years (2016 - 2026)\n")
cat("========================================================================================\n\n")

print_s <- df_stand
print_s$Final_10k_Str <- sprintf("$%.2f", print_s$Final_10k)
print_s$CAGR_Str <- sprintf("%+.2f%%", print_s$CAGR)
print_s$MaxDD_Str <- sprintf("%.2f%%", print_s$MaxDD)
print_s$Sharpe_Str <- sprintf("%.2f", print_s$Sharpe)
print_s$AnnVol_Str <- sprintf("%.1f%%", print_s$AnnVol)
print_s$Calmar_Str <- sprintf("%.2f", print_s$Calmar)

hdr_s <- sprintf("%-35s | %-22s | %14s | %10s | %10s | %8s | %8s | %8s",
                 "Protection Mode", "Inverse Allocation", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe", "Ann Vol", "Calmar")
cat(hdr_s, "\n", paste(rep("-", nchar(hdr_s)), collapse=""), "\n")

for (i in 1:nrow(print_s)) {
  cat(sprintf("%-35s | %-22s | %14s | %10s | %10s | %8s | %8s | %8s\n",
              print_s$Mode[i], print_s$Allocation[i], print_s$Final_10k_Str[i],
              print_s$CAGR_Str[i], print_s$MaxDD_Str[i], print_s$Sharpe_Str[i],
              print_s$AnnVol_Str[i], print_s$Calmar_Str[i]))
}

# Generate Comparison Chart
png("output/compare_fngd_sqqq_allocations.png", width = 1200, height = 750, res = 120)
par(mfrow = c(2, 1), mar = c(3, 4.5, 2.5, 1.5))

# Plot 1: Full Portfolio Equity Curves (Pure 15-Day Time Stop)
p_keys <- c(
  "Pure 15-Day Time Stop | 75% FNGD + 25% SQQQ",
  "Pure 15-Day Time Stop | 50% FNGD + 50% SQQQ",
  "Pure 15-Day Time Stop | 25% FNGD + 75% SQQQ"
)
colors <- c("#0072B2", "#009E73", "#D55E00")

y_max <- max(sapply(p_keys, function(k) max(equity_curves[[k]]))) * 10000
plot(eval_dates, equity_curves[[p_keys[1]]] * 10000, type = "l", col = colors[1], lwd = 2.2, log = "y",
     main = "10-Year Portfolio Growth ($10k Starting, Log Scale, -12% Circuit Breaker, 15-Day Time Stop)",
     ylab = "Portfolio Value ($)", xlab = "", ylim = c(5000, y_max * 1.5))
grid()
lines(eval_dates, equity_curves[[p_keys[2]]] * 10000, col = colors[2], lwd = 2.0)
lines(eval_dates, equity_curves[[p_keys[3]]] * 10000, col = colors[3], lwd = 2.0)

legend("topleft", legend = c(
  sprintf("75%% FNGD + 25%% SQQQ: $%.2fM (CAGR +%.1f%%, MaxDD %.1f%%, Sh %.2f)", 
          df_port$Final_10k[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "75% FNGD + 25% SQQQ"] / 1e6,
          df_port$CAGR[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "75% FNGD + 25% SQQQ"],
          df_port$MaxDD[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "75% FNGD + 25% SQQQ"],
          df_port$Sharpe[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "75% FNGD + 25% SQQQ"]),
  sprintf("50%% FNGD + 50%% SQQQ: $%.2fM (CAGR +%.1f%%, MaxDD %.1f%%, Sh %.2f)", 
          df_port$Final_10k[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "50% FNGD + 50% SQQQ"] / 1e6,
          df_port$CAGR[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "50% FNGD + 50% SQQQ"],
          df_port$MaxDD[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "50% FNGD + 50% SQQQ"],
          df_port$Sharpe[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "50% FNGD + 50% SQQQ"]),
  sprintf("25%% FNGD + 75%% SQQQ: $%.2fM (CAGR +%.1f%%, MaxDD %.1f%%, Sh %.2f)", 
          df_port$Final_10k[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "25% FNGD + 75% SQQQ"] / 1e6,
          df_port$CAGR[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "25% FNGD + 75% SQQQ"],
          df_port$MaxDD[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "25% FNGD + 75% SQQQ"],
          df_port$Sharpe[df_port$Mode == "Pure 15-Day Time Stop" & df_port$Allocation == "25% FNGD + 75% SQQQ"])
), col = colors, lwd = 2.2, bty = "n", cex = 0.85)

# Plot 2: Drawdowns across time
plot(eval_dates, (equity_curves[[p_keys[1]]] - cummax(equity_curves[[p_keys[1]]])) / cummax(equity_curves[[p_keys[1]]]) * 100,
     type = "l", col = colors[1], lwd = 1.8, ylim = c(-35, 0),
     main = "Portfolio Drawdown (%) Across 10-Year Horizon", ylab = "Drawdown (%)", xlab = "")
grid()
lines(eval_dates, (equity_curves[[p_keys[2]]] - cummax(equity_curves[[p_keys[2]]])) / cummax(equity_curves[[p_keys[2]]]) * 100,
      col = colors[2], lwd = 1.6)
lines(eval_dates, (equity_curves[[p_keys[3]]] - cummax(equity_curves[[p_keys[3]]])) / cummax(equity_curves[[p_keys[3]]]) * 100,
      col = colors[3], lwd = 1.6)
abline(h = -12.0, col = "red", lty = 2, lwd = 1.5)
text(x = eval_dates[100], y = -14.0, labels = "-12.0% Circuit Breaker Threshold", col = "red", cex = 0.8, adj = 0)

dev.off()
cat("\nChart saved to output/compare_fngd_sqqq_allocations.png\n")


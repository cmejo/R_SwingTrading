#!/usr/bin/env Rscript
# ==============================================================================
# Holding Period Optimization & Risk Analysis Backtest Engine
# Compares Max Hold Days: 5 Days, 7 Days, 10 Days, 15 Days, 20 Days
# Across 4 Universes: symbols_broad.txt, symbols_all2.txt, symbols_300.txt, symbols.txt
# Across 4 Horizons: 6 Months (126 bars), 12 Months (252 bars), 24 Months (504 bars), 60 Months (1,260 bars)
# ==============================================================================

suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
})

source("R/01_data_loader.R")
source("R/08_metrics.R")

OUTPUT_DIR   <- "output"
CACHE_DIR    <- "data/cache_wf"
ARTIFACT_DIR <- "/Users/cmejo/.gemini/antigravity/brain/785859a9-21ba-4446-b7f5-980fd7d1dd48"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

cat("\n========================================================================================\n")
cat(" HOLDING PERIOD SENSITIVITY & RISK OPTIMIZATION ENGINE (5d vs 7d vs 10d vs 15d vs 20d)\n")
cat("========================================================================================\n\n")

# 1. Load Benchmarks
vti_ohlcv <- readRDS("data/data_vti.rds")
qqq_ohlcv <- readRDS("data/data_qqq.rds")
vti_cl <- Cl(vti_ohlcv)
qqq_cl <- Cl(qqq_ohlcv)

vti_ret_all <- na.omit(vti_cl / lag.xts(vti_cl, 1) - 1)
qqq_ret_all <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)

# Helper to read symbols file
read_universe_symbols <- function(filepath) {
  if (!file.exists(filepath)) return(character(0))
  lines <- readLines(filepath, warn = FALSE)
  lines <- gsub("#.*", "", lines)
  syms  <- unique(toupper(trimws(unlist(strsplit(lines, "[, \\t\\r\\n]+")))))
  syms[syms != ""]
}

universes <- list(
  "Broad (symbols_broad.txt)" = read_universe_symbols("symbols_broad.txt"),
  "All2 (symbols_all2.txt)"   = read_universe_symbols("symbols_all2.txt"),
  "300+ (symbols_300.txt)"    = read_universe_symbols("symbols_300.txt"),
  "Focus (symbols.txt)"       = read_universe_symbols("symbols.txt")
)

all_unique_syms <- unique(unlist(universes))
cat(sprintf("Loaded 4 Universes (%d total unique symbols to analyze).\n", length(all_unique_syms)))

# 2. Load cached 1260wf objects
cat("Loading cached walk-forward feature & prediction datasets...\n")
base_sim_data <- list()
for (s in all_unique_syms) {
  cf <- file.path(CACHE_DIR, sprintf("%s_1260wf.rds", tolower(gsub("[^A-Za-z0-9]", "_", s))))
  if (file.exists(cf)) {
    base_sim_data[[s]] <- readRDS(cf)
  }
}
cat(sprintf("Successfully loaded %d / %d simulated assets from cache.\n\n", length(base_sim_data), length(all_unique_syms)))

# 3. Simulate bracket execution for arbitrary max_hold_days
simulate_bracket_for_hold_days <- function(sym, wf_obj, max_hold_days) {
  ohlcv <- tryCatch(load_stock_data(sym, offline_only = TRUE), error = function(e) NULL)
  if (is.null(ohlcv)) return(wf_obj$bracket_ret)
  
  common_d <- wf_obj$dates
  cl <- Cl(ohlcv)[common_d]
  op <- Op(ohlcv)[common_d]
  hi <- Hi(ohlcv)[common_d]
  lo <- Lo(ohlcv)[common_d]
  
  atr_xts <- tryCatch({
    TTR::ATR(HLC(ohlcv), n = 14)$atr[common_d]
  }, error = function(e) cl * 0.02)
  
  n_days <- length(common_d)
  c_p_vec <- as.numeric(cl)
  o_p_vec <- as.numeric(op)
  h_p_vec <- as.numeric(hi)
  l_p_vec <- as.numeric(lo)
  atr_vec <- as.numeric(atr_xts)
  gvol_v  <- as.numeric(wf_obj$garch_vol)
  sig_v   <- as.numeric(wf_obj$pred_class)
  
  bracket_ret_vec <- numeric(n_days)
  in_pos <- FALSE
  pos_shares_pct <- 0
  entry_p <- 0
  stop_p  <- 0
  t1_p    <- 0
  t2_p    <- 0
  high_p  <- 0
  days_h  <- 0
  t1_hit  <- FALSE
  t2_hit  <- FALSE
  
  for (d in 2:n_days) {
    cur_sig <- sig_v[d - 1]
    cur_o   <- o_p_vec[d]
    cur_h   <- h_p_vec[d]
    cur_l   <- l_p_vec[d]
    cur_c   <- c_p_vec[d]
    prev_c  <- c_p_vec[d - 1]
    cur_atr <- ifelse(is.na(atr_vec[d]), cur_c * 0.02, atr_vec[d])
    
    # Entry trigger
    if (!in_pos && !is.na(cur_sig) && cur_sig == 1) {
      in_pos <- TRUE
      entry_p <- ifelse(!is.na(cur_o) && cur_o > 0, cur_o, cur_c)
      high_p  <- entry_p
      days_h  <- 0
      t1_hit  <- FALSE
      t2_hit  <- FALSE
      pos_shares_pct <- 1.0
      
      daily_v <- pmax(0.05, gvol_v[d - 1]) / sqrt(252)
      risk_1r <- 2.0 * daily_v * entry_p
      stop_p  <- entry_p - risk_1r
      t1_p    <- entry_p + 1.5 * risk_1r
      t2_p    <- entry_p + 3.0 * risk_1r
      
      bracket_ret_vec[d] <- (cur_c - entry_p) / entry_p - (10 / 10000)
      next
    }
    
    if (in_pos) {
      days_h <- days_h + 1
      high_p <- max(high_p, cur_h)
      chand_stop <- high_p - 2.5 * cur_atr
      
      # 1. Stop loss hit
      if (cur_l <= stop_p) {
        exit_p <- min(cur_o, stop_p)
        bar_r  <- (exit_p - prev_c) / prev_c
        bracket_ret_vec[d] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      # 2. Tier 1 target hit: exit 50%
      if (!t1_hit && cur_h >= t1_p) {
        t1_hit <- TRUE
        pos_shares_pct <- 0.50
        stop_p <- max(entry_p, chand_stop)
      }
      
      # 3. Tier 2 target hit: exit 25%, leave 25% runner
      if (t1_hit && !t2_hit && cur_h >= t2_p) {
        t2_hit <- TRUE
        pos_shares_pct <- 0.25
        stop_p <- max(stop_p, chand_stop)
      }
      
      # Trailing stop update
      if (t1_hit) {
        stop_p <- max(stop_p, chand_stop)
      }
      
      # Time-decay ratchet: if held for (max_hold_days - 1) days and >= 1.0R profit, ratchet Tier 1 down to +1.1R
      if (!t1_hit && days_h >= (max_hold_days - 1) && cur_h >= (entry_p + 1.0 * risk_1r)) {
        t1_p <- entry_p + 1.1 * risk_1r
      }
      
      # Expiration at max_hold_days for non-runners
      if (!t2_hit && days_h >= max_hold_days) {
        bar_r <- (cur_c - prev_c) / prev_c
        bracket_ret_vec[d] <- pos_shares_pct * bar_r - (10 / 10000)
        in_pos <- FALSE
        pos_shares_pct <- 0
        next
      }
      
      bar_r <- (cur_c - prev_c) / prev_c
      bracket_ret_vec[d] <- pos_shares_pct * bar_r
    }
  }
  
  xts(bracket_ret_vec, order.by = common_d)
}

# 4. Portfolio Evaluator for a given Universe, Horizon, and Hold Days
evaluate_portfolio_hold_days <- function(u_syms, horizon_bars, hold_days, u_name, custom_bracket_rets) {
  avail_syms <- intersect(u_syms, names(base_sim_data))
  if (length(avail_syms) == 0) return(NULL)
  
  all_dates <- do.call(c, lapply(avail_syms, function(s) base_sim_data[[s]]$dates))
  date_counts <- table(all_dates)
  shared_dates <- as.Date(names(date_counts)[date_counts >= max(3, floor(0.20 * length(avail_syms)))])
  shared_dates <- sort(intersect(shared_dates, index(qqq_ret_all)))
  
  if (length(shared_dates) < horizon_bars) {
    eval_dates <- shared_dates
  } else {
    eval_dates <- tail(shared_dates, horizon_bars)
  }
  
  n_eval <- length(eval_dates)
  if (n_eval < 10) return(NULL)
  
  port_daily_ret <- numeric(n_eval)
  
  qqq_idx <- match(eval_dates, index(qqq_ret_all))
  qqq_sub <- as.numeric(qqq_ret_all)[qqq_idx]
  qqq_sub[is.na(qqq_sub)] <- 0
  
  vti_idx <- match(eval_dates, index(vti_ret_all))
  vti_sub <- as.numeric(vti_ret_all)[vti_idx]
  vti_sub[is.na(vti_sub)] <- 0
  
  n_syms <- length(avail_syms)
  mat_prob  <- matrix(0.5,  nrow = n_eval, ncol = n_syms)
  mat_rs    <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
  mat_vol   <- matrix(0.25, nrow = n_eval, ncol = n_syms)
  mat_ret_b <- matrix(0.0,  nrow = n_eval, ncol = n_syms)
  
  for (j in seq_along(avail_syms)) {
    s <- avail_syms[j]
    obj <- base_sim_data[[s]]
    b_ret_series <- custom_bracket_rets[[s]]
    
    idx_p <- match(eval_dates, index(obj$pred_prob))
    v_p <- !is.na(idx_p)
    if (any(v_p)) mat_prob[v_p, j] <- as.numeric(obj$pred_prob)[idx_p[v_p]]
    
    idx_rs <- match(eval_dates, index(obj$rs_spy))
    v_rs <- !is.na(idx_rs)
    if (any(v_rs)) mat_rs[v_rs, j] <- as.numeric(obj$rs_spy)[idx_rs[v_rs]]
    
    idx_v <- match(eval_dates, index(obj$garch_vol))
    v_v <- !is.na(idx_v)
    if (any(v_v)) {
      raw_vol <- as.numeric(obj$garch_vol)[idx_v[v_v]]
      raw_vol[is.na(raw_vol) | raw_vol < 0.05] <- 0.25
      mat_vol[v_v, j] <- raw_vol
    }
    
    idx_b <- match(eval_dates, index(b_ret_series))
    v_b <- !is.na(idx_b)
    if (any(v_b)) mat_ret_b[v_b, j] <- as.numeric(b_ret_series)[idx_b[v_b]]
  }
  
  for (t in seq_along(eval_dates)) {
    p_ups   <- mat_prob[t, ]
    rs_vals <- mat_rs[t, ]
    vols    <- mat_vol[t, ]
    rets_b  <- mat_ret_b[t, ]
    
    qualify <- which(p_ups >= 0.58 & rs_vals > 0)
    if (length(qualify) > 0) {
      scores <- p_ups[qualify] * (1 + rs_vals[qualify])
      top_order <- qualify[order(-scores)]
      top_idx   <- head(top_order, 5)
    } else {
      top_idx <- integer(0)
    }
    
    k_slots <- length(top_idx)
    if (k_slots == 0) {
      port_daily_ret[t] <- qqq_sub[t]
    } else {
      stock_alloc_total <- 0
      stock_pnl_total   <- 0
      slot_size         <- 1.0 / 5.0
      
      for (idx in top_idx) {
        vol_scale <- pmin(1.5, 0.45 / vols[idx])
        w_i <- slot_size * vol_scale
        stock_alloc_total <- stock_alloc_total + slot_size
        stock_pnl_total   <- stock_pnl_total + (w_i * rets_b[idx])
      }
      
      unalloc_cash <- max(0, 1.0 - stock_alloc_total)
      cash_yield_pnl <- unalloc_cash * qqq_sub[t]
      port_daily_ret[t] <- stock_pnl_total + cash_yield_pnl
    }
  }
  
  m_strat <- calc_performance_metrics(port_daily_ret, name = sprintf("%s (%dd)", u_name, hold_days))$raw
  m_qqq   <- calc_performance_metrics(qqq_sub, name = "QQQ")$raw
  m_vti   <- calc_performance_metrics(vti_sub, name = "VTI")$raw
  
  list(
    universe      = u_name,
    horizon_bars  = n_eval,
    hold_days     = hold_days,
    dates         = eval_dates,
    strat_ret     = m_strat$cum_ret,
    strat_ann_ret = m_strat$ann_ret,
    strat_vol     = m_strat$ann_vol,
    strat_sharpe  = m_strat$sharpe,
    strat_sortino = m_strat$sortino,
    strat_max_dd  = m_strat$max_dd,
    strat_winrate = m_strat$win_rate,
    strat_pf      = m_strat$profit_factor,
    qqq_ret       = m_qqq$cum_ret,
    vti_ret       = m_vti$cum_ret,
    daily_returns = port_daily_ret,
    qqq_daily     = qqq_sub,
    vti_daily     = vti_sub
  )
}

# ------------------------------------------------------------------------------
# 5. Execute Simulation Grid across Hold Days & Horizons
# ------------------------------------------------------------------------------
hold_day_options <- c(5, 7, 10, 15, 20)
horizons_map <- list(
  "6 Months"  = 126,
  "12 Months" = 252,
  "24 Months" = 504,
  "60 Months" = 1260
)

scorecard_rows <- list()
all_evals <- list()

cat("Beginning Grid Simulation across Hold Days (5d, 7d, 10d, 15d, 20d)...\n")

for (hd in hold_day_options) {
  cat(sprintf("\n>>> Simulating Bracket Returns for Max Hold = %d Days <<<\n", hd))
  
  # Precompute custom bracket returns for this hold_days setting
  custom_brackets <- list()
  for (s in names(base_sim_data)) {
    if (hd == 5) {
      custom_brackets[[s]] <- base_sim_data[[s]]$bracket_ret
    } else {
      custom_brackets[[s]] <- simulate_bracket_for_hold_days(s, base_sim_data[[s]], max_hold_days = hd)
    }
  }
  
  for (h_name in names(horizons_map)) {
    h_bars <- horizons_map[[h_name]]
    
    for (u_name in names(universes)) {
      u_syms <- universes[[u_name]]
      ev <- evaluate_portfolio_hold_days(u_syms, h_bars, hd, u_name, custom_brackets)
      
      if (!is.null(ev)) {
        key <- sprintf("%s_%s_%dd", u_name, h_name, hd)
        all_evals[[key]] <- ev
        
        term_wealth <- 10000 * (1 + ev$strat_ret)
        scorecard_rows[[length(scorecard_rows) + 1]] <- data.frame(
          Universe        = u_name,
          Horizon         = h_name,
          Hold_Days       = hd,
          Trading_Days    = ev$horizon_bars,
          Terminal_Wealth = sprintf("$%.2f", term_wealth),
          Cum_Return      = sprintf("%+.2f%%", ev$strat_ret * 100),
          Ann_Return      = sprintf("%+.2f%%", ev$strat_ann_ret * 100),
          Volatility      = sprintf("%.2f%%",  ev$strat_vol * 100),
          Sharpe_Ratio    = sprintf("%.2f",    ev$strat_sharpe),
          Sortino_Ratio   = sprintf("%.2f",    ev$strat_sortino),
          Max_Drawdown    = sprintf("%.2f%%",  ev$strat_max_dd * 100),
          Win_Rate        = sprintf("%.2f%%",  ev$strat_winrate * 100),
          Profit_Factor   = sprintf("%.2f",    ev$strat_pf),
          QQQ_Return      = sprintf("%+.2f%%", ev$qqq_ret * 100),
          VTI_Return      = sprintf("%+.2f%%", ev$vti_ret * 100),
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

scorecard_df <- do.call(rbind, scorecard_rows)

csv_path <- file.path(OUTPUT_DIR, "holding_period_scorecard.csv")
write.csv(scorecard_df, csv_path, row.names = FALSE)
cat(sprintf("\n[Saved] Scorecard saved to: %s (%d configurations)\n", csv_path, nrow(scorecard_df)))

# ------------------------------------------------------------------------------
# 6. Generate Multi-Panel Comparison Visualization
# ------------------------------------------------------------------------------
plot_png <- file.path(OUTPUT_DIR, "holding_period_comparison_chart.png")
png(plot_png, width = 2200, height = 1500, res = 150)
par(mfrow = c(2, 2), mar = c(4.8, 5.0, 3.5, 1.2), oma = c(1, 1, 3.5, 1))

cols_hd <- c("5" = "#1b9e77", "7" = "#377eb8", "10" = "#ff7f00", "15" = "#e41a1c", "20" = "#984ea3")

# Panel 1: symbols_broad.txt Equity Curves across Hold Days (60 Months / 5 Years)
u_target <- "Broad (symbols_broad.txt)"
eval_60m <- list()
for (hd in hold_day_options) {
  k <- sprintf("%s_%s_%dd", u_target, "60 Months", hd)
  if (!is.null(all_evals[[k]])) eval_60m[[as.character(hd)]] <- all_evals[[k]]
}

eq_list <- lapply(eval_60m, function(e) cumprod(1 + e$daily_returns))
ymin <- min(sapply(eq_list, min)) * 0.90
ymax <- max(sapply(eq_list, max)) * 1.15

plot(eq_list[["5"]], type = "l", col = cols_hd["5"], lwd = 2.8, log = "y", ylim = c(ymin, ymax),
     main = "symbols_broad.txt: 5-Year Equity Curves by Hold Days [Log Scale]",
     xlab = "Trading Days", ylab = "Portfolio Equity (Log Base 1.0)",
     cex.main = 1.25, cex.axis = 0.95, font.main = 2)
grid(col = "gray85")
lines(eq_list[["7"]],  col = cols_hd["7"],  lwd = 2.2, lty = 2)
lines(eq_list[["10"]], col = cols_hd["10"], lwd = 2.2, lty = 3)
lines(eq_list[["15"]], col = cols_hd["15"], lwd = 2.2, lty = 4)
lines(eq_list[["20"]], col = cols_hd["20"], lwd = 2.2, lty = 5)

legend("topleft",
       legend = sapply(names(eq_list), function(h) {
         ret <- (tail(eq_list[[h]], 1) - 1) * 100
         sprintf("%s Days [%+.0f%% | $%.0fk]", h, ret, 10 * (1 + ret/100))
       }),
       col = cols_hd[names(eq_list)],
       lwd = c(2.8, 2.2, 2.2, 2.2, 2.2), lty = c(1, 2, 3, 4, 5),
       cex = 0.82, bg = rgb(1, 1, 1, 0.92), box.col = "gray75")

# Panel 2: Sharpe Ratio Comparison by Hold Days (Broad & All2 across Horizons)
sharpe_mat <- matrix(0, nrow = length(hold_day_options), ncol = 3)
rownames(sharpe_mat) <- paste0(hold_day_options, "d")
colnames(sharpe_mat) <- c("12M Sharpe", "24M Sharpe", "60M Sharpe")

for (i in seq_along(hold_day_options)) {
  hd <- hold_day_options[i]
  k12 <- sprintf("%s_12 Months_%dd", u_target, hd)
  k24 <- sprintf("%s_24 Months_%dd", u_target, hd)
  k60 <- sprintf("%s_60 Months_%dd", u_target, hd)
  if (!is.null(all_evals[[k12]])) sharpe_mat[i, 1] <- all_evals[[k12]]$strat_sharpe
  if (!is.null(all_evals[[k24]])) sharpe_mat[i, 2] <- all_evals[[k24]]$strat_sharpe
  if (!is.null(all_evals[[k60]])) sharpe_mat[i, 3] <- all_evals[[k60]]$strat_sharpe
}

barplot(t(sharpe_mat), beside = TRUE, col = c("#2b83ba", "#fdae61", "#d7191c"),
        main = "Risk-Adjusted Efficiency (Sharpe Ratio by Hold Days)",
        ylab = "Annualized Sharpe Ratio", ylim = c(0, max(sharpe_mat) * 1.25),
        cex.main = 1.25, cex.axis = 0.95, font.main = 2)
grid(col = "gray85")
legend("topright", legend = c("12 Months", "24 Months", "60 Months"),
       fill = c("#2b83ba", "#fdae61", "#d7191c"), cex = 0.85, bg = rgb(1, 1, 1, 0.92))

# Panel 3: Maximum Drawdown Impact by Hold Days
dd_mat <- matrix(0, nrow = length(hold_day_options), ncol = 3)
rownames(dd_mat) <- paste0(hold_day_options, "d")
colnames(dd_mat) <- c("12M Max DD", "24M Max DD", "60M Max DD")

for (i in seq_along(hold_day_options)) {
  hd <- hold_day_options[i]
  k12 <- sprintf("%s_12 Months_%dd", u_target, hd)
  k24 <- sprintf("%s_24 Months_%dd", u_target, hd)
  k60 <- sprintf("%s_60 Months_%dd", u_target, hd)
  if (!is.null(all_evals[[k12]])) dd_mat[i, 1] <- all_evals[[k12]]$strat_max_dd * 100
  if (!is.null(all_evals[[k24]])) dd_mat[i, 2] <- all_evals[[k24]]$strat_max_dd * 100
  if (!is.null(all_evals[[k60]])) dd_mat[i, 3] <- all_evals[[k60]]$strat_max_dd * 100
}

barplot(t(dd_mat), beside = TRUE, col = c("#abd9e9", "#fee090", "#f46d43"),
        main = "Downside Risk Exposure (Max Drawdown by Hold Days)",
        ylab = "Max Drawdown (%)", ylim = c(0, max(dd_mat) * 1.30),
        cex.main = 1.25, cex.axis = 0.95, font.main = 2)
grid(col = "gray85")
legend("topleft", legend = c("12 Months", "24 Months", "60 Months"),
       fill = c("#abd9e9", "#fee090", "#f46d43"), cex = 0.85, bg = rgb(1, 1, 1, 0.92))

# Panel 4: Terminal Wealth Comparison ($10,000 Capital over 5 Years) across 4 Universes
wealth_mat <- matrix(0, nrow = 4, ncol = length(hold_day_options))
rownames(wealth_mat) <- c("Broad", "All2", "300+", "Focus")
colnames(wealth_mat) <- paste0(hold_day_options, "d")

u_keys <- c("Broad (symbols_broad.txt)", "All2 (symbols_all2.txt)", "300+ (symbols_300.txt)", "Focus (symbols.txt)")
for (u_i in seq_along(u_keys)) {
  uk <- u_keys[u_i]
  for (h_i in seq_along(hold_day_options)) {
    hd <- hold_day_options[h_i]
    k <- sprintf("%s_60 Months_%dd", uk, hd)
    if (!is.null(all_evals[[k]])) {
      wealth_mat[u_i, h_i] <- (10000 * (1 + all_evals[[k]]$strat_ret)) / 1000 # in $k
    }
  }
}

barplot(wealth_mat, beside = TRUE, col = c("#2b83ba", "#d7191c", "#4daf4a", "#984ea3"),
        main = "5-Year Terminal Wealth ($k) by Hold Days Across Universes",
        ylab = "Terminal Wealth ($ in Thousands)", ylim = c(0, max(wealth_mat) * 1.25),
        cex.main = 1.25, cex.axis = 0.95, font.main = 2)
grid(col = "gray85")
legend("topleft", legend = c("Broad (symbols_broad.txt)", "All2 (symbols_all2.txt)", "300+ (symbols_300.txt)", "Focus (symbols.txt)"),
       fill = c("#2b83ba", "#d7191c", "#4daf4a", "#984ea3"), cex = 0.82, bg = rgb(1, 1, 1, 0.92))

mtext("Holding Period Sensitivity Study: 5d vs 7d vs 10d vs 15d vs 20d",
      outer = TRUE, cex = 1.45, font = 2, col = "#111111")
dev.off()
cat(sprintf("[Saved] Comparison chart saved to: %s\n", plot_png))

if (dir.exists(ARTIFACT_DIR)) {
  file.copy(plot_png, file.path(ARTIFACT_DIR, "holding_period_comparison_chart.png"), overwrite = TRUE)
  cat(sprintf("[Saved] Chart copied to artifact directory: %s\n", file.path(ARTIFACT_DIR, "holding_period_comparison_chart.png")))
}

cat("\nHolding period backtest finished successfully!\n")

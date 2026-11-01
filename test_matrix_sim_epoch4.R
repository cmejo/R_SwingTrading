suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
  library(jsonlite)
})

source("R/08_metrics.R")

HIST_DIR <- "data/data_historical"

# 1. Load Benchmarks
ndx_raw   <- readRDS(file.path(HIST_DIR, "data_^ndx.rds"))
qqq_raw   <- readRDS(file.path(HIST_DIR, "data_qqq.rds"))
vfinx_raw <- readRDS(file.path(HIST_DIR, "data_vfinx.rds"))
vti_raw   <- readRDS(file.path(HIST_DIR, "data_vti.rds"))
gspc_raw  <- readRDS(file.path(HIST_DIR, "data_^gspc.rds"))
spy_raw   <- readRDS(file.path(HIST_DIR, "data_spy.rds"))
irx_raw   <- readRDS(file.path(HIST_DIR, "data_^irx.rds"))

# Spliced QQQ
ndx_cl <- Cl(ndx_raw); qqq_cl <- Cl(qqq_raw)
ndx_ret <- na.omit(ndx_cl / lag.xts(ndx_cl, 1) - 1) - (0.0020 / 252)
qqq_ret <- na.omit(qqq_cl / lag.xts(qqq_cl, 1) - 1)
qqq_start <- index(qqq_ret)[1]
qqq_spliced <- rbind(ndx_ret[index(ndx_ret) < qqq_start], qqq_ret)

# Spliced SPY
gspc_cl <- Cl(gspc_raw); spy_cl <- Cl(spy_raw)
gspc_ret <- na.omit(gspc_cl / lag.xts(gspc_cl, 1) - 1)
spy_ret  <- na.omit(spy_cl / lag.xts(spy_cl, 1) - 1)
spy_start <- index(spy_ret)[1]
spy_spliced <- rbind(gspc_ret[index(gspc_ret) < spy_start], spy_ret)
spy_level   <- cumprod(1 + spy_spliced) * 100

# 2. Test Epoch 4 (2012 to 2021)
ep_start <- as.Date("2012-01-01")
ep_end   <- as.Date("2021-12-31")

common_dates <- sort(intersect(index(qqq_spliced), index(spy_spliced)))
eval_dates   <- common_dates[common_dates >= ep_start & common_dates <= ep_end]
n_eval       <- length(eval_dates)

qqq_sub <- as.numeric(qqq_spliced)[match(eval_dates, index(qqq_spliced))]
spy_sub <- as.numeric(spy_spliced)[match(eval_dates, index(spy_spliced))]
qqq_sub[is.na(qqq_sub)] <- 0
spy_sub[is.na(spy_sub)] <- 0

# Load stocks and compute bracket returns
stock_files <- list.files(HIST_DIR, pattern = "^data_[a-z0-9_]+\\.rds$", full.names = TRUE)
stock_files <- stock_files[!grepl("data_(\\^|vti|qqq|spy|vfinx|gspc|ndx|irx)", stock_files)]

sim_stocks <- list()
for (f in stock_files) {
  sym <- toupper(sub("^data_", "", sub("\\.rds$", "", basename(f))))
  d <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 200) next
  
  df_sub <- tryCatch(na.locf(na.locf(d, na.rm = FALSE), fromLast = TRUE), error = function(e) NULL)
  if (is.null(df_sub) || nrow(df_sub) < 100) next
  
  all_idx <- index(df_sub)
  cl_xts  <- Cl(df_sub)
  
  # RS vs SPY
  spy_aligned <- spy_level[all_idx]
  rs_xts <- (cl_xts / lag.xts(cl_xts, 63)) - (spy_aligned / lag.xts(spy_aligned, 63))
  
  # ATR and brackets
  c_sub   <- as.numeric(cl_xts)
  h_sub   <- as.numeric(Hi(df_sub))
  l_sub   <- as.numeric(Lo(df_sub))
  atr_res <- tryCatch(ATR(HLC(df_sub), n = 14)$atr, error = function(e) cl_xts * 0.02)
  atr_abs <- as.numeric(atr_res)
  atr_abs[is.na(atr_abs) | atr_abs <= 0] <- c_sub[is.na(atr_abs) | atr_abs <= 0] * 0.02
  
  nb <- length(c_sub)
  b_ret <- numeric(nb)
  pos_active <- FALSE
  entry_p <- 0; stop_p <- 0; pt_p <- 0; bars_held <- 0; prev_c <- 0
  
  for (i in 1:nb) {
    curr_c <- c_sub[i]; curr_h <- h_sub[i]; curr_l <- l_sub[i]; curr_atr <- atr_abs[i]
    if (pos_active) {
      bars_held <- bars_held + 1
      is_stopped <- (curr_l <= stop_p)
      is_pt      <- (curr_h >= pt_p)
      is_time    <- (bars_held >= 7)
      
      if (is_stopped) {
        exit_p <- min(prev_c, stop_p)
        b_ret[i] <- (exit_p / prev_c) - 1
        pos_active <- FALSE
      } else if (is_pt) {
        exit_p <- max(prev_c, pt_p)
        b_ret[i] <- (exit_p / prev_c) - 1
        pos_active <- FALSE
      } else if (is_time) {
        b_ret[i] <- (curr_c / prev_c) - 1
        pos_active <- FALSE
      } else {
        b_ret[i] <- (curr_c / prev_c) - 1
        stop_p <- max(stop_p, curr_c - 1.5 * curr_atr)
        prev_c <- curr_c
      }
    } else {
      entry_p <- curr_c
      stop_p  <- entry_p - 1.5 * curr_atr
      pt_p    <- entry_p + 2.5 * curr_atr
      prev_c  <- entry_p
      bars_held <- 0
      pos_active <- TRUE
      b_ret[i] <- 0
    }
  }
  
  # Align to eval dates
  m_idx <- match(eval_dates, all_idx)
  sim_stocks[[sym]] <- list(
    rs    = as.numeric(rs_xts)[m_idx],
    b_ret = b_ret[m_idx],
    bars  = m_idx
  )
}

avail_syms <- names(sim_stocks)
cat(sprintf("Precomputed %d stocks for Epoch 4.\n", length(avail_syms)))

# Build matrices
mat_rs    <- matrix(0, nrow = n_eval, ncol = length(avail_syms))
mat_ret_b <- matrix(0, nrow = n_eval, ncol = length(avail_syms))
mat_valid <- matrix(FALSE, nrow = n_eval, ncol = length(avail_syms))

for (j in seq_along(avail_syms)) {
  s <- avail_syms[j]
  obj <- sim_stocks[[s]]
  v <- !is.na(obj$bars) & obj$bars >= 500  # Option A: >= 500 bars
  mat_valid[v, j] <- TRUE
  mat_rs[v, j]    <- obj$rs[v]
  mat_ret_b[v, j] <- obj$b_ret[v]
}

# Run portfolio simulation: max_pos = 3
max_pos <- 3
port_daily_ret <- numeric(n_eval)

for (t in 1:n_eval) {
  rs_vals <- mat_rs[t, ]
  rets_b  <- mat_ret_b[t, ]
  val_flg <- mat_valid[t, ]
  
  qualify <- which(val_flg & rs_vals > 0.02)
  top_idx <- integer(0)
  
  if (length(qualify) > 0) {
    ranked <- qualify[order(-rs_vals[qualify])]
    top_idx <- head(ranked, max_pos)
  }
  
  n_sel <- length(top_idx)
  if (n_sel > 0) {
    swing_exposure <- n_sel / max_pos
    swing_ret <- mean(rets_b[top_idx])
    cash_ret  <- qqq_sub[t]
    port_daily_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_ret
  } else {
    port_daily_ret[t] <- qqq_sub[t]
  }
}

m_strat <- calc_performance_metrics(xts(port_daily_ret, order.by = eval_dates))$raw
m_qqq   <- calc_performance_metrics(xts(qqq_sub, order.by = eval_dates))$raw

cat(sprintf("Momentum Strategy (Epoch 4): CAGR = %+.2f%%, MaxDD = -%.2f%%, Sharpe = %.2f\n", 
            m_strat$ann_ret * 100, m_strat$max_dd * 100, m_strat$sharpe))
cat(sprintf("QQQ Benchmark (Epoch 4):     CAGR = %+.2f%%, MaxDD = -%.2f%%, Sharpe = %.2f\n", 
            m_qqq$ann_ret * 100, m_qqq$max_dd * 100, m_qqq$sharpe))

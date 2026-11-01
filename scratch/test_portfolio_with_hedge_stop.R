suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
})

source("R/02_trend_lmMA.R")

raw_qqq <- suppressWarnings(getSymbols("QQQ", src="yahoo", from="2010-01-01", auto.assign=FALSE))
raw_gld <- suppressWarnings(getSymbols("GLD", src="yahoo", from="2010-01-01", auto.assign=FALSE))
raw_bil <- suppressWarnings(getSymbols("BIL", src="yahoo", from="2010-01-01", auto.assign=FALSE))
raw_sgov <- suppressWarnings(getSymbols("SGOV", src="yahoo", from="2010-01-01", auto.assign=FALSE))
raw_sqqq <- suppressWarnings(getSymbols("SQQQ", src="yahoo", from="2010-01-01", auto.assign=FALSE))
raw_fngd <- suppressWarnings(getSymbols("FNGD", src="yahoo", from="2010-01-01", auto.assign=FALSE))

get_ret <- function(sym_data) {
  ad <- Ad(sym_data)
  na.omit(ad / lag.xts(ad, 1) - 1)
}

ret_qqq  <- get_ret(raw_qqq)
ret_gld  <- get_ret(raw_gld)
ret_bil  <- get_ret(raw_bil)
ret_sgov <- get_ret(raw_sgov)
ret_sqqq <- get_ret(raw_sqqq)
ret_fngd <- get_ret(raw_fngd)

sgov_spliced <- ret_bil
sgov_common  <- intersect(index(ret_sgov), index(sgov_spliced))
sgov_spliced[sgov_common] <- ret_sgov[sgov_common]

qqq_lm <- lmMA(Cl(raw_qqq), n = 50)
gld_lm <- lmMA(Cl(raw_gld), n = 50)

common_10y <- sort(intersect(intersect(intersect(
  index(ret_qqq), index(ret_gld)), index(sgov_spliced)), index(ret_sqqq)))
common_10y <- sort(intersect(common_10y, index(na.omit(qqq_lm$fit))))
common_10y <- sort(intersect(common_10y, index(na.omit(gld_lm$fit))))

eval_dates <- tail(common_10y, 2520)
n_total <- length(eval_dates)

qqq_v  <- as.numeric(ret_qqq[eval_dates])
gld_v  <- as.numeric(ret_gld[eval_dates])
sgov_v <- as.numeric(sgov_spliced[eval_dates])
sqqq_v <- as.numeric(ret_sqqq[eval_dates])

fngd_v <- numeric(n_total)
for (i in 1:n_total) {
  d <- eval_dates[i]
  if (d %in% index(ret_fngd)) {
    fngd_v[i] <- as.numeric(ret_fngd[d])
  } else {
    fngd_v[i] <- sqqq_v[i]
  }
}

qqq_cl_v   <- as.numeric(Cl(raw_qqq)[eval_dates])
qqq_fit_v  <- as.numeric(qqq_lm$fit[eval_dates])
qqq_slp_v  <- as.numeric(qqq_lm$slope[eval_dates])
macro_bull <- (qqq_cl_v >= qqq_fit_v) & (qqq_slp_v > 0)

gld_cl_v   <- as.numeric(Cl(raw_gld)[eval_dates])
gld_fit_v  <- as.numeric(gld_lm$fit[eval_dates])
gld_slp_v  <- as.numeric(gld_lm$slope[eval_dates])
gld_bull   <- (gld_cl_v >= gld_fit_v) & (gld_slp_v > 0)

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

fb_raw <- 0.75 * fngd_v + 0.25 * sqqq_v

sim_port_with_stop <- function(max_loss_pct = NULL, max_days = NULL) {
  cash_ret <- numeric(n_total)
  in_episode <- FALSE
  ep_cum <- 1.0
  stopped_out <- FALSE
  ep_day <- 0
  
  for (t in 1:n_total) {
    if (macro_bull[t]) {
      cash_ret[t] <- qqq_v[t]
      in_episode <- FALSE
      stopped_out <- FALSE
      ep_cum <- 1.0
      ep_day <- 0
    } else if (gld_bull[t]) {
      cash_ret[t] <- gld_v[t]
      in_episode <- FALSE
      stopped_out <- FALSE
      ep_cum <- 1.0
      ep_day <- 0
    } else {
      if (!in_episode) {
        in_episode <- TRUE
        ep_cum <- 1.0
        stopped_out <- FALSE
        ep_day <- 0
      }
      ep_day <- ep_day + 1
      if (!is.null(max_days) && ep_day > max_days) stopped_out <- TRUE
      if (stopped_out) {
        cash_ret[t] <- sgov_v[t]
      } else {
        r <- fb_raw[t]
        ep_cum <- ep_cum * (1 + r)
        if (!is.null(max_loss_pct) && (ep_cum - 1.0) <= -max_loss_pct) stopped_out <- TRUE
        cash_ret[t] <- r
      }
    }
  }
  
  port_ret <- numeric(n_total)
  for (t in 1:n_total) {
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
    cash_day_ret <- cash_ret[t]
    if (n_sel > 0) {
      inv_v <- 1 / vols[top_idx]
      weights <- inv_v / sum(inv_v)
      swing_exposure <- min(1.0, n_sel / 3.0)
      swing_ret <- sum(weights * rets_b[top_idx])
      port_ret[t] <- swing_exposure * swing_ret + (1 - swing_exposure) * cash_day_ret
    } else {
      port_ret[t] <- cash_day_ret
    }
  }
  
  eq <- cumprod(1 + port_ret)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  sharpe <- (mean(port_ret) * 252 - 0.04) / (sd(port_ret) * sqrt(252))
  final_10k <- 10000 * tail(eq, 1)
  
  c(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe)
}

res_port_list <- list(
  "No Stop (Base 75/25 FNGD+SQQQ)" = sim_port_with_stop(NULL, NULL),
  "Max Loss -8% -> SGOV"           = sim_port_with_stop(0.08, NULL),
  "Max Loss -10% -> SGOV"          = sim_port_with_stop(0.10, NULL),
  "Time Stop: Max 20 Days -> SGOV" = sim_port_with_stop(NULL, 20),
  "Combo: -10% Stop OR 25 Days -> SGOV" = sim_port_with_stop(0.10, 25)
)

cat("=== TESTING MAX LOSS & TIME STOPS IN FULL SWING PORTFOLIO ===\n\n")
header <- sprintf("%-38s | %14s | %9s | %9s | %7s", "Stop Configuration", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe")
cat(header, "\n", paste(rep("-", nchar(header)), collapse=""), "\n")
for (nm in names(res_port_list)) {
  r <- res_port_list[[nm]]
  cat(sprintf("%-38s | %14s | %+8.2f%% | %8.2f%% | %6.2f\n",
              nm, sprintf("$%.2fM", r["Final_10k"] / 1e6), r["CAGR"] * 100, r["MaxDD"] * 100, r["Sharpe"]))
}

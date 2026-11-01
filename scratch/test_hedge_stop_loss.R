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

# Simulate raw fallback daily return
fb_raw <- 0.75 * fngd_v + 0.25 * sqqq_v

# Test different max loss thresholds on the hedge per episode:
# When in HEDGE episode, track cumulative equity from start of episode.
# If cumulative episode equity drops below (1 - max_loss), rotate to SGOV for rest of episode.
sim_with_stop <- function(max_loss_pct = NULL, max_days = NULL) {
  ret_out <- numeric(n_total)
  in_episode <- FALSE
  ep_cum <- 1.0
  stopped_out <- FALSE
  ep_day <- 0
  
  for (t in 1:n_total) {
    if (macro_bull[t]) {
      ret_out[t] <- qqq_v[t]
      in_episode <- FALSE
      stopped_out <- FALSE
      ep_cum <- 1.0
      ep_day <- 0
    } else if (gld_bull[t]) {
      ret_out[t] <- gld_v[t]
      in_episode <- FALSE
      stopped_out <- FALSE
      ep_cum <- 1.0
      ep_day <- 0
    } else {
      # In HEDGE episode
      if (!in_episode) {
        in_episode <- TRUE
        ep_cum <- 1.0
        stopped_out <- FALSE
        ep_day <- 0
      }
      ep_day <- ep_day + 1
      
      # Time stop check
      if (!is.null(max_days) && ep_day > max_days) {
        stopped_out <- TRUE
      }
      
      if (stopped_out) {
        ret_out[t] <- sgov_v[t]
      } else {
        r <- fb_raw[t]
        ep_cum <- ep_cum * (1 + r)
        if (!is.null(max_loss_pct) && (ep_cum - 1.0) <= -max_loss_pct) {
          stopped_out <- TRUE
        }
        ret_out[t] <- r
      }
    }
  }
  
  eq <- cumprod(1 + ret_out)
  cagr <- tail(eq, 1)^(252 / n_total) - 1
  mdd <- min((eq - cummax(eq)) / cummax(eq))
  sharpe <- (mean(ret_out) * 252 - 0.04) / (sd(ret_out) * sqrt(252))
  final_10k <- 10000 * tail(eq, 1)
  
  c(CAGR = cagr, Final_10k = final_10k, MaxDD = mdd, Sharpe = sharpe)
}

res_list <- list(
  "No Stop (Base 75/25 FNGD+SQQQ)" = sim_with_stop(NULL, NULL),
  "Max Loss -5% -> SGOV"            = sim_with_stop(0.05, NULL),
  "Max Loss -8% -> SGOV"            = sim_with_stop(0.08, NULL),
  "Max Loss -10% -> SGOV"           = sim_with_stop(0.10, NULL),
  "Max Loss -15% -> SGOV"           = sim_with_stop(0.15, NULL),
  "Time Stop: Max 20 Days -> SGOV"  = sim_with_stop(NULL, 20),
  "Time Stop: Max 30 Days -> SGOV"  = sim_with_stop(NULL, 30),
  "Combo: -10% Stop OR 25 Days -> SGOV" = sim_with_stop(0.10, 25)
)

cat("=== TESTING MAX LOSS & TIME STOPS ON STANDALONE HEDGE SLEEVE ===\n\n")
header <- sprintf("%-38s | %12s | %9s | %9s | %7s", "Stop Configuration", "Final $ ($10k)", "10Y CAGR", "Max DD", "Sharpe")
cat(header, "\n", paste(rep("-", nchar(header)), collapse=""), "\n")
for (nm in names(res_list)) {
  r <- res_list[[nm]]
  cat(sprintf("%-38s | %12s | %+8.2f%% | %8.2f%% | %6.2f\n",
              nm, sprintf("$%.2fM", r["Final_10k"] / 1e6), r["CAGR"] * 100, r["MaxDD"] * 100, r["Sharpe"]))
}

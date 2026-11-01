suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(quantmod)
  library(TTR)
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

# Standalone returns
fb_75_25 <- 0.75 * fngd_v + 0.25 * sqqq_v
fb_50_50 <- 0.50 * fngd_v + 0.50 * sqqq_v

r_with_gld <- sapply(1:n_total, function(t) if (macro_bull[t]) qqq_v[t] else if (gld_bull[t]) gld_v[t] else fb_75_25[t])
r_no_gld_75 <- ifelse(macro_bull, qqq_v, fb_75_25)
r_no_gld_50 <- ifelse(macro_bull, qqq_v, fb_50_50)
r_no_gld_fngd <- ifelse(macro_bull, qqq_v, fngd_v)
r_no_gld_sqqq <- ifelse(macro_bull, qqq_v, sqqq_v)
r_base <- sapply(1:n_total, function(t) if (macro_bull[t]) qqq_v[t] else if (gld_bull[t]) gld_v[t] else sgov_v[t])

# Load portfolio swing data
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

sim_p <- function(cash_r) {
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
    cash_day_ret <- cash_r[t]
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
  return(port_ret)
}

p_with_gld    <- sim_p(r_with_gld)
p_no_gld_75   <- sim_p(r_no_gld_75)
p_no_gld_50   <- sim_p(r_no_gld_50)
p_no_gld_fngd <- sim_p(r_no_gld_fngd)
p_no_gld_sqqq <- sim_p(r_no_gld_sqqq)
p_base        <- sim_p(r_base)

png("output/no_gld_comparison_chart.png", width = 1300, height = 900, res = 130)
par(mfrow = c(2, 1), mar = c(3, 4, 3, 1), oma = c(1, 1, 1, 1))

# Panel 1: Full Portfolio
eq_p_gld  <- 10000 * cumprod(1 + p_with_gld)
eq_p_fngd <- 10000 * cumprod(1 + p_no_gld_fngd)
eq_p_75   <- 10000 * cumprod(1 + p_no_gld_75)
eq_p_50   <- 10000 * cumprod(1 + p_no_gld_50)
eq_p_sqqq <- 10000 * cumprod(1 + p_no_gld_sqqq)
eq_p_base <- 10000 * cumprod(1 + p_base)

dates_plot <- as.Date(eval_dates)

plot(dates_plot, eq_p_gld, type = "l", col = "#15803d", lwd = 2.5, log = "y",
     main = "Full Portfolio Growth of $10,000 (With GLD vs Direct No-GLD Hedges)",
     ylab = "Portfolio Equity (Log Scale)", xlab = "", yaxt = "n")
lines(dates_plot, eq_p_fngd, col = "#2563eb", lwd = 1.8)
lines(dates_plot, eq_p_75,   col = "#d97706", lwd = 1.8)
lines(dates_plot, eq_p_50,   col = "#9333ea", lwd = 1.8)
lines(dates_plot, eq_p_sqqq, col = "#dc2626", lwd = 1.8)
lines(dates_plot, eq_p_base, col = "#64748b", lwd = 1.5, lty = 2)

y_ticks <- c(10000, 100000, 1000000, 10000000, 100000000)
axis(2, at = y_ticks, labels = c("$10k", "$100k", "$1M", "$10M", "$100M"), las = 2)
grid(col = "#e2e8f0", lty = "dotted")
legend("topleft", 
       legend = c(
         "With GLD: [75% FNGD + 25% SQQQ] ($84.66M, +147.0% CAGR)",
         "No GLD: 100% FNGD ($51.93M, +135.3% CAGR)",
         "No GLD: 75% FNGD + 25% SQQQ ($51.29M, +135.0% CAGR)",
         "No GLD: 50% FNGD + 50% SQQQ ($50.33M, +134.5% CAGR)",
         "No GLD: 100% SQQQ ($47.52M, +133.2% CAGR)",
         "Baseline: GLD / SGOV ($23.27M, +117.1% CAGR)"
       ),
       col = c("#15803d", "#2563eb", "#d97706", "#9333ea", "#dc2626", "#64748b"),
       lwd = c(2.5, 1.8, 1.8, 1.8, 1.8, 1.5), lty = c(1, 1, 1, 1, 1, 2), cex = 0.82, bg = "white")

# Panel 2: Standalone Cash Switch Strategy
eq_s_gld  <- 10000 * cumprod(1 + r_with_gld)
eq_s_sqqq <- 10000 * cumprod(1 + r_no_gld_sqqq)
eq_s_50   <- 10000 * cumprod(1 + r_no_gld_50)
eq_s_75   <- 10000 * cumprod(1 + r_no_gld_75)
eq_s_fngd <- 10000 * cumprod(1 + r_no_gld_fngd)
eq_s_base <- 10000 * cumprod(1 + r_base)

plot(dates_plot, eq_s_gld, type = "l", col = "#15803d", lwd = 2.5, log = "y",
     main = "Standalone Cash Switch Growth of $10,000 (100% Switch Only)",
     ylab = "Cash Sleeve Equity (Log Scale)", xlab = "", yaxt = "n")
lines(dates_plot, eq_s_sqqq, col = "#dc2626", lwd = 1.8)
lines(dates_plot, eq_s_50,   col = "#9333ea", lwd = 1.8)
lines(dates_plot, eq_s_75,   col = "#d97706", lwd = 1.8)
lines(dates_plot, eq_s_fngd, col = "#2563eb", lwd = 1.8)
lines(dates_plot, eq_s_base, col = "#64748b", lwd = 1.5, lty = 2)

y_ticks2 <- c(10000, 50000, 200000, 1000000, 10000000)
axis(2, at = y_ticks2, labels = c("$10k", "$50k", "$200k", "$1M", "$10M"), las = 2)
grid(col = "#e2e8f0", lty = "dotted")
legend("topleft", 
       legend = c(
         "With GLD: [75% FNGD + 25% SQQQ] ($8.77M, MaxDD -51.4%)",
         "Baseline: GLD / SGOV ($1.58M, MaxDD -5.6%)",
         "No GLD: 100% SQQQ ($1.14M, MaxDD -66.1%)",
         "No GLD: 50% FNGD + 50% SQQQ ($957k, MaxDD -67.9%)",
         "No GLD: 75% FNGD + 25% SQQQ ($801k, MaxDD -69.7%)",
         "No GLD: 100% FNGD ($630k, MaxDD -72.5%)"
       ),
       col = c("#15803d", "#64748b", "#dc2626", "#9333ea", "#d97706", "#2563eb"),
       lwd = c(2.5, 1.5, 1.8, 1.8, 1.8, 1.8), lty = c(1, 2, 1, 1, 1, 1), cex = 0.82, bg = "white")

dev.off()
cat("Chart successfully created at output/no_gld_comparison_chart.png\n")

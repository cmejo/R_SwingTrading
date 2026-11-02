source("R/02_trend_lmMA.R")
source("R/10_option_d_features.R")
source("R/11_option_d_engine.R")

src <- readLines("scratch/compare_symbol_lists.R")
idx <- grep("^lists <- c", src)
eval(parse(text = src[1:(idx - 1)]))
invisible(set_universe(read_list("symbols_broad.txt")))

D <- prepare_option_d_data(list(cal = cal, syms = syms, P = P, R = R, MEM = MEM, q = q, rq = rq,
                                r_gld = r_gld, rf_daily = rf_daily, VOL63 = VOL63, vol20 = vol20,
                                month_end = month_end))

runs <- list()
for (lev in c(1.50, 1.60, 1.75, 2.00, 2.50)) {
  for (m in c(0.30, 0.40, 0.50)) {
    cfg <- option_d_config(
      preset = "legacy",
      base_margin = min(1.50, lev),
      turbo_margin = lev,
      use_turbo = (lev > 1.50),
      maintenance = m,
      borrow_spread = 0.0820, # Schwab spread
      drift_band = 0.15,
      residual_to = "gld",
      stop_replace_size = "proceeds"
    )
    r <- sim_option_d(D, cfg, as.Date("2007-01-03"), as.Date("2026-10-06"))
    ev <- which(cal >= as.Date("2007-01-03") & cal <= as.Date("2026-10-06"))
    ev <- ev[ev >= 2]
    m_res <- option_d_metrics(r, rf_daily[ev])
    tag <- sprintf("Lev_%.2fx_Maint_%.2f", lev, m)
    runs[[tag]] <- data.frame(
      Leverage = lev,
      Maint = m,
      CAGR = round(unname(m_res["CAGR"]), 2),
      MaxDD = round(unname(m_res["MaxDD"]), 2),
      Sharpe = round(unname(m_res["Sharpe"]), 3),
      MarginCalls = round(unname(m_res["MarginCalls"]), 0),
      Ruined = unname(m_res["Ruined"])
    )
  }
}
res_df <- do.call(rbind, runs)
write.csv(res_df, "scratch/scorecard_margin_regt_scrutiny.csv", row.names = FALSE)
print(res_df)

# CLAUDE.md: ground rules for this repository

This repo runs a live, leveraged momentum strategy in a real Schwab account. Treat every
change to the live path as production code, and every backtest claim as suspect until it
passes the evaluation rules below.

## What is live

- **Live engine:** `core_satellite_signal.R` ("Option D"), launched by `auto_trade_coresat_schwab.sh`
  through `schwab_bridge.py`. The README's ML swing engine (`daily_signal.R`, `R/01`–`R/09`) is
  NOT what trades.
- **Rules:** top 5 by dual-lookback RS vs QQQ (50% 126d + 50% 63d), blended 70/30 with
  frog-in-the-pan (`USE_FIP`); retention buffer rank 8; −25% disaster stop with immediate
  replacement; monthly rebalance; 1.5x margin in bull, 1.60x "turbo" in super-bull,
  1.0x and min-variance stocks in bear; 22% vol scale on QQQ realized vol, remainder in GLD.
- **Flags off by default** (validated as not improving CAGR, see results below):
  `--risk_target_lev` (F1), `--resid_mom` (F2), `--skip_days` (F3), `--convex_sizing`.
- **Shared logic:** `R/10_option_d_features.R` is sourced by BOTH the live script and the
  backtest engine. Put new ranking or sizing logic there, never only in one place.

## Evaluation rules (non-negotiable)

1. **Point-in-time universe only.** Any CAGR claim must come from
   `scratch/backtest_option_d_pit.R pit_n100`. Static lists (`symbols_broad.txt` and friends)
   are 2026 snapshots: hindsight- and survivorship-biased. On them the strategy shows
   100–196% CAGR; on the PIT Nasdaq-100 the same strategy shows about 30%. Results from
   `set_universe(read_list(...))` scripts are not evidence.
2. **Realistic costs.** Use `--broker=schwab` (12.075% effective margin rate on debits under
   $25k, modeled as T-bill + 8.2%) with `--maint=0.30` margin calls, unless the account moves.
   The legacy assumption (T-bill + 1.75%, no margin calls) understates costs.
3. **Acceptance rule for any feature:**
   - test-period CAGR (2017-01 → 2026-10) beats the LIVE config row;
   - every value in the feature's parameter grid (at least 3 values, chosen before running)
     moves in the same direction; non-monotone grids are noise;
   - deflated Sharpe ratio > 0.95, counting `--prior_trials` (about 250 configurations were
     tried on this history before the PIT harness existed).
4. **Report 3/10/20-year windows** (2023-10-03, 2016-10-03, 2007-01-02 → 2026-10-06), pre- and
   after-tax, next to the QQQ and QLD buy-and-hold rows the runner prints.
5. **Taxes:** the account owner's combined marginal rates are 40% short-term and 20% long-term:
   `--tax_st=0.40 --tax_lt=0.20`. The strategy realizes nearly all gains short-term every
   year; buy-and-hold defers. Always compare after-tax.
6. **Live/backtest parity.** `R/11_option_d_engine.R` must reproduce what the live script would
   do. Its default config reproduces the original `sim_buffer_engine()` bit for bit; keep that
   parity test green. Any live change needs the matching engine change and a test.

## Results so far (PIT Nasdaq-100, Schwab financing, run 2026-11-02)

| Row | CAGR 3y / 10y / 20y | Max DD 20y | Note |
|---|---|---|---|
| LIVE config | 25.9% / 30.5% / 32.1% | 66.1% | Sharpe 0.81 |
| Turbo OFF (1.5x) | about −0.4pp full | 64.2% | turbo adds nothing measurable |
| FIP OFF | −3.2pp full, −3.6pp test | 68.4% | FIP kept; only one weight tested |
| F2 residual, F3 skip | worse on CAGR | | rejected |
| F1 risk target 25/30/35% | −4.7 to −6.7pp full | 38–48% | best Sharpe (0.90); not yet tested at matched vol |

Open items: an F1 grid at matched volatility (sigma 0.40/0.45/0.50 × kelly 0.5/0.75); a FIP
weight grid (0.15/0.30/0.45); a decision on the turbo; benchmark and after-tax rows with
the owner's rates.

## Known divergences and hazards

- The live scanner trades `symbols_broad.txt` (`SYMBOLS_FILE`), but only the PIT Nasdaq-100 is
  validated. The live universe is not the tested universe.
- Bear regime: live equal-weights the 5 lowest-vol names; the engine uses `solve_min_var()`.
- In residual mode (F2) the base score is an information ratio (about 1–3), so the FIP term
  (±0.15) is negligible; standardize scores cross-sectionally before blending.
- `tests/test_clean_engine.R` fails on main; it predates this work and covers
  `R/09_portfolio_backtest_clean.R`, which the live path does not use.
- Reg T caps overnight gross at 2x in a standard margin account. Margin-call thresholds:
  1 − (G − 1)/(G(1 − m)), i.e. a 46% decline at 1.6x and 30% maintenance.

## Commands

```bash
bash scratch/build_option_d_caches.sh      # once: gitignored price caches (needs Yahoo access)
Rscript scratch/backtest_option_d_pit.R pit_n100 --broker=schwab --tax_st=0.40 --tax_lt=0.20
Rscript tests/test_option_d_features.R     # unit + engine parity + smoke (must stay green)
Rscript tests/smoke_core_satellite_signal.R  # offline end-to-end run of the live script
Rscript tests/run_tests.R                  # legacy suite
./auto_trade_coresat_schwab.sh --dry_run=true   # never run with --dry_run=false from an agent
```

## Working conventions

- Never place, modify or cancel live orders, and never run the Schwab pipeline with
  `--dry_run=false`.
- New features go behind a flag that defaults to off, with an engine implementation, a test,
  and a PIT run before the default changes.
- Keep commits small and logically separated; a fix to the live path should be
  cherry-pickable on its own.

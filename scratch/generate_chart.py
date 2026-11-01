import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import shutil

plt.style.use('seaborn-v0_8-whitegrid' if 'seaborn-v0_8-whitegrid' in plt.style.available else 'default')
fig, axes = plt.subplots(2, 2, figsize=(16, 11), dpi=300)
fig.patch.set_facecolor('#ffffff')

# Load data
df_standalone = pd.read_csv("output/defensive_cash_standalone_backtest_scorecard.csv")
df_port = pd.read_csv("output/defensive_cash_portfolio_backtest_scorecard.csv")

horizons = ["1 Month", "6 Months", "12 Months", "24 Months", "36 Months", "60 Months", "120 Months"]
strategies = [
    "Without PSQ (0% Cash)",
    "100% SGOV (Yield)",
    "75% SGOV / 25% PSQ",
    "50% SGOV / 50% PSQ",
    "25% SGOV / 75% PSQ",
    "100% PSQ (Full Hedge)",
    "GLD / SGOV Switch"
]

colors = {
    "Without PSQ (0% Cash)": "#7f8c8d",
    "100% SGOV (Yield)": "#2980b9",
    "75% SGOV / 25% PSQ": "#16a085",
    "50% SGOV / 50% PSQ": "#e67e22",
    "25% SGOV / 75% PSQ": "#d35400",
    "100% PSQ (Full Hedge)": "#c0392b",
    "GLD / SGOV Switch": "#f39c12"
}

# 1. Panel A: Standalone CAGR across horizons
ax1 = axes[0, 0]
x = np.arange(len(horizons))
width = 0.11

for i, strat in enumerate(strategies):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [sub[sub['Horizon'] == h]['CAGR'].values[0] if len(sub[sub['Horizon'] == h]) > 0 else 0 for h in horizons]
    ax1.bar(x + (i - 3) * width, vals, width, label=strat, color=colors[strat], alpha=0.9)

ax1.set_title("A. Standalone Cash/Hedge Module: CAGR (%) across Horizons", fontsize=12, fontweight='bold', pad=10)
ax1.set_xticks(x)
ax1.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax1.set_ylabel("Annualized Return (CAGR %)", fontweight='bold')
ax1.legend(loc='upper right', fontsize=8, frameon=True)
ax1.set_ylim(0, 160)

# 2. Panel B: Standalone Max Drawdown across horizons
ax2 = axes[0, 1]
for i, strat in enumerate(strategies):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [abs(sub[sub['Horizon'] == h]['Max_DD'].values[0]) if len(sub[sub['Horizon'] == h]) > 0 else 0 for h in horizons]
    ax2.bar(x + (i - 3) * width, vals, width, label=strat, color=colors[strat], alpha=0.9)

ax2.set_title("B. Standalone Cash/Hedge Module: Max Drawdown (%) across Horizons", fontsize=12, fontweight='bold', pad=10)
ax2.set_xticks(x)
ax2.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax2.set_ylabel("Max Drawdown (%) [Lower is Safer]", fontweight='bold')
ax2.set_ylim(0, 32)

# 3. Panel C: Standalone Sharpe Ratio across horizons
ax3 = axes[1, 0]
for i, strat in enumerate(strategies):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [sub[sub['Horizon'] == h]['Sharpe'].values[0] if len(sub[sub['Horizon'] == h]) > 0 else 0 for h in horizons]
    ax3.bar(x + (i - 3) * width, vals, width, label=strat, color=colors[strat], alpha=0.9)

ax3.set_title("C. Standalone Cash/Hedge Module: Risk-Adjusted Sharpe Ratio", fontsize=12, fontweight='bold', pad=10)
ax3.set_xticks(x)
ax3.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax3.set_ylabel("Sharpe Ratio [Higher is Better]", fontweight='bold')
ax3.set_ylim(0, 6.5)

# 4. Panel D: Full Integrated Portfolio Terminal Wealth on $10,000 (10-Year)
ax4 = axes[1, 1]
sub_10y = df_port[df_port['Horizon'] == "120 Months"].set_index('Strategy')
w_vals = [sub_10y.loc[s, 'Terminal_Wealth'] / 1e6 for s in strategies]
bar_cols = [colors[s] for s in strategies]
bars = ax4.barh(strategies, w_vals, color=bar_cols, alpha=0.9, height=0.6)

for bar in bars:
    w = bar.get_width()
    ax4.text(w + 0.3, bar.get_y() + bar.get_height()/2, f"${w:.2f}M", 
             va='center', ha='left', fontsize=9, fontweight='bold')

ax4.set_title("D. Full Swing Portfolio Terminal Wealth (10-Year, $10K Starting)", fontsize=12, fontweight='bold', pad=10)
ax4.set_xlabel("Terminal Wealth ($ Millions)", fontweight='bold')
ax4.set_xlim(0, 26)

plt.tight_layout()
os.makedirs("output", exist_ok=True)
chart_path = "output/defensive_cash_comparison_chart.png"
plt.savefig(chart_path, dpi=300, bbox_inches='tight')
print(f"Chart successfully saved to {chart_path}")

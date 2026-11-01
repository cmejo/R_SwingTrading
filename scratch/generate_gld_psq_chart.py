import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os
import shutil

plt.style.use('seaborn-v0_8-whitegrid' if 'seaborn-v0_8-whitegrid' in plt.style.available else 'default')
fig, axes = plt.subplots(2, 2, figsize=(16, 11), dpi=300)
fig.patch.set_facecolor('#ffffff')

df_standalone = pd.read_csv("output/gld_psq_sgov_standalone_scorecard.csv")
df_port = pd.read_csv("output/gld_psq_sgov_portfolio_scorecard.csv")

horizons = ["1 Month", "6 Months", "12 Months", "24 Months", "36 Months", "60 Months", "120 Months"]
focus_strats = [
    "GLD / 100% SGOV Switch",
    "GLD / (25% PSQ + 75% SGOV) Switch",
    "GLD / (50% PSQ + 50% SGOV) Switch",
    "GLD / (75% PSQ + 25% SGOV) Switch"
]

colors = {
    "GLD / 100% SGOV Switch": "#f39c12",               # Gold / Amber
    "GLD / (25% PSQ + 75% SGOV) Switch": "#27ae60",     # Emerald green (Optimal Sortino)
    "GLD / (50% PSQ + 50% SGOV) Switch": "#2980b9",     # Blue
    "GLD / (75% PSQ + 25% SGOV) Switch": "#c0392b"      # Red (Aggressive bear)
}

labels = {
    "GLD / 100% SGOV Switch": "GLD / 100% SGOV",
    "GLD / (25% PSQ + 75% SGOV) Switch": "GLD / [25% PSQ + 75% SGOV]",
    "GLD / (50% PSQ + 50% SGOV) Switch": "GLD / [50% PSQ + 50% SGOV]",
    "GLD / (75% PSQ + 25% SGOV) Switch": "GLD / [75% PSQ + 25% SGOV]"
}

x = np.arange(len(horizons))
width = 0.18

# 1. Panel A: Standalone CAGR
ax1 = axes[0, 0]
for i, strat in enumerate(focus_strats):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [sub[sub['Horizon'] == h]['CAGR'].values[0] for h in horizons]
    ax1.bar(x + (i - 1.5) * width, vals, width, label=labels[strat], color=colors[strat], alpha=0.9)

ax1.set_title("A. Standalone Defensive Module: Annualized Return (CAGR %)", fontsize=12, fontweight='bold', pad=10)
ax1.set_xticks(x)
ax1.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax1.set_ylabel("CAGR (%)", fontweight='bold')
ax1.legend(loc='upper right', fontsize=9, frameon=True)

# 2. Panel B: Standalone Max Drawdown
ax2 = axes[0, 1]
for i, strat in enumerate(focus_strats):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [abs(sub[sub['Horizon'] == h]['Max_DD'].values[0]) for h in horizons]
    ax2.bar(x + (i - 1.5) * width, vals, width, label=labels[strat], color=colors[strat], alpha=0.9)

ax2.set_title("B. Standalone Max Drawdown (%) [Lower is Safer]", fontsize=12, fontweight='bold', pad=10)
ax2.set_xticks(x)
ax2.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax2.set_ylabel("Max Drawdown (%)", fontweight='bold')
ax2.set_ylim(0, 18)

# 3. Panel C: Downside Sortino Ratio
ax3 = axes[1, 0]
for i, strat in enumerate(focus_strats):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [sub[sub['Horizon'] == h]['Sortino'].values[0] for h in horizons]
    ax3.bar(x + (i - 1.5) * width, vals, width, label=labels[strat], color=colors[strat], alpha=0.9)

ax3.set_title("C. Downside Risk-Adjusted Quality: Sortino Ratio [Higher is Better]", fontsize=12, fontweight='bold', pad=10)
ax3.set_xticks(x)
ax3.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax3.set_ylabel("Sortino Ratio", fontweight='bold')
ax3.set_ylim(0, 15)

# 4. Panel D: Full Swing Portfolio Terminal Wealth (10-Year, $10K Starting)
ax4 = axes[1, 1]
sub_10y = df_port[df_port['Horizon'] == "120 Months"].set_index('Strategy')
w_vals = [sub_10y.loc[s, 'Terminal_Wealth'] / 1e6 for s in focus_strats]
short_labels = [labels[s] for s in focus_strats]
bar_cols = [colors[s] for s in focus_strats]
bars = ax4.barh(short_labels, w_vals, color=bar_cols, alpha=0.9, height=0.55)

for bar in bars:
    w = bar.get_width()
    ax4.text(w + 0.4, bar.get_y() + bar.get_height()/2, f"${w:.2f}M", 
             va='center', ha='left', fontsize=10, fontweight='bold')

ax4.set_title("D. Full Swing Portfolio Terminal Wealth (10-Year, $10K Starting)", fontsize=12, fontweight='bold', pad=10)
ax4.set_xlabel("Terminal Wealth ($ Millions)", fontweight='bold')
ax4.set_xlim(0, 36)

plt.tight_layout()
os.makedirs("output", exist_ok=True)
chart_path = "output/gld_psq_sgov_comparison_chart.png"
plt.savefig(chart_path, dpi=300, bbox_inches='tight')
print(f"Chart saved to {chart_path}")

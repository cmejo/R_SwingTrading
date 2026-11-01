import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os

plt.style.use('seaborn-v0_8-whitegrid' if 'seaborn-v0_8-whitegrid' in plt.style.available else 'default')
fig, axes = plt.subplots(2, 2, figsize=(18, 12), dpi=300)
fig.patch.set_facecolor('#ffffff')

df_standalone = pd.read_csv("output/inverse_gld_standalone_scorecard.csv")
df_port = pd.read_csv("output/inverse_gld_portfolio_scorecard.csv")

horizons = ["1 Month", "6 Months", "12 Months", "24 Months", "36 Months", "60 Months", "120 Months"]

# Highlight key representative strategies across categories
key_strats = [
    ("1. GLD / 100% SGOV (Baseline)", "#f39c12", "GLD / 100% SGOV (Baseline)"),
    ("3. GLD / [25% PSQ + 75% SGOV]", "#27ae60", "GLD / [25% PSQ + 75% SGOV] (Top Sortino)"),
    ("2. GLD / [75% PSQ + 25% SGOV]", "#16a085", "GLD / [75% PSQ + 25% SGOV]"),
    ("8. GLD / [50% QID + 50% SGOV]", "#2980b9", "GLD / [50% QID (2x) + 50% SGOV]"),
    ("10. GLD / [50% SQQQ + 50% SGOV]", "#8e44ad", "GLD / [50% SQQQ (3x) + 50% SGOV]"),
    ("6. GLD / [50% FNGD + 50% SGOV]", "#d35400", "GLD / [50% FNGD (-3x) + 50% SGOV]"),
    ("4. GLD / [50% SOXS + 50% SGOV]", "#c0392b", "GLD / [50% SOXS (-3x) + 50% SGOV]")
]

x = np.arange(len(horizons))
width = 0.12

# 1. Panel A: Standalone CAGR across horizons
ax1 = axes[0, 0]
for i, (strat, color, label) in enumerate(key_strats):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [sub[sub['Horizon'] == h]['CAGR'].values[0] for h in horizons]
    ax1.bar(x + (i - 3) * width, vals, width, label=label, color=color, alpha=0.9)

ax1.set_title("A. Standalone Defensive Module: Annualized Return (CAGR %)", fontsize=11, fontweight='bold', pad=10)
ax1.set_xticks(x)
ax1.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax1.set_ylabel("CAGR (%)", fontweight='bold')
ax1.legend(loc='upper right', fontsize=8, frameon=True)

# 2. Panel B: Standalone Max Drawdown (Risk of Decay)
ax2 = axes[0, 1]
for i, (strat, color, label) in enumerate(key_strats):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [abs(sub[sub['Horizon'] == h]['Max_DD'].values[0]) for h in horizons]
    ax2.bar(x + (i - 3) * width, vals, width, label=label, color=color, alpha=0.9)

ax2.set_title("B. Standalone Max Drawdown (%) [Lower is Safer - Note SOXS Drag]", fontsize=11, fontweight='bold', pad=10)
ax2.set_xticks(x)
ax2.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax2.set_ylabel("Max Drawdown (%)", fontweight='bold')
ax2.set_ylim(0, 60)

# 3. Panel C: Downside Sortino Ratio across horizons
ax3 = axes[1, 0]
for i, (strat, color, label) in enumerate(key_strats):
    sub = df_standalone[df_standalone['Strategy'] == strat]
    vals = [sub[sub['Horizon'] == h]['Sortino'].values[0] for h in horizons]
    ax3.bar(x + (i - 3) * width, vals, width, label=label, color=color, alpha=0.9)

ax3.set_title("C. Downside Risk-Adjusted Quality: Sortino Ratio [Higher is Better]", fontsize=11, fontweight='bold', pad=10)
ax3.set_xticks(x)
ax3.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax3.set_ylabel("Sortino Ratio", fontweight='bold')
ax3.set_ylim(0, 14)

# 4. Panel D: Full Swing Portfolio 10-Year Terminal Wealth ($10K Starting)
ax4 = axes[1, 1]
sub_10y = df_port[df_port['Horizon'] == "120 Months"].set_index('Strategy')
all_14_strats = df_port[df_port['Horizon'] == "120 Months"]['Strategy'].tolist()

w_vals = [sub_10y.loc[s, 'Terminal_Wealth'] / 1e6 for s in all_14_strats]
short_names = [
    "GLD/SGOV Baseline",
    "GLD + 75% PSQ/25% SGOV",
    "GLD + 25% PSQ/75% SGOV",
    "GLD + 50% SOXS/50% SGOV",
    "GLD + 75% SOXS/25% SGOV",
    "GLD + 50% FNGD/50% SGOV",
    "GLD + 75% FNGD/25% SGOV",
    "GLD + 50% QID/50% SGOV",
    "GLD + 75% QID/25% SGOV",
    "GLD + 50% SQQQ/50% SGOV",
    "GLD + 75% SQQQ/25% SGOV",
    "GLD + [25% FNGD, 25% PSQ]",
    "GLD + [25% FNGD, 25% SQQQ]",
    "GLD + [25% QID, 25% FNGD]"
]

bar_colors = [
    "#f39c12", "#16a085", "#27ae60", "#e74c3c", "#c0392b",
    "#e67e22", "#d35400", "#2980b9", "#1f618d", "#8e44ad",
    "#6c3483", "#1abc9c", "#34495e", "#2c3e50"
]

bars = ax4.barh(short_names, w_vals, color=bar_colors, alpha=0.9, height=0.65)
for bar in bars:
    w = bar.get_width()
    ax4.text(w + 0.8, bar.get_y() + bar.get_height()/2, f"${w:.1f}M", 
             va='center', ha='left', fontsize=8.5, fontweight='bold')

ax4.set_title("D. Full Swing Portfolio Terminal Wealth (10-Year, $10K Starting)", fontsize=11, fontweight='bold', pad=10)
ax4.set_xlabel("Terminal Wealth ($ Millions)", fontweight='bold')
ax4.set_xlim(0, 75)

plt.tight_layout()
os.makedirs("output", exist_ok=True)
chart_path = "output/inverse_gld_comparison_chart.png"
plt.savefig(chart_path, dpi=300, bbox_inches='tight')
print(f"Chart saved to {chart_path}")

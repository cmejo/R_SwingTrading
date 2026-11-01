import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os

plt.style.use('seaborn-v0_8-whitegrid' if 'seaborn-v0_8-whitegrid' in plt.style.available else 'default')
fig, axes = plt.subplots(2, 2, figsize=(18, 12), dpi=300)
fig.patch.set_facecolor('#ffffff')

df_port = pd.read_csv("output/fngd_combo_portfolio_scorecard.csv")
df_cash = pd.read_csv("output/fngd_combo_standalone_scorecard.csv")

horizons = ["1 Month", "6 Months", "12 Months", "24 Months", "36 Months", "60 Months", "120 Months"]

# 10-year terminal wealth ranking
sub_10y = df_port[df_port['Horizon'] == "120 Months"].sort_values(by="Terminal_Wealth", ascending=False)
ranked_strats = sub_10y['Strategy'].tolist()

short_labels = {
    "1. GLD / 100% SGOV (Baseline)": "GLD / 100% SGOV",
    "2. GLD / [25% PSQ + 75% SGOV]": "GLD / [25% PSQ + 75% SGOV]",
    "3. GLD / [50% QID + 50% SGOV]": "GLD / [50% QID + 50% SGOV]",
    "4. GLD / [75% FNGD + 25% SGOV]": "GLD / [75% FNGD + 25% SGOV]",
    "5. GLD / [50% FNGD + 50% QID]": "GLD / [50% FNGD + 50% QID]",
    "6. GLD / [75% FNGD + 25% QID]": "GLD / [75% FNGD + 25% QID]",
    "7. GLD / [75% FNGD + 25% SQQQ]": "GLD / [75% FNGD + 25% SQQQ]",
    "8. GLD / [50% FNGD + 50% SQQQ]": "GLD / [50% FNGD + 50% SQQQ]",
    "9. GLD / [75% FNGD + 25% PSQ]": "GLD / [75% FNGD + 25% PSQ]",
    "10. GLD / [50% FNGD + 50% PSQ]": "GLD / [50% FNGD + 50% PSQ]"
}

colors = [
    "#c0392b", "#e74c3c", "#d35400", "#e67e22", "#f39c12",
    "#9b59b6", "#8e44ad", "#2980b9", "#27ae60", "#16a085"
]

# Panel A: Full Portfolio CAGR across horizons (Top 5 vs SGOV baseline)
ax1 = axes[0, 0]
top_strats = [ranked_strats[0], ranked_strats[1], ranked_strats[2], ranked_strats[7], ranked_strats[8], ranked_strats[9]]
x = np.arange(len(horizons))
width = 0.14
for i, s in enumerate(top_strats):
    sub = df_port[df_port['Strategy'] == s]
    vals = [sub[sub['Horizon'] == h]['CAGR'].values[0] for h in horizons]
    ax1.bar(x + (i - 2.5) * width, vals, width, label=short_labels[s], alpha=0.9)

ax1.set_title("A. Full Swing Portfolio CAGR (%) Across Horizons", fontsize=11, fontweight='bold', pad=10)
ax1.set_xticks(x)
ax1.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax1.set_ylabel("Annualized Return (CAGR %)", fontweight='bold')
ax1.legend(loc='upper right', fontsize=8, frameon=True)

# Panel B: Standalone Max Drawdown across horizons
ax2 = axes[0, 1]
for i, s in enumerate(top_strats):
    sub = df_cash[df_cash['Strategy'] == s]
    vals = [abs(sub[sub['Horizon'] == h]['Max_DD'].values[0]) for h in horizons]
    ax2.bar(x + (i - 2.5) * width, vals, width, label=short_labels[s], alpha=0.9)

ax2.set_title("B. Standalone Cash Module Max Drawdown (%) [Lower is Safer]", fontsize=11, fontweight='bold', pad=10)
ax2.set_xticks(x)
ax2.set_xticklabels(["1M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y"], fontweight='bold')
ax2.set_ylabel("Max Drawdown (%)", fontweight='bold')
ax2.set_ylim(0, 55)

# Panel C: Standalone 10-Year Sortino Ratio (Downside Quality)
ax3 = axes[1, 0]
sub_10y_cash = df_cash[df_cash['Horizon'] == "120 Months"].set_index('Strategy')
sortino_vals = [sub_10y_cash.loc[s, 'Sortino'] for s in ranked_strats]
bars_s = ax3.barh([short_labels[s] for s in ranked_strats], sortino_vals, color=colors, alpha=0.9, height=0.6)
for bar in bars_s:
    w = bar.get_width()
    ax3.text(w + 0.15, bar.get_y() + bar.get_height()/2, f"{w:.2f}", 
             va='center', ha='left', fontsize=9, fontweight='bold')

ax3.set_title("C. 10-Year Standalone Sortino Ratio [Higher is Better - Note 25% PSQ]", fontsize=11, fontweight='bold', pad=10)
ax3.set_xlabel("Sortino Ratio", fontweight='bold')
ax3.set_xlim(0, 10.5)

# Panel D: 10-Year Portfolio Terminal Wealth ($ Millions)
ax4 = axes[1, 1]
wealth_vals = [sub_10y.loc[sub_10y['Strategy'] == s, 'Terminal_Wealth'].values[0] / 1e6 for s in ranked_strats]
bars_w = ax4.barh([short_labels[s] for s in ranked_strats], wealth_vals, color=colors, alpha=0.9, height=0.6)
for bar in bars_w:
    w = bar.get_width()
    ax4.text(w + 1.2, bar.get_y() + bar.get_height()/2, f"${w:.1f}M", 
             va='center', ha='left', fontsize=9, fontweight='bold')

ax4.set_title("D. 10-Year Swing Portfolio Terminal Wealth ($ Millions on $10K Start)", fontsize=11, fontweight='bold', pad=10)
ax4.set_xlabel("Terminal Wealth ($ Millions)", fontweight='bold')
ax4.set_xlim(0, 98)

plt.tight_layout()
os.makedirs("output", exist_ok=True)
chart_path = "output/fngd_hybrid_combos_chart.png"
plt.savefig(chart_path, dpi=300, bbox_inches='tight')
print(f"Chart saved to {chart_path}")

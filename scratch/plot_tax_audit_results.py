#!/usr/bin/env python3
"""
Generate publication-quality infographic of 3-Year, 10-Year, and 20-Year After-Tax Performance.
Comparing 6 explicit tax regimes on a $10,000 initial investment:
  (1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp @ 28.4%)
  (2) Model: Taxable NYC Resident (51.2% STCG/ordinary)
  (3) Model: Taxable NYS Resident (47.7% STCG/ordinary)
  (4) Benchmark: QQQ Buy & Hold (NYC Taxable @ 34.1% LTCG/div)
  (5) Benchmark: QQQ Buy & Hold (NYS Taxable @ 30.7% LTCG/div)
  (6) Tax-Advantaged (IRA / 401k / Act 60 @ 0.0%)
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
from matplotlib.gridspec import GridSpec

# Read scorecard
df = pd.read_csv("scratch/scorecard_tax_audit_3_10_20yr.csv")

# Set overall style
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['Helvetica', 'Arial', 'DejaVu Sans']
fig = plt.figure(figsize=(18, 12), dpi=150)
fig.patch.set_facecolor('#f8f9fa')

gs = GridSpec(3, 2, figure=fig, width_ratios=[1.3, 1.0], hspace=0.35, wspace=0.22,
              top=0.92, bottom=0.06, left=0.07, right=0.95)

windows = ["3-Year (2023 - 2026)", "10-Year (2016 - 2026)", "20-Year (2007 - 2026)"]
window_titles = [
    "3-Year Horizon (2023 – 2026): Post-Bear AI Bull Market",
    "10-Year Horizon (2016 – 2026): Secular Tech Expansion & Volatility",
    "20-Year Horizon (2007 – 2026): Full Cycle (GFC, 2020, 2022, 2026)"
]

# Color map for regimes
color_map = {
    "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)": "#0D47A1",        # Deep Navy
    "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)": "#1976D2",   # Vibrant Blue
    "(3) Model: Taxable NYS Resident (Fed + State)": "#42A5F5",          # Light Blue
    "(2) Model: Taxable NYC Resident (Fed + State + City)": "#E65100",   # Deep Orange
    "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)": "#2E7D32",   # Forest Green
    "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)": "#78909C",            # Slate Grey
    "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)": "#B0BEC5"             # Light Grey
}

short_names = {
    "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)": "Model: Tax-Advantaged (0%)",
    "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)": "Model: TTS S-Corp (28.4%)",
    "(3) Model: Taxable NYS Resident (Fed + State)": "Model: NYS Taxable (47.7%)",
    "(2) Model: Taxable NYC Resident (Fed + State + City)": "Model: NYC Taxable (51.2%)",
    "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)": "QQQ: Tax-Advantaged (0%)",
    "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)": "QQQ: NYS Taxable (30.7%)",
    "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)": "QQQ: NYC Taxable (34.1%)"
}

def fmt_dollars(val):
    if val >= 1e9:
        return f"${val/1e9:.2f}B"
    elif val >= 1e6:
        return f"${val/1e6:.2f}M"
    elif val >= 1e3:
        return f"${val/1e3:.1f}k"
    else:
        return f"${val:.0f}"

# Desired display order (top to bottom)
regime_order = [
    "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)",
    "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)",
    "(3) Model: Taxable NYS Resident (Fed + State)",
    "(2) Model: Taxable NYC Resident (Fed + State + City)",
    "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)",
    "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)",
    "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)"
]

fig.suptitle("Ultimate Alpha Master — After-Tax Performance & Wealth Compounding Audit\nInitial Investment: $10,000 | 6 Tax Regimes across 3-Year, 10-Year, and 20-Year Backtests",
             fontsize=16, fontweight='bold', color='#1A237E', y=0.97)

for row_idx, (w_name, w_title) in enumerate(zip(windows, window_titles)):
    sub = df[df["Window"] == w_name].copy()
    sub.set_index("Regime", inplace=True)
    sub = sub.reindex(regime_order)
    
    # Left Column: After-Tax CAGR (%)
    ax_cagr = fig.add_subplot(gs[row_idx, 0])
    ax_cagr.set_facecolor('#ffffff')
    ax_cagr.grid(axis='x', linestyle='--', alpha=0.5, zorder=0)
    
    y_pos = np.arange(len(regime_order))
    y_pos_rev = y_pos[::-1]
    
    cagrs = sub["CAGR"].values
    colors = [color_map[r] for r in regime_order]
    labels = [short_names[r] for r in regime_order]
    
    bars = ax_cagr.barh(y_pos_rev, cagrs, height=0.68, color=colors, zorder=3, edgecolor='none')
    
    # Value annotations on bars
    max_cagr = max(cagrs)
    for idx, (bar, cagr, r_name) in enumerate(zip(bars, cagrs, regime_order)):
        w_terminal = sub.loc[r_name, "Terminal_Wealth"]
        text_str = f" {cagr:+.1f}%  ({fmt_dollars(w_terminal)})"
        ax_cagr.text(cagr + (max_cagr * 0.015), bar.get_y() + bar.get_height()/2, text_str,
                     va='center', ha='left', fontsize=9.5, fontweight='bold', color='#263238')
        
    ax_cagr.set_yticks(y_pos_rev)
    ax_cagr.set_yticklabels(labels, fontsize=9.5, fontweight='semibold')
    ax_cagr.set_xlim(0, max_cagr * 1.35)
    ax_cagr.set_xlabel("Net Annualized After-Tax CAGR (%)", fontsize=9.5, fontweight='bold', color='#37474F')
    ax_cagr.set_title(w_title, fontsize=11, fontweight='bold', color='#0D47A1', pad=8, loc='left')
    ax_cagr.tick_params(axis='both', which='both', length=0)
    
    # Right Column: Max Drawdown (%) & Taxes Paid Comparison
    ax_risk = fig.add_subplot(gs[row_idx, 1])
    ax_risk.set_facecolor('#ffffff')
    ax_risk.grid(axis='x', linestyle='--', alpha=0.5, zorder=0)
    
    max_dds = sub["Max_Drawdown"].values
    bars_dd = ax_risk.barh(y_pos_rev, max_dds, height=0.68, color=colors, alpha=0.85, zorder=3)
    
    max_val_dd = max(max_dds)
    for idx, (bar, dd, r_name) in enumerate(zip(bars_dd, max_dds, regime_order)):
        tax_paid = sub.loc[r_name, "Total_Tax_Paid"]
        tax_str = f" -{dd:.1f}%  [Tax: {fmt_dollars(tax_paid)}]" if tax_paid > 0 else f" -{dd:.1f}%  [Tax-Free]"
        ax_risk.text(dd + 1.5, bar.get_y() + bar.get_height()/2, tax_str,
                     va='center', ha='left', fontsize=9, fontweight='semibold', color='#37474F')
        
    ax_risk.set_yticks(y_pos_rev)
    ax_risk.set_yticklabels([])  # Hide y-labels on right panel
    ax_risk.set_xlim(0, max(85, max_val_dd * 1.35))
    ax_risk.set_xlabel("Peak-to-Trough Max Drawdown (%)", fontsize=9.5, fontweight='bold', color='#37474F')
    ax_risk.set_title(f"Risk & Friction: Max Drawdown & Total Tax Paid ({w_name.split()[0]})",
                      fontsize=11, fontweight='bold', color='#37474F', pad=8, loc='left')
    ax_risk.tick_params(axis='both', which='both', length=0)

plt.savefig("/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c/tax_audit_3_10_20yr_comparison.png",
            dpi=150, facecolor=fig.get_facecolor(), edgecolor='none')
print("Successfully generated infographic: tax_audit_3_10_20yr_comparison.png")

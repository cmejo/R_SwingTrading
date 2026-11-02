#!/usr/bin/env python3
"""
factsheet/make_tax_chart_asset.py
Generates a compact, high-impact horizontal bar chart of After-Tax CAGR and Terminal Wealth
for embedding directly into Page 3 of the 3-Page Factsheet.
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_csv("scratch/scorecard_tax_audit_3_10_20yr.csv")

plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.sans-serif'] = ['Helvetica', 'Arial', 'DejaVu Sans']

fig, axes = plt.subplots(1, 3, figsize=(15, 3.8), dpi=220)
fig.patch.set_facecolor('#ffffff')
plt.subplots_adjust(top=0.82, bottom=0.18, left=0.18, right=0.96, wspace=0.35)

windows = ["3-Year (2023 - 2026)", "10-Year (2016 - 2026)", "20-Year (2007 - 2026)"]
titles = ["3-Year (2023–26)", "10-Year (2016–26)", "20-Year (2007–26)"]

color_map = {
    "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)": "#0D47A1",        # Deep Navy
    "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)": "#1976D2",   # Blue
    "(3) Model: Taxable NYS Resident (Fed + State)": "#42A5F5",          # Light Blue
    "(2) Model: Taxable NYC Resident (Fed + State + City)": "#E65100",   # Orange
    "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)": "#2E7D32",   # Green
    "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)": "#78909C",            # Slate
    "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)": "#B0BEC5"             # Grey
}

short_names = {
    "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)": "Model: Tax-Adv (0%)",
    "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)": "Model: TTS S-Corp (28.4%)",
    "(3) Model: Taxable NYS Resident (Fed + State)": "Model: NYS Taxable (47.7%)",
    "(2) Model: Taxable NYC Resident (Fed + State + City)": "Model: NYC Taxable (51.2%)",
    "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)": "QQQ: Tax-Adv (0%)",
    "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)": "QQQ: NYS Taxable (30.7%)",
    "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)": "QQQ: NYC Taxable (34.1%)"
}

regime_order = [
    "(6a) Model: Tax-Advantaged (IRA / 401k / Act 60)",
    "(1) Model: Trader Tax Status (TTS Sec. 475(f) S-Corp)",
    "(3) Model: Taxable NYS Resident (Fed + State)",
    "(2) Model: Taxable NYC Resident (Fed + State + City)",
    "(6b) Benchmark: QQQ Buy & Hold (Tax-Advantaged / IRA)",
    "(5) Benchmark: QQQ Buy & Hold (NYS Taxable)",
    "(4) Benchmark: QQQ Buy & Hold (NYC Taxable)"
]

def fmt_dollars(val):
    if val >= 1e9:
        return f"${val/1e9:.2f}B"
    elif val >= 1e6:
        return f"${val/1e6:.1f}M"
    elif val >= 1e3:
        return f"${val/1e3:.0f}k"
    else:
        return f"${val:.0f}"

for idx, (w_name, title) in enumerate(zip(windows, titles)):
    ax = axes[idx]
    sub = df[df["Window"] == w_name].copy()
    sub.set_index("Regime", inplace=True)
    sub = sub.reindex(regime_order)
    
    y_pos = np.arange(len(regime_order))[::-1]
    cagrs = sub["CAGR"].values
    colors = [color_map[r] for r in regime_order]
    
    bars = ax.barh(y_pos, cagrs, height=0.68, color=colors, edgecolor='none')
    max_cagr = max(cagrs)
    
    for bar, cagr, r_name in zip(bars, cagrs, regime_order):
        w_terminal = sub.loc[r_name, "Terminal_Wealth"]
        label = f" {cagr:+.1f}% ({fmt_dollars(w_terminal)})"
        ax.text(cagr + (max_cagr * 0.02), bar.get_y() + bar.get_height()/2, label,
                va='center', ha='left', fontsize=8, fontweight='bold', color='#263238')
        
    ax.set_yticks(y_pos)
    if idx == 0:
        labels = [short_names[r] for r in regime_order]
        ax.set_yticklabels(labels, fontsize=8.2, fontweight='bold')
    else:
        ax.set_yticklabels([])
        
    ax.set_xlim(0, max_cagr * 1.45)
    ax.set_title(title, fontsize=10.5, fontweight='bold', color='#0B3D91', pad=6)
    ax.set_xlabel("After-Tax CAGR (%)", fontsize=8.5, fontweight='bold', color='#5F6368')
    ax.grid(axis='x', linestyle='--', alpha=0.5)
    ax.tick_params(axis='both', which='both', length=0)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    ax.spines['bottom'].set_color('#CFD8DC')

fig.suptitle("After-Tax Compounding & Terminal Wealth on $10,000 Invested (6 Tax Structures)",
             fontsize=12, fontweight='bold', color='#0B3D91', y=0.96)

out_path = "factsheet/img/tax_compounding_bars.png"
plt.savefig(out_path, facecolor='#ffffff', edgecolor='none')
print(f"Saved {out_path}")

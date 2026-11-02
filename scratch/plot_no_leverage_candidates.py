#!/usr/bin/env python3
import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

def plot_candidates():
    f10 = "scratch/scorecard_10yr_no_leverage_candidates.csv"
    f20 = "scratch/scorecard_20yr_no_leverage_candidates.csv"
    if not os.path.exists(f10) or not os.path.exists(f20):
        print("Scorecard files not found.")
        return

    df10 = pd.read_csv(f10, index_col=0)
    df20 = pd.read_csv(f20, index_col=0)

    # Clean candidate names for labels
    labels = [
        "Baseline (2.5x Turbo)",
        "1. Bi-Weekly Reb (10d)",
        "2. Frog-in-the-Pan (FIP)",
        "3. R² Trend Smoothness",
        "4. Residual Momentum",
        "5. Profit Trim (+100%)",
        "6. EWMA Vol Target (0.94)",
        "7. Dynamic Breadth (3/5)",
        "8. Apex Combo (FIP+EWMA)"
    ]

    fig, axes = plt.subplots(2, 2, figsize=(16, 12))
    plt.subplots_adjust(hspace=0.35, wspace=0.25)

    base_cagr_10 = df10.loc[df10.index[0], "CAGR"]
    base_cagr_20 = df20.loc[df20.index[0], "CAGR"]

    # 1. 10-Year CAGR
    ax1 = axes[0, 0]
    c10 = df10["CAGR"].values
    colors10 = ['#2b5c8f' if i == 0 else ('#2ca02c' if c >= base_cagr_10 + 5.0 else ('#1f77b4' if c >= base_cagr_10 else '#d62728')) for i, c in enumerate(c10)]
    bars1 = ax1.barh(range(len(labels)), c10, color=colors10, height=0.65)
    ax1.set_yticks(range(len(labels)))
    ax1.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax1.axvline(base_cagr_10, color='gray', linestyle='--', alpha=0.7, label=f'Baseline ({base_cagr_10:.1f}%)')
    ax1.axvline(base_cagr_10 + 5.0, color='gold', linestyle='--', linewidth=2, label=f'+5% Target ({base_cagr_10+5.0:.1f}%)')
    ax1.set_title("10-Year CAGR (2016 - 2026) [Zero Extra Leverage]", fontsize=12, fontweight='bold', pad=10)
    ax1.set_xlabel("CAGR (%)", fontsize=10)
    ax1.invert_yaxis()
    ax1.legend(loc="lower right", fontsize=9)
    for bar in bars1:
        w = bar.get_width()
        ax1.text(w + 1.5, bar.get_y() + bar.get_height()/2, f"{w:.1f}%", va='center', ha='left', fontsize=9, fontweight='bold')
    ax1.set_xlim(0, max(c10) * 1.15)

    # 2. 20-Year CAGR
    ax2 = axes[0, 1]
    c20 = df20["CAGR"].values
    colors20 = ['#2b5c8f' if i == 0 else ('#2ca02c' if c >= base_cagr_20 + 5.0 else ('#1f77b4' if c >= base_cagr_20 else '#d62728')) for i, c in enumerate(c20)]
    bars2 = ax2.barh(range(len(labels)), c20, color=colors20, height=0.65)
    ax2.set_yticks(range(len(labels)))
    ax2.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax2.axvline(base_cagr_20, color='gray', linestyle='--', alpha=0.7, label=f'Baseline ({base_cagr_20:.1f}%)')
    ax2.axvline(base_cagr_20 + 5.0, color='gold', linestyle='--', linewidth=2, label=f'+5% Target ({base_cagr_20+5.0:.1f}%)')
    ax2.set_title("20-Year CAGR (2007 - 2026) [Zero Extra Leverage]", fontsize=12, fontweight='bold', pad=10)
    ax2.set_xlabel("CAGR (%)", fontsize=10)
    ax2.invert_yaxis()
    ax2.legend(loc="lower right", fontsize=9)
    for bar in bars2:
        w = bar.get_width()
        ax2.text(w + 1.5, bar.get_y() + bar.get_height()/2, f"{w:.1f}%", va='center', ha='left', fontsize=9, fontweight='bold')
    ax2.set_xlim(0, max(c20) * 1.15)

    # 3. 20-Year Max Drawdown
    ax3 = axes[1, 0]
    dd20 = df20["MaxDD"].values
    colors_dd = ['#2b5c8f' if i == 0 else ('#2ca02c' if d < df20.loc[df20.index[0], "MaxDD"] else '#d62728') for i, d in enumerate(dd20)]
    bars3 = ax3.barh(range(len(labels)), dd20, color=colors_dd, height=0.65)
    ax3.set_yticks(range(len(labels)))
    ax3.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax3.axvline(df20.loc[df20.index[0], "MaxDD"], color='gray', linestyle='--', alpha=0.7, label=f"Baseline ({df20.loc[df20.index[0], 'MaxDD']:.1f}%)")
    ax3.set_title("20-Year Max Drawdown (% Depth, Lower is Better)", fontsize=12, fontweight='bold', pad=10)
    ax3.set_xlabel("Max Drawdown (%)", fontsize=10)
    ax3.invert_yaxis()
    ax3.legend(loc="lower right", fontsize=9)
    for bar in bars3:
        w = bar.get_width()
        ax3.text(w + 0.8, bar.get_y() + bar.get_height()/2, f"-{w:.1f}%", va='center', ha='left', fontsize=9, fontweight='bold')
    ax3.set_xlim(0, max(dd20) * 1.15)

    # 4. 20-Year Calmar Ratio
    ax4 = axes[1, 1]
    cal20 = df20["Calmar"].values
    base_cal = df20.loc[df20.index[0], "Calmar"]
    colors_cal = ['#2b5c8f' if i == 0 else ('#2ca02c' if c >= base_cal else '#d62728') for i, c in enumerate(cal20)]
    bars4 = ax4.barh(range(len(labels)), cal20, color=colors_cal, height=0.65)
    ax4.set_yticks(range(len(labels)))
    ax4.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax4.axvline(base_cal, color='gray', linestyle='--', alpha=0.7, label=f"Baseline ({base_cal:.2f})")
    ax4.set_title("20-Year Calmar Ratio (CAGR / MaxDD, Higher is Better)", fontsize=12, fontweight='bold', pad=10)
    ax4.set_xlabel("Calmar Ratio", fontsize=10)
    ax4.invert_yaxis()
    ax4.legend(loc="lower right", fontsize=9)
    for bar in bars4:
        w = bar.get_width()
        ax4.text(w + 0.05, bar.get_y() + bar.get_height()/2, f"{w:.2f}", va='center', ha='left', fontsize=9, fontweight='bold')
    ax4.set_xlim(0, max(cal20) * 1.15)

    fig.suptitle("Alpha Optimization Discovery: +5% CAGR Without Extra Leverage\nStrict Constraint: 2.50x Super-Bull / 1.50x Bull / 1.00x Bear", fontsize=15, fontweight='bold', y=0.98)
    
    out_path = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c/no_leverage_cagr_plus5pct_comparison.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight')
    print(f"Chart saved to {out_path}")

if __name__ == "__main__":
    plot_candidates()

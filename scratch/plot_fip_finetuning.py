#!/usr/bin/env python3
import os
import pandas as pd
import matplotlib.pyplot as plt

def plot_finetune():
    f3 = "scratch/scorecard_3yr_fip_finetuning.csv"
    f10 = "scratch/scorecard_10yr_fip_finetuning.csv"
    f20 = "scratch/scorecard_20yr_fip_finetuning.csv"
    if not (os.path.exists(f3) and os.path.exists(f10) and os.path.exists(f20)):
        print("Files not found.")
        return

    df3 = pd.read_csv(f3, index_col=0)
    df10 = pd.read_csv(f10, index_col=0)
    df20 = pd.read_csv(f20, index_col=0)

    labels = [
        "0. Baseline (Standard RS)",
        "1. FIP Standard (0.70/0.30)",
        "2. FIP Additive (+0.25)",
        "3. FIP Additive (+0.50)",
        "4. FIP Additive (+1.00)",
        "5. FIP Multiplicative",
        "6. Triple Horizon RS + FIP",
        "7. Dual Horizon FIP (63+126d)"
    ]

    fig, axes = plt.subplots(1, 3, figsize=(20, 7))
    plt.subplots_adjust(wspace=0.28)

    # 3-Year CAGR
    ax1 = axes[0]
    base3 = df3.loc[df3.index[0], "CAGR"]
    c3 = df3["CAGR"].values
    colors3 = ['#2b5c8f' if i == 0 else ('#2ca02c' if c >= base3 + 5.0 else ('#1f77b4' if c >= base3 else '#d62728')) for i, c in enumerate(c3)]
    bars1 = ax1.barh(range(len(labels)), c3, color=colors3, height=0.6)
    ax1.set_yticks(range(len(labels)))
    ax1.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax1.axvline(base3, color='gray', linestyle='--', alpha=0.7, label=f'Baseline ({base3:.1f}%)')
    ax1.axvline(base3 + 5.0, color='gold', linestyle='--', linewidth=1.5, label=f'+5% Target ({base3+5.0:.1f}%)')
    ax1.set_title("3-Year CAGR (2023 - 2026)", fontsize=12, fontweight='bold')
    ax1.set_xlabel("CAGR (%)", fontsize=10)
    ax1.invert_yaxis()
    ax1.legend(loc="lower right", fontsize=8)
    for bar in bars1:
        w = bar.get_width()
        ax1.text(w + 3.0, bar.get_y() + bar.get_height()/2, f"{w:.1f}%", va='center', ha='left', fontsize=8.5, fontweight='bold')
    ax1.set_xlim(0, max(c3) * 1.15)

    # 10-Year CAGR
    ax2 = axes[1]
    base10 = df10.loc[df10.index[0], "CAGR"]
    c10 = df10["CAGR"].values
    colors10 = ['#2b5c8f' if i == 0 else ('#2ca02c' if c >= base10 + 5.0 else ('#1f77b4' if c >= base10 else '#d62728')) for i, c in enumerate(c10)]
    bars2 = ax2.barh(range(len(labels)), c10, color=colors10, height=0.6)
    ax2.set_yticks(range(len(labels)))
    ax2.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax2.axvline(base10, color='gray', linestyle='--', alpha=0.7, label=f'Baseline ({base10:.1f}%)')
    ax2.axvline(base10 + 5.0, color='gold', linestyle='--', linewidth=1.5, label=f'+5% Target ({base10+5.0:.1f}%)')
    ax2.set_title("10-Year CAGR (2016 - 2026)", fontsize=12, fontweight='bold')
    ax2.set_xlabel("CAGR (%)", fontsize=10)
    ax2.invert_yaxis()
    ax2.legend(loc="lower right", fontsize=8)
    for bar in bars2:
        w = bar.get_width()
        ax2.text(w + 2.0, bar.get_y() + bar.get_height()/2, f"{w:.1f}%", va='center', ha='left', fontsize=8.5, fontweight='bold')
    ax2.set_xlim(0, max(c10) * 1.15)

    # 20-Year CAGR
    ax3 = axes[2]
    base20 = df20.loc[df20.index[0], "CAGR"]
    c20 = df20["CAGR"].values
    colors20 = ['#2b5c8f' if i == 0 else ('#2ca02c' if c >= base20 + 3.0 else ('#1f77b4' if c >= base20 else '#d62728')) for i, c in enumerate(c20)]
    bars3 = ax3.barh(range(len(labels)), c20, color=colors20, height=0.6)
    ax3.set_yticks(range(len(labels)))
    ax3.set_yticklabels(labels, fontsize=10, fontweight='bold')
    ax3.axvline(base20, color='gray', linestyle='--', alpha=0.7, label=f'Baseline ({base20:.1f}%)')
    ax3.axvline(base20 + 3.0, color='gold', linestyle='--', linewidth=1.5, label=f'+3% Target ({base20+3.0:.1f}%)')
    ax3.set_title("20-Year CAGR (2007 - 2026)", fontsize=12, fontweight='bold')
    ax3.set_xlabel("CAGR (%)", fontsize=10)
    ax3.invert_yaxis()
    ax3.legend(loc="lower right", fontsize=8)
    for bar in bars3:
        w = bar.get_width()
        ax3.text(w + 1.2, bar.get_y() + bar.get_height()/2, f"{w:.1f}%", va='center', ha='left', fontsize=8.5, fontweight='bold')
    ax3.set_xlim(0, max(c20) * 1.15)

    fig.suptitle("Continuous Momentum (Frog-in-the-Pan) Formulation Optimization Across 3, 10 & 20-Year Horizons\nStrict Constraint: Zero Extra Leverage (2.50x Super-Bull / 1.50x Bull / 1.00x Bear)", fontsize=14, fontweight='bold', y=1.02)

    out_path = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c/fip_finetuning_3_10_20yr_comparison.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight')
    print(f"Chart saved to {out_path}")

if __name__ == "__main__":
    plot_finetune()

#!/usr/bin/env python3
import os
import pandas as pd
import matplotlib.pyplot as plt

def plot_fip_tax():
    f = "scratch/scorecard_fip_vs_baseline_tax_audit.csv"
    if not os.path.exists(f):
        print("Tax scorecard not found.")
        return

    df = pd.read_csv(f)
    
    # Filter for 20-Year horizon
    df20 = df[df["Horizon"] == "20-Year (2007-2026)"].copy()
    
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 7))
    plt.subplots_adjust(wspace=0.3)
    
    # 1. 20-Year Terminal Wealth Comparison
    regimes = ["TTS Sec. 475(f)", "Taxable NYS", "Taxable NYC", "Tax-Advantaged (IRA)"]
    base_wealth = []
    fip_wealth = []
    
    for r in regimes:
        bw = df20[(df20["Strategy"] == "Baseline") & (df20["Regime"] == r)]["Terminal"].values[0] / 1e6
        fw = df20[(df20["Strategy"] == "FIP Winner") & (df20["Regime"] == r)]["Terminal"].values[0] / 1e6
        base_wealth.append(bw)
        fip_wealth.append(fw)
        
    y = range(len(regimes))
    h = 0.35
    
    ax1.barh([p - h/2 for p in y], base_wealth, height=h, label="Baseline (Standard RS)", color="#2b5c8f")
    ax1.barh([p + h/2 for p in y], fip_wealth, height=h, label="FIP Continuous Momentum", color="#2ca02c")
    ax1.set_yticks(y)
    ax1.set_yticklabels(regimes, fontsize=10, fontweight='bold')
    ax1.set_xscale('log')
    ax1.set_xlabel("Terminal Wealth ($ Millions, Log Scale)", fontsize=11, fontweight='bold')
    ax1.set_title("20-Year Terminal Wealth from $10k Initial Investment\n(Strict Zero Extra Leverage: 2.50x Turbo)", fontsize=12, fontweight='bold')
    ax1.legend(loc="lower right", fontsize=10)
    ax1.grid(True, which="both", ls="--", alpha=0.4)
    ax1.invert_yaxis()
    
    # 2. 20-Year After-Tax CAGR Comparison
    base_cagr = []
    fip_cagr = []
    for r in regimes:
        bc = df20[(df20["Strategy"] == "Baseline") & (df20["Regime"] == r)]["CAGR"].values[0]
        fc = df20[(df20["Strategy"] == "FIP Winner") & (df20["Regime"] == r)]["CAGR"].values[0]
        base_cagr.append(bc)
        fip_cagr.append(fc)
        
    ax2.barh([p - h/2 for p in y], base_cagr, height=h, label="Baseline (Standard RS)", color="#2b5c8f")
    ax2.barh([p + h/2 for p in y], fip_cagr, height=h, label="FIP Continuous Momentum", color="#2ca02c")
    ax2.set_yticks(y)
    ax2.set_yticklabels(regimes, fontsize=10, fontweight='bold')
    ax2.set_xlabel("Net After-Tax CAGR (%)", fontsize=11, fontweight='bold')
    ax2.set_title("20-Year Net After-Tax Compound Annual Growth Rate (CAGR)", fontsize=12, fontweight='bold')
    ax2.legend(loc="lower right", fontsize=10)
    ax2.grid(True, ls="--", alpha=0.4)
    ax2.invert_yaxis()
    
    for i, (b, f_val) in enumerate(zip(base_cagr, fip_cagr)):
        diff = f_val - b
        ax2.text(f_val + 1.0, i + h/2, f"{f_val:.1f}% (+{diff:.1f}%)", va='center', fontsize=9, fontweight='bold', color='#1b5e20')
        ax2.text(b + 1.0, i - h/2, f"{b:.1f}%", va='center', fontsize=9, fontweight='bold', color='#0d47a1')
        
    out_path = "/Users/cmejo/.gemini/antigravity/brain/64019830-0cac-4bb1-904e-d4560e71468c/fip_tax_comparison_20yr.png"
    plt.savefig(out_path, dpi=300, bbox_inches='tight')
    print(f"Chart saved to {out_path}")

if __name__ == "__main__":
    plot_fip_tax()

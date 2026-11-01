#!/usr/bin/env python3
"""
scratch/plot_statistical_accuracy_and_stability.py
Plots Monotonic Quintile Markout Curves and Rolling Parameter Adaptation Time Series.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def main():
    scorecard = pd.read_csv("output/univariate_feature_accuracy_scorecard.csv")
    quintiles = pd.read_csv("output/feature_quintile_markout_returns.csv")
    betas_df  = pd.read_csv("output/feature_rolling_parameter_betas.csv")
    
    # --------------------------------------------------------------------------
    # 1. Monotonic Quintile Markout Curves (Top 8 Informative Features)
    # --------------------------------------------------------------------------
    top_feats = [
        "CMF_20D", "CLV_5D", "IntradayVsGap_10D", "Prox52WHigh",
        "GARCH_Vol", "ATR_Comp_5v20", "EfficiencyRatio_20D", "VolSqueeze"
    ]
    
    fig, axes = plt.subplots(2, 4, figsize=(18, 9), sharey=False)
    axes = axes.flatten()
    
    q_cols = ["Q1", "Q2", "Q3", "Q4", "Q5"]
    x_q = [1, 2, 3, 4, 5]
    
    for i, f in enumerate(top_feats):
        ax = axes[i]
        row = quintiles[quintiles["Feature"] == f]
        sc_row = scorecard[scorecard["Feature"] == f]
        
        if len(row) > 0 and len(sc_row) > 0:
            y_vals = row[q_cols].values[0]
            mono = sc_row["Monotonicity_Score"].values[0]
            spread = sc_row["Q5_minus_Q1_Spread"].values[0]
            rank_ic = sc_row["Rank_IC"].values[0]
            
            # Plot quintile bars
            bar_colors = ["#4575b4", "#74add1", "#abd9e9", "#fdae61", "#d73027"]
            if rank_ic < 0:
                # If negative factor, reverse color palette to highlight favorable bucket
                bar_colors = bar_colors[::-1]
                
            ax.bar(x_q, y_vals, color=bar_colors, edgecolor="black", width=0.6, alpha=0.85)
            # Add trend line
            ax.plot(x_q, y_vals, color="navy", marker="o", linewidth=2.0, markersize=6)
            
            ax.set_title(f"{f}\nRank IC: {rank_ic:+.3f} | Mono: {mono:.2f} | Spread: {spread:+.2f}%",
                         fontsize=10, fontweight="bold")
            ax.set_xticks(x_q)
            ax.set_xticklabels(["Q1\n(Low)", "Q2", "Q3", "Q4", "Q5\n(High)"], fontsize=8)
            ax.set_ylabel("7-Day Forward Return (%)", fontsize=9)
            ax.grid(axis="y", linestyle="--", alpha=0.5)
            ax.axhline(0, color="gray", linestyle="-", linewidth=0.8)
    
    plt.suptitle("Out-of-Sample Monotonic Quintile Forward Markouts (7-Day Horizon, 2016-2026)",
                 fontsize=14, fontweight="bold", y=0.98)
    plt.tight_layout(rect=[0, 0, 1, 0.95])
    plt.savefig("output/feature_quintile_markout_curves.png", dpi=300)
    plt.close()
    print("[Success] Generated output/feature_quintile_markout_curves.png")

    # --------------------------------------------------------------------------
    # 2. Rolling Parameter Adaptation Time Series (Refit Dynamics across Regimes)
    # --------------------------------------------------------------------------
    fig, axes = plt.subplots(2, 3, figsize=(16, 8), sharex=True)
    axes = axes.flatten()
    
    eval_features = [
        "UpDownVolRatio_20D", "SlopeFast", "CMF_20D",
        "IntradayVsGap_10D", "GARCH_Vol", "RS_60"
    ]
    
    for i, f in enumerate(eval_features):
        ax = axes[i]
        sub = betas_df[betas_df["Feature"] == f].sort_values("Year")
        sc_row = scorecard[scorecard["Feature"] == f]
        
        if len(sub) > 0 and len(sc_row) > 0:
            stab = sc_row["Parameter_Stability"].values[0]
            sign_match = sc_row["Sign_Consistency_Pct"].values[0]
            cv_val = sc_row["CV_Beta1"].values[0]
            
            years = sub["Year"].tolist()
            betas = sub["Beta1"].tolist()
            
            color = "#2b5c8f" if "STABLE" in stab or "MODERATE" in stab else "#d95f02"
            ax.plot(years, betas, marker="s", linewidth=2.0, color=color, label="Rolling 2-Yr Beta")
            ax.axhline(0, color="black", linestyle="--", linewidth=1.0)
            ax.axhline(np.mean(betas), color="green", linestyle=":", linewidth=1.5, label=f"Mean Beta ({np.mean(betas):+.3f})")
            
            # Highlight COVID crash (2020) and 2022 bear market
            ax.axvspan(2019.5, 2020.5, color="gray", alpha=0.15)
            ax.axvspan(2021.5, 2022.5, color="gray", alpha=0.15)
            
            ax.set_title(f"{f}\nCV: {cv_val:.2f} | Sign Consistency: {sign_match:.1f}%\nStatus: {stab.split(' ')[0]}",
                         fontsize=9, fontweight="bold")
            ax.set_ylabel("Standardized Beta", fontsize=8)
            ax.grid(True, linestyle=":", alpha=0.5)
            if i == 0:
                ax.legend(loc="upper left", fontsize=7)
                
    for ax in axes[-3:]:
        ax.set_xlabel("Refit Year Window Ending", fontsize=9, fontweight="bold")
        
    plt.suptitle("Parameter Adaptation & Refit Stability Over Time (Rolling 2-Year Logistic Regressions)",
                 fontsize=14, fontweight="bold", y=0.98)
    plt.tight_layout(rect=[0, 0, 1, 0.95])
    plt.savefig("output/feature_parameter_adaptation_timeseries.png", dpi=300)
    plt.close()
    print("[Success] Generated output/feature_parameter_adaptation_timeseries.png")

if __name__ == "__main__":
    main()

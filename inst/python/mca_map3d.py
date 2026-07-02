#!/usr/bin/env python3
# Generic colour 3D MCA/HCPC cluster map + 2D projections. Fully data-driven:
# clusters, groups, labels and axis % all come from the CSVs exported by
# categorical::mca_export_fig3d(). No project-specific hardcoding.
import os, numpy as np, pandas as pd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt, matplotlib.patches as mpatches, matplotlib.colors as mcolors
from matplotlib.patches import Polygon
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from mpl_toolkits.mplot3d import proj3d
from scipy.spatial import ConvexHull

D    = os.environ.get("FIGDIR", ".")
seg  = pd.read_csv(f"{D}/fig3d_seg.csv"); catf = pd.read_csv(f"{D}/fig3d_cat.csv")
meta = pd.read_csv(f"{D}/fig3d_meta.csv"); pct = meta["benz"].tolist()
def _try(fn):
    try: return pd.read_csv(f"{D}/{fn}")
    except Exception: return None
labf = _try("fig3d_labels.csv"); perf = _try("fig3d_period.csv"); covf = _try("fig3d_cov.csv")

PAL = ["#1b9e77","#2c7fb8","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d"]
ncl = int(catf["clu"].max()) + 1
COLOR = {c: PAL[c % len(PAL)] for c in range(ncl)}
CLU = {int(r.clu): str(r["label"]) for _, r in labf.iterrows()} if labf is not None else {c: f"Cluster {c+1}" for c in range(ncl)}
AX  = {k: f"Dim {k+1} ({pct[k]:.1f}%)" for k in range(3)}
Pxyz = seg[["d1","d2","d3"]].values; lab = seg["clu"].values

groups, sig, cen = [], {}, {}
if perf is not None:
    groups = list(dict.fromkeys(perf["group"]))
    sig = {g: perf[(perf.group==g)&(perf.kind=="sig")][["d1","d2","d3"]].values[0] for g in groups}
    cen = {g: perf[(perf.group==g)&(perf.kind=="cent")][["d1","d2","d3"]].values[0] for g in groups}
MK = ["o","s","^","D","v","P"]
def tint(c, f=0.74):
    r,g,b = mcolors.to_rgb(c); return (r+(1-r)*f, g+(1-g)*f, b+(1-b)*f)

def _hulls_dots(ax, XY, three=False):
    for c in range(ncl):
        pts = XY[lab==c]
        if three: ax.scatter(pts[:,0],pts[:,1],pts[:,2], s=6, color=COLOR[c], edgecolors="none", alpha=0.28, zorder=1)
        else:     ax.scatter(pts[:,0],pts[:,1], s=8, color=COLOR[c], edgecolors="none", alpha=0.30, zorder=1)
        try:
            h = ConvexHull(pts)
            if three:
                poly = Poly3DCollection([pts[s] for s in h.simplices], alpha=0.12, facecolor=COLOR[c], edgecolor=COLOR[c], linewidths=0.2)
                poly.set_zorder(2); ax.add_collection3d(poly)
            else:
                ax.add_patch(Polygon(pts[h.vertices], closed=True, fc=COLOR[c], ec=COLOR[c], alpha=0.12, lw=0.6, zorder=2))
        except Exception: pass

def build3d(fname, azim=-52):
    fig = plt.figure(figsize=(11,8.6), dpi=200); ax = fig.add_axes([0.04,0.04,0.92,0.9], projection="3d")
    _hulls_dots(ax, Pxyz, three=True)
    labelset = []
    for _,r in catf[catf.is_top].iterrows():
        c=int(r.clu); ax.scatter([r.d1],[r.d2],[r.d3], marker="+", s=34, linewidths=1.1, color=COLOR[c], zorder=15)
        labelset.append((r["name"], c, r.d1, r.d2, r.d3))
    if groups:
        Cs = np.array([sig[g] for g in groups]); ax.plot(Cs[:,0],Cs[:,1],Cs[:,2], color="black", lw=1.8, zorder=20)
        for i,g in enumerate(groups):
            ax.scatter(*sig[g], marker=MK[i%len(MK)], s=110, facecolors="black", edgecolors="black", zorder=21)
    ax.set_xlabel(AX[0], fontsize=8.5, labelpad=10); ax.set_ylabel(AX[1], fontsize=8.5, labelpad=10); ax.set_zlabel(AX[2], fontsize=8.5, labelpad=6)
    ax.view_init(elev=22, azim=azim)
    for pane in (ax.xaxis,ax.yaxis,ax.zaxis):
        pane.pane.set_facecolor("white"); pane.pane.set_edgecolor("#eee"); pane._axinfo["grid"]["color"]="#f4f4f4"; pane.line.set_color("#999")
    ax.tick_params(colors="#333", labelsize=7)
    try: ax.set_box_aspect((1,1,0.85))
    except Exception: pass
    fig.canvas.draw()
    for nm,c,x,y,z in labelset:
        x2,y2 = proj3d.proj_transform(x,y,z, ax.get_proj())[:2]
        ax.annotate(nm, (x2,y2), fontsize=8, ha="center", color="black", zorder=30,
                    bbox=dict(boxstyle="round,pad=0.25", fc=tint(COLOR[c]), ec=COLOR[c], lw=0.5, alpha=0.9))
    prox = [mpatches.Patch(facecolor=COLOR[c], edgecolor=COLOR[c], alpha=0.6, label=f"{CLU[c]} (n={int((lab==c).sum())})") for c in range(ncl)]
    ax.legend(handles=prox, loc="upper left", bbox_to_anchor=(0.0,0.98), bbox_transform=fig.transFigure, frameon=False, fontsize=8)
    fig.savefig(f"{D}/{fname}.png", bbox_inches="tight", pad_inches=0.4)
    fig.savefig(f"{D}/{fname}.pdf", bbox_inches="tight", pad_inches=0.4); plt.close(fig); print("saved", fname)

def build2d(dims, fname):
    i,j = dims; XY = Pxyz[:,[i,j]]
    fig, ax = plt.subplots(figsize=(7,6.4), dpi=200)
    ax.axhline(0, color="#ccc", lw=0.6, ls=(0,(4,4))); ax.axvline(0, color="#ccc", lw=0.6, ls=(0,(4,4)))
    _hulls_dots(ax, XY)
    mx,my = XY[:,0].mean(), XY[:,1].mean()
    for _,r in catf[catf.is_top].iterrows():
        c=int(r.clu); x,y = r[["d1","d2","d3"]].values[[i,j]]
        ax.scatter([x],[y], marker="+", s=40, linewidths=1.2, color=COLOR[c], zorder=6)
        dx = 6 if x>=mx else -6; ha = "left" if x>=mx else "right"
        ax.annotate(r["name"], (x,y), xytext=(dx,4), textcoords="offset points", fontsize=8, ha=ha, va="bottom", color="black", zorder=7)
    if groups:
        Cs = np.array([sig[g][[i,j]] for g in groups]); ax.plot(Cs[:,0],Cs[:,1], color="black", lw=1.6, zorder=8)
        for k,g in enumerate(groups):
            ax.scatter(*sig[g][[i,j]], marker=MK[k%len(MK)], s=100, facecolors="black", edgecolors="black", zorder=9)
    ax.set_xlabel(AX[i], fontsize=9); ax.set_ylabel(AX[j], fontsize=9); ax.set_aspect("equal", adjustable="datalim")
    for s in ax.spines.values(): s.set_color("#888"); s.set_linewidth(0.7)
    ax.spines["top"].set_visible(False); ax.spines["right"].set_visible(False)
    prox = [mpatches.Patch(facecolor=COLOR[c], edgecolor=COLOR[c], alpha=0.6, label=f"{CLU[c]}") for c in range(ncl)]
    ax.legend(handles=prox, loc="best", frameon=False, fontsize=7.5)
    fig.tight_layout(); fig.savefig(f"{D}/{fname}.png", bbox_inches="tight", pad_inches=0.3)
    fig.savefig(f"{D}/{fname}.pdf", bbox_inches="tight", pad_inches=0.3); plt.close(fig); print("saved", fname)

build3d("FIG_cluster_map_3d_color")
build2d((0,1), "FIG_map_2d_D1D2"); build2d((0,2), "FIG_map_2d_D1D3"); build2d((1,2), "FIG_map_2d_D2D3")

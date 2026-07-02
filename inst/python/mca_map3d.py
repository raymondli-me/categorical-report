import os, textwrap, numpy as np, pandas as pd
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt, matplotlib.patches as mpatches, matplotlib.colors as mcolors
from matplotlib.lines import Line2D
from matplotlib.patches import Polygon, Ellipse
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from mpl_toolkits.mplot3d import proj3d
from scipy.spatial import ConvexHull

SHOW_ELL = False          # True -> draw the grey 95% confidence ellipsoids/ellipses
D = os.environ.get("FIGDIR", ".")
seg = pd.read_csv(f"{D}/fig3d_seg.csv"); catf = pd.read_csv(f"{D}/fig3d_cat.csv")
perf = pd.read_csv(f"{D}/fig3d_period.csv"); covf = pd.read_csv(f"{D}/fig3d_cov.csv")
meta = pd.read_csv(f"{D}/fig3d_meta.csv"); pct = meta["benz"].tolist()

COLOR = {0:"#1b9e77", 1:"#2c7fb8", 2:"#d95f02", 3:"#7570b3"}
CLU_LONG = {0:"Relational care practice", 1:"Reflexive professional expertise",
            2:"Organizational risk & compliance", 3:"Structural justice & advocacy"}
AXLAB = {0:"Dim 1 ({:.1f}%)\nProf. self-governance  <->  Structural advocacy".format(pct[0]),
         1:"Dim 2 ({:.1f}%)\nOrg. infrastructure  <->  Professional autonomy".format(pct[1]),
         2:"Dim 3 ({:.1f}%)\nRelational care  <->  Expert responsibilization".format(pct[2])}
PERLAB = {"P1":"P1 Welfare stewardship (1940-64)",
          "P2":"P2 Administrative risk governance (1970-94)",
          "P3":"P3 Reconciliation & legal accountability (2005-24)"}
SHAPE_P = {"P1":"o", "P2":"s", "P3":"^"}; PK = ["P1","P2","P3"]
def tint(c, f=0.74):
    r,g,b = mcolors.to_rgb(c); return (r+(1-r)*f, g+(1-g)*f, b+(1-b)*f)

Pxyz = seg[["d1","d2","d3"]].values; lab = seg["clu"].values
sig = {p: perf[(perf.period==p)&(perf.kind=="sig")][["d1","d2","d3"]].values[0] for p in PK}
cen = {p: perf[(perf.period==p)&(perf.kind=="cent")][["d1","d2","d3"]].values[0] for p in PK}
cov = {p: covf[covf.period==p][[f"c{i}" for i in range(1,10)]].values[0].reshape(3,3) for p in PK}

def draw_ellipsoid(ax, center, C, color, alpha=0.10, chi2=7.815, n=20):
    ev, V = np.linalg.eigh(C); ev = np.clip(ev, 1e-9, None); radii = np.sqrt(chi2*ev)
    u = np.linspace(0,2*np.pi,n); v = np.linspace(0,np.pi,n)
    sph = np.stack([np.outer(np.cos(u),np.sin(v)).ravel(), np.outer(np.sin(u),np.sin(v)).ravel(),
                    np.outer(np.ones_like(u),np.cos(v)).ravel()])
    tp = (V @ np.diag(radii) @ sph) + np.asarray(center).reshape(3,1)
    ax.plot_surface(tp[0].reshape(n,n), tp[1].reshape(n,n), tp[2].reshape(n,n),
                    color=color, alpha=alpha, linewidth=0, shade=False, zorder=10)

def draw_ellipse2d(ax, center, C2, color, chi2=5.991):
    ev, V = np.linalg.eigh(C2); ev = np.clip(ev, 1e-12, None)
    ang = np.degrees(np.arctan2(V[1,0], V[0,0])); w,h = 2*np.sqrt(chi2*ev)
    ax.add_patch(Ellipse(center, w, h, angle=ang, fc=color, ec=color, alpha=0.10, lw=0.5, zorder=3))

# label offsets (points) for the 3D view; each cluster's top-3, drawn order matters
BASE_OFF = {0:[(-10,-80),(-30,-70),(30,-40)],
            1:[(40,60),(5,90),(80,44)],
            2:[(-70,-60),(-70,-30),(-70,-50)],
            3:[(10,70),(-3,100),(10,40)]}

def build(fname, title, azim=-52):
    fig = plt.figure(figsize=(12.2,9.0), dpi=300)
    ax = fig.add_axes([0.14,0.08,0.82,0.81], projection="3d")
    for c in range(4):
        pts = Pxyz[lab==c]
        ax.scatter(pts[:,0],pts[:,1],pts[:,2], s=6, color=COLOR[c], edgecolors="none", alpha=0.28, zorder=1)
    proxies=[]
    for c in range(4):
        pts = Pxyz[lab==c]
        try:
            poly = Poly3DCollection([pts[s] for s in ConvexHull(pts).simplices],
                                    alpha=0.12, facecolor=COLOR[c], edgecolor=COLOR[c], linewidths=0.2)
            poly.set_zorder(2); ax.add_collection3d(poly)
        except Exception: pass
        proxies.append(mpatches.Patch(facecolor=COLOR[c], edgecolor=COLOR[c], alpha=0.55,
                       label=f"{CLU_LONG[c]}  (n={int((lab==c).sum())})"))
    labelset=[]
    for _,r in catf[catf.is_top].iterrows():
        c=int(r.clu); ax.scatter([r.d1],[r.d2],[r.d3], marker="+", s=34, linewidths=1.1, color=COLOR[c], zorder=15)
        labelset.append((r["name"], c, r.d1, r.d2, r.d3))
    for p in PK:
        a,b = sig[p], cen[p]; ax.plot([a[0],b[0]],[a[1],b[1]],[a[2],b[2]], color="#bbbbbb", lw=0.7, zorder=17)
    Cc = np.array([cen[p] for p in PK]); ax.plot(Cc[:,0],Cc[:,1],Cc[:,2], color="#d62728", lw=1.5, zorder=24)
    for p in PK:
        x,y,z=cen[p]; ax.scatter([x],[y],[z], marker=SHAPE_P[p], s=80, facecolors="#d62728", edgecolors="black", linewidths=1.0, zorder=25)
    if SHOW_ELL:
        for p in PK: draw_ellipsoid(ax, sig[p], cov[p], "black", alpha=0.10)
    Cs = np.array([sig[p] for p in PK]); ax.plot(Cs[:,0],Cs[:,1],Cs[:,2], color="black", lw=1.9, zorder=27)
    for p in PK:
        x,y,z=sig[p]; ax.scatter([x],[y],[z], marker=SHAPE_P[p], s=130, facecolors="black", edgecolors="black", linewidths=1.3, zorder=28)
    pad=0.25
    ax.set_xlim(Pxyz[:,0].min()-pad,Pxyz[:,0].max()+pad); ax.set_ylim(Pxyz[:,1].min()-pad,Pxyz[:,1].max()+pad)
    ax.set_zlim(Pxyz[:,2].min()-pad,Pxyz[:,2].max()+pad)
    ax.set_xlabel(AXLAB[0], fontsize=8.5, labelpad=14); ax.set_ylabel(AXLAB[1], fontsize=8.5, labelpad=14)
    ax.set_zlabel(AXLAB[2], fontsize=8.5, labelpad=8)
    ax.view_init(elev=22, azim=azim)
    for pane in (ax.xaxis,ax.yaxis,ax.zaxis):
        pane.pane.set_facecolor("white"); pane.pane.set_edgecolor("#f0f0f0"); pane.pane.set_alpha(1.0)
        pane._axinfo["grid"]["color"]="#f4f4f4"; pane._axinfo["grid"]["linewidth"]=0.3
        pane.line.set_color("#555555"); pane.line.set_linewidth(0.9)
    ax.grid(True); ax.tick_params(colors="#333333", labelsize=7.5)
    try: ax.set_box_aspect((1,1,0.85))
    except Exception: pass
    if title: fig.text(0.56,0.965, title, ha="center", fontsize=14, fontweight="bold")
    fig.canvas.draw()
    pr = {nm: proj3d.proj_transform(x,y,z, ax.get_proj())[:2] for nm,c,x,y,z in labelset}
    byclu={}
    for nm,c,x,y,z in labelset: byclu.setdefault(c,[]).append(nm)
    for c,cats in byclu.items():
        zone=BASE_OFF.get(c,[(60,30),(60,0),(60,-30)])
        for i,nm in enumerate(cats):
            dx,dy=zone[i] if i<len(zone) else (60,-30*i); ha="left" if dx>=0 else "right"
            ax.annotate(nm, xy=pr[nm], xytext=(dx,dy), textcoords="offset points", fontsize=9.2,
                        color="black", ha=ha, va="center", zorder=31,
                        bbox=dict(boxstyle="round,pad=0.32", fc=tint(COLOR[c]), ec=COLOR[c], lw=0.6, alpha=0.95),
                        arrowprops=dict(arrowstyle="-", lw=0.5, color="#888888", shrinkA=0, shrinkB=3))
    poff={"P1":(-28,18),"P2":(2,-24),"P3":(28,18)}
    for p in PK:
        x2,y2 = proj3d.proj_transform(*sig[p], ax.get_proj())[:2]
        ax.annotate(p, xy=(x2,y2), xytext=poff[p], textcoords="offset points", fontsize=8, fontweight="bold",
                    color="black", ha="center", va="center", zorder=32,
                    bbox=dict(boxstyle="round,pad=0.28", fc="white", ec="black", lw=0.7, alpha=0.95))
    cl_leg = ax.legend(handles=proxies, loc="upper left", bbox_to_anchor=(0.15,.85),
                       bbox_transform=fig.transFigure, frameon=False, fontsize=8, title="Clusters", title_fontsize=10, alignment="left")
    ax.add_artist(cl_leg)
    traj=[Line2D([0],[0],color="black",lw=2.2,label="Period's top-5 categories (signature)"),
          Line2D([0],[0],color="#d62728",lw=1.6,label="Period's average (centroid)"),
          Line2D([0],[0],color="white",label=" ")]+\
         [Line2D([0],[0],color="0.35",marker=SHAPE_P[p],ls="none",mfc="0.8",mec="black",label=PERLAB[p]) for p in PK]
    ax.legend(handles=traj, loc="upper left", bbox_to_anchor=(0.15,0.74), bbox_transform=fig.transFigure,
              frameon=False, fontsize=8, title="Period Trajectory", title_fontsize=10, alignment="left")
    fig.savefig(f"{D}/{fname}.pdf", bbox_inches="tight", pad_inches=0.55)
    fig.savefig(f"{D}/{fname}.png", bbox_inches="tight", pad_inches=0.55)
    plt.close(fig); print("saved", fname)

def build2d(dims, fname, title=""):
    i,j = dims; XY = Pxyz[:,[i,j]]
    fig, ax = plt.subplots(figsize=(7.4,6.6), dpi=300)
    ax.axhline(0, color="#cccccc", lw=0.6, ls=(0,(4,4)), zorder=0)
    ax.axvline(0, color="#cccccc", lw=0.6, ls=(0,(4,4)), zorder=0)
    proxies=[]
    for c in range(4):
        pts = XY[lab==c]
        ax.scatter(pts[:,0],pts[:,1], s=8, color=COLOR[c], edgecolors="none", alpha=0.30, zorder=1)
        try:
            h = ConvexHull(pts)
            ax.add_patch(Polygon(pts[h.vertices], closed=True, fc=COLOR[c], ec=COLOR[c], alpha=0.12, lw=0.6, zorder=2))
        except Exception: pass
        proxies.append(mpatches.Patch(facecolor=COLOR[c], edgecolor=COLOR[c], alpha=0.55,
                       label=f"{CLU_LONG[c]}  (n={int((lab==c).sum())})"))
    mx,my = XY[:,0].mean(), XY[:,1].mean()
    for _,r in catf[catf.is_top].iterrows():
        c=int(r.clu); x,y = r[["d1","d2","d3"]].values[[i,j]]
        ax.scatter([x],[y], marker="+", s=40, linewidths=1.2, color=COLOR[c], zorder=6)
        dx = 10 if x>=mx else -10; ha = "left" if x>=mx else "right"; dy = 8 if y>=my else -8
        ax.annotate(r["name"], xy=(x,y), xytext=(dx,dy), textcoords="offset points", fontsize=8.5,
                    color="black", ha=ha, va="center", zorder=7,
                    bbox=dict(boxstyle="round,pad=0.3", fc=tint(COLOR[c]), ec=COLOR[c], lw=0.6, alpha=0.95),
                    arrowprops=dict(arrowstyle="-", lw=0.5, color="#999999", shrinkA=0, shrinkB=2))
    for p in PK:
        a,b = sig[p][[i,j]], cen[p][[i,j]]; ax.plot([a[0],b[0]],[a[1],b[1]], color="#bbbbbb", lw=0.7, zorder=4)
    if SHOW_ELL:
        for p in PK: draw_ellipse2d(ax, sig[p][[i,j]], cov[p][np.ix_([i,j],[i,j])], "black")
    Cc = np.array([cen[p][[i,j]] for p in PK]); ax.plot(Cc[:,0],Cc[:,1], color="#d62728", lw=1.4, zorder=8)
    Cs = np.array([sig[p][[i,j]] for p in PK]); ax.plot(Cs[:,0],Cs[:,1], color="black", lw=1.8, zorder=9)
    for p in PK:
        ax.scatter(*cen[p][[i,j]], marker=SHAPE_P[p], s=70, facecolors="#d62728", edgecolors="black", linewidths=1.0, zorder=10)
        ax.scatter(*sig[p][[i,j]], marker=SHAPE_P[p], s=120, facecolors="black", edgecolors="black", linewidths=1.2, zorder=11)
        ax.annotate(p, xy=sig[p][[i,j]], xytext=(0,12), textcoords="offset points", fontsize=8.5, fontweight="bold",
                    ha="center", va="center", zorder=12,
                    bbox=dict(boxstyle="round,pad=0.25", fc="white", ec="black", lw=0.7, alpha=0.95))
    ax.set_xlabel(AXLAB[i], fontsize=8.2); ax.set_ylabel(AXLAB[j], fontsize=8.2)
    ax.set_aspect("equal", adjustable="datalim"); ax.tick_params(labelsize=7.5, colors="#333333")
    for s in ax.spines.values(): s.set_color("#888888"); s.set_linewidth(0.7)
    if title: ax.set_title(title, fontsize=12, fontweight="bold")
    ax.legend(handles=proxies, loc="best", frameon=False, fontsize=7.6, title="Clusters", title_fontsize=8.6, alignment="left")
    fig.tight_layout()
    fig.savefig(f"{D}/{fname}.pdf", bbox_inches="tight", pad_inches=0.3)
    fig.savefig(f"{D}/{fname}.png", bbox_inches="tight", pad_inches=0.3)
    plt.close(fig); print("saved", fname)

build("FIG_cluster_map_3d_color", "")
build2d((0,1), "FIG_map_2d_D1D2")
build2d((0,2), "FIG_map_2d_D1D3")
build2d((1,2), "FIG_map_2d_D2D3")

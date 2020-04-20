import daft
from matplotlib import rc

rc("font", family="serif", size=12)
rc("text", usetex=True)

p_color = {"ec": "#46a546"}

pgm = daft.PGM()

# Data.
pgm.add_node("D", r"$D_{t,m}$", 2.5, 1, observed=True)
pgm.add_node("psi", r"$\psi$", 4, 2)
pgm.add_node("d", r"$d_{t,m}$", 2.5, 2,plot_params=p_color)
pgm.add_node("pi", r"$\pi_m$", 3.3, 3)
pgm.add_node("ifr", r"$ifr_m$", 3.3, 3.5, fixed=True)
pgm.add_node("emp", r"$Emp$", 4, 3.5)
pgm.add_node("c", r"$c_{t,m}$", 2.5, 3,plot_params=p_color)
pgm.add_node("g", r"$g$", 1.5, 4)
pgm.add_node("R", r"$R_{t,m}$", 2.5, 4,plot_params=p_color)
pgm.add_node("I", r"$I_{k,t,m}$", 2.5, 5,observed=True)
pgm.add_node("R0", r"$R_{0,m}$", 3.3, 5)
pgm.add_node("alpha", r"$\alpha_k$", 1.5, 5)
pgm.add_node("kappa", r"$\kappa$", 3.3, 6)


# Add in the edges.
pgm.add_edge("d", "D")
pgm.add_edge("psi", "D")
pgm.add_edge("pi", "d")
pgm.add_edge("c", "d")
pgm.add_edge("ifr", "pi")
pgm.add_edge("emp", "pi")
pgm.add_edge("R", "c")
pgm.add_edge("g", "c")
pgm.add_edge("I", "R")
pgm.add_edge("R0", "R")
pgm.add_edge("alpha", "R")
pgm.add_edge("kappa", "R0")


# And a plate.
pgm.add_plate([2, 0.5,1.7, 5], label=r"$t = 1,..., N$", shift=-0.1,fontsize=7,label_offset=[3, 3])
pgm.add_plate([2, 0.5, 1.7, 5], label=r"$m = 1,..., 32$", shift=-0.1,fontsize=7,position="bottom right",label_offset=[1,3])
pgm.add_plate([1, 4.5,1.9, 1], label=r"$k = 1,..., 4$", shift=-0.1,fontsize=7,position="top left",label_offset=[1,3])

# Render and save.
pgm.render()
pgm.savefig("dag_COVID.png", dpi=150)
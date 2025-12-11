"""
3D plots of 2D functions

@@author: Rodolphe Le Riche, Brian Dédji Whannou
"""

import numpy as np
import matplotlib.pyplot as plt
# import matplotlib  # maybe useful Ubuntu rodo ISIMA
# matplotlib.use('Qt5Agg') # maybe useful Ubuntu rodo ISIMA

import test_functions

# function definition
dim = 2
LB = [-5, -5]
UB = [5, 5]
fun = test_functions.michalewicz  # quadratic, ackley, sphere, rosen, L1norm, sphereL1, linear_function, rastrigin, schwefel, michalewicz

# start drawing the function (necessarily dim==2)
no_grid = 100
#
# execute " %matplotlib qt5 " in the spyder console for interactive 3D plots
# " %matplotlib inline " will get back to normal docking
x1 = np.linspace(start=LB[0], stop=UB[0], num=no_grid)
x2 = np.linspace(start=LB[1], stop=UB[1], num=no_grid)
x, y = np.meshgrid(x1, x2)
xy = np.array([x, y])
z = np.apply_along_axis(fun, 0, xy)
figure = plt.figure()
axis = figure.add_subplot(111, projection='3d')
axis.set_zlim(np.min(z)-0.1, np.max(z)+0.1)
axis.plot_surface(x, y, z, cmap="jet", shade="false")
plt.xlabel(xlabel="x1")
plt.ylabel(ylabel="x2")
plt.title(label=fun.__name__)
axis.set_zlabel("f")
plt.show()
contour = plt.contour(x, y, z, cmap="jet")
plt.colorbar(contour)
plt.show()
# figure.savefig('plot.pdf')

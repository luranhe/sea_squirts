import numpy as np
def action(y, t, tin, tout, topen, tclose, vcrit):
  h, v = y
  dydt = [np.piecewise(h, [v < vcrit, v >= vcrit], [lambda h: (1-h)/topen,
    lambda h: -h/tclose]), h*(1-v)*v**2/tin-v/tout
    +np.piecewise(t, [t%500 < 1, t%500 >= 1], [0.5, 0.0])]
  return dydt

tin = 0.1
tout = 2.4
topen = 130.0
tclose = 150.0
vcrit = 0.13

dt = 0.001
tt = 5000
y0 = [1.0, 0.5]
t = np.linspace(0, tt, round(tt/dt)+1)

from scipy.integrate import odeint
sol = odeint(action, y0, t, args=(tin, tout, topen, tclose, vcrit))

import matplotlib.pyplot as plt
plt.plot(t, sol[:, 0], 'g', label='h')
plt.plot(t, sol[:, 1], 'b', label='v')
plt.legend(loc='best')
plt.xlabel('t')
plt.grid()
plt.show()

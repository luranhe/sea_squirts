import numpy as np

tin = 0.1
tout = 2.4
topen = 130.0
tclose = 150.0
vcrit = 0.13
v0 = 0.5
h0 = 1.0

dt = 0.001
tt = 5000
n = round(tt/dt)+1

t = np.linspace(0, tt, n)

from numba import jit
@jit(nogil=True)
def action():
  h, v = np.zeros([n]), np.zeros([n])
  h[0], v[0] = h0, v0
  for i in range(1, n):
    if v[i-1] < vcrit:
      h[i] = h[i-1]+dt*(1-h[i-1])/topen
    else:
      h[i] = h[i-1]+dt*-h[i-1]/tclose
    v[i] = v[i-1]+dt*(h[i-1]*(1-v[i-1])*v[i-1]**2/tin-v[i-1]/tout
      +np.piecewise(t[i], [np.mod(t[i], 500) < 1, np.mod(t[i], 500) >= 1],
      [0.5, 0.0]))
  return h, v

h, v = action()

import matplotlib.pyplot as plt
plt.plot(t, h, 'g', label='h')
plt.plot(t, v, 'b', label='v')
plt.legend(loc='best')
plt.xlabel('t')
plt.grid()
plt.show()

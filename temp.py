
import numpy as np
import holoviews as hv
import pandas as pd
hv.extension('matplotlib')
# hv.notebook_extension('plotly')
vdata = pd.read_csv("./votingdata.csv")
vdata['log_DiffAdd'] = np.log(vdata.DiffAdd)

from matplotlib import pyplot
from mpl_toolkits.mplot3d import Axes3D
import random
# pyplot.style.use('ggplot') # pretty matplotlib plots


fig = pyplot.figure()
ax = Axes3D(fig)

ax.scatter(vdata.log_DiffAdd, np.power(vdata.log_DiffAdd,2), vdata.GenEl2004)
ax.set_xlabel("log")
ax.set_ylabel("log^2")
ax.set_zlabel("GenEl")
ax.view_init(90, 0)
pyplot.show()

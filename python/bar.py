import numpy as np
import matplotlib.pyplot as plt

labels = []
values = []

x = np.arange(len(labels))
width = 0.35

fig, ax = plt.subplots()

rect = ax.bar(x, values, width)
ax.set_xticks(x)
ax.set_xticklabels(labels)

plt.show()

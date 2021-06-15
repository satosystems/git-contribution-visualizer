#!/usr/bin/env python3

import sys
import re
import numpy as np
import matplotlib.pyplot as plt

pattern = re.compile(r'\t|\n')

def split(s):
    l = pattern.split(s)
    return (l[0].strip(), l[1].strip())

labels = []
values = []

s = sys.stdin.readlines()
for p in list(map(split, s)):
    labels.append(p[1])
    values.append(int(p[0]))

x = np.arange(len(labels))
width = 0.5

_, ax = plt.subplots()

rect = ax.bar(x, values, width)
ax.set_xticks(x)
ax.set_xticklabels(labels)

plt.show()

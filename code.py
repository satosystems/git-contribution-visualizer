#!/usr/bin/env python3

import sys
import re
import subprocess
import numpy as np
import matplotlib.pyplot as plt

pattern = re.compile(r'\t|\n')

def split(s):
    return pattern.split(s)[1]

labels = []
values = []

s = sys.stdin.readlines()
for n in list(map(split, s)):
    labels.append(n)
    command = "git log --author='" + n + "' --pretty=tformat: --numstat | awk '{s+=$1}END{print s}'"
    res = subprocess.check_output(command, shell=True)
    if res == b'\n':
        count = 0
    else:
        count = int(res.strip().decode('utf-8'))
    values.append(count)

x = np.arange(len(labels))
width = 0.5

_, ax = plt.subplots()

rect = ax.bar(x, values, width)
ax.set_xticks(x)
ax.set_xticklabels(labels)

plt.show()

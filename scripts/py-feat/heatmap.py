##################################################################################################################################
# Plotting AU heatmaps with Py-Feat
# Mark Szenteczki
#
# Description: This script automates the creation AU heatmaps using Py-Feat (https://py-feat.org/content/plotting.html)
# Input file: A CSV (comma-separated) table with 21 columns: ID, followed by quantitative date for the 20 AUs expected by Py-Feat
# AU1, 2, 4, 5, 6, 7, 9, 10, 12, 14, 15, 17, 18, 20, 23, 24, 25, 26, 28, 43
#
# Requirements:
# 1) Py-Feat must be installed: https://py-feat.org/content/installation.html
# 2) AU columns must be in the order given above
# 3) If you are missing some of these AUs in your dataset, add a column of 0s prior to analysis
#
# Typical Usage: python3 heatmap.py inputfile.csv
#

import sys
from csv import reader
import numpy as np
import matplotlib.pyplot as plt
from feat.utils import load_h5
from feat.plotting import plot_face


with open(sys.argv[1], 'r') as file:
	csv_reader = reader(file)
	next(csv_reader, None)  # skip the headers
	# Iterate over each row in the csv
	for row in csv_reader:
		data = np.genfromtxt(row, dtype=str) 
		ID = np.array(data)[0]
		print("Processing" + '  ' + ID + " ...")
		AU = np.asarray((data)[1:21], dtype=float)
		# Load Py-Feat model
		model = load_h5()
		# Add muscles
		muscles = {'all': 'heatmap'}
		try:
			# Plot face
			face = plot_face(model=model, au = 	AU, muscles = muscles, color='k', linewidth=1, linestyle='-', gaze = None)
			# Save plot
			face.figure.savefig(ID + '.png')
			plt.close()
		except:
			print('Error: unable to plot ' + ID)
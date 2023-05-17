##################################################################################################################################
# Iterating OpenFace 2 Analysis over many video files
# Mark Szenteczki
#
# Description: This small script executes the Feature Extraction module of OpenFace 2.0 on all video files placed in the same folder as the script.
# Input: The script below assumes .mp4 format files. You may change "mp4" in the script below to any other format supported by OpenFace.
#
# Requirements:
# 1) OpenFace must be installed, see: https://github.com/TadasBaltrusaitis/OpenFace/wiki/Unix-Installation OR https://github.com/TadasBaltrusaitis/OpenFace/wiki/Mac-Installation
# 2) This script should be placed in a folder containing 1) the OpenFace binaries (i.e. the /bin/ folder) and 2) all of the video files you wish to analyze
# 3) The raw outputs produced by OpenFace can be trimmed down to keep only columns relevant to AU Analysis using our accompanying script "cut.sh"
#
# Typical Usage (in terminal): ./batch.sh
#

#!/usr/bin/env bash

for i in $(ls *.mp4)

do

echo "processing... $i"
./FeatureExtraction -f "$i"

done

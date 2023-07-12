#!/bin/bash

# ensure xfdesktop quit fully
xfdesktop --quit
while pidof -q xfdesktop; do
	sleep 1
done

# set the background
feh --bg-scale $HOME/Pictures/ongaku1.png

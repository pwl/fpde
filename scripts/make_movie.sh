#!/bin/sh

ls -1 $1/*.png|sort > filelist.txt
mencoder -ovc lavc -lavcopts vbitrate=800 -mf type=png:fps=30 -o $1/movie.avi mf://@filelist.txt

mplayer $1/movie.avi

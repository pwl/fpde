from argparse import ArgumentParser
import os
import re
import shutil
import sys
from fpde import *

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(dest="file", help="File with data to plot")
    parser.add_argument("-u", "--using", dest="using",
                        help="Set using, can set several directives"
                        " e.g. '1:2|1:3'", default="1:2")
    parser.add_argument("-o", "--out", dest="out",
                        help="Name of the generated gnuplot script",
                        default="plot.gp")
    parser.add_argument("-w", "--with", dest="wit",
                        help="Set with", default="l")
    parser.add_argument("-t", "--title", dest="title",
                        help="Set title", default='')
    parser.add_argument("-T", "--plot-title", dest="plot_title",
                        help="Set title", default='')


    options = parser.parse_args()
    dict = scalar_dictionary(open(options.file).readline())
    out = open(options.out, 'w')

    generate_plot_cmd(out, options, dict)

    os.system("gnuplot -persist {0}".format(options.out))

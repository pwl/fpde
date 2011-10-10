#!/usr/bin/python2.7
from argparse import ArgumentParser
import os
import re
import shutil
import sys
from fpde import *

if __name__ == "__main__":
    parser = ArgumentParser()

    add_default_arguments(parser)
    parser.set_defaults(
        using = "({t}):({dt})")

    options = parser.parse_args()

    if( options.file == "" ):
        file = find_latest_data_dir(options.dir)
        file = os.path.join(file,"module_print_scalar_data","data")
    else:
        file = options.file

    dict = scalar_dictionary(open(file).readline())
    out = open(options.out, 'w')

    options_fix_using(options)

    generate_plot_cmd(out, options, dict, file)

    out.close()

    os.system("gnuplot -persist {0}".format(options.out))

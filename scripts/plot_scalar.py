from argparse import ArgumentParser
import os
import re
import shutil
import sys
from fpde import *

if __name__ == "__main__":
    parser = ArgumentParser()

    add_default_arguments(parser)
    parser.set_defaults(using = "({t}):({dt})")

    options = parser.parse_args()

    if( options.file == "" ):
        file = sorted(d for d in os.listdir(options.dir) \
               if os.path.isdir(d) \
               and re.match("\d{8}-\d{6}\.\d{3}",d))[-1]
        file = os.path.join(options.dir, file)
        file = os.path.join(file, os.listdir(file)[0],
                            "module_print_scalar_data","data")
    else:
        file = options.file

    dict = scalar_dictionary(open(file).readline())
    out = open(options.out, 'w')

    generate_plot_cmd(out, options, dict, file)

    os.system("gnuplot -persist {0}".format(options.out))

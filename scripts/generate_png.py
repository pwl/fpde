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

    parser.add_argument("-s", "--scripts", dest="scripts",
                        help="Set path to fpde/scripts",
                        default="/usr/local/bin")
    parser.add_argument("-m", "--movie", dest="movie",
                        help="Generate movie", action="store_true")
    parser.add_argument("-r", "--recreate", dest="recreate",
                        help="Force recreation of png files"
                        " and movie.avi", action="store_true", default = False)
    parser.add_argument("-e", "--every", dest="every",
                        help="Take only every N-th file",
                        default = 1, type=int, metavar='N')

    parser.set_defaults(plot_title = "t={t}")

    options = parser.parse_args()

    if not os.path.exists(
        os.path.join(
            options.scripts,"make_movie.sh")):
        print "'make_movie.sh' not found in '", options.scripts,"'"
        print "Consider setting the correct path using '-s'"
        sys.exit()

    if(options.file == ""):
        file = find_latest_data_dir(options.dir)
        file = os.path.join(file,"module_print_data")
    else:
        file = options.file

    if(options.recreate):
        [os.remove(f) for f in list_dat(file,".png")]

    out = open(options.out, 'w')

    files = list_dat(file)[::options.every]
    n = len(files)
    print "Files to process: {0}".format(n)

    options_fix_using(options)

    out.write("set terminal png\n")

    for i, filename in enumerate(files):
        png_filename = filename +  ".png"
        if not os.path.exists(png_filename) \
                or options.recreate:
            # this will forece the recreation of movie.avi if there
            # are new png files
            options.recreate = True
            data_file = open(filename, 'r')
            dict = get_vars(data_file.read())
            out.write('print "generating {0}/{1} : {name}"\n'.\
                      format(i,n, name=png_filename))
            out.write('set output "{0}"\n'.format(png_filename))
            generate_plot_cmd(out, options, dict, filename)
            data_file.close()

    out.close()

    os.system("gnuplot {0}".format(options.out))

    if options.movie:
        movie_file = os.path.join(file,"movie.avi")
        # play a movie if it does not exist
        if os.path.exists(movie_file) and not options.recreate:
            os.system("mplayer -loop 0 {file}".\
                          format(file=movie_file))
        # else generate a movie
        else:
            os.system("{scripts}/make_movie.sh {dir}".format(
                    dir=file,scripts=options.scripts))

from argparse import ArgumentParser
import os
import re
import shutil
import sys

# @todo plot several functions
def main(files, out, options):
    out.write("set terminal png\n")
    using = options.using
    wit   = options.wit
    title = options.title
    n = len(files)
    print "Files to process: {0}".format(n)

    for i, filename in enumerate(files):
        text = open(filename).read()
        data_vars = get_vars(text)

        out.write('print "{0}/{1}"\n'.format(i,n))
        out.write('set output "{0}.png"\n'.format(filename))
        out.write('plot "{0}" u {using} w {wit} t "{title}"'.\
                      format(filename,
                             using=using[0].format(**data_vars),
                             wit=wit,
                             title=title[0].format(**data_vars)))

        for u, t in zip(using[1:],title[1:]):
            u = u.format(**data_vars)
            t = t.format(**data_vars)
            out.write(', "" u {using} w {wit} t "{title}"'.\
                          format(using=u,
                                 wit=wit,
                                 title=t))
        out.write('\n')

    out.close()
    os.system("gnuplot {0}".format(out_filename))

    # @todo set apropriate system path in CMakeList.txt
    if options.movie:
        os.system("../fpde/scripts/make_movie.sh {0}".format(dir))

def list_dat(dir, format=".dat"):
    return [os.path.join(dir, f) \
                for f in os.listdir(dir) if f.endswith(format)]

def get_vars(text):
    vars = {}
    for match in re.finditer(
        r"#\s*(?P<name>\w+)\s*=\s*(?P<value>\S*)", text):
        vars[match.group('name')] = match.group('value')

    return vars

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(dest="dir", help="Data directory")
    parser.add_argument("-u", "--using", dest="using",
                        help="Set using", default="1:2")
    parser.add_argument("-w", "--with", dest="wit",
                        help="Set with", default="lp")
    parser.add_argument("-t", "--title", dest="title",
                        help="Set title", default='')
    parser.add_argument("-m", "--movie", dest="movie",
                        help="Generate movie?", action="store_true")


    options = parser.parse_args()

    options.using = options.using.split("|")
    titles = options.title.split("|")
    options.title = [""] * len(options.using)
    options.title[:len(titles)] = titles

    dir = options.dir
    [os.remove(f) for f in list_dat(dir,".png")]

    out_filename = "plot.gp"
    out = open(out_filename, 'w')

    main(list_dat(dir), out, options)

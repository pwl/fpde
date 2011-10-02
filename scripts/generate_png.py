from argparse import ArgumentParser
import os
import sys
import re


def main(dir, out, options):
    out.write("set terminal png\n")
    using = options.using
    wit   = options.wit
    title = options.title
    for filename in (f for f in os.listdir(dir) if f.endswith(".dat")):
        filename = os.path.join(dir, filename)
        text = open(filename).read()
        data_vars = get_vars(text)

        out.write('set output "{0}.png"\n'.format(filename))
        out.write('plot "{0}" u {using} w {wit} t {title}\n'.\
                      format(filename,\
                                 using=using.format(**data_vars),\
                                 wit=wit,\
                                 title=title.format(**data_vars)))

    out.close()
    os.system("gnuplot {0}".format(out_filename))

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
                        help="Set title", default='"title"')

    options = parser.parse_args()

    dir = options.dir
    out_filename = "plot.gp"
    out = open(out_filename, 'w')

    main(dir, out, options)

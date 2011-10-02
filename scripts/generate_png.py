from argparse import ArgumentParser
import os
import sys
import re


def main(dir, out, using):
    out.write("set terminal png\n")
    for filename in (f for f in os.listdir(dir) if f.endswith(".dat")):
        filename = os.path.join(dir, filename)
        text = open(filename).read()
        data_vars = get_vars(text)
        using = using.format(**data_vars)

        out.write('set output "{0}.png"\n'.format(filename))
        out.write('plot "{0}" u {using} w l\n'.\
                      format(filename, using=using))

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

    options = parser.parse_args()

    dir = options.dir
    out_filename = "plot.gp"
    out = open(out_filename, 'w')

    main(dir, out, options.using)

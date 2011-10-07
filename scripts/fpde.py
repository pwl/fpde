import os
import re


def get_vars(text):
    vars = {}
    for match in re.finditer(
        r"#\s*(?P<name>\w+)\s*=\s*(?P<value>\S*)", text):
        vars[match.group('name')] = match.group('value')

    return vars

def list_dat(dir, format=".dat"):
    return [os.path.join(dir, f) \
                for f in os.listdir(dir) if f.endswith(format)]


# returns a list of the form ['#':'$0', 'n_iter':'$1',...]
def scalar_dictionary(line):
    names = line.split()
    return {names[i]:"$"+str(i) for i in range(len(names))}

def add_default_arguments(parser):
    parser.add_argument("-f", "--file", dest="file",
                        help="File with data to plot, if not given defaults to the latest file generated by the solver", default="")
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
    parser.add_argument("-d", "--data-dir", dest="dir",
                        help="Set data directory", default='.')
    parser.add_argument("-T", "--plot-title", dest="plot_title",
                        help="Set title", default='')


def options_fix_using(options):
    options.using = options.using.split("|")
    titles = options.title.split("|")
    options.title = [""] * len(options.using)
    options.title[:len(titles)] = titles

def generate_plot_cmd(out, options, dict, file):
    wit = options.wit

    options_fix_using(options)

    using = [u.format(**dict) for u in options.using]
    title = [t.format(**dict) for t in options.title]

    out.write('set title "{0}"\n'.format(
            options.plot_title.format(**dict)))
    # out.write('set output "{0}.png"\n'.format(file))
    out.write('plot "{file}" u {using} w {wit} t "{title}"'.\
                  format(file=file,
                         using=using[0],
                         wit=wit,
                         title=title[0]))

    for u, t in zip(using[1:],title[1:]):
        out.write(', "" u {using} w {wit} t "{title}"'.\
                      format(using=u,
                             wit=wit,
                             title=t))
        out.write('\n')

    out.close()

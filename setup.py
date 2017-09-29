#!/usr/bin/env python3
from multiprocessing import cpu_count
from shutil import which
import subprocess
from os import path
from string import Template

PREQUEL_BIN_NAME = "prequel.opt"
SEARCH_ORDER = ['/bin/', '/usr/bin/', '/usr/local/', path.expanduser('~'), '/']

def locate(name):
    """Tries to find `name` using locate command"""
    result_path = ""
    for directory in SEARCH_ORDER:
        research_exp = directory + '*' + name
        try:
            result = subprocess.run(["locate", "--limit=1", research_exp],
                                    stdout=subprocess.PIPE, check=1)
            result_path = result.stdout.splitlines()[0]
            break
        except:
            continue
    return result_path.decode()


def get_default_cores():
    """Returns the number of logical cores available"""
    return cpu_count()


def get_default_prequel_bin():
    """Returns a default path for the prequel native binary"""
    prequel_bin = which(PREQUEL_BIN_NAME)

    if prequel_bin:
        return prequel_bin

    try:
        return locate(PREQUEL_BIN_NAME)
    except:
        return ""


def get_default_indices_dir():
    """Returns a default path for the directory containing prequel indices"""
    try:
        index_path = locate("linux_idutils_minus_word_U0.index")
        return path.dirname(index_path)
    except:
        return ""


def ask_field(field, default=""):
    """Asks questions to the user in a `adduser` command fashion"""
    answer = ""
    while not answer:
        answer = input("%s [%s]: " % (field, default))
        if not answer:
            answer = default

    return answer


if __name__=='__main__':
    print("Enter the new value, or press ENTER for the default")
    nb_cores = int(ask_field("Number of threads",
                             default=get_default_cores())
    )
    prequel_bin = path.abspath(ask_field("Path to %s binary" % PREQUEL_BIN_NAME,
                                         default=get_default_prequel_bin())
    )
    indices_dir = path.abspath(ask_field("Path to indices directory",
                                         default=get_default_indices_dir())
    )

    with open("templates/Makefile.tmpl", "r") as f:
        makefile_tmpl = Template(f.read())

    makefile = makefile_tmpl.safe_substitute(nb_cores=nb_cores,
                                             prequel_bin=prequel_bin,
                                             indices_dir=indices_dir)

    with open("templates/Makefile", "w") as f:
        f.write(makefile)

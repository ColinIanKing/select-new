#!/usr/bin/env python3
from multiprocessing import cpu_count
from shutil import which
import subprocess
from os import path
from string import Template

PREQUEL_BIN_NAME = "prequel.opt"

def locate(name):
    result = subprocess.run(["locate", "--limit=1", name],
                            stdout=subprocess.PIPE, check=1)
    result_path = result.stdout.splitlines()[0]
    return result_path.decode()


def get_default_cores():
    return cpu_count()


def get_default_prequel_bin():
    prequel_bin = which(PREQUEL_BIN_NAME)

    if prequel_bin:
        return prequel_bin

    try:
        return locate(PREQUEL_BIN_NAME)
    except:
        return ""


def get_default_indices_dir():
    try:
        index_path = locate("linux_idutils_minus_word_U0.index")
        return path.dirname(index_path)
    except:
        return ""


def ask_field(field, default=""):
    answer = ""
    while not answer:
        answer = input("%s [%s]: " % (field, default))
        if not answer:
            answer = default

    return answer


if __name__=='__main__':
    print("Enter the new value, or press ENTER for the default")
    nb_cores = int(ask_field("Number of threads:",
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

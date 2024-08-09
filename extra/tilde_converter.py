"""
Package converter - replace dependency data and rename package to tilde format.
"""

import os
import shutil
import subprocess
import sys


def transform_dep(line):
    """
    convert nsv, e.g.
    before:   desktop-file-utils-primary-standard: "0.27_1"
    after :   "desktop-file-utils~primary~std": "0.27_1"
    """

    def format_subpackage(pkg):
        if pkg == "complete":
            return "set"
        return pkg

    def format_variant(pkg):
        if pkg == "standard":
            return "std"
        return pkg

    parts = line.split(":")
    nsv = parts[0].strip()
    version = parts[1].strip()

    nsvparts = nsv.split("-")
    num_nsvparts = len(nsvparts)
    new_line = '  "' + nsvparts[0]
    for x in range(1, num_nsvparts - 2):
        new_line = new_line + "-" + nsvparts[x]
    new_line = new_line + "~" + format_subpackage(nsvparts[num_nsvparts - 2])
    new_line = new_line + "~" + format_variant(nsvparts[num_nsvparts - 1])
    new_line = new_line + '": ' + version + "\n"
    return new_line


def metadata_converter(dir_path, rvn_archive):
    """
    Replace dependency items with new tilde format
    """
    source = dir_path + "/" + rvn_archive + ".metadata"
    with open(source) as fin:
        lines = fin.readlines()

    deps_open = False
    with open(dir_path + "/metafile", "w") as fout:
        for line in lines:
            if deps_open:
                if line == "}\n":
                    deps_open = False
                else:
                    line = transform_dep(line)
            if line == 'variant: "standard"\n':
                line = 'variant: "std"\n'
            elif line == 'subpackage: "complete"\n':
                line = 'subpackage: "set"\n'
            fout.write(line)
            if line == "deps: {\n":
                deps_open = True


def repackage(work_dir, output_dir, old_archive):
    """
    Run rvn-create to repackage this
    """
    metadata_file = work_dir + "/metafile"
    stagedir = work_dir + "/stagedir"
    cmd = ["rvn", "create", "-m", metadata_file, "-o", output_dir, "-r", stagedir]
    subprocess.run(cmd)
    print("done with " + old_archive)


def extract_rvn_package(work_dir, rvn_archive):
    """
    1) Delete existing work_dir tree
    2) create <work_dir>/stagedir
    3) extract archive into <work_dir>/stagedir
    4) extract metadata file to <work_dir> (joe-single-standard-4.6_4.rvn.metadata)
    """
    stagedir = work_dir + "/stagedir"
    if os.path.exists(work_dir):
        shutil.rmtree(work_dir)

    os.mkdir(work_dir)
    os.mkdir(stagedir)

    cmd = ["xrvn", "-x", "-o", stagedir, rvn_archive]
    subprocess.run(cmd)

    cmd = ["xrvn", "-m", "-o", work_dir, rvn_archive]
    subprocess.run(cmd)


def main():
    """Script entry point."""
    if len(sys.argv) == 1:
        print("Path to rvn archive is required.")
        return 1

    rvn_archive = sys.argv[1]
    if not os.path.exists(rvn_archive):
        print(rvn_archive + " is not a file")
        return 1

    if rvn_archive[-4:] != ".rvn":
        print("file does not have the expected .rvn extension")
        return 1

    base_filename = os.path.basename(rvn_archive)
    work_dir = "/tmp/work_" + base_filename[:-4]
    output_dir = work_dir

    if len(sys.argv) > 2:
        output_dir = sys.argv[2]
        if not os.path.isdir(output_dir):
            print("The optional arg2 (output directory) is not a directory")
            return 1

    extract_rvn_package(work_dir, rvn_archive)
    metadata_converter(work_dir, base_filename)
    repackage(work_dir, output_dir, rvn_archive)
    if output_dir != work_dir:
        shutil.rmtree(work_dir)
    return 0


if __name__ == "__main__":
    sys.exit(main())

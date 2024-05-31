"""
Package converter - Ravensw tzst format to RVN format
"""

import json
import os
import shutil
import subprocess
import sys

def write_string(handle, key, val):
    """
    Writes key : "val" on the file handle
    """
    escaped_val = val.replace("'",  "\\'")
    handle.write(key + ": '" + escaped_val + "'\n")


def write_number(handle, key, val):
    """
    Writes key : val on the file handle
    """
    handle.write(key + ": " + str(val) + "\n")


def write_multiline (handle, key, val):
    """
    Writes key : <<EOD
    val
    EOD
    on the file handle
    """
    handle.write(key + ": <<EOD\n")
    handle.write(val)
    if val[-1:] == "\n":
        handle.write("EOD\n")
    else:
        handle.write("\nEOD\n")


def write_abi(handle, abi):
    """
    converts ravensw abi to lower(<osname>):x86_64:<release>
    """
    parts = abi.split(":")
    new_abi = parts[0].lower() + ":x86_64:" + parts[1]
    write_string(handle, "abi", new_abi)


def write_array(handle, key, json_object):
    """
    If list is not empty, write it
    """
    if key in json_object:
        valuelist = json_object[key]
        if len(valuelist) == 0:
            return
        value = "','".join(valuelist)
        handle.write(key + ": ['" + value + "']\n")


def write_directories(handle, key, json_object):
    """
    If list is not empty, write it
    original: "directories":{"/var/spool/postfix":"y"}
    new:      directories: [{path: "/var/spool/postfix"}]
    """
    if key in json_object:
        paths = json_object[key].keys()
        valuelist = []
        if len(paths) == 0:
            return
        for path in paths:
            valuelist.append("{path:'" + path + "'}");

        value = ",".join(valuelist)
        handle.write(key + ": [" + value + "]\n")


def write_nvpairs(handle, key, json_object):
    """
    if object is not empty, write it
    """
    lines = []
    if key in json_object:
        tags = json_object[key].keys()
        handle.write(key + ": ")
        for tag in tags:
            lines.append(tag + ": '" + json_object[key][tag] + "'")
        valuelist = "{" + ",".join(lines) + "}\n";
        handle.write(valuelist);


def write_scripts(handle, json_object):
    """
    1) reformat to new structure (args are blank)
    2) HEREDOC on actual scripts
    Per code, this is the expected format:
    # scripts: {"phase1": [{args: "", code:""},{}],"phase2": [...]
    """
    lines = []
    key = "scripts"
    if key in json_object:
        phases = json_object[key].keys()
        handle.write(key + ": ")
        for phase in phases:
            code = json_object[key][phase]
            lines.append(phase + ": [{args: '',code: <<EOD\n" + code + "\nEOD\n}]")
        valuelist = "{" + ",".join(lines) + "}\n";
        handle.write(valuelist);


def write_messages(handle, json_object):
    """
    insert "type" into messages object
    """
    lines = []
    key = "messages"
    if not key in json_object:
        return

    handle.write(key + ": [\n")
    for msgobj in json_object[key]:
        msg = msgobj["message"]
        handle.write("  {type: 'install',\n");
        handle.write("   message: <<EOD\n" + msg + "\nEOD\n  }\n")
    handle.write("]\n");


def write_nested_objs(handle, key, json_object):
    """
    this was broken and it's not even the correct format
    if object is not empty, write it
    # "deps":{"nss-caroot-standard":{"origin":"nss:standard","version":"3.100"},
    """
    lines = []
    if key in json_object:
        deps = json_object[key].keys()
        handle.write(key + ": ")
        inner = []
        for dep in deps:
            inner_obj = json_object[key][dep]
            tags = json_object[key][dep].keys()
            for tag in tags:
                inner.append(tag + ":'" + json_object[key][dep][tag] + "'")
            valuelist = ",".join(inner);
            lines.append(dep + ":{" + valuelist + "}")
        handle.write("{" + ",".join(lines) + "}\n");

def write_dependencies(handle, json_object):
    """
    tzst format> deps: {"<nsv>": {origin: "<n:v>", version: "<ver>"}, .. }
    rvn format>  deps: {"<nsv>": "<ver>"}, ...}
    """
    lines = []
    key = "deps"
    if key in json_object:
        dep_nsv = json_object[key].keys()
        handle.write(key + ": ")
        for nsv in dep_nsv:
            lines.append('"' + nsv + '":"' + json_object[key][nsv]["version"] + '"')
            valuelist = "{" + ",".join(lines) + "}\n";
        handle.write(valuelist);


def metadata_converter(dir_path):
    """
    1) JSON parses for {dir_path}/+MANIFEST
    2) Writes equivalent ucl-encoded {dir_path}/metafile
    """
    source = dir_path + "/MANIFEST";
    with open(source) as fin:
        data = json.load(fin)

    nsv = data["name"].split("-")
    variant = nsv.pop()
    subpackage = nsv.pop()
    namebase = "-".join(nsv)
    with open(dir_path + "/metafile", "w") as fout:
        write_string(fout, "namebase", namebase)
        write_string(fout, "subpackage", subpackage)
        write_string(fout, "variant", variant)
        write_string(fout, "version", data["version"])
        write_string(fout, "comment", data["comment"])
        write_multiline(fout, "desc", data["desc"])
        write_string(fout, "www", data["www"])
        write_string(fout, "maintainer", data["maintainer"])
        write_string(fout, "prefix", data["prefix"])
        # rvn provides flatsize and abi
        # write_number(fout, "flatsize", data["flatsize"])
        # write_abi (fout, data["abi"])
        write_array (fout, "categories", data)
        write_string(fout, "licenselogic", data["licenselogic"])
        write_array (fout, "licenses", data)
        write_array (fout, "users", data)
        write_array (fout, "groups", data)
        write_directories (fout, "directories", data)   # this is wrong
        # rvn handles shlibs
        # write_array (fout, "shlibs_provided", data)
        # write_array (fout, "shlibs_required", data)
        write_nvpairs (fout, "annotations", data)
        write_nvpairs (fout, "options", data)
        write_dependencies (fout, data)
        write_scripts (fout, data)
        write_messages (fout, data)



def extract_zstd_package(work_dir, tzst_archive):
    """
    1) Delete existing work_dir tree
    2) create <work_dir>/stagedir
    3) extract archive into <work_dir>/stagedir
    """
    stagedir = work_dir + "/stagedir"
    if os.path.exists(work_dir):
        shutil.rmtree(work_dir)

    os.mkdir(work_dir)
    os.mkdir(stagedir)

    cmd = ["tar", "-x", "-C", stagedir, "-f", tzst_archive]
    subprocess.run(cmd)

    os.rename(stagedir + "/+MANIFEST", work_dir + "/MANIFEST")
    os.remove(stagedir + "/+COMPACT_MANIFEST")


def repackage(work_dir, output_dir, old_archive):
    """
    Run rvn-create to repackage this
    """
    metadata_file = work_dir + "/metafile"
    stagedir = work_dir + "/stagedir"
    cmd = [
        "rvn", "create",
        "-m", metadata_file,
        "-o", output_dir,
        "-r", stagedir
    ]
    subprocess.run(cmd)
    print("done with " + old_archive)


def main():
    """Script entry point."""
    if len(sys.argv) == 1:
        print("Path to tzst archive is required.")
        return 1

    tzst_archive = sys.argv[1]
    if not os.path.exists(tzst_archive):
        print(tzst_archive + " is not a file");
        return 1

    if tzst_archive[-5:] != ".tzst":
        print("file does not have the expected .tzst extension")
        return 1

    base_filename = os.path.basename(tzst_archive)
    work_dir = "/tmp/work_" + base_filename[:-5]
    output_dir = work_dir

    if len(sys.argv) > 2:
        output_dir = sys.argv[2]
        if not os.path.isdir(output_dir):
            print("The optional arg2 (output directory) is not a directory")
            return 1

    extract_zstd_package(work_dir, tzst_archive)
    metadata_converter(work_dir)
    repackage(work_dir, output_dir, tzst_archive)
    if output_dir != work_dir:
        shutil.rmtree(work_dir)


if __name__ == "__main__":
    sys.exit(main())

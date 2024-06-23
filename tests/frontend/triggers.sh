#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	cleanup_lua \
	dirpath \
	glob_trigger

#	 \
#	regex_trigger \
#	path_trigger \
#	pkg_exec_no_sandbox \
#	pkg_add


#################
#  cleanup_lua  #
#################
cleanup_lua_body() {
	mkdir files
	mkdir target
	touch test.plist
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	cat <<EOF >> test.ucl
triggers: [
  {
    cleanup: <<EOS
pkg.print_msg("cleanup-message-test")
print "Cleaning up"
EOS
    trigger: <<EOS
return 0
EOS
  }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -y --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check -o inline:"Cleaning up\ncleanup-message-test\n" \
		-e empty -s exit:0  rvn -o ${TMPDIR}/files -r ${TMPDIR}/target remove -qy test

}

#############
#  dirpath  #
#############
dirpath_body() {
	mkdir files
	mkdir target
	mkdir share
	echo "hello" > share/hello.txt
	echo "bob" > bob.txt
	echo "bob.txt"          > test.plist
	echo "share/hello.txt" >> test.plist
	echo "@dir byteme"     >> test.plist
	echo "@dir /var/run/x" >> test.plist
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF


	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat <<EOF >> test.ucl
triggers: [
  {
    dir_path: ["/share"]
    trigger: <<EOS
pkg.print_msg("message-test")
print("triggered " .. arg[1])
EOS
  }
]
EOF
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -o inline:"triggered /share\nmessage-test\n" -e empty -s exit:0 rvn -r ${TMPDIR}/target \
		-o REPOS_DIR="${TMPDIR}/reposconf" install -qy test
}



##################
#  glob_trigger  #
##################
glob_trigger_body() {
	mkdir files
	mkdir target
	mkdir share
	echo "hello" > share/hello.txt
	echo "bob" > share/hello.cpp
	echo "jim" > share/hello.doc
	echo "share/hello.cpp" > test.plist
	echo "share/hello.txt" >> test.plist
	echo "share/hello.doc" >> test.plist
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat <<EOF >> test.ucl
triggers: [
  {
    file_glob: ['/share/hello.*']
    trigger: <<EOS
pkg.print_msg("origin namebase   : " .. pkg_namebase)
pkg.print_msg("origin subpackage : " .. pkg_subpackage)
pkg.print_msg("origin variant    : " .. pkg_variant)
pkg.print_msg("origin prefix     : " .. pkg_prefix)
  for i,v in ipairs(arg) do
    print("arg " .. i .. ": " .. v)
  end
EOS
  }
]
EOF


	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -o inline:"arg 1: /share/hello.cpp\narg 2: /share/hello.doc\narg 3: /share/hello.txt\norigin namebase   : test\norigin subpackage : single\norigin variant    : standard\norigin prefix     : /\n" \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy test

}
























##################
#  path_trigger  #
##################
path_trigger_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "1" "/"
	mkdir trigger_dir/
	cat << EOF >> trigger_dir/trigger.ucl
path: [ "${TMPDIR}/trigger_dir" ]
trigger: {
	type: lua
	script: <<EOS
pkg.print_msg("message-test")
print("triggered " .. arg[1])
EOS
}
EOF
	echo ${TMPDIR}/trigger_dir/trigger.ucl > plist
	atf_check pkg create -M test.ucl -p plist
	mkdir target
	unset PKG_TRIGGERS_DIR
	atf_check -o inline:"triggered ${TMPDIR}/trigger_dir\n" pkg -o REPOS_DIR=/dev/null -o PKG_TRIGGERS_DIR="${TMPDIR}/trigger_dir" install -qfy ${TMPDIR}/test-1.pkg
}









regex_trigger_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "1" "/"
	mkdir trigger_dir/
	cat << EOF >> trigger_dir/trigger.ucl
path_regex: [ ".*trigger.*" ]
trigger: {
	type: lua
	script: <<EOS
	print("triggered " .. arg[1])
EOS
}
EOF
	echo ${TMPDIR}/trigger_dir/trigger.ucl > plist
	atf_check pkg create -M test.ucl -p plist
	mkdir target
	unset PKG_TRIGGERS_DIR
	atf_check -o inline:"triggered ${TMPDIR}/trigger_dir\n" pkg -o REPOS_DIR=/dev/null -o PKG_TRIGGERS_DIR="${TMPDIR}/trigger_dir" install -qfy ${TMPDIR}/test-1.pkg
}




pkg_exec_no_sandbox_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "1" "/"
	mkdir trigger_dir/
	cat << EOF >> trigger_dir/trigger.ucl
path_glob: [ "/*" ]
trigger: {
	type: lua
	sandbox: false
	script: <<EOS
pkg.exec({"echo", "plop"})
EOS
}
EOF
	echo ${TMPDIR}/trigger_dir/trigger.ucl > plist
	atf_check pkg create -M test.ucl -p plist
	mkdir target
	unset PKG_TRIGGERS_DIR
	atf_check -o inline:"plop\n" pkg -o REPOS_DIR=/dev/null -o PKG_TRIGGERS_DIR="${TMPDIR}/trigger_dir" install -qfy ${TMPDIR}/test-1.pkg
}

pkg_add_body() {
	atf_check -s exit:0 ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "1" "/"
	mkdir trigger_dir/
	cat << EOF >> trigger_dir/trigger.ucl
path_glob: [ "/*" ]
trigger: {
	type: lua
	sandbox: false
	script: <<EOS
print("plop")
EOS
}
EOF
	echo "${TMPDIR}"/trigger_dir/trigger.ucl > plist
	atf_check pkg create -M test.ucl -p plist

	atf_check -s exit:0 ${RESOURCEDIR}/test_subr.sh new_pkg "meh" "meh" "1" "/"
	cat << EOF >> meh.ucl
deps: { test: { version: "1", origin: "dontcare" } }
EOF
	echo "@dir ${TMPDIR}"/trigger_dir/ > plist
	mkdir target
	unset PKG_TRIGGERS_DIR
	atf_check pkg create -M meh.ucl -p plist
OUTPUT="Installing meh-1...
\`-- Installing test-1...
\`-- Extracting test-1:  done
Extracting meh-1:  done
==> Running trigger: trigger.ucl
plop
"
	atf_check -o inline:"${OUTPUT}" pkg -o PKG_TRIGGERS_DIR="${TMPDIR}/trigger_dir" add meh-1.pkg
}

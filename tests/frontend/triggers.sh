#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	cleanup_lua \
	dirpath \
	glob_trigger \
	regex_trigger \
	path_trigger \
	no_trigger \
	swap_order \
	upgrade \
	dir_island


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
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -y --file ${TMPDIR}/files/test~single~standard~1.rvn

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


###################
#  regex_trigger  #
###################
regex_trigger_body() {
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
    file_regexp: ["hello[.][a-z]{3}"]
    trigger: <<EOS
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

	atf_check -o inline:"arg 1: /share/hello.cpp\narg 2: /share/hello.doc\narg 3: /share/hello.txt\n" \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy test

}


##################
#  path_trigger  #
##################
path_trigger_body() {
	# trigger in the prequel, activated with the second install
	mkdir files
	mkdir target
	mkdir share
	mkdir bin
	echo "hello" > share/hello.txt
	echo "bob" > share/hello.cpp
	echo "jim" > share/hello.doc
	echo "echo thursday" > bin/dow.sh
	echo "share/hello.cpp" > test.plist
	echo "share/hello.txt" >> test.plist
	echo "share/hello.doc" >> test.plist
	echo "bin/dow.sh" > prequel.plist
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "prequel" "prequel" "single" "standard" "1" "/"
	cat <<EOF >> prequel.ucl
triggers: [
  {
    file_path: ["/share/hello.cpp"]
    trigger: <<EOS
  pkg.print_msg("origin rootdir: " .. pkg_rootdir)
  for i,v in ipairs(arg) do
    print("arg " .. i .. ": " .. v)
  end
EOS
  }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m prequel.ucl -w prequel.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -o empty -e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy prequel
	atf_check \
		-o match:"^arg 1: /share/hello.cpp$" \
		-o match:"^origin rootdir: .*/work/target$" \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy test

}



################
#  no_trigger  #
################
no_trigger_body() {
	# do not trigger if matching file is already installed
	mkdir files
	mkdir target
	mkdir share
	mkdir bin
	echo "hello" > share/hello.txt
	echo "bob" > share/hello.cpp
	echo "jim" > share/hello.doc
	echo "echo thursday" > bin/dow.sh
	echo "share/hello.cpp" > test.plist
	echo "share/hello.txt" >> test.plist
	echo "share/hello.doc" >> test.plist
	echo "bin/dow.sh" > prequel.plist
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "prequel" "prequel" "single" "standard" "1" "/"
	cat <<EOF >> test.ucl
triggers: [
  {
    dir_path: ["/bin"]
    trigger: <<EOS
  for i,v in ipairs(arg) do
    print("arg " .. i .. ": " .. v)
  end
EOS
  }
]
EOF

atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m prequel.ucl -w prequel.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -o empty -e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy prequel
	atf_check -o empty -e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy test
}




################
#  swap_order  #
################
swap_order_body() {
	# do not trigger if matching file is already installed
	mkdir files
	mkdir target
	mkdir share
	mkdir bin
	echo "hello" > share/hello.txt
	echo "bob" > share/hello.cpp
	echo "jim" > share/hello.doc
	echo "echo thursday" > bin/dow.sh
	echo "share/hello.cpp" > test.plist
	echo "share/hello.txt" >> test.plist
	echo "share/hello.doc" >> test.plist
	echo "bin/dow.sh" > prequel.plist
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "prequel" "prequel" "single" "standard" "1" "/"
	cat <<EOF >> test.ucl
triggers: [
  {
    dir_path: ["/bin"]
    trigger: <<EOS
  for i,v in ipairs(arg) do
    print("arg " .. i .. ": " .. v)
  end
EOS
  }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m prequel.ucl -w prequel.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -o empty -e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy test
	atf_check -o inline:"arg 1: /bin\n" -e empty -s exit:0 rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy prequel
}


#############
#  upgrade  #
#############
upgrade_body() {
	mkdir files
	mkdir target
	mkdir bin
	echo "echo thursday" > bin/dow.sh
	echo "echo friday, I'm in love." > bin/love.sh
	echo "bin/dow.sh" > test1.plist
	echo "bin/dow.sh" > test2.plist
	echo "bin/love.sh" >> test2.plist
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test1" "test" "single" "standard" "1" "/"
	cat <<EOF >> test1.ucl
triggers: [
  {
    dir_path: ["/bin"]
    trigger: <<EOS
if pkg_upgrade then
   print "bin directory hit, upgraded"
else
   print "bin directory hit, installed"
end
EOS
  }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test1.ucl -w test1.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f
	atf_check -o inline:"bin directory hit, installed\n" -e empty -s exit:0 \
		rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" install -qy test

	rm ${TMPDIR}/files/test~single~standard~1.rvn
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test2" "test" "single" "standard" "2" "/"
	cat <<EOF >> test2.ucl
triggers: [
  {
    dir_path: ["/bin"]
    trigger: <<EOS
if pkg_upgrade then
   print "bin directory hit, upgraded"
else
   print "bin directory hit, installed"
end
EOS
  }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test2.ucl -w test2.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f
	atf_check -o inline:"bin directory hit, upgraded\n" -e empty -s exit:0 \
		rvn -r ${TMPDIR}/target -o REPOS_DIR="${TMPDIR}/reposconf" upgrade -qy test
}


################
#  dir_island  #
################
dir_island_body() {
mkdir files
	mkdir target
	mkdir share
	echo "hello" > share/hello.txt
	echo "@dir byteme"     >> test.plist
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
    dir_path: ["/byteme"]
    trigger: <<EOS
pkg.print_msg("island-test")
EOS
  }
]
EOF
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -o inline:"island-test\n" -e empty -s exit:0 rvn -r ${TMPDIR}/target \
		-o REPOS_DIR="${TMPDIR}/reposconf" install -qy test
}
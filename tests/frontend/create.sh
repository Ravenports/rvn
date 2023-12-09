#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

# The tests with "_from_plist" are misnomers
# PLIST are not supported.  All tests are generated with a UCL metadata file
# and possible a whitelist.  The names are kept to allow comparision with FreeBSD's pkg

tests_init \
	create_from_plist \
	create_from_plist_set_owner \
	create_from_plist_set_group_space \
	create_from_plist_gather_mode \
	create_from_plist_set_mode \
	create_from_plist_mini \
	create_from_plist_with_keyword_arguments \
	create_from_plist_missing_file \
	create_from_manifest_and_plist \
	create_from_manifest \
	create_from_manifest_dir \
	create_from_plist_pkg_descr \
	create_from_plist_with_keyword_and_message \
	create_from_plist_keyword_deprecated \
	create_with_hardlink \
	create_from_plist_keyword_validation \
	create_from_plist_keyword_lua_actions \


# LUA	create_from_plist_keyword_real_args


genmanifest() {
	cat << EOF >> METADATA
namebase: test
subpackage: single
variant: standard
version: "1"
maintainer: test
categories: [test]
comment: a test
www: http://test
prefix: /
abi: "*:*:0"
desc: <<EOD
Yet another test
EOD
EOF
}

genprefixmanifest() {
	cat << EOF >> METADATA
namebase: test
subpackage: single
variant: standard
version: "1"
maintainer: test
categories: [test]
comment: a test
www: http://test
prefix: /prefix
abi: "*:*:0"
desc: <<EOD
Yet another test
EOD
EOF
}

genplist() {
	cat << EOF >> test.plist
$@
EOF
}

preparetestcredentials() {
	touch file1

	genmanifest
	genplist "@$1 file1"
}

basic_validation() {
	test -f test-single-standard-1.rvn || atf_fail "Package not created"
}

create_with_hardlink_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg \
	METADATA "test" "single" "standard" "1" "/"
	echo "blah" >> foo
	ln foo bar
	echo "@(root,wheel,0555) /foo" >> test.plist
	echo "@(root,wheel,0644) /bar" >> test.plist

cat << EOF >> output.listing
-rw-r--r--     root    wheel        5 1970-01-03 00:00 bar
hr-xr-xr-x     root    wheel        0 1970-01-03 00:00 foo
EOF

	atf_check \
		-o ignore \
		-e ignore \
		env SOURCE_DATE_EPOCH=172800 rvn create -o ${TMPDIR} -r . -m METADATA.ucl -w test.plist

	atf_check \
		-o file:output.listing \
		-e empty \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn

}

create_from_plist_body() {
	touch file1
	genmanifest
	genplist "file1"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

	basic_validation
	atf_check \
		-o match:"-rw-r--r-- .*root[ ]+wheel.*[ ]file1$" \
		-e ignore \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_missing_file_body() {
	# touch file1
	genmanifest
	genplist "file1"

	atf_check \
		-o empty \
		-e match:"Manifest entity [[]file1[]] does not exist"\
		-s exit:1 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist
}

create_from_plist_set_owner_body() {

	preparetestcredentials "(plop,,)"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

	basic_validation
	atf_check \
		-o match:"-rw-r--r-- .*plop[ ]+wheel.*[ ]file1$" \
		-e ignore \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_set_group_body() {

	preparetestcredentials "(,bla,)"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -m METADATA -w test.plist -r .

	basic_validation
	atf_check \
		-o match:"-rw-r--r-- .*root[ ]+bla.*[ ]file1$" \
		-e ignore \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}


create_from_plist_set_group_space_body() {

	preparetestcredentials "(, bla,)"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

	basic_validation
	atf_check \
		-o match:"-rw-r--r-- .*root[ ]+bla.*[ ]file1$" \
		-e ignore \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_gather_mode_body() {

	preparetestcredentials "(plop,bla,)"

	chmod 777 file1 || atf_fail "Impossible to change mode"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

	basic_validation
	atf_check \
		-o match:"-rwxrwxrwx .*plop[ ]+bla.*[ ]file1$" \
		-e ignore \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_set_mode_body() {

	preparetestcredentials "(,,2755)"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

	basic_validation
	atf_check \
		-o match:"-rwxr-sr-x .*root[ ]+wheel.*[ ]file1$" \
		-e ignore \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_mini_body() {

	preparetestcredentials "(plop,)"

	atf_check \
		-o empty \
		-e match:"Manifest entity [[][@][(]plop,[)] file1[]] keyword line failed to parse" \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

}

create_from_plist_keyword_real_args_body() {
	preparetestcredentials "test"

cat << EOF > test.ucl
actions: []
arguments: true
post-install-lua: <<EOS
if arg ~= nil then
	print("yes")
end
for i = 1, #arg do
	print(arg[i])
end
EOS
EOF

	genplist "@test A B C D"

mkdir target

	atf_check \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .

	atf_check \
		-o inline:"yes\nfile1\nyes\nA\nB\nC\nD\n" \
		rvn -o REPOS_DIR=/dev/null -r ${TMPDIR}/target install -qfy ${TMPDIR}/test-1.rvn
}

create_from_plist_keyword_validation_body() {
	preparetestcredentials "test"

cat << EOF > test.ucl
actions: []
prepackaging: <<EOS
io.stderr:write("meh\n")
return 1
EOS
EOF
	atf_check \
		-o empty \
		-e inline:"meh\nFail to apply keyword 'test'\n" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .

cat << EOF > test.ucl
actions: []
prepackaging: <<EOS
print(line)
io.stderr:write("meh\n")
return 1
EOS
EOF
	atf_check \
		-o inline:"file1\n" \
		-e inline:"meh\nFail to apply keyword 'test'\n" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .

cat << EOF > test.ucl
actions: []
prepackaging: <<EOS
print(#arg)
io.stderr:write("meh\n")
return 1
EOS
EOF
	# arguments: has no effect.  they are always one.
	atf_check \
		-o inline:"1\n" \
		-e inline:"meh\nFail to apply keyword 'test'\n" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .

cat << EOF > test.ucl
actions: []
arguments: true
prepackaging: <<EOS
print(#arg)
io.stderr:write("meh\n")
return 1
EOS
EOF
	atf_check \
		-o inline:"1\n" \
		-e inline:"meh\nFail to apply keyword 'test'\n" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .

	genplist "@test A B"
	genplist "@test A A"
	genplist "@test A B C"
cat << EOF > test.ucl
actions: []
arguments: true
prepackaging: <<EOS
if #arg == 1 then
  return
end
if #arg == 2 then
  if arg[1] == arg[2] then
    io.stderr:write("The first and the second argument are identical\n")
    return 1
  end
  return 1
end
io.stderr:write("Invalid number of arguments '".. #arg .. "' expecting 1 or 2\n")
return 1
EOS
EOF
output="Fail to apply keyword 'test'
The first and the second argument are identical
Fail to apply keyword 'test'
Invalid number of arguments '3' expecting 1 or 2
Fail to apply keyword 'test'
"
	atf_check \
		-e inline:"${output}" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .
}

create_from_plist_with_keyword_arguments_body() {
	preparetestcredentials "testkeyword"

	atf_check \
		-o empty \
		-e match:"testkeyword[.]ucl: UCL keyword not found, fatal[.]$" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

cat << EOF >> testkeyword.ucl
actions: []
arguments: true
post-install:
	echo %1 %2
EOF

	atf_check \
		-o empty \
		-e inline:"Requesting argument %2 while only 1 arguments are available\nFailed to apply keyword 'testkeyword'\n" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

cat << EOF > testkeyword.ucl
actions: [file(%1)]
arguments: true
post-install:
	echo %1 %2
EOF

	echo "@testkeyword A B" > test.plist

	atf_check \
		-o empty \
		-e inline:"testkeyword.ucl: action 'file(%1)' not recognized\n" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

cat << EOF > testkeyword.ucl
actions: [file, dir]
arguments: true
post-install:
	echo %1 %2
EOF

	atf_check \
		-o empty \
		-e match:"Manifest file [[]A[]] does not exist, ignoring$" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

	touch A
	mkdir B

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

cat << EOF >> output.ucl
abi: '*:*:0'
categories: [
  'test'
]
comment: 'a test'
desc: 'Yet another test'
directories: [
  {
    group: false
    owner: false
    path: 'B'
    perms: false
  }
]
flatsize: 0
maintainer: 'test'
messages: {
}
namebase: 'test'
prefix: '/'
scripts: {
  post-install: [
    'echo A B'
  ]
}
subpackage: 'single'
variant: 'standard'
version: '1'
www: 'http://test'

EOF

	atf_check \
		-o file:output.ucl \
		-e empty \
		-s exit:0 \
		rvn info -R -F ${TMPDIR}/test-single-standard-1.rvn
}

create_from_manifest_and_plist_body() {
	genmanifest
	touch testfile
	genplist "testfile"
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist

cat << EOF >> output.ucl
abi: '*:*:0'
categories: [
  'test'
]
comment: 'a test'
desc: 'Yet another test'
directories: [
]
flatsize: 0
maintainer: 'test'
messages: {
}
namebase: 'test'
prefix: '/'
scripts: {
}
subpackage: 'single'
variant: 'standard'
version: '1'
www: 'http://test'

EOF

	atf_check \
		-o file:output.ucl \
		-e empty \
		-s exit:0 \
		rvn info -R -F ${TMPDIR}/test-single-standard-1.rvn

	atf_check \
		-o match:"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262 testfile$" \
		-e empty \
		-s exit:0 \
		rvn info --list-digests -F ${TMPDIR}/test-single-standard-1.rvn
}

create_from_manifest_body() {
	genmanifest
	genplist "@(,,0644) testfile"

	touch testfile
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist --timestamp 100020008

	atf_check \
		-o match:"-rw-r--r--     root    wheel        0 1973-03-03 15:20 testfile$" \
		-e empty \
		-s exit:0 \
		rvn info --list-extended -F ${TMPDIR}/test-single-standard-1.rvn
}

create_from_manifest_dir_body() {
	genmanifest
	genplist "@(,,0644) testfile"
	genplist "@dir(,,0644) testdir"

	touch testfile
	mkdir testdir
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist --timestamp 100020008

cat << EOF >> output.ucl
abi: '*:*:0'
categories: [
  'test'
]
comment: 'a test'
desc: 'Yet another test'
directories: [
  {
    group: false
    owner: false
    path: 'testdir'
    perms: 420
  }
]
flatsize: 0
maintainer: 'test'
messages: {
}
namebase: 'test'
prefix: '/'
scripts: {
}
subpackage: 'single'
variant: 'standard'
version: '1'
www: 'http://test'

EOF

	atf_check \
		-o file:output.ucl \
		-e empty \
		-s exit:0 \
		rvn info -R -F ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_pkg_descr_body() {
	genmanifest
	touch testfile
	genplist "testfile"

	atf_check rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist
	atf_check \
		-o match:"^Yet another test$" \
		-e empty \
		-s exit:0 \
		rvn info -D -F ${TMPDIR}/test-single-standard-1.rvn


cat << EOF >> METADATA
messages: {
  always: "This message always displayed."
  install: "This is install message."
  upgrade: "This is upgrade message."
}
EOF

	atf_check rvn create -o ${TMPDIR} -r . -m METADATA -w test.plist
	atf_check \
		-o match:"^This message always displayed[.]$" \
		-e empty \
		-s exit:0 \
		rvn info --pkg-message -F ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_with_keyword_and_message_body() {
	genmanifest
	genplist "@showmsg plop"

cat << EOF > showmsg.ucl
actions: []
messages: [
	{ message: "always" },
	{ message: "on upgrade";type = "upgrade" },
	{ message: "on install"; type = "install" },
]
EOF

cat << EOF >> output.ucl
abi: '*:*:0'
categories: [
  'test'
]
comment: 'a test'
desc: 'Yet another test'
directories: [
]
flatsize: 0
maintainer: 'test'
messages: {
  always: [
    'always'
  ]
  install: [
    'on install'
  ]
  upgrade: [
    'on upgrade'
  ]
}
namebase: 'test'
prefix: '/'
scripts: {
}
subpackage: 'single'
variant: 'standard'
version: '1'
www: 'http://test'

EOF

	atf_check rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist
	atf_check \
		-o file:output.ucl \
		-e empty \
		-s exit:0 \
		rvn info --raw -F ${TMPDIR}/test-single-standard-1.rvn

}


create_from_plist_keyword_lua_actions_body()
{
	genmanifest
	genplist "@test A B C D"

cat << EOF > test.ucl
actions: [file]
attributes: {owner: plop}
prepackaging: <<EOS
ok = true
for i = 1, #arg do
	if not pkg.stat(arg[i]) then
		io.stderr:write("Unable to access file " .. arg[i] .. ":No such file or directory\n")
		ok = false
	end
end
if not ok then
	return 1
end
EOS
EOF

touch C
touch D

output="Unable to access file A:No such file or directory
Unable to access file B:No such file or directory
Fail to apply keyword 'test'
"

	atf_check \
		-e inline:"${output}" \
		-s exit:1 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .

touch A B
	atf_check \
		-s exit:0 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -m METADATA -w test.plist -r .
	atf_check \
		-o match:"-rw-r--r-- .*plop[ ]+wheel.*[ ]A$" \
		-e empty \
		-s exit:0 \
		xrvn -la ${TMPDIR}/test-single-standard-1.rvn
}

create_from_plist_keyword_deprecated_body()
{
	genmanifest
	genplist "@test A B C D"

cat << EOF > test.ucl
arguments: true
deprecated: true
EOF

	atf_check \
		-e inline:"The use of '@test' is deprecated\n" \
		-s exit:0 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

cat << EOF > test.ucl
arguments: true
deprecated: true
deprecation_message: <<EOM
we don't like it anymore
EOM
EOF

	atf_check \
		-e inline:"The use of '@test' is deprecated: we don't like it anymore\n" \
		-s exit:0 \
		rvn -o KEYWORDS_DIR=. create -o ${TMPDIR} -r . -m METADATA -w test.plist

}

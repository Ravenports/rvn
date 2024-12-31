#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	issue1881 \
	issue1881_newdep \
	three_digit_revision \
	dual_conflict \
	file_become_dir \
	dir_become_file \
	dir_is_symlink_to_a_dir \
	no_dep_conflicts

issue1881_body() {
	touch dummy.plist
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg1" "pkg_a" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg2" "pkg_a" "single" "standard" "1_1" "/"

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg3" "pkg_b" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg4" "pkg_c" "single" "standard" "2" "/"

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}       -r . -m pkg1.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg2.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg3.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg4.ucl -w dummy.plist

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg_a~single~standard~1.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check \
		-o not-match:"[.] pkg_[bc]-single-standard-"  \
		-e ignore \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r . upgrade -y 'pkg_'
}

issue1881_newdep_body() {
	touch dummy.plist
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg1" "pkg_a" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg2" "pkg_a" "single" "standard" "1_1" "/"

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg3" "pkg_b" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg4" "pkg_c" "single" "standard" "2" "/"

	cat <<EOF >> pkg2.ucl 
deps: {
	pkg_b-single-standard: "1"
}
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}       -r . -m pkg1.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg2.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg3.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg4.ucl -w dummy.plist

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg_a~single~standard~1.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check \
		-o match:'1[.]  [|]` pkg_b-single-standard-1' \
		-o match:"2[.]  pkg_a-single-standard-1_1 \[U\]" \
		-e ignore \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r . upgrade -y 'pkg_'
}


three_digit_revision_body() {

	touch dummy.plist
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg1" "pkg_a" "single" "standard" "1_90" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg2" "pkg_a" "single" "standard" "1_125" "/"

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}       -r . -m pkg1.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg2.ucl -w dummy.plist

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg_a-single-standard-1_90.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check \
		-o match:"1[.]  pkg_a-single-standard-1_125 \[U\]" \
		-e ignore \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r . upgrade -y 'pkg_'

	atf_check \
		-o inline:"pkg_a-single-standard version: 1_125\n" \
		-e empty \
		-s exit:0 \
		rvn query --all "{nsv} version: {version}"
}

dual_conflict_body()
{
	touch dummy.plist
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	echo "file-pkg-1" > plist-1
	echo "entry" > file-pkg-1

	echo "file-pkg-2" > plist-2
	echo "entry" > file-pkg-2

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-1" "pkg-1" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-2" "pkg-2" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-1U" "pkg-1" "single" "standard" "2" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-2U" "pkg-2" "single" "standard" "2" "/"

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-1.ucl -w plist-1
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-2.ucl -w plist-2

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target install -qy pkg-

	test -f ${TMPDIR}/target/file-pkg-1 || atf_fail "file absent"
	test -f ${TMPDIR}/target/file-pkg-2 || atf_fail "file absent"

	# reverse file manifests
	rm ${TMPDIR}/files/*
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-1U.ucl -w plist-2
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-2U.ucl -w plist-1

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -e empty -s exit:0 \
		-o match:"\[1\/2\]  pkg-1-single-standard-2 \[U\][ ]+\[ok\]" \
		-o match:"\[2\/2\]  pkg-2-single-standard-2 \[U\][ ]+\[ok\]" \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -y

	atf_check \
		-o inline:'package pkg-2-single-standard-2\n' \
		rvn -r ${TMPDIR}/target which -q /file-pkg-1

	atf_check \
		-o inline:'package pkg-1-single-standard-2\n' \
		rvn -r ${TMPDIR}/target which -q /file-pkg-2
}

file_become_dir_body() {

	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	echo "file-pkg-1" > plist-1
	echo "entry" > file-pkg-1

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-1" "pkg" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-2" "pkg" "single" "standard" "2" "/"

	# install file-pkg-1 as a file
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/      -r . -m pkg-1.ucl -w plist-1
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg~single~standard~1.rvn

	# change file-pkg-1 from file to directory, and install new file
	rm file-pkg-1
	mkdir file-pkg-1
	echo "entry" > file-pkg-1/newfile
	echo "file-pkg-1/newfile" > plist-2

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-2.ucl -w plist-2
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -e empty -s exit:0 \
		-o match:"\[1\/1\]  pkg-single-standard-2 \[U\][ ]+\[ok\]" \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -y

	atf_check \
		-o inline:'package pkg-single-standard-2\n' \
		rvn -r ${TMPDIR}/target which -q /file-pkg-1/newfile
}


dir_become_file_body() {
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	mkdir file-pkg-1
	echo "entry" > file-pkg-1/file
	echo "file-pkg-1/file" > plist-1

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-1" "pkg" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-2" "pkg" "single" "standard" "2" "/"

	# install file-pkg-1 as a directory
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/      -r . -m pkg-1.ucl -w plist-1
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg~single~standard~1.rvn

	# change file-pkg-1 from a directory to a file
	rm -rf file-pkg-1
	echo "entry" > file-pkg-1
	echo "file-pkg-1" > plist-2

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-2.ucl -w plist-2
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -e empty -s exit:0 \
		-o match:"\[1\/1\]  pkg-single-standard-2 \[U\][ ]+\[ok\]" \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -y

	atf_check \
		-o inline:'package pkg-single-standard-2\n' \
		rvn -r ${TMPDIR}/target which -q /file-pkg-1
}



dir_is_symlink_to_a_dir_body()
{
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir target
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	mkdir share lib lib/something
	ln -sf ../lib/something share/something
	echo "entry" > lib/something/file
	echo "lib/something/file" > plist-1
	echo "share/something" >> plist-1

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-1" "pkg" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg-2" "pkg" "single" "standard" "2" "/"

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/      -r . -m pkg-1.ucl -w plist-1
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg~single~standard~1.rvn

	rm share/something
	mkdir share/something
	echo "entry" > share/something/file
	echo "lib/something/file" > plist-2
	echo "share/something/file" >> plist-2

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg-2.ucl -w plist-2
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check -e empty -s exit:0 \
		-o match:"\[1\/1\]  pkg-single-standard-2 \[U\][ ]+\[ok\]" \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -y

	atf_check \
		-o inline:'package pkg-single-standard-2\n' \
		rvn -r ${TMPDIR}/target which -q /share/something/file
}


# verifies this issue is solved
# Proceed with fetching packages? [y/n]:
# [1/1] ravenadm-single-standard-3.03_2                             4424 KiB [ok]
#
# Conflict found: ncurses-primary-standard package installs files in the same location as ncurses-primary-standard
# Conflict found: ncurses-terminfo-standard package installs files in the same location as ncurses-terminfo-standard
#
no_dep_conflicts_body()
{
	mkdir -p ${TMPDIR}/target/var/cache/rvn
	mkdir files
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "ravenadm-1" "ravenadm" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "ravenadm-2" "ravenadm" "single" "standard" "2" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "ncurses" "ncurses" "primary" "standard" "1" "/"
	cat << EOF >> ${TMPDIR}/ravenadm-1.ucl
deps: {"ncurses-primary-standard": "1"}
EOF

	cat << EOF >> ${TMPDIR}/ravenadm-2.ucl
deps: {"ncurses-primary-standard": "1"}
EOF

	echo "file1" > file1
	echo "file2" > file2
	echo "file1" >  ncurses.plist
	echo "file2" >> ncurses.plist

	echo "ravenadm" > rfile1
	echo "rfile1" > ravenadm.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m ncurses.ucl -w ncurses.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m ravenadm-1.ucl -w ravenadm.plist

	# generate first version of the repository
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# install ravenadm
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target install -U -y ravenadm

	# confirmed what it installed
	atf_check -o inline:"ncurses-primary-standard-1\nravenadm-single-standard-1\n" \
		-e empty -s exit:0 rvn -r . info -q

	# update ravenadm
	rm ${TMPDIR}/files/raven*
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m ravenadm-2.ucl -w ravenadm.plist

	# regenerate repository and catalog
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# upgrade ravenadm
	atf_check -e empty -s exit:0 \
		-o match:"\[1\/1\]  ravenadm-single-standard-2 \[U\][ ]*\[ok\]" \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -U -y
}

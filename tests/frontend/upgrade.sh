#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	issue1881 \
	issue1881_newdep \
	three_digit_revision \
	dual_conflict \


#	file_become_dir \
#	dir_become_file \
#	dir_is_symlink_to_a_dir

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

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg_a-single-standard-1.rvn

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

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/pkg_a-single-standard-1.rvn

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
		rvn -r ${TMPDIR}/target which -q ${TMPDIR}/target/file-pkg-1

	atf_check \
		-o inline:'package pkg-1-single-standard-2\n' \
		rvn -r ${TMPDIR}/target which -q ${TMPDIR}/target/file-pkg-2
}

file_become_dir_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "1"
	echo "${TMPDIR}/file-pkg-1" > plist-1
	echo "entry" > file-pkg-1
	atf_check pkg create -M pkg.ucl -p plist-1
	mkdir target
	atf_check -o ignore pkg -o REPOS_DIR="${TMPDIR}" -r ${TMPDIR}/target install -Uy ${TMPDIR}/pkg-1.pkg
	atf_check test -f target/${TMPDIR}/file-pkg-1
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "2"
	rm file-pkg-1
	mkdir file-pkg-1
	echo entry > file-pkg-1/file
	echo "${TMPDIR}/file-pkg-1/file" > plist-2
	atf_check pkg create -M pkg.ucl -p plist-2
	atf_check -o ignore pkg -o REPOS_DIR="${TMPDIR}" -r ${TMPDIR}/target install -Uy ${TMPDIR}/pkg-2.pkg
}

dir_become_file_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "1"
	mkdir file-pkg-1
	echo entry > file-pkg-1/file
	echo "${TMPDIR}/file-pkg-1/file" > plist-1
	atf_check pkg create -M pkg.ucl -p plist-1
	mkdir target
	atf_check -o ignore pkg -o REPOS_DIR="${TMPDIR}" -r ${TMPDIR}/target install -Uy ${TMPDIR}/pkg-1.pkg
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "2"
	rm -rf file-pkg-1
	echo entry > file-pkg-1
	echo "${TMPDIR}/file-pkg-1" > plist-2
	atf_check pkg create -M pkg.ucl -p plist-2
	atf_check -o ignore pkg -o REPOS_DIR="${TMPDIR}" -r ${TMPDIR}/target install -Uy ${TMPDIR}/pkg-2.pkg
}

dir_is_symlink_to_a_dir_body()
{
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "1"
	mkdir share lib lib/something
	ln -sf ../lib/something share/something
	echo "entry" > lib/something/file
	echo "${TMPDIR}/lib/something/file" > plist-1
	echo "${TMPDIR}/share/something" >> plist-1
	atf_check pkg create -M pkg.ucl -p plist-1
	mkdir target
	atf_check -o ignore pkg -o REPOS_DIR="${TMPDIR}" -r ${TMPDIR}/target install -Uy ${TMPDIR}/pkg-1.pkg
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "2"
	rm share/something
	mkdir share/something
	echo "entry" > share/something/file
	echo "${TMPDIR}/lib/something/file" > plist-2
	echo "${TMPDIR}/share/something/file" >> plist-2
	atf_check pkg create -M pkg.ucl -p plist-2
	atf_check -o ignore pkg -o REPOS_DIR="${TMPDIR}" -r ${TMPDIR}/target install -Uy ${TMPDIR}/pkg-2.pkg
}

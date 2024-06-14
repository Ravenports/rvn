#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	reinstall \
#	pre_script_fail \
#	post_script_ignored \
#	install_missing_dep

xxxtest_setup()
{
	# Do a local config to avoid permissions-on-system-db errors.
        cat > ${TMPDIR}/rvn.conf << EOF
RVN_CACHEDIR=${TMPDIR}/cache
RVN_DBDIR=${TMPDIR}
REPOS_DIR=[
	${TMPDIR}/reposconf
]
repositories: {
        local: { url : file://${TMPDIR} }
}
EOF
	mkdir -p ${TMPDIR}/reposconf
	cat << EOF > ${TMPDIR}/reposconf/repo.conf
local: {
	url: file:///$TMPDIR,
	enabled: true
}
EOF
}

reinstall_body()
{
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	mkdir -p ${TMPDIR}/files
    touch dummy.plist
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/files -r ${TMPDIR} -m test.ucl -w dummy.plist

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR} install -q --only-registration --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn genrepo ${TMPDIR}

	mkdir reposconf
	cat << EOF > reposconf/repo.conf
local: {
	url: file:///$TMPDIR,
	enabled: true
}
EOF

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR} catalog -f

	mkdir -p ${TMPDIR}/var/cache/rvn
	atf_check \
		-o match:"\[1\/1\]  test-single-standard-1 \[\*\][ ]+\[ok\]" \
		-s exit:0 \
		rvn -o REPOS_DIR="${TMPDIR}/reposconf" -r ${TMPDIR} install -fy test
}

pre_script_fail_body()
{
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg test test 1
	cat << EOF >> test.ucl
scripts: {
   pre-install: "exit 1"
}
EOF

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		pkg create -M test.ucl

	atf_check -o ignore \
		-e inline:"${PROGNAME}: PRE-INSTALL script failed\n" \
		-s exit:3 \
		pkg -o REPOS_DIR="/dev/null" install -y ${TMPDIR}/test-1.pkg
}

post_script_ignored_body()
{
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg test test 1
	cat << EOF >> test.ucl
scripts: {
   post-install: "exit 1"
}
EOF

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		pkg create -M test.ucl

	atf_check -o ignore \
		-e inline:"${PROGNAME}: POST-INSTALL script failed\n" \
		-s exit:0 \
		pkg -o REPOS_DIR="/dev/null" install -y ${TMPDIR}/test-1.pkg
}

install_missing_dep_body()
{
	test_setup

	# Create one package so we at least have a repo.
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg ${TMPDIR}/test test 1 /usr/local
	cat << EOF >> ${TMPDIR}/test.ucl
deps: {
	b: {
		origin: "wedontcare",
		version: "1"
	}
}
EOF
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		pkg -C "${TMPDIR}/pkg.conf" create -o ${TMPDIR} -M ${TMPDIR}/test.ucl

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		pkg  -C "${TMPDIR}/pkg.conf" repo ${TMPDIR}

	mkdir -p ${TMPDIR}/reposconf
	cat << EOF > ${TMPDIR}/reposconf/repo.conf
local: {
	url: file:///$TMPDIR,
	enabled: true
}
EOF

	atf_check \
		-o ignore \
		-e not-empty \
		-s not-exit:0 \
		pkg -C "${TMPDIR}/pkg.conf" install -y test
}

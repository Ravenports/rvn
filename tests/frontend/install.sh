#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	reinstall \
	pre_script_fail \
	post_script_ignored \
	install_missing_dep

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
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
   pre-install: [{code: "exit 1", args: ""}]
}
EOF

	mkdir -p ${TMPDIR}/files
	touch dummy.plist
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/files -r ${TMPDIR} -m test.ucl -w dummy.plist

	atf_check -o ignore \
		-o match:"pre-install Bourne shell script number 0 failed" \
		-s exit:0 \
		rvn -r ${TMPDIR} -o REPOS_DIR="/dev/null" install -y --file ${TMPDIR}/files/test-single-standard-1.rvn
}

post_script_ignored_body()
{
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
   post-install: [{code: "exit 1", args: ""}]
}
EOF

	mkdir -p ${TMPDIR}/files
	touch dummy.plist
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/files -r ${TMPDIR} -m test.ucl -w dummy.plist

	atf_check -o ignore \
		-o match:"post-install Bourne shell script number 0 failed" \
		-s exit:0 \
		rvn -r ${TMPDIR} -o REPOS_DIR="/dev/null" install -y --file ${TMPDIR}/files/test-single-standard-1.rvn
}

install_missing_dep_body()
{
	# Create one package so we at least have a repo.
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> ${TMPDIR}/test.ucl
deps: {"a-primary-standard": "1"}
EOF

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
		rvn genrepo ${TMPDIR}

	mkdir -p ${TMPDIR}/reposconf
	cat << EOF > ${TMPDIR}/reposconf/repo.conf
local: {
	url: file:///$TMPDIR,
	enabled: true
}
EOF

	mkdir -p ${TMPDIR}/target
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target catalog -f

	mkdir -p ${TMPDIR}/target/var/cache/rvn
	atf_check \
		-o empty \
		-e match:"^Corruption detected[.] The a-primary-standard dependency is missing from the catalog" \
		-s exit:1 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target install -U -y test-single-standard
}
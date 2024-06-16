#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	basic

basic_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	mkdir reposconf1
	cat <<EOF >> reposconf1/repo.conf
local: {
	url: file:///${TMPDIR}/repo1,
	enabled: true
}
EOF

	mkdir reposconf2
	cat <<EOF >> reposconf2/repo.conf
local: {
	url: file:///${TMPDIR}/repo2,
	enabled: true
}
EOF

	mkdir -p ${TMPDIR}/target
	mkdir -p ${TMPDIR}/repo1/files
	mkdir -p ${TMPDIR}/repo2/files
	mkdir -p ${TMPDIR}/target/var/cache/rvn
	touch dummy.plist

	# Generate first version repo and catalog
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/repo1/files -r ${TMPDIR} -m test.ucl -w dummy.plist

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn genrepo ${TMPDIR}/repo1

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf1" -r ${TMPDIR}/target catalog -f

	# Generate second version repo and catalog
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "2" "/"

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/repo2/files -r ${TMPDIR} -m test.ucl -w dummy.plist

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn genrepo ${TMPDIR}/repo2

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf1" -r ${TMPDIR}/target catalog -f

	# install first version
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf1" -r ${TMPDIR}/target install -y test-single

	# refetch catalog
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf2" -r ${TMPDIR}/target catalog -f

	# upgrade
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf2" -r ${TMPDIR}/target upgrade -y test-single

	# clean
	atf_check \
		-o match:"^obsolete: test-single-standard-1[~][0123456789abcdef]{10}[.]rvn" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target clean -y


}

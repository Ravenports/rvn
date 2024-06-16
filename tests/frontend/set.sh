#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	set_automatic

initialize_pkg() {
	# install --automatic and install --manual require the package to be present in the catalog.
	# It's an unintended limitation of the implementation.  But do we really care about changing
	# the flag of a package that can't be upgraded??

	touch dummy.plist
	mkdir -p ${TMPDIR}/var/cache/rvn
	mkdir files
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r . catalog -f
	atf_check -o ignore -e empty -s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r . install -U -qy test
}

set_automatic_body() {
	initialize_pkg

	atf_check \
		-o inline:"0\n" \
		-e empty \
		-s exit:0 \
		rvn -r . query "{auto}" test

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r . install -U --automatic --exact-match --quiet test-single-standard

	atf_check \
		-o inline:"1\n" \
		-e empty \
		-s exit:0 \
		rvn query "{auto}" test

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r . install -U --manual --exact-match --quiet test-single-standard

	atf_check \
		-o inline:"0\n" \
		-e empty \
		-s exit:0 \
		rvn -r . query "{auto}" test
}

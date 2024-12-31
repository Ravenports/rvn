#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	search \
	search_options

search_body() {
	export REPOS_DIR=/nonexistent
	atf_check \
		-e match:"Catalog database is missing, should be here:" \
		-o empty \
		-s exit:1 \
		rvn search -Q comment --namebase pkg
}

search_options_body() {

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

	atf_check \
		-o match:"^test[~]single[~]standard[~]1[ ]+test" \
		-e ignore \
		-s exit:0 \
		rvn -r . search -U test

	atf_check \
		-o inline:"test~single~standard~1\n" \
		-e ignore \
		-s exit:0 \
		rvn -r . search -U -q test

	atf_check \
		-o inline:"test~single~standard~1\ncomment      : a test\ndescription  : \nThis is a test\n" \
		-e ignore \
		-s exit:0 \
		rvn -r . search -U -Q des -Q comm test

}

#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	update_error

update_error_body() {

	mkdir repos
	mkdir empty
	cat > repos/test.conf << EOF
test: {
  url: "file://empty/",
}
EOF

	atf_check \
		-o empty \
		-e inline:"No repositories available from which to fetch checksum\n" \
		-s exit:1 \
		rvn -R repos catalog
}

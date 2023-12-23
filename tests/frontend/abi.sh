#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh
tests_init \
	basic

basic_body() {
	_expected="FreeBSD:13:amd64\n"
	atf_check \
		-o inline:"${_expected}" \
		rvn -o ABI_FILE=$(atf_get_srcdir)/artifacts/fbsd.binin config abi

	atf_check \
		-o inline:"dragonfly:5.10:x86:64\n" \
		rvn -o ABI_FILE=$(atf_get_srcdir)/artifacts/dfly.binin config abi
}

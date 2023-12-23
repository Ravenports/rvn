#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh
tests_init \
	basic_freebsd \
	basic_dragonfly

basic_freebsd_body() {
	atf_only_on FreeBSD rvn abi interpretation is hardcoded
	atf_check \
		-o inline:"freebsd:x86_64:13\n" \
		env ABI_FILE=$(atf_get_srcdir)/artifacts/fbsd.binin rvn config abi
}

basic_dragonfly_body() {
	atf_only_on DragonFly rvn abi interpretation is hardcoded
	atf_check \
		-o inline:"dragonfly:x86_64:5.10\n" \
		env ABI_FILE=$(atf_get_srcdir)/artifacts/dfly.binin rvn config abi
}

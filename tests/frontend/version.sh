#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	version \
    test_pattern \
    test_pattern_names_stdin \
    test_patterns_stdin \
#	compare

version_body() {
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1 2
	atf_check -o inline:">\n" -s exit:0 rvn version -t 2 1
	atf_check -o inline:"=\n" -s exit:0 rvn version -t 2 2
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 2 1,1
	# Special prefixes
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1.pl1 1.alpha1
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1.alpha1 1.beta1
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1.beta1 1.pre1
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1.pre1 1.rc1
	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1.rc1 1

	atf_check -o inline:"<\n" -s exit:0 rvn version -t 1.pl1 1.snap1
	atf_check -o inline:">\n" -s exit:0 rvn version -t 1.snap1 1.alpha1
}

test_pattern_body() {
    atf_check -o ignore           rvn version -T "joe" "j[airo]e"
    atf_check -o ignore -s exit:1 rvn version -T "joe" "j?r"
}

test_pattern_names_stdin_body() {
	cat << EOF >> PACKAGES
getconf
getent
getopt
gprof
grep
groups
gunzip
EOF
    atf_check -o match:"getconf getopt" echo $(cat PACKAGES | rvn version -T - "get[co]*")
}

test_patterns_stdin_body() {
    cat << EOF >> PATTERNS
AdaSAT-dev-standard-[0-9][0-9].0.0
AdaSAT-*-standard-24.0.0
AdaSAT-*
GeoIP-single-standard*
adasat-dev-standard
AdaSAT-dev-standard
A?????-*
EOF
    atf_check -o inline:"AdaSAT-dev-standard-[0-9][0-9].0.0 AdaSAT-*-standard-24.0.0 AdaSAT-* A?????-*\n"\
    echo $(cat PATTERNS | rvn version -T "AdaSAT-dev-standard-24.0.0" -)
}

compare_body() {
    # rvn info doesn't do this and likely will not ever do this (undocumented in man page)
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg test test 5.20_3

	atf_check \
		-o match:".*Installing.*" \
		rvn register -M test.ucl
	atf_check \
		-o ignore \
		rvn info "test>0"
	atf_check \
		-o ignore \
		-e ignore \
		-s exit:1 \
		rvn info "test<5"
	atf_check \
		-o ignore \
		rvn info "test>5<6"
	atf_check \
		-o ignore \
		-e ignore \
		-s exit:1 \
		rvn info "test>5<5.20"
	atf_check \
		-o ignore \
		-e ignore \
		-s exit:1 \
		rvn info "test>5.20_3<6"
}

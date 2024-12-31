#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	query

query_body() {
	touch plop
	touch bla
	touch dummy.plist
	echo "plop" >  test.plist
	echo "bla" >> test.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test2" "test2" "single" "standard" "2" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "plop" "plop" "single" "standard" "1" "/"

	cat >> test.ucl << EOF
options: {
	"OPT1": "on"
	"OPT2": "off"
}
EOF

	cat >> test2.ucl << EOF
options: {
	"OPT1": "on"
	"OPT2": "off"
}
EOF

	sed -ie 's/comment: a test/comment: Nothing to see here/' plop.ucl
	cat >> plop.ucl << EOF
deps: {
	test~single~standard: "1"
}
EOF

	atf_check -o empty -e empty -s exit:0 rvn -r . create -o ${TMPDIR} -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn -r . create -o ${TMPDIR} -r . -m plop.ucl -w dummy.plist

	atf_check -o ignore -e empty -s exit:0 rvn -r . install -qy --file ${TMPDIR}/test~single~standard~1.rvn
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -qy --file ${TMPDIR}/plop~single~standard~1.rvn

	atf_check \
		-o inline:"plop\ntest\n" \
		-e empty \
		-s exit:0 \
		rvn -r . query --all "{name}"

	atf_check \
		-o inline:"test\n" \
		-e empty \
		-s exit:0 \
		rvn -r . query --all "{name}" -e "{#opts} > 0"

	atf_check \
		-o inline:"test: plop~single~standard  1\n" \
		-e empty \
		-s exit:0 \
		rvn query --all -e "{#rdeps} > 0" "{name}: {xrdep:nsv}  {xrdep:ver}"

	atf_check \
		-o inline:"test: plop~single~standard  1\n" \
		-e empty \
		-s exit:0 \
		rvn query -e "{#rdeps}> 0" "{name}: {xrdep:nsv}  {xrdep:ver}" test

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn query -e "{#rdeps} > 0" "{name}: {xrdep:nsv}  {xrdep:ver}" plop

	atf_check \
		-o empty \
		-e match:"rvn: Attempt to redefine file name pattern from 'plop' to 'test'" \
		-s exit:1 \
		rvn query -e "{#rdeps} > 0"  "{name}: {xrdep:nsv}  {xrdep:ver}" plop test

	atf_check \
		-o inline:"test: plop~single~standard  1\n" \
		-e empty \
		-s exit:0 \
		rvn query -e "{#rdeps} > 0"  "{name}: {xrdep:nsv}  {xrdep:ver}" -C "[pt][le][os][pt]-*"

	atf_check \
		-o inline:"test: plop~single~standard  1\n" \
		-e empty \
		-s exit:0 \
		rvn query -e "{#rdeps} > 0"  "{name}: {xrdep:nsv}  {xrdep:ver}" -C "[pt]??[pt]-*"

	atf_check \
		-o inline:"test: plop~single~standard  1\n" \
		-e empty \
		-s exit:0 \
		rvn query -e "{#rdeps} > 0"  "{name}: {xrdep:nsv}  {xrdep:ver}" -E test~single~standard

	atf_check \
		-o inline:"plop: test~single~standard  1\n" \
		-e empty \
		-s exit:0 \
		rvn query --all -e "{#deps} > 0"  "{name}: {xdep:nsv}  {xdep:ver}"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn query --all -e "{#deps} > 0 & {#rdeps} > 0" "{name}"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn query --all -e "{#opts} > 0 & {#dirs} > 0" "{name}"

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn remove -y plop

	atf_check \
		-o inline:"test 2\n" \
		-e empty \
		-s exit:0 \
		rvn query --all "{name} {#opts}"

	atf_check \
		-o inline:"" \
		-e empty \
		-s exit:0 \
		rvn query --all -e "{#opts} eq 0" "{name}"

	atf_check \
		-o empty \
		-e inline:"evaluation problem: invalid operator for number comparison\n" \
		-s exit:1 \
		rvn query --all -e "{#opts} = 0" "{name}"
}

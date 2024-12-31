#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	dead_symlink \
	good_symlink \
	nondir_symlink


dead_symlink_body() {

	mkdir ${TMPDIR}/plop
	mkdir ${TMPDIR}/target
	echo "@dir plop" > test.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test.ucl -w test.plist

	ln -s plop2 ${TMPDIR}/target/plop

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -y --file ${TMPDIR}/test~single~standard~1.rvn
	test -d ${TMPDIR}/target/plop || atf_fail "directory not created"
}

good_symlink_body() {
	mkdir ${TMPDIR}/plop
	mkdir ${TMPDIR}/target
	echo "@dir plop" > test.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test.ucl -w test.plist

	mkdir ${TMPDIR}/target/plop2
	ln -s plop2 ${TMPDIR}/target/plop

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -y --file ${TMPDIR}/test~single~standard~1.rvn
	test -h ${TMPDIR}/target/plop || atf_fail "Symlink deleted"
}

nondir_symlink_body() {
	mkdir ${TMPDIR}/plop
	mkdir ${TMPDIR}/target
	echo "@dir plop" > test.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test.ucl -w test.plist

	ln test.plist ${TMPDIR}/target/plop

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -y --file ${TMPDIR}/test~single~standard~1.rvn
	test -d ${TMPDIR}/target/plop || atf_fail "directory not created"
}
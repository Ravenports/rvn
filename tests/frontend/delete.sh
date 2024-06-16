#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	delete_all \
	delete_rvn \
	simple_delete \
	simple_delete_prefix_ending_with_slash \
	delete_with_directory_owned


delete_all_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "foo" "foo" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "pkg" "pkg" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	touch dummy.plist
	mkdir files
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m foo.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m pkg.ucl -w dummy.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/foo-single-standard-1.rvn
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/pkg-single-standard-1.rvn
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check -o ignore rvn -o ${TMPDIR}/files -r . remove -ay
}

delete_rvn_body() {
	touch dummy.plist
	mkdir files
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "rvn" "rvn" "single" "standard" "1" "/"
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m rvn.ucl -w dummy.plist
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/rvn-single-standard-1.rvn

	# try removing rvn via normal LIKE% matching (expected to not match)
	atf_check -o inline:"No installed packages were selected for removal.\n" \
		-e empty -s exit:0  rvn -o ${TMPDIR}/files -r . remove -y rvn

	# try removing rvn via glob matching (expected to not match)
	atf_check -o inline:"No installed packages were selected for removal.\n" \
		-e empty -s exit:0  rvn -o ${TMPDIR}/files -r . remove -y -C rvn-single-standard

	# try removing rvn via exact matching (expected to match)
	atf_check -o match:"Dry run: The following packages will be removed:" \
		-o match:"1. rvn-single-standard" \
		-e empty -s exit:0  rvn -o ${TMPDIR}/files -r . remove --dry-run -y -E rvn-single-standard

	# finally remove rvn (without the use off --all)
	atf_check -o match:"\[1\/1\] Removing rvn-single-standard-1" \
		-e empty -s exit:0  rvn -o ${TMPDIR}/files -r . remove --force -y rvn
}

simple_delete_body() {
	mkdir dir
	mkdir files

	touch file1
	touch dir/file2

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" ""
	echo "file1" >  test.plist
	echo "dir/file2" >> test.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r ${TMPDIR} -m test.ucl -w test.plist
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/test-single-standard-1.rvn
	atf_check -o ignore rvn -o ${TMPDIR}/files -r . remove -y test

	test -f file1 && atf_fail "'file1' still present"
	test -f dir/file2 && atf_fail "'dir/file2' still present"
	test -d dir && atf_fail "'dir' still present"
	test -d ${TMPDIR} || atf_fail "Prefix have been removed"
}

simple_delete_prefix_ending_with_slash_body() {
	mkdir dir
	mkdir files

	touch ${TMPDIR}/file1
	touch ${TMPDIR}/dir/file2

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	echo "file1" >  test.plist
	echo "dir/file2" >> test.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/test-single-standard-1.rvn
	atf_check -o ignore rvn -o ${TMPDIR}/files -r . remove -y test

	test -f file1 && atf_fail "'file1' still present"
	test -f dir/file2 && atf_fail "'dir/file2' still present"
	test -d dir && atf_fail "'dir' still present"
	test -d ${TMPDIR} || atf_fail "Prefix have been removed"
}

delete_with_directory_owned_body() {
	mkdir dir1
	mkdir dir2
	mkdir files

	touch file1
	touch dir1/file2
	touch dummy.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test2" "test2" "single" "standard" "1" "/"
	echo "file1" >  test.plist
	echo "dir1/file2" >> test.plist

	echo "@dir dir2" >> test2.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test2.ucl -w test2.plist

	rm -rf dir1
	rm -rf dir2
	rm file1

	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/test-single-standard-1.rvn
	atf_check -o ignore -e empty -s exit:0 rvn -r . install -q --file ${TMPDIR}/files/test2-single-standard-1.rvn

	test -d dir1 || atf_fail "'dir1' was not created"
	test -d dir2 || atf_fail "'dir2' was not created"

	atf_check -o ignore rvn -o ${TMPDIR}/files -r . remove -y test-single

	test -f file1 && atf_fail "'file1' still present"
	test -f dir1/file2 && atf_fail "'dir1/file2' still present"
	test -d dir1 && atf_fail "'dir1' is still present"

	atf_check -o ignore rvn -o ${TMPDIR}/files -r . remove -y test2-single

	test -d dir2 && atf_fail "'dir2' still present"
	test -d ${TMPDIR} || atf_fail "Prefix has been removed"
}

#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	autoremove \
	autoremove_quiet \
	autoremove_dryrun

autoremove_prep() {

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "master" "master" "single" "standard" "1" "/"

	cat << EOF >> test.ucl
scripts: {
	post-deinstall: [
		{
			args: ""
			code: "exit 1"
		}
	]
}
EOF

	cat << EOF >> master.ucl
deps: {
  test-single-standard: "1"
}
EOF

	mkdir -p ${TMPDIR}/target/var/cache/rvn
	mkdir files
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	touch dummy.plist
	touch file1
	touch file2
	echo "file1" >  test.plist
	echo "file2" >> test.plist

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/files -r . -m test.ucl -w test.plist

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/files -r . -m master.ucl -w dummy.plist

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn genrepo ${TMPDIR}

	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target catalog -f

	# install master which pulls in test package
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target install -y master-single-standard

	# verify that
	atf_check \
		-o inline:"master-single-standard\ntest-single-standard\n" \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target query "{nsv}" -a

	# delete master, leaving test package orphaned
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target remove -y master-single-standard

	# verify that
	atf_check \
		-o inline:"test-single-standard\n" \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target query "{nsv}" -a
}

autoremove_body() {
	autoremove_prep

	atf_check \
		-o match:"\[1\/1\] Removing test-single-standard-1" \
		-o match:"POST-DEINSTALL: Bourne script 0 failed" \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target autoremove -y

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target info -a

	test ! -f ${TMPDIR}/target/file1 -o ! -f ${TMPDIR}/target/file2 || atf_fail "Files are still present"
}


autoremove_quiet_body() {
	autoremove_prep

	atf_check \
		-o match:"POST-DEINSTALL: Bourne script 0 failed" \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target autoremove -yq

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target info -a

	test ! -f ${TMPDIR}/target/file1 -o ! -f ${TMPDIR}/target/file2 || atf_fail "Files are still present"
}

autoremove_dryrun_body() {
	autoremove_prep

	atf_check \
		-o match:"Dry run: The following packages will be removed:" \
		-o match:"1. test-single-standard" \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target autoremove -ny

	atf_check \
		-o inline:"test-single-standard\n" \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target query "{nsv}" -a

	test -f ${TMPDIR}/target/file1 -o -f ${TMPDIR}/target/file2 || atf_fail "Files are missing"
}

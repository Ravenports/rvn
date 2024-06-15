#!/usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	basic \
	basic_dirs \
	setuid \
	setuid_hardlinks \
	symlinks

basic_body()
{
	mkdir files
	mkdir ${TMPDIR}/target
	echo "test" > a
	echo "${TMPDIR}/a" >  test.plist
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r / -m test.ucl -w test.plist
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/files/test-single-standard-1.rvn


OUTPUT="${TMPDIR}/target/local.sqlite
${TMPDIR}/target${TMPDIR}/a
"
	atf_check \
		-o inline:"${OUTPUT}" \
		-e empty \
		-s exit:0 \
		find ${TMPDIR}/target -type f -print | sort

	echo "test2" > a
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r / -m test.ucl -w test.plist

	# check no leftovers during upgrades/reinstallation
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --force \
		--file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check \
		-o inline:"${OUTPUT}" \
		-e empty \
		-s exit:0 \
		find ${TMPDIR}/target -type f -print | sort

}

basic_dirs_body()
{
	mkdir files
	mkdir ${TMPDIR}/plop
	mkdir ${TMPDIR}/target
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	echo "@dir ${TMPDIR}/plop" >  test.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r / -m test.ucl -w test.plist
	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/files/test-single-standard-1.rvn

	test -d ${TMPDIR}/target${TMPDIR}/plop || atf_fail "directory not extracted"
}

setuid_body()
{
	touch ${TMPDIR}/a
	echo "${TMPDIR}/a" >  test.plist
	chmod 04554 ${TMPDIR}/a || atf_fail "Fail to chmod"
	mkdir ${TMPDIR}/target
	mkdir ${TMPDIR}/files
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r / -m test.ucl -w test.plist

	atf_check \
		-o match:"^-r-sr-xr-- " \
		-e ignore \
		rvn info -X --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check \
		-o match:"^-r-sr-xr-- " \
		-e empty \
		-s exit:0 \
		ls -l ${TMPDIR}/target${TMPDIR}/a
}

setuid_hardlinks_body()
{
	touch ${TMPDIR}/a
	ln ${TMPDIR}/a ${TMPDIR}/b
	chmod 04554 ${TMPDIR}/a || atf_fail "Fail to chmod"
	chmod 04554 ${TMPDIR}/b || atf_fail "Fail to chmod"
	mkdir ${TMPDIR}/target
	mkdir ${TMPDIR}/files

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	echo "${TMPDIR}/a" >  test.plist
	echo "${TMPDIR}/b" >>  test.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r / -m test.ucl -w test.plist
	atf_check \
		-o match:"^-r-sr-xr--.*a$" \
		-o match:"^hr-sr-xr--.*b$" \
		-e ignore \
		rvn info -X --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check \
		-o match:"^-r-sr-xr-- " \
		-e empty \
		-s exit:0 \
		ls -l ${TMPDIR}/target${TMPDIR}/a

	atf_check \
		-o match:"^-r-sr-xr-- " \
		-e empty \
		-s exit:0 \
		ls -l ${TMPDIR}/target${TMPDIR}/b
}


symlinks_body()
{
	ln -sf nothing a
	mkdir ${TMPDIR}/target
	mkdir ${TMPDIR}/files
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	echo "${TMPDIR}/a" >  test.plist

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r / -m test.ucl -w test.plist

	atf_check \
		-o match:"^lrwxr-xr-x.*a$" \
		-e ignore \
		rvn info -X --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check -o ignore -e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/files/test-single-standard-1.rvn

	atf_check \
		-o match:"^lrwxr-xr-x.*a -> nothing$" \
		-e empty \
		-s exit:0 \
		ls -l ${TMPDIR}/target${TMPDIR}/a
}

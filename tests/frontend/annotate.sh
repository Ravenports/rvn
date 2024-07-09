#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	annotate \
	annotate_multiple

annotate_body() {

	touch dummy.plist

	for pkg in "png-primary-standard" "sqlite-primary-standard" ; do
		atf_check \
			-o empty \
			-e empty \
			-s exit:0 \
			rvn create -o ${TMPDIR} -r ${TMPDIR} -m ${RESOURCEDIR}/$pkg.ucl -w dummy.plist
	done

	for pkg in "png-primary-standard-1.6.43" "sqlite-primary-standard-3.46.0" ; do
		atf_check \
			-o empty \
			-e empty \
			-s exit:0 \
			rvn -r ${TMPDIR} install -q --only-registration --file ${TMPDIR}/$pkg.rvn
	done

	[ -f "./local.sqlite" ] || \
		atf_fail "Can't populate $PKG_DBDIR/local.sqlite"

	# set TEST1=>test1 in png-primary-standard
	atf_check \
		-o match:"Annotation definition complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate --set --tag TEST1 --note test1 --yes png-

	# show all annotatations in png-primary-standard with info command
	# verify TEST1 is defined
	atf_check \
		-o match:"TEST1 => test1" \
		-e empty \
		-s exit:0 \
		rvn info -A png-

	# show value of TEST1 annotation in all installed png packages
	atf_check \
		-o match:"^png-primary-standard[ ]+TEST1 => test1" \
		-e empty \
		-s exit:0 \
		rvn annotate --find --tag TEST1 png-

	# set TEST2=>test2 in png-primary-standard
	atf_check \
		-o match:"Annotation definition complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate --set --tag TEST2 --note test2 --yes png-primary-standard

	# modifies TEST1 value for all installed png packages
	atf_check \
		-o match:"Annotation definition complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate -y --set -t TEST1 -n test1-modified png-

	# show test TEXT? tag values for png packages with info command
	atf_check \
		-o match:"TEST1 => test1-modified" \
		-o match:"TEST2 => test2" \
		-e empty \
		-s exit:0 \
		rvn info -A png

	# set TEST1=>test1-modified in sqlite-primary-standard
	# pkg uses -A/-M. -A fails to create if tag doesn't exist but -M always succeeds.
	# rvn --set work list -M.  There's no -A equivalent on rvn
	atf_check \
		-o match:"Annotation definition complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate --set --tag TEST1 --note test1-modified --yes sqlite-primary

	# show value of TEST1 annotation in all installed png packages
	atf_check \
		-o match:"^png-primary-standard[ ]+TEST1 => test1-modified" \
		-e empty \
		-s exit:0 \
		rvn annotate --find --tag TEST1 png-

	# delete png tag TEST1
	atf_check \
		-o match:"1. png-primary-standard[ ]+note: test1-modified" \
		-e empty \
		-s exit:0 \
		rvn annotate --delete --tag TEST1 --yes png

	# prove png TEST1 annotation is gone
	atf_check \
		-o not-match:"TEST1 => test1-modified" \
		-o match:"TEST2 => test2" \
		-e empty \
		-s exit:0 \
		rvn info -A png

	# delete png tag TEST2
	atf_check \
		-o match:"1. png-primary-standard[ ]+note: test2" \
		-e empty \
		-s exit:0 \
		rvn annotate --delete --tag TEST2 --yes png

	# prove png TEST1 and TEST2 annotations are both gone
	atf_check \
		-o not-match:": TEST1 => test1-modified" \
		-o not-match:": TEST2 => test2" \
		-e empty \
		-s exit:0 \
		rvn info -A png
}


annotate_multiple_body() {
	touch dummy.plist

	for pkg in "png-primary-standard" "sqlite-primary-standard" ; do
		atf_check \
			-o empty \
			-e empty \
			-s exit:0 \
			rvn create -o ${TMPDIR} -r ${TMPDIR} -m ${RESOURCEDIR}/$pkg.ucl -w dummy.plist
	done

	for pkg in "png-primary-standard-1.6.43" "sqlite-primary-standard-3.46.0" ; do
		atf_check \
			-o empty \
			-e empty \
			-s exit:0 \
			rvn -r ${TMPDIR} install -q --only-registration --file ${TMPDIR}/$pkg.rvn
	done

	# Check setting all packages with annotations
	atf_check \
		-o match:"png-primary-standard[ ]+no existing note found$" \
		-o match:"sqlite-primary-standard[ ]+no existing note found$" \
		-o match:"Annotation definition complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate --set --tag TEST1 --note test1 --yes '^.'

	# show changes
	atf_check \
		-o match:"png-primary-standard[ ]+TEST1 => test1$" \
		-o match:"sqlite-primary-standard[ ]+TEST1 => test1$" \
		-e empty \
		-s exit:0 \
		rvn annotate --find -t TEST1

	# update those tags for all
	atf_check \
		-o match:"png-primary-standard[ ]+note: test1$" \
		-o match:"sqlite-primary-standard[ ]+note: test1$" \
		-o match:"Annotation definition complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate --set -t TEST1 -n test-changed -y "^."

	# show those changes
	atf_check \
		-o match:"png-primary-standard[ ]+TEST1 => test-changed$" \
		-o match:"sqlite-primary-standard[ ]+TEST1 => test-changed$" \
		-e empty \
		-s exit:0 \
		rvn annotate --find --tag TEST1

	# delete all matching regex pattern
	atf_check \
		-o match:"png-primary-standard[ ]+note: test-changed$" \
		-o match:"sqlite-primary-standard[ ]+note: test-changed$" \
		-o match:"Annotation removal complete[.]" \
		-e empty \
		-s exit:0 \
		rvn annotate --yes --delete --tag TEST1 "(png|sqlite)"

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn annotate --find --tag TEST1
}

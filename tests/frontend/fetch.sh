#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

TEST_ROOT=${TMPDIR}
CONF=${TEST_ROOT}/conf
REPOS=${TEST_ROOT}/repos
REPO=${TEST_ROOT}/repo
DB=${TEST_ROOT}/db
CACHE=${TEST_ROOT}/cache

PKG_DBDIR=${DB}

tests_init \
	fetch_dep_success \
	fetch_missing \
	fetch_missing_dep \
	fetch_missing_file \
	fetch_missing_dep_file

test_setup()
{
	variant=$1
	atf_check rm -rf ${TEST_ROOT}/*
	atf_check mkdir -p ${CONF}/repos ${REPOS} ${REPO}/files ${DB} ${CACHE}

	touch dummy.plist
	cat <<EOF >> ${CONF}/repos/repo.conf
testrepo: {
	url: file://${REPO},
	enabled: true
}
EOF

	# Do a local config to avoid permissions-on-system-db errors.
	if ! cat > ${CONF}/rvn.conf << EOF
RVN_CACHEDIR=${CACHE}
RVN_DBDIR=${DB}
REPOS_DIR=[
	${REPOS}
]
EOF
	then
		atf_fail
	fi

	# Create two packages so we at least have a repo.
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	if ! cat << EOF >> test.ucl
deps: {
	b-single-standard: "1"
}
EOF
	then
		atf_fail
	fi

	atf_check -o empty -e empty -s exit:0 \
		rvn -C "${CONF}/rvn.conf" -o KEYWORDS_DIR=. \
			create -o ${REPO}/files -r / -m test.ucl -w dummy.plist

	if [ "${variant}" != "missing-pkg" ]; then
		atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "b" "b" "single" "standard" "1" "/"
		atf_check -o empty -e empty -s exit:0 \
			rvn -C "${CONF}/rvn.conf" -o KEYWORDS_DIR=. \
				create -o ${REPO}/files -r / -m b.ucl -w dummy.plist
	fi

	atf_check -o empty -e empty -s exit:0 \
		rvn -C "${CONF}/rvn.conf" genrepo --quiet ${REPO}

	if [ "${variant}" = "missing-file" ]; then
		atf_check rm -f ${REPO}/files/test~single~standard~1.rvn
	fi
	if [ "${variant}" = "missing-dep-file" ]; then
		atf_check rm -f ${REPO}/files/b~single~standard~1.rvn
	fi

	atf_check -o ignore rvn -C "${CONF}/pkg.conf" -R ${CONF}/repos catalog -f
}

fetch_dep_success_body()
{
	test_setup

	atf_check -o ignore -e empty -s exit:0 \
		rvn -C "${CONF}/rvn.conf" -R ${CONF}/repos fetch -U -r testrepo -y -d test
}

fetch_missing_body()
{
	test_setup

	atf_check -o empty -s exit:1 \
		-e inline:"No files matching the given pattern(s) have been found in the catalog.\n" \
		rvn -C "${CONF}/rvn.conf" -R ${CONF}/repos fetch -r testrepo -y missing
}

fetch_missing_dep_body()
{
	test_setup missing-pkg

	atf_check \
		-e inline:"Repository corruption detection: b-single-standard dependency is missing from the catalog.\n" \
		-s exit:1 \
		rvn -C "${CONF}/rvn.conf" -R ${CONF}/repos fetch -r testrepo -d -y test
}

fetch_missing_file_body()
{
	test_setup missing-file

	atf_check \
		-o ignore \
		-e inline:"\nFailed download: test~single~standard~1.rvn\n\n" \
		-s exit:1 \
		rvn -C "${CONF}/rvn.conf" -R ${CONF}/repos fetch -r testrepo -d -y test
}

fetch_missing_dep_file_body()
{
	test_setup missing-dep-file

	atf_check \
		-o ignore \
		-e inline:"\nFailed download: b~single~standard~1.rvn\n\n" \
		-s exit:1 \
		rvn -C "${CONF}/rvn.conf" -R ${CONF}/repos fetch -r testrepo -d -y test
}

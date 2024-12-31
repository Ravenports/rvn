#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	messages \
	messages_install \
	messages_below_lower_threshold_upgrade \
	messages_above_lower_threshold_upgrade \
	messages_exact_upper_threshold_upgrade \
	messages_above_upper_threshold_upgrade \
	messages_no_threshold_upgrade


test_setup () {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "5.20_3" "/"
	cat <<EOF >> test.ucl 
messages: [
	{ message: "package being removed", type: "remove" },
	{ message: "package being installed", type: "install" },
	{ message: "package is being upgraded", type: "upgrade" },
	{ message: "Upgrading from lower than 1.0", max_version: "1.0", type: "upgrade" },
	{ message: "Upgrading from higher than 1.0", min_version: "1.0", type: "upgrade"  },
	{ message: "Upgrading from >= 1.0 <= 3.0", max_version: "3.0", min_version: "1.0", type: "upgrade"  }
]
EOF

	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF
}

messages_body() {
	touch dummy.plist
	mkdir ${TMPDIR}/target

	test_setup

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist
	atf_check -o inline:"package being installed\n" \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/test~single~standard~5.20_3.rvn

	atf_check \
		-o match:"^test[~]single[~]standard deinstallation messages" \
		-o match:"^package being removed$" \
		rvn -r ${TMPDIR}/target remove -qy test
}

messages_install_body() {
	touch dummy.plist
	mkdir target
	mkdir files

	test_setup

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	atf_check \
		-o match:"^test[~]single[~]standard installation messages" \
		-o match:"^package being installed$" \
		-e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . install -qy test
}

messages_below_lower_threshold_upgrade_body() {
touch dummy.plist
	mkdir target
	mkdir files

	test_setup

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test091" "test" "single" "standard" "0.91" "/"
cat <<EOF >> test091.ucl 
messages: [
	{ message: "package 0.91 being removed", type: "remove" }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test091.ucl -w dummy.plist

	atf_check -o ignore \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/test~single~standard~0.91.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# don't show deinstallation messages of old package when upgrading
	atf_check \
		-o match:"^test[~]single[~]standard upgrade messages" \
		-o match:"^package is being upgraded$" \
		-o match:"^Upgrading from lower than 1.0$" \
		-e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . upgrade -qy test
}

messages_above_lower_threshold_upgrade_body() {
	touch dummy.plist
	mkdir target
	mkdir files

	test_setup

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test151" "test" "single" "standard" "1.51" "/"
cat <<EOF >> test151.ucl 
messages: [
	{ message: "package 1.51 being removed", type: "remove" }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test151.ucl -w dummy.plist

	atf_check -o ignore \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/test~single~standard~1.51.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# don't show deinstallation messages of old package when upgrading
	atf_check \
		-o match:"^test[~]single[~]standard upgrade messages" \
		-o match:"^package is being upgraded$" \
		-o match:"^Upgrading from higher than 1.0$" \
		-o match:"^Upgrading from >= 1.0 <= 3.0$" \
		-e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . upgrade -qy test
}

messages_exact_upper_threshold_upgrade_body() {
	touch dummy.plist
	mkdir target
	mkdir files

	test_setup

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test300" "test" "single" "standard" "3" "/"
cat <<EOF >> test300.ucl 
messages: [
	{ message: "package 3.00 being removed", type: "remove" }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test300.ucl -w dummy.plist

	atf_check -o ignore \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/test~single~standard~3.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# don't show deinstallation messages of old package when upgrading
	atf_check \
		-o match:"^test[~]single[~]standard upgrade messages" \
		-o match:"^package is being upgraded$" \
		-o match:"^Upgrading from higher than 1.0$" \
		-o match:"^Upgrading from >= 1.0 <= 3.0$" \
		-e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . upgrade -qy test
}

messages_above_upper_threshold_upgrade_body() {
	touch dummy.plist
	mkdir target
	mkdir files

	test_setup

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test301" "test" "single" "standard" "3.01" "/"
cat <<EOF >> test301.ucl 
messages: [
	{ message: "package 3.00 being removed", type: "remove" }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test301.ucl -w dummy.plist

	atf_check -o ignore \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/test~single~standard-3.01.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# don't show deinstallation messages of old package when upgrading
	atf_check \
		-o match:"^test[~]single[~]standard upgrade messages" \
		-o match:"^package is being upgraded$" \
		-o match:"^Upgrading from higher than 1.0$" \
		-e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . upgrade -qy test
}

messages_no_threshold_upgrade_body() {
	touch dummy.plist
	mkdir target
	mkdir files

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "5.20_3" "/"
	cat <<EOF >> test.ucl 
messages: [
	{ message: "package being removed", type: "remove" },
	{ message: "package being installed", type: "install" },
	{ message: "package is being upgraded", type: "upgrade" },
]
EOF

	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test301" "test" "single" "standard" "3.01" "/"
cat <<EOF >> test301.ucl 
messages: [
	{ message: "package 3.00 being removed", type: "remove" }
]
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR} -r . -m test301.ucl -w dummy.plist

	atf_check -o ignore \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target install -qy --file ${TMPDIR}/test~single~standard~3.01.rvn

	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . catalog -f

	# don't show deinstallation messages of old package when upgrading
	atf_check \
		-o match:"^test[~]single[~]standard upgrade messages" \
		-o match:"^package is being upgraded$" \
		-e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r . upgrade -qy test
}


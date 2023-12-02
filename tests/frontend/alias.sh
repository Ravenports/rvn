#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	alias \
	alias_from_env \
	alias_from_conf

alias_body() {
	. $(atf_get_srcdir)/test_environment.sh

	atf_check \
		-o inline:"ALIAS                     ARGUMENTS\n"\
		-e empty \
		-s exit:0 \
		rvn -C "" alias

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -C "" alias -q

	atf_check \
		-o empty \
		-e inline:"${PROGNAME}: No such alias: 'nonexistent'\n" \
		-s exit:1 \
		rvn -C "" alias nonexistent
}

alias_from_env_body() {
	. $(atf_get_srcdir)/test_environment.sh
	export ALIAS="showaliases=alias -q,list=info -q"

OUTPUT="showaliases          'alias -q'
list                 'info -q'
"
	atf_check \
		-o inline:"ALIAS                ARGUMENTS\n${OUTPUT}" \
		-e empty \
		-s exit:0 \
		rvn -C "" alias

	atf_check \
		-o inline:"${OUTPUT}" \
		-e empty \
		-s exit:0 \
		rvn -C "" alias -q

	atf_check \
		-o inline:"${OUTPUT}" \
		-e empty \
		-s exit:0 \
		rvn -C "" showaliases

	atf_check \
		-o empty \
		-e inline:"${PROGNAME}: No such alias: 'nonexistent'\n" \
		-s exit:1 \
		rvn -C "" alias nonexistent
}

alias_from_conf_body() {
	. $(atf_get_srcdir)/test_environment.sh
	unset ALIAS

cat << EOF > config
ALIAS: {
	showaliases: "alias -q",
	list: "info -q"
}
EOF

OUTPUT="showaliases          'alias -q'
list                 'info -q'
"
	atf_check \
		-o inline:"ALIAS                ARGUMENTS\n${OUTPUT}" \
		-e empty \
		-s exit:0 \
		rvn -C config alias

	atf_check \
		-o inline:"${OUTPUT}" \
		-e empty \
		-s exit:0 \
		rvn -C config alias -q

	atf_check \
		-o inline:"${OUTPUT}" \
		-e empty \
		-s exit:0 \
		rvn -C config showaliases

	atf_check \
		-o empty \
		-e inline:"${PROGNAME}: No such alias: 'nonexistent'\n" \
		-s exit:1 \
		rvn -C config alias nonexistent
}

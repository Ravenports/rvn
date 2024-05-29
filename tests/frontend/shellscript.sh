#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	basic \
	message \
	daemon \

#	upgrade  (requires rvn genrepo to be implemented)

basic_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install: [
    {
       args: ''
       code: <<EOS
	echo this is post install1
	echo this is post install2
EOS
    }
  ]
}
EOF

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"this is post install1\nthis is post install2\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test-single-standard-1.rvn

}

message_body() {
	# The message should be the last thing planned
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install: [
    {
       args: ''
       code: <<EOS
	echo this is post install1
	echo this is a message >> \${PKG_OUTFILE}
	echo this is post install2
EOS
     }
  ]
}
EOF

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"this is post install1\nthis is post install2\nthis is a message\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test-single-standard-1.rvn

}

daemon_body() {
	# We should not see the daemon's message
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install: [
    {
       args: ''
       code: <<EOS
	echo this is post install1
	echo this is a message >> \${PKG_OUTFILE}
	(sleep 2; echo this is a daemon >> \${PKG_OUTFILE}) < /dev/null > /dev/null 2>&1 &
	echo this is post install2
EOS
    }
  ]
}
EOF

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"this is post install1\nthis is post install2\nthis is a message\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test-single-standard-1.rvn

}

upgrade_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install: [
    {
       args: ''
       code: <<EOS
if [ -n "\${PKG_UPGRADE+x}" ]; then
   echo "upgrade:\${PKG_UPGRADE}">> \${PKG_OUTFILE}
fi
EOS
    }
  ]
}
EOF

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist
		
	mkdir -p ${TMPDIR}/target
	atf_check \
		-e empty \
		-o ignore \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test-single-standard-1.rvn

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "2" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install: [
    {
       args: ''
       code: <<EOS
if [ -n "\${PKG_UPGRADE+x}" ]; then
   echo "upgrade:\${PKG_UPGRADE}">> \${PKG_OUTFILE}
fi
EOS
    }
  ]
}
EOF

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist
		
	mkdir -p ${TMPDIR}/target
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn info -R -F ./test-2.rvn
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn genrepo .
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF
	atf_check \
		-e empty \
		-o match:"upgrade:true" \
		-s exit:0 \
		pkg -o REPOS_DIR="${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -y
}

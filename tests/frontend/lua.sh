#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init \
	script_basic \
	script_message \
	script_rooteddir \
	script_remove \
	script_execute \
	script_rename \
	script_filecmp \
	script_filecmp_symlink \
	script_copy \
	script_copy_symlink \
	script_sample_not_exists \
	script_sample_not_exists_symlink \
	script_sample_not_exists_two_files \
	script_sample_exists \
	script_stat \
	script_arguments \
	script_upgrade \
	script_pkg_execute


script_basic_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: 'print("this is post install1")'
    }
    {
       args: ''
       code: 'print("this is post install2")'
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
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
}

script_message_body() {
	# The message should be the last thing planned
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
      args: ''
      code: "print(\"this is post install1\")\npkg.print_msg(\"this is a message\")"
    }
    {
      args: ''
      code: 'print("this is post install2")'
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
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

}

script_rooteddir_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
      args: ''
      code: <<EOS
test = io.open("/file.txt", "w+")
test:write("test\n")
io.close(test)
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
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	[ -f ${TMPDIR}/target/file.txt ] || atf_fail "File not created in the rootdir"
	# test the mode
	atf_check -o match:"^-rw-r--r--" ls -l ${TMPDIR}/target/file.txt
	atf_check \
		-e empty \
		-o inline:"test\n" \
		-s exit:0 \
		cat ${TMPDIR}/target/file.txt

}

script_remove_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
      args: ''
      code: <<EOS
os.remove("/file")
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

	mkdir -p ${TMPDIR}/target/file
	atf_check \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
	test -d ${TMPDIR}/target/file && atf_fail "directory not removed"

	touch ${TMPDIR}/target/file
	atf_check \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
	test -f ${TMPDIR}/target/file && atf_fail "file not removed"
	return 0
}

script_rename_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
os.rename("/file","/plop")
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
	touch ${TMPDIR}/target/file
	atf_check \
		-e inline:"${ERR}" \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
	test -f ${TMPDIR}/target/file && atf_fail "File not renamed"
	test -f ${TMPDIR}/target/plop || atf_fail "File not renamed"
	return 0
}

script_execute_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
      args: ''
      code: <<EOS
os.execute("echo yeah")
EOS
    }
  ]
}
EOF

ERR="Failed to execute Lua script:LUA_ERRRUN\n[string \"os.execute(\"echo yeah\")\"]:1: os.execute not available\npost-install-lua Lua script number 0 failed\n"

	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w dummy.plist

	mkdir -p ${TMPDIR}/target
	atf_check \
		-e inline:"${ERR}" \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
}

script_upgrade_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
if pkg_upgrade then
 pkg.print_msg("upgrade : ".. tostring(pkg_upgrade))
end
EOS
    }
  ]
}
EOF

	mkdir -p ${TMPDIR}/files
	mkdir -p ${TMPDIR}/oneoff
	touch dummy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/oneoff -r . -m test.ucl -w dummy.plist

	mkdir -p ${TMPDIR}/target
	atf_check \
		-e empty \
		-o empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --file ${TMPDIR}/oneoff/test~single~standard~1.rvn

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "2" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
if pkg_upgrade then
 pkg.print_msg("upgrade:".. tostring(pkg_upgrade))
end
EOS
    }
  ]
}
EOF

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR}/files -r . -m test.ucl -w dummy.plist

	mkdir -p ${TMPDIR}/target
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
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target catalog -f

	mkdir -p ${TMPDIR}/target/var/cache/rvn
	atf_check \
		-e empty \
		-o match:"upgrade:true" \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -U -y
}

script_filecmp_body() {
	echo "sametext" > a
	echo "sametext" > b
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
  if pkg.filecmp("a", "b") == 0 then
     pkg.print_msg("same")
  else
     pkg.print_msg("different")
  end
EOS
    }
  ]
}
EOF

	echo "a" >  filecmp.plist
	echo "b" >> filecmp.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w filecmp.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"same\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	rm -rf ${TMPDIR}/target

	echo "sametext" > a
	echo "differenttext" > b
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
  if pkg.filecmp("a", "b") == 0 then
     pkg.print_msg("same")
  else
     pkg.print_msg("different")
  end
EOS
    }
  ]
}
EOF

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w filecmp.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"different\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
}

script_filecmp_symlink_body() {
	echo "sametext" > a
	echo "sametext" > b
	ln -s a c
	ln -s b d
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
  if pkg.filecmp("c", "d") == 0 then
     pkg.print_msg("same")
  else
     pkg.print_msg("different")
  end
EOS
    }
  ]
}
EOF

	echo "a" >  filecmp_symlink.plist
	echo "b" >> filecmp_symlink.plist
	echo "c" >> filecmp_symlink.plist
	echo "d" >> filecmp_symlink.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w filecmp_symlink.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"same\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn


	rm -rf ${TMPDIR}/target
	rm a b c d

	echo "sametext" > a
	echo "differenttext" > b
	ln -s a c
	ln -s b d
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
  if pkg.filecmp("c", "d") == 0 then
     pkg.print_msg("same")
  else
     pkg.print_msg("different")
  end
EOS
    }
  ]
}
EOF

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w filecmp_symlink.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"different\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
}

script_copy_body() {
	echo "sample text" > a.sample
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
   pkg.copy("a.sample", "a")
EOS
    }
  ]
}
EOF

	echo "a.sample" > copy.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w copy.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a.sample
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		cmp -s ${TMPDIR}/target/a.sample ${TMPDIR}/target/a
}

script_copy_symlink_body() {
	echo "sample text" > a.sample
	ln -s a.sample b
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
   pkg.copy("b", "a")
EOS
    }
  ]
}
EOF

	echo "a.sample" > copy_symlink.plist
	echo "b" >> copy_symlink.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w copy_symlink.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a.sample
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/b
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		cmp -s ${TMPDIR}/target/a.sample ${TMPDIR}/target/a
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		cmp -s ${TMPDIR}/target/b ${TMPDIR}/target/a
}

script_sample_not_exists_body() {
	echo "sample text" > a.sample
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: 'a.sample'
       code: <<EOS
  sample_file = pkg.prefixed_path(arg[1])
  if arg[2] == nil then
    target_file = string.gsub(sample_file,'.sample', "")
  else
    target_file = pkg.prefixed_path(arg[2])
  end
  if not pkg.stat(target_file) then
    pkg.copy(sample_file, target_file)
  end
EOS
     }
  ]
}
EOF

	echo "a.sample" > sample_not_exists.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w sample_not_exists.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a.sample
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		cmp -s ${TMPDIR}/target/a.sample ${TMPDIR}/target/a
}

script_sample_not_exists_symlink_body() {
	echo "sample text" > a
	ln -s a b.sample
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: 'b.sample'
       code: <<EOS
  sample_file = pkg.prefixed_path(arg[1])
  if arg[2] == nil then
    target_file = string.gsub(sample_file,'.sample', "")
  else
    target_file = pkg.prefixed_path(arg[2])
  end
  if not pkg.stat(target_file) then
    pkg.copy(sample_file, target_file)
  end
EOS
    }
  ]
}
EOF

	echo "a"         > sne_symlink.plist
	echo "b.sample" >> sne_symlink.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w sne_symlink.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/b.sample
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/b
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		cmp -s ${TMPDIR}/target/b.sample ${TMPDIR}/target/b
}

script_sample_not_exists_two_files_body() {
	echo "sample text" > a
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: 'a b'
       code: <<EOS
  sample_file = pkg.prefixed_path(arg[1])
  if arg[2] == nil then
    target_file = string.gsub(sample_file,'%.sample$', "")
  else
    target_file = pkg.prefixed_path(arg[2])
  end
  if not pkg.stat(target_file) then
    pkg.copy(sample_file, target_file)
  end
EOS
    }
  ]
}
EOF

	echo "a" > sne2_files.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w sne2_files.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/a
	atf_check -o inline:"sample text\n" cat ${TMPDIR}/target/b
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		cmp -s ${TMPDIR}/target/a ${TMPDIR}/target/b
}

script_sample_exists_body() {
	echo "sample text" > a.sample
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: 'a.sample'
       code: <<EOS
  sample_file = pkg.prefixed_path(arg[1])
  if arg[2] == nil then
    target_file = string.gsub(sample_file,'%.sample$', "")
  else
    target_file = pkg.prefixed_path(arg[2])
  end
  if not pkg.stat(target_file) then
    pkg.copy(sample_file, target_file)
  end
EOS
     }
  ]
}
EOF

	mkdir -p ${TMPDIR}/target
	echo "text modified" > ${TMPDIR}/target/a

	echo "a.sample" > sample_exists.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w sample_exists.plist

	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

	atf_check \
		-o empty \
		-e empty \
		-s exit:1 \
		cmp -s ${TMPDIR}/target/a.sample ${TMPDIR}/target/a
}

script_stat_body() {
	touch plop
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
       args: ''
       code: <<EOS
  local st = pkg.stat("plop")
  if st.size == 0 then
     pkg.print_msg("zero")
  end
  pkg.print_msg(st.type)
EOS
    }
  ]
}
EOF
	echo "plop" > stat.plist
	atf_check \
		-o empty \
		-e empty \
		-s exit:0 \
		rvn create -o ${TMPDIR} -r . -m test.ucl -w stat.plist

	mkdir ${TMPDIR}/target
	atf_check \
		-o inline:"zero\nreg\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
}

script_arguments_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
      args: 'plop'
      code: 'print(arg[1])'
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
		-o inline:"plop\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn

}


script_pkg_execute_body() {
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "test" "test" "single" "standard" "1" "/"
	cat << EOF >> test.ucl
scripts: {
  post-install-lua: [
    {
      args: ''
      code: <<EOS
pkg.print_msg("ALPHA")
pkg.exec({"echo","valid-command"})
pkg.print_msg("BRAVO")
pkg.exec({"/usr/bin/cmd-dne"})
pkg.print_msg("CHARLIE")
pkg.exec({"/usr/bin/find","/dne"})
pkg.print_msg("DELTA")
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
		-o inline:"ALPHA-to-be-done\n" \
		-e empty \
		-s exit:0 \
		rvn -r ${TMPDIR}/target install -q --no-registration --file ${TMPDIR}/test~single~standard~1.rvn
}
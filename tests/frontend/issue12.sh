#! /usr/bin/env atf-sh

. $(atf_get_srcdir)/test_environment.sh

tests_init replace_library


gencfiles() {
	cat << EOF >> mylibrary.h
#ifndef MYLIBRARY_H
#define MYLIBRARY_H

int add(int a, int b);
void print_hello();

#endif
EOF

	cat << EOF >> mylibrary.c
#include "mylibrary.h"
#include <stdio.h> // For printf

int add(int a, int b) {
   return a + b;
}

void print_hello() {
   printf("Hello from mylibrary!\n");
}
EOF

	cat << EOF >> mylibrary2.c
#include "mylibrary.h"
#include <stdio.h> // For printf

int add(int a, int b) {
   return a + b;
}

void print_hello() {
   printf("Hello from mylibrary version 2.0!\n");
}
EOF

	cat << EOF >> main.c
#include <stdio.h>
#include "mylibrary.h"

int main() {
    print_hello();
    int result = add(5, 3);
    printf("Result: %d\n", result);
    return 5;
}
EOF
}

gencheck() {
	cat << EOF >> get_soname.sh
#!/bin/sh
readelf -d \$1 | grep SONAME
EOF
}

replace_library_body() {
	touch dummy.plist
	mkdir -p ${TMPDIR}/target/var/cache/rvn
	mkdir files
	mkdir reposconf
	cat <<EOF >> reposconf/repo.conf
local: {
	url: file:///${TMPDIR},
	enabled: true
}
EOF

	cat << EOF >> lib.plist
libadd.so
libadd.so.1
EOF

	cat << EOF >> lib2.plist
libadd.so
libadd.so.2
EOF

	cat << EOF >> bin.plist
helloworld
EOF

	gencfiles
	gencheck
	cc -fPIC -c mylibrary.c -o mylibrary.o
	cc -shared -Wl,-soname,libadd.so.1 -o libadd.so.1 mylibrary.o
	ln -s libadd.so.1 libadd.so
	atf_check \
		-o match:".*Library soname: \[libadd.so.1\]$" \
		-s exit:0 /bin/sh get_soname.sh "libadd.so.1"

	cc main.c -L${TMPDIR} -Wl,-z,origin -Wl,-rpath,\$ORIGIN -ladd -o ${TMPDIR}/helloworld
	atf_check -o inline:"Hello from mylibrary!\nResult: 8\n" -s exit:5 ${TMPDIR}/helloworld

	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "lib1" "mylib" "single" "standard" "1" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "lib2" "mylib" "single" "standard" "2" "/"
	atf_check -s exit:0 sh ${RESOURCEDIR}/test_subr.sh new_pkg "binary" "helloworld" "single" "standard" "1" "/"

	cat << EOF >> ${TMPDIR}/binary.ucl
deps: {"mylib~single~standard": "1"}
EOF

	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m lib1.ucl -w lib.plist
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m binary.ucl -w bin.plist

	# remove built files
	rm libadd.so libadd.so.1 helloworld

	# generate first version of the repository
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}
	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target catalog -f

	# install helloworld
	atf_check \
		-o ignore \
		-e empty \
		-s exit:0 \
		rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target install -U -y helloworld

	# ./target/helloworld
	# confirmed what it installed
	atf_check -o inline:"mylib~single~standard~1\nhelloworld~single~standard~1\n" \
		-e empty -s exit:0 rvn -r ${TMPDIR}/target info -q

	# replace library package and rebuild helloworld
	rm ${TMPDIR}/files/mylib~single~standard~1.rvn

	cc -fPIC -c mylibrary2.c -o mylibrary2.o
	cc -shared -Wl,-soname,libadd.so.2 -o libadd.so.2 mylibrary2.o
	ln -s libadd.so.2 libadd.so
	atf_check \
		-o match:".*Library soname: \[libadd.so.2\]$" \
		-s exit:0 /bin/sh get_soname.sh "libadd.so.2"
	cc main.c -L${TMPDIR} -Wl,-z,origin -Wl,-rpath,\$ORIGIN -ladd -o ${TMPDIR}/helloworld
	atf_check -o inline:"Hello from mylibrary version 2.0!\nResult: 8\n" -s exit:5 ${TMPDIR}/helloworld
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m lib2.ucl -w lib2.plist

	sed -i'' -e '/mylib/ s|1|2|' ${TMPDIR}/binary.ucl

	# regenerate helloworld package
	atf_check -o empty -e empty -s exit:0 rvn create -o ${TMPDIR}/files -r . -m binary.ucl -w bin.plist

	# remove built files
	rm libadd.so libadd.so.2 helloworld

	# regen repository
	atf_check -o empty -e empty -s exit:0 rvn -r . genrepo --quiet ${TMPDIR}

	atf_check -o ignore -e empty -s exit:0 rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target catalog -f

	find .
	rvn -R "${TMPDIR}/reposconf" -r ${TMPDIR}/target upgrade -U -y

	readelf -d ./target/helloworld
	atf_check -o inline:"Hello from mylibrary version 2.0!\nResult: 8\n" -s exit:5 target/helloworld
}

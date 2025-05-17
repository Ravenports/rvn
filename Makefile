all: sqlite/libcustom_sqlite_pic.a
	/bin/sh extra/set_system.sh "${PREFIX}"
	gprbuild -p -P rvn
	gprbuild -p -P src/rvn-format/programs/rvnprogs
	gprbuild -p -P ravensign

sqlite/libcustom_sqlite_pic.a:
	${MAKE} -C sqlite

install:
	${BSD_INSTALL_PROGRAM} ${WRKSRC}/obj/rvn ${DESTDIR}${PREFIX}/sbin/
	${BSD_INSTALL_PROGRAM} ${WRKSRC}/src/rvn-format/programs/obj/xrvn \
		${DESTDIR}${PREFIX}/bin/
	${BSD_INSTALL_PROGRAM} ${WRKSRC}/obj-rs/ravensign ${DESTDIR}${PREFIX}/bin/
	${BSD_INSTALL_MAN} ${WRKSRC}/manpages/*.5 ${DESTDIR}${PREFIX}/share/man/man5/
	${BSD_INSTALL_MAN} ${WRKSRC}/manpages/*.7 ${DESTDIR}${PREFIX}/share/man/man7/
	${BSD_INSTALL_MAN} ${WRKSRC}/manpages/*.8 ${DESTDIR}${PREFIX}/share/man/man8/
	${BSD_INSTALL_DATA} ${WRKSRC}/extra/rvn.conf.sample ${DESTDIR}${PREFIX}/etc/
	mkdir -p ${DESTDIR}${PREFIX}/etc/rvn/repos
	mkdir -p ${DESTDIR}${PREFIX}/etc/ravensign
	mkdir -p ${DESTDIR}/var/run/ravensign

	# generate signserver.py from template
	sed -e "s/%%USER%%/${RVNUSER}/; s/%%GROUP%%/${RVNGROUP}/; s|%%PYTHON_CMD%%|${PY3COMMAND}|" \
		${WRKSRC}/extra/signserver.py.in > ${DESTDIR}${PREFIX}/sbin/signserver.py
	chmod 755 ${DESTDIR}${PREFIX}/sbin/signserver.py

	# generate systemd service manifest from template
	mkdir -p ${DESTDIR}${PREFIX}/share/rvn
	sed -e "s/%%USER%%/${RVNUSER}/" \
		${WRKSRC}/extra/signserver.service.in > ${DESTDIR}${PREFIX}/share/rvn/signserver.service

	# generate rc script from template
	sed -e "s/%%USER%%/${RVNUSER}/; s/%%GROUP%%/${RVNGROUP}/; s|%%PYTHON_CMD%%|${PY3COMMAND}|" \
		-e "s|%%PREFIX%%|${PREFIX}|g" \
		${WRKSRC}/extra/ravensign.in > ${DESTDIR}${PREFIX}/etc/rc.d/ravensign
	chmod 755 ${DESTDIR}${PREFIX}/etc/rc.d/ravensign

manpage-footers:
	@(cd ${.CURDIR}/manpages && perl fix-xrefs rvn*.[58]) && echo "done"

kyua-test:
	@rm -rf ${.CURDIR}/tests/html
	@(cd ${.CURDIR}/tests && /bin/sh ./exec_test.sh) ||:

# future installation perhaps
# etc/bash_completion.d/_rvn.bash
# etc/periodic/daily/490.status-rvn-changes
# etc/periodic/security/410.rvn-audit
# etc/periodic/security/460.rvn-checksum
# etc/periodic/weekly/400.status-rvn
# share/zsh/site-functions/_rvn

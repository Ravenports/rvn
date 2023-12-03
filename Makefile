all:
	/bin/sh extra/set_system.sh "${PREFIX}"
	gprbuild -p -P rvn
	gprbuild -p -P src/rvn-format/programs/rvnprogs

install:
	${BSD_INSTALL_PROGRAM} ${WRKSRC}/obj/rvn ${DESTDIR}${PREFIX}/sbin/
	${BSD_INSTALL_PROGRAM} ${WRKSRC}/src/rvn-format/programs/obj/xrvn \
		${DESTDIR}${PREFIX}/bin/
	${BSD_INSTALL_MAN} ${WRKSRC}/manpages/*.5 ${DESTDIR}${PREFIX}/share/man/man5/
	${BSD_INSTALL_MAN} ${WRKSRC}/manpages/*.8 ${DESTDIR}${PREFIX}/share/man/man8/
	${BSD_INSTALL_DATA} ${WRKSRC}/extra/rvn.conf.sample ${DESTDIR}${PREFIX}/etc/
	mkdir -p ${STAGEDIR}${PREFIX}/etc/rvn/repos

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

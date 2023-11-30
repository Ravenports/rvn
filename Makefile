all:
	/bin/sh extra/set_system.sh "${PREFIX}"
	gprbuild -p -P rvn
	gprbuild -p -P src/rvn-format/programs/rvnprogs

install:
	${BSD_INSTALL_PROGRAM} obj/rvn ${DESTDIR}${PREFIX}/sbin/
	${BSD_INSTALL_PROGRAM} src/rvn-format/programs/rvnprogs/obj/xrvn \
		${DESTDIR}${PREFIX}/bin/
	${BSD_INSTALL_MAN} manpages/*.5 ${STAGEDIR}${PREFIX}/share/man/man5/
	${BSD_INSTALL_MAN} manpages/*.8 ${STAGEDIR}${PREFIX}/share/man/man8/
	${BSD_INSTALL_DATA} extra/rvn.conf.sample ${DESTDIR}${PREFIX}/etc/
	mkdir -p ${STAGEDIR}${PREFIX}/etc/rvn/repos

# future installation perhaps
# etc/bash_completion.d/_rvn.bash
# etc/periodic/daily/490.status-rvn-changes
# etc/periodic/security/410.rvn-audit
# etc/periodic/security/460.rvn-checksum
# etc/periodic/weekly/400.status-rvn
# share/zsh/site-functions/_rvn

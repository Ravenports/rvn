/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#if !defined _WIN32

#include <sys/ioctl.h>

#if defined(__sun__)
#include <unistd.h>
#include <sys/termios.h>
#endif

unsigned short
get_columns (void) {
	struct winsize w;
	ioctl(0, TIOCGWINSZ, &w);
	return w.ws_col;
}

#endif

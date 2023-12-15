/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#if !defined _WIN32

#include <sys/stat.h>

int port_lstatat (int fd, const char *path, struct stat *sb)
{
  return fstatat(fd, path, sb, AT_SYMLINK_NOFOLLOW);
}

#endif

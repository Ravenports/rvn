/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#if !defined _WIN32

#include <unistd.h>
#include <sys/stat.h>
#include <sys/fcntl.h>

int port_lstatat (int fd, const char *path, struct stat *sb)
{
  return fstatat(fd, path, sb, AT_SYMLINK_NOFOLLOW);
}

int
faccessat_readable (int fd, const char *path)
{
  return faccessat (fd, path, R_OK, 0);
}

int
faccessat_writable (int fd, const char *path)
{
  return faccessat (fd, path, W_OK, 0);
}

int
faccessat_file_exists (int fd, const char *path)
{
  return faccessat (fd, path, F_OK, 0);
}


#endif

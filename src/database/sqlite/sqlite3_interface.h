/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#ifndef _WIN32
#include <sys/stat.h>

int
dbdir_open(const char *path, int flags, int mode);

int
dbdir_access(const char *path, int mode);

int
dbdir_stat(const char * path, struct stat * sb);

int
dbdir_lstat(const char * path, struct stat * sb);

int
dbdir_unlink(const char *path);

int
dbdir_mkdir(const char *path, mode_t mode);

void
rdb_syscall_overload (void);

#endif

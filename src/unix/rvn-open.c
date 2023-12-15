/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#if !defined _WIN32

#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK 0
#endif

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif

#ifndef O_DIRECTORY
#define O_DIRECTORY 0
#endif


int
rvn_try_open
  (const char *path,
   int flag_rdonly,
   int flag_wronly,
   int flag_nonblock,
   int flag_directory,
   int flag_cloexec,
   int flag_creat,
   int flag_trunc)
{
  int flags = 0;

  if (flag_rdonly && !flag_wronly)
    flags |= O_RDONLY;

  if (flag_wronly && !flag_rdonly)
    flags |= O_WRONLY;

  if (flag_rdonly && flag_wronly)
    flags |= O_RDWR;

  if (flag_nonblock)
    flags |= O_NONBLOCK;

  if (flag_cloexec)
    flags |= O_CLOEXEC;

  if (flag_directory)
    flags |= O_DIRECTORY;

  if (flag_creat)
    flags |= O_CREAT;

  if (flag_trunc)
    flags |= O_TRUNC;

  if (flag_creat)
    return (open (path, flags, 0644));
  else
    return (open (path, flags));
}

int
rvn_try_openat
  (int dirfd,
   const char *path,
   int flag_rdonly,
   int flag_wronly,
   int flag_nonblock,
   int flag_directory,
   int flag_cloexec,
   int flag_creat,
   int flag_trunc)
{
  int flags = 0;

  if (flag_rdonly && !flag_wronly)
    flags |= O_RDONLY;

  if (flag_wronly && !flag_rdonly)
    flags |= O_WRONLY;

  if (flag_rdonly && flag_wronly)
    flags |= O_RDWR;

  if (flag_nonblock)
    flags |= O_NONBLOCK;

  if (flag_cloexec)
    flags |= O_CLOEXEC;

  if (flag_directory)
    flags |= O_DIRECTORY;

  if (flag_creat)
    flags |= O_CREAT;

  if (flag_trunc)
    flags |= O_TRUNC;

  if (flag_creat)
    return (openat (dirfd, path, flags, 0644));
  else
    return (openat (dirfd, path, flags));
}

#endif /* !defined _WIN32 */

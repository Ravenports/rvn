/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#if defined _WIN32
/* dummy functions to allow building on windows */


int detect_IPC (const char *path) { return 1; }
int connect_socket (const char *path, int newfd) { return (5); }
int socket_pair_stream (int *sv) {return (6); }
void set_nonblocking(int fd);
void set_blocking(int fd);


#else  /* defined _WIN32 */

#include <sys/socket.h>
#include <sys/un.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>


int
detect_IPC (const char *path)
{
  struct stat st;

  if (stat(path, &st) != 0) {
    return (-1);
  }

  if (S_ISFIFO(st.st_mode)) {
    return (1);
  }

  if (S_ISSOCK(st.st_mode)) {
    return (2);
  }

  return (3);
}


int
connect_socket (const char *path, int newfd)
{
  struct sockaddr_un sock;

  memset(&sock, 0, sizeof(struct sockaddr_un));
  sock.sun_family = AF_UNIX;

  if (strlen (path) >= sizeof(sock.sun_path)) {
    newfd = -1;
    return (3);
  }
  strncpy (sock.sun_path, path, sizeof(sock.sun_path));

  newfd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (newfd == -1) {
    return (2);
  }

  if (connect(newfd, (struct sockaddr *)&sock, SUN_LEN(&sock)) == -1) {
    close (newfd);
    newfd = -1;
    return (4);
  }

  return (1);
}

int
socket_pair_stream (int	*sv) {
  return socketpair(AF_UNIX, SOCK_STREAM, 0, sv);
}

void
set_nonblocking(int fd)
{
  int flags;

  if ((flags = fcntl(fd, F_GETFL)) == -1) {
    return;
  }
  if (!(flags & O_NONBLOCK)) {
    flags |= O_NONBLOCK;
    fcntl(fd, F_SETFL, flags);
  }
}

void
set_blocking(int fd)
{
  int flags;

  if ((flags = fcntl(fd, F_GETFL)) == -1) {
    return;
  }
  if (flags & O_NONBLOCK) {
    flags &= ~O_NONBLOCK;
    fcntl(fd, F_SETFL, flags);
  }
}

#endif  /* defined _WIN32 */
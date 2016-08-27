#include <poll.h>
#include <fcntl.h>
#include <arpa/inet.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <err.h>

int
read_exact(int fd, void *buf, int len)
{
	int n;
	char *p = buf, *endp = p + len;

	for (; p < endp; p += n)
		if ((n = read(fd, p, endp - p)) <= 0)
			return (n);
	return (len);
}

int
write_exact(int fd, void *buf, int len)
{
	int n;
	char *p = buf, *endp = p + len;

	for (; p < endp; p += n)
		if ((n = write(fd, p, endp - p)) <= 0)
			return (n);
	return (len);
}

int
tbh_read(int fd, void *buf, int len)
{
	int hlen, nlen;

	if (len < 2 || read(fd, &nlen, 2) != 2)
		return (-1);
	if ((hlen = ntohs(nlen)) > len)
		return (-1);
	return (read(fd, buf, hlen));
}

int
tbh_write(int fd, void *buf, int len)
{
	int nlen = htons(len);

	if (write(fd, &nlen, 2) != 2)
		return (-1);
	return write(fd, buf, len);
}

#define nelems(X) (sizeof(X)/sizeof((X)[0]))
int dev = -1;

void
handler(int sig)
{
	int save_errno = errno;

/*	warnx("closing down (sig %d)", sig);*/
	if (dev != -1)
		close(dev);
	signal(sig, SIG_DFL);
	errno = save_errno;
	kill(getpid(), sig);
}

int
main(int argc, char *argv[])
{
	char buf[256];
	struct pollfd fds[2];
	int n, flags;
	char *file = "/dev/cu.usbmodemv2_r1";

	if (argc > 1)
		file = argv[1];
	for (n = 1; n < 32; n++) {
		if (n == SIGKILL || n == SIGSTOP)
			continue;
		signal(n, handler);
	}
	flags = O_RDWR | O_NONBLOCK | O_NOCTTY;
       	if ((dev = open(file, flags)) < 0)
		err(1, "open");

	read(dev, buf, sizeof(buf)); /* clear crap on the line */

	fds[0].events = POLLIN;
	fds[0].fd = 0;
	fds[1].events = POLLIN;
	fds[1].fd = dev;
	while (1) {
		switch (poll(fds, nelems(fds), -1)) {
		case -1:
			warn("poll");
			continue;
		case 0:
			warnx("timeout");
			continue;
		}
		if (fds[0].revents & POLLIN) {
			if ((n = tbh_read(0, buf, sizeof(buf))) < 1) {
				warn("read(%d)", 0);
				break;
			}
			if (write_exact(dev, buf, n) != n) {
				warn("write(%d)", dev);
				break;
			}
		}
		if (fds[1].revents & POLLIN) {
			if ((n = read(dev, buf, sizeof(buf))) < 1) {
				warn("read(%d)", dev);
				break;
			}
			if (tbh_write(1, buf, n) != n) {
				warn("write(%d)", 1);
				break;
			}
		}
	}
	close(dev);
	exit(1);
	return (0);
}

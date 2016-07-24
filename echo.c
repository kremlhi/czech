#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include "p8.h"

#define nelems(X) (sizeof(X)/sizeof((X)[0]))

void
pretty(void *buf, int len)
{
    char *p = buf;
    char *endp = p + len;

    for (; p < endp; p++)
        fprintf(stderr, "\\x%02hhx", *p);
    fprintf(stderr, "\n");
}

void
p8write(int fd, void *buf, int len)
{
    unsigned short lbuf = ntohs(len + 2);
    struct iovec iov[] = {
        {NULL, 2},
        {BEG, 1},
        {NULL, 0},
        {END, 1}
    };

    iov[0].iov_base = &lbuf;
    iov[2].iov_base = buf;
    iov[2].iov_len = len;
    (void)writev(fd, iov, nelems(iov));
}

void
reply(void *buf, int len)
{
    char obuf[256], *p = buf;
    char *endp = p + len;
    int flags = 0;
    
    for (; p < endp; ) {
        if (*p != BEG[0]) {
            fprintf(stderr, "unexpected byte 0x%02hhx\n", *p);
            pretty(p, endp - p);
            return;
        }
        switch (p[1]) {
        case P8_CMD_FIRMWARE_VSN:
            memcpy(obuf, "\x00\x00\x03", 4);
            obuf[0] = p[1];
            p8write(1, obuf, 3);
            break;
        case P8_CMD_GET_BUILDDATE:
            obuf[0] = p[1];
            memcpy(obuf+1, "\x57\x92\xc6\xb6", 4);
            p8write(1, obuf, 5);
            break;
        case P8_CMD_GET_ADAPTER_TYPE:
            obuf[0] = p[1];
            obuf[1] = 0x01;
            p8write(1, obuf, 2);
            break;
        case P8_CMD_GET_AUTO_ENABLED:
            obuf[0] = p[1];
            obuf[1] = 0x01;
            p8write(1, obuf, 2);
            break;
        case P8_CMD_GET_PADDR:
            obuf[0] = p[1];
            memcpy(obuf+1, "\x20\x00", 2);
            p8write(1, obuf, 3);
            break;
        case P8_CMD_TX_EOM:
            obuf[0] = 0x08;
            obuf[1] = p[1];
            p8write(1, obuf, 2);
            obuf[0] = (flags ? P8_IND_TX_ACK : P8_IND_TX_NACK);
            p8write(1, obuf, 1);
            flags = 0;
            break;
        case P8_CMD_TX_SET_IDLE:
        case P8_CMD_TX_SET_ACK_P:
        case P8_CMD_TX_SET_TIMEOUT:
            flags++;
        default:
            obuf[0] = 0x08;
            obuf[1] = p[1];
            p8write(1, obuf, 2);
            break;
        }
        for (; p < endp && *p != *END; p++)
            ;
        p++;
    }
}

int
main()
{
    char buf[256];
    int n, len;
    unsigned short nlen;

    while ((n = read(0, buf, sizeof(buf))) > 0) {
        nlen = *(unsigned short *)buf;
        len = ntohs(nlen);
        if (len != n - 2) {
            fprintf(stderr, "wrong len %d != %d\n", len, n-2);
            continue;
        }
        reply(buf + 2, len);
    }
    return 0;
}

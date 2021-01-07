#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>

int main(int argc, char *argv[])
{
	struct sockaddr_in sadd, srl;
	int sfd;

	sfd = socket(AF_INET, SOCK_STREAM, 0);

	sadd.sin_family = AF_INET;
	sadd.sin_port = htons(0);
	sadd.sin_addr.s_addr = htonl(INADDR_ANY);

	if  (bind(sfd, (struct sockaddr *) &sadd, sizeof(sadd)) != 0) {
		perror("bind failed!");
		exit(1);
	}
	socklen_t addrsize = sizeof(srl);
	getsockname(sfd, (struct sockaddr *) &srl, &addrsize);
	printf("%d", srl.sin_port);
	return close(sfd);
}

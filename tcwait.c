#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <string.h>
#include <sys/uio.h>
#include <unistd.h>

#include <pthread.h>

/* the TCP port that is used for this example */
#define TCP_PORT   8090

/* function prototypes and global variables */
void *do_chld(void *);

main()
{
   size_t sockfd, newsockfd, clilen;
   struct sockaddr_in cli_addr, serv_addr;
   
   if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    fprintf(stderr,"server: can't open stream socket\n"), exit(0);
   
   memset((char *) &serv_addr, 0, sizeof(serv_addr));
   serv_addr.sin_family = AF_INET;
   serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
   serv_addr.sin_port = htons(TCP_PORT);
   
   if(bind(sockfd, (struct sockaddr *)&serv_addr,sizeof(serv_addr)) < 0)
    fprintf(stderr,"server: can't bind local address\n"), exit(0);
   
   listen(sockfd, 5);
   
   for(;;){
      clilen = sizeof(cli_addr);
      newsockfd = accept(sockfd, (struct sockaddr *) &cli_addr, &clilen);
      if(newsockfd < 0)
        fprintf(stderr,"server: accept error\n"), exit(0);
        
      do_chld((void *)newsockfd);
   }     
   return(0);
}

void *do_chld(void *arg)
{
   int     mysocfd = (int) arg;
   char    data[100];
   int     i;

   memset(data, 0, 99);
   /* read from the given socket */
   read(mysocfd, data, 40);

   printf("Lu: %s\n", data);
   write(mysocfd, "OK", 3);

   /* close the socket and exit this thread */
   close(mysocfd);
}
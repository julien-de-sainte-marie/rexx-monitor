#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <signal.h>


const char clogfile[]     = "log/wsock.log";
const char clogfilelost[] = "log/wsock_lost.log";
const char cOnWsock[]    = "TourDeControle.active";

void error(char *msg)
{
    /* perror(msg); */
    exit(0);
}

void writelog(char *filename, char *msg)
{
   FILE *h;
   int i;
   char *ptr, *ptr2;
   
   ptr   = ptr2 = msg;
   ptr2 += strlen(msg) - 4;
   
   i=0;
   while(i<2)
   {
      if(*ptr == ' ') i++;
      if(ptr >= ptr2) break;
      ptr++;
   }
      
   if(strncmp(ptr, "CPU ", 4) != 0 && strncmp(ptr, "LOK ", 4) != 0 && strncmp(ptr, "WFD ", 4) != 0)
   {
      h = fopen(filename, "a+");
      if (h != NULL)
      {
         fwrite(msg,strlen(msg),1,h);
         fclose(h);
      }
   }
}

int isTCon(void)
{
   FILE *h;
   int   cr;
   
   h = fopen(cOnWsock,"r");
   if (h != NULL)
   {
      cr = 1;
      fclose(h);
   }
   else
      cr = 0;
      
   return(cr);
}      

void dummy(int s)
{
/* 
** Don't ignore the interrupt, but just do nothing, maybe you will have to 
** set the handler again
*/
}

int main(int argc, char *argv[])
{
   int sockfd, portno, n, loop;
   struct sockaddr_in serv_addr;
   struct hostent *server;
   char notactived[] = "0";
   char logfile[255];
   char *pathmon = getenv("MONITOR");
   char *pathenv = getenv("HOME");
   char *actived = getenv("WSOCKIP_OnOff");
   char *path;
   char buffer[512], buffer2[512];
   
   if(!pathmon && !pathenv)
   {
      fprintf(stderr,"Either HOME or MONITOR must be declared\n");
      exit(0);
   }
   
   if(!pathmon)
      path = pathenv;
   else 
      path = pathmon;
      
   sprintf(logfile, "%s/%s", path, clogfile);
   
   if (argc < 4) 
   {
      fprintf(stderr,"usage %s hostname port message\n", argv[0]);
      exit(0);
   }
   
   bzero(buffer,sizeof(buffer));
   sprintf(buffer,"%s %s %s\n", argv[1], argv[2], argv[3]);
   
   bzero(buffer2,sizeof(buffer2));
   sprintf(buffer2,"%s\r\n", argv[3]);
   
   writelog(logfile,buffer);
   
   if(!actived) actived = notactived;
   if(actived[0] == '1' || isTCon())
   {
      portno = atoi(argv[2]);
      sockfd = socket(AF_INET, SOCK_STREAM, 0);
      if (sockfd < 0) 
      {
         error("ERROR opening socket");
      }
      
      server = gethostbyname(argv[1]);
      if (server == NULL) 
      {
         fprintf(stderr,"ERROR, no such host\n");
         exit(0);
      }
      
      bzero((char *) &serv_addr, sizeof(serv_addr));
      serv_addr.sin_family = AF_INET;
      bcopy((char *)server->h_addr, (char *)&serv_addr.sin_addr.s_addr, server->h_length);
      serv_addr.sin_port = htons(portno);
   
      signal(SIGALRM, dummy);
      alarm(2);
   
      if (connect(sockfd,&serv_addr,sizeof(serv_addr)) < 0) 
      {
         error("ERROR connecting");
      }
         
      n = write(sockfd,buffer2,strlen(buffer2));
      if (n < 0) 
      {
         error("ERROR_writing_to_socket");
      }
   }
   return 0;
}


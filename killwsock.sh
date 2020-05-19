#!/usr/bin/ksh
if [ "$1" = "" ] || [ "$2" = "" ];
then
   echo "Syntaxe : killwsock.sh propriétaire process"
   echo "Calling killwsock.sh monitor wsock"
   . ./killwsock.sh monitor wsock
else
   cpt=`ps -fu $1 | grep -c $2`
   until [ $cpt -eq 1 ];
   do
      for li in `ps -fu $1 | grep $2 | awk '{print $2}'`
      do 
         kill $li
      done
      sleep 5
      cpt=`ps -fu $1 | grep -c $2`
   done
fi
return
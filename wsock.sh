#!/bin/sh
function verend
{
  if [ "$TOTO" != "HELO" ]
  then 
    echo $pArg >> /home/monitor/log/wsock_lost.log
  fi
}
pArg="$1 $2 $3"
echo $pArg >> /home/monitor/log/wsock.log
exec 2>/dev/null
trap 'verend' 0
exec 3<> /dev/tcp/$1/$2
if [ "$?" = "0" ]
then
   print -u3 -f "%s\r\n" "$3"
   TOTO=`cat <&3 | awk '{print $1}'`
   TOTO=`echo $TOTO | awk '{print $1}'`
fi


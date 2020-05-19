#!/usr/bin/ksh
m=`ps -fu monitor | grep -c cmd/monitor.rexx`
if [ $m = 2 ]
then
  wsock $WSOCKIP "WFD MONITOR;Internal;UP"
fi
for i in `ps -fu monitor | grep rexx | awk '{ print $10}'`
do
  q $i EXEC call throwlog tllpMsg
done
m=`ps -fu guichet | grep -c waitfiled`
if [ $m = 1 ]
then
  wsock $WSOCKIP "WFD Gestionnaire;Master;UP"
fi

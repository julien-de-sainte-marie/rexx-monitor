/*************************************************************************************
                           PROLOGUE des outils de MONITOR
                           ==============================
Ce produit est un graticiel (houla...)
Ecrit et maintenu par Julien de Sainte Marie
mailto:julien.desaintemarie@gmail.com

****************************************************************************************/
Parse Arg ProcessName" "Reste
Trace Off

ProcessName = Strip( ProcessName )
if ProcessName = "" then do
   Parse Source A1 A2 FullProcessName Reste

   psProcessName = FileSpec( "name", FullProcessName )
   Parse var psProcessName ProcessName"."Reste
End

ProcessName = Translate(Strip( ProcessName ))

share                   = "ENVIRONMENT"
SysVars.FicName         = ProcessName
SysVars.PID             = getpid()
SysVars.SysVersion.0    = 3
SysVars.SysVersion.1    = "MOM$ version 2.1"
SysVars.SysVersion.2    = ""
SysVars.SysVersion.3    = ""
SysVars.SysPath         = VALUE("PWD",,share)
SysVars.SysTimerEnabled = 0
SysVars.SysInSysout     = 0
SysVars.SysTimeReset    = Time('R')
SysVars.SysLockPath     = "./lock/"
SysVars.SysQueue        = "PIPE"
SysVars.SysMonName      = "MONITOR"
SysVars.SysLTimeOut     = "!TIMEOUT!"
SysVars.SysLHalt        = "!HALT!"
SysVars.SysLIdle        = "!IDLE!"
SysVars.SysLInit        = "!INIT!"
SysVars.SysLBusy        = "!BUSY!"                 /* New */
SysVars.SysLDeadLock    = "!DEADLOCK!"             /* New */
SysVars.SysLCommit      = "COMMIT"                 /* New */
SysVars.SysLEnd         = "END"
SysVars.SysWhoAmI       = ""
SysVars.SysStopping     = 0
SysVars.SysTimerEnable  = 0
SysVars.SysElapsedIdle  = 0
SysVars.SysElapsedCmd   = 0
SysVars.SysElapsedAll   = 0
SysVars.SysRacine       = ""
SysVars.fileWSOCK       = "/etc/xchglistener.ini"
SysVars.soSay           = GetProfileString( , "SYSOUT", "SAY", "NO" )
SysVars.SysWriteSysout  = GetProfileString( , "SYSOUT", "TRACE", "NO" )
SysVars.SysSleepDelay   = GetProfileString( , "SYSTEM", "SLEEP_DELAY", "1" )
SysVars.SysSleepexDelay = GetProfileString( , "SYSTEM", "SLEEPEX_DELAY", "0.2" )
SysVars.SysLoopDelay    = GetProfileString( , "SYSTEM", "LOOP_DELAY", "1" )
SysVars.SysLoopBreak    = GetProfileString( , "SYSTEM", "LOOP_BREAK", "1" )
SysVars.SysTSleep       = GetProfileString( , "SYSTEM", "LOOP_SLEEP", "0.5" )
SysVars.SysLoopResend   = GetProfileString( , "SYSTEM", "LOOP_RESEND", "5" )
SysVars.SysLoopForceTO  = GetProfileString( , "SYSTEM", "LOOP_FORCETIMEOUT", "5" )
SysVars.SysLoopWM       = GetProfileString( , "SYSTEM", "LOOP_WAITMESSAGE", "0.5" )
SysVars.SysDetectMon    = Translate(GetProfileString( , "SYSTEM", "DETECT_RUN_LOGON", "YES" )) /* JSM 13.03.2003 Here */
SysVars.SysStopRedoT    = GetProfileString( , "SYSTEM", "LOOP_BEFORE_FORCE", "100" )
SysVars.SysSendMessageQueue = ""
SysVars.SysProcessLockName  = SysVars.SysLockPath""SysVars.SysMonName".LOK"
SysVars.FirstInstance   = 0
SysVars.TID             = "0"
SysVars.PPRIO           = "0"
SysVars.TPRIO           = "0"
SysVars.PTIME           = "0"
SysVars.SysEtatQueue    = "ETAT_CPU"
SysVars.SysPushQueue    = 0
CpuQueue                = "PIPE_CPU"
ResourceLocked          = SysLCommit
AnswerQueue             = ""
CreatedProcess          = 0
ParentQ                 = ""
LockedProcess           = 0
CtrlAttn                = 0
LocalQueue.0            = 0
LocalProcess.0          = 0
TimeToWait              = GetProfileString( , "SYSTEM", "LOOP_TIMETOWAIT", "1" )
NumberProcess           = 0
ProcessInitialized      = 0
ProcessLockName         = SysVars.SysLockPath""ProcessName".LOK"
EndForce                = 0
WaitingIdle             = 0
devnull                 = " > /dev/null 2>&1"
wsockIPP                = Strip(GetProfileString(,"SYSOUT","WSOCKIP",""))

Call getWSOCKIP

if ProcessName = SysVars.SysMonName then
   IAmMonitor = 1
Else
   IAmMonitor = 0

rep            = VALUE("PIP_REP",,share)
drive          = VALUE("PIP_DRV",,share)

if strip(rep)   = "" then rep   = DIRECTORY()
if strip(drive) = "" then drive = ""

curdir         = DIRECTORY()
PipDir         = drive""rep
newdir         = DIRECTORY(PipDir)

tempFirstInstance = 1
if IAmMonitor = 0 then do
   if SysVars.SysDetectMon = "YES" Then /* JSM 13.03.2003 Here */
      if Stream( SysVars.SysProcessLockName, 'c', 'query exists' ) = "" then do
         Say "Le moniteur n'est pas actif !"
         Exit
      End
End
Else do
    SysVars.FirstInstance = 1
    nbps = 0
    Address SYSTEM 'ps -eaf | grep "'psProcessName'" | grep -v grep | grep -v 'SysVars.PID'> /tmp/monitor.ps.list'
    Do While Lines('/tmp/monitor.ps.list') > 0
        a=LineIn('/tmp/monitor.ps.list')
         if pos(psProcessName,a) > 0 then do
                nbps = nbps + 1
         end
    end
    If nbps > 0 Then
        tempFirstInstance = 0
   Address SYSTEM 'rm lock/* 1>/dev/null 2>&1' /* */
End

if Stream( ProcessLockName, 'c', 'query exists' ) = "" then do
   SysVars.FirstInstance  = tempFirstInstance
   Rc                     = Stream( ProcessLockName, 'c', 'open write' )
   Rc                     = Stream( ProcessLockName, 'c', 'close' )
End

if IAmMonitor = 1 then Call MonitorPrologue

Signal On Syntax Name StandardSyntax0
Call   On Halt   Name CallNoHalt

IsCreating = 1
if CreateProcess( ProcessName ) = 0 then do
   Say "Impossible de creer le process !"
   Return
End
IsCreating = 0

Signal On Syntax Name StandardSyntax

Call ThrowLog "In loop"

if IAmMonitor = 0 then
   Call SysMainLoop
Else
   Call MonSysMainLoop

Call ThrowLog "Out loop"
/* -----------------------------------------------------------------------------------
   StandardSyntax
-------------------------------------------------------------------------------------- */
StandardSyntax:
Trace Off
Signal Off Syntax

Call ThrowLog "FREEING"

tm = FreeProcess()
do I = 1 to LocalQueue.0
   if LocalQueue.I \= "" then
      Call RxQueue 'Delete', LocalQueue.I"@127.0.0.1"
End

if SysVars.FirstInstance  = 1 then
   Address SYSTEM 'rm 'ProcessLockName' 1>/dev/null 2>&1'

Call ThrowLog "DOWN"

If IAmMonitor = 1 then Do
   Call MonitorStopQueueSys
End

exit
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   StandardSyntax0
-------------------------------------------------------------------------------------- */
StandardSyntax0:
Say SysVars.SysMonName" n'est pas actif !!!"
exit
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   StandardHalt
-------------------------------------------------------------------------------------- */
StandardHalt:
Trace Off

Call ProcStandardHalt

Tm = SetQueue( FormatQueue( SysVars.SysWhoAmI ))
Call PushData "SYS_STOP~"SysVars.SysLEnd
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   CallNoHalt
-------------------------------------------------------------------------------------- */
CallNoHalt:
Call On Halt Name CallNoHalt
Call ProcStandardHalt
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   ProcStandardHalt
-------------------------------------------------------------------------------------- */
ProcStandardHalt:
if LockedProcess = 0 then CtrlAttn = 1
Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SayHelp
-------------------------------------------------------------------------------------- */
SayHelp:
Trace Off

Do I = 1 to Help.0
   Say Help.I
End
Return
/* =================================================================================== */
/*************************************************************************************
                           API des outils de MONITOR
                           =========================
****************************************************************************************/

/* -----------------------------------------------------------------------------------
   AllocQueue
-------------------------------------------------------------------------------------- */
AllocQueue:
Trace off
Arg pArg1
Call SysOut "AllocQueue: "pArg1

oldQ = rxQueue('get')

if pArg1 = '' then do

   aqNom          = RxQueue( "create" )
   Parse Var aqNom aqNom"@"ResteX
   I              = LocalQueue.0
   I              = I + 1
   LocalQueue.I   = aqNom
   LocalQueue.0   = I

End
Else do

   aqNom = RxQueue( 'create', pArg1"@127.0.0.1" )
   Parse Var aqNom naqNom"@"ResteX

   if naqNom = pArg1 then do

      aqNom          = naqNom
      I              = LocalQueue.0
      I              = I + 1
      LocalQueue.I   = aqNom
      LocalQueue.0   = I

   End
   Else do

      Call RxQueue 'delete', aqNom
      aqNom = ""

   End

End
tm = rxQueue('set', oldQ)

Call SysOut "AllocQueue returns: "aqNom
return aqNom
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FreeQueue
-------------------------------------------------------------------------------------- */
FreeQueue:
Trace Off
Arg fqArg1
Call SysOut "FreeQueue: "fqArg1

fqOk = 9
do I = 1 to LocalQueue.0
   if LocalQueue.I = fqArg1 then do
      LocalQueue.I = ""
      fqOk         = RxQueue( "delete", fqArg1"@127.0.0.1" )
      Leave
   End
End
Call SysOut "FreeQueue returns: "fqOk
return fqOk
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FormatQueue
-------------------------------------------------------------------------------------- */
FormatQueue: PROCEDURE EXPOSE LockedProcess CtrlAttn
Trace Off
Arg arg1, arg2

if arg1 \= "" & Pos( '_', arg1 ) > 0 then
   Parse var arg1 P"_"Q
Else Do

   Q = ""
   P = arg1

End

if arg2 \= "" then
   R = P
Else
   R = Q

return R
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   SetQueue
-------------------------------------------------------------------------------------- */
SetQueue: PROCEDURE EXPOSE LockedProcess CtrlAttn SysVars.
Trace off
Arg Arg1
Signal On Syntax Name SetQueueSyntax
Call SysOut "SetQueue: "Arg1

if Arg1 = "" then Signal SetQueueSyntax

Nom = RxQueue( "set", arg1"@127.0.0.1"  )
Parse Var Nom Nom"@"ResteX

Signal SetQueueRetour

SetQueueSyntax:
Nom = ""

SetQueueRetour:
Signal On Syntax Name StandardSyntax
Call SysOut "SetQueue returns: "Nom
return Nom
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   Get Queue name
-------------------------------------------------------------------------------------- */
GetQueueName:
Trace Off

qName = rxQueue('get')
Parse Var qName qName"@"ResteX

return qName
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FormatMessage
-------------------------------------------------------------------------------------- */
FormatMessage: PROCEDURE EXPOSE LockedProcess CtrlAttn
Trace Off
Parse Arg msgFrom, msgTo, msgId, msgData

if msgId = "" then msgId = "USR_MSG"
if msgTo \= "" & msgId \= "" then do

   Dj    = GetDate()
   Hj    = GetTime()
   MSG   = msgId'~'msgFrom'~'msgTo'~'Dj'~'Hj'#'msgData

End
Else MSG = 0

return MSG
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   GetDate
-------------------------------------------------------------------------------------- */
GetDate: PROCEDURE EXPOSE LockedProcess CtrlAttn
Trace Off
Dd = Date('S')
Dj = Left( Dd, 4 )'/'Substr( Dd, 5, 2 )'/'Right( Dd, 2 )
return Dj
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   GetTime
-------------------------------------------------------------------------------------- */
GetTime: PROCEDURE EXPOSE LockedProcess CtrlAttn
Trace Off
Hj = Left( Time('L'), 11 )
return Hj
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   PostMessage
-------------------------------------------------------------------------------------- */
PostMessage: PROCEDURE EXPOSE SysVars. LockedProcess CtrlAttn
Trace Off
Parse Arg msgTo, msgId, msgData

Signal On Syntax Name PostMessageError
Call SysOut "PostMessage: "msgTo","msgId","msgData

if msgTo = '' then msgTo = SysVars.SysMonName

MSG = FormatMessage( SysVars.SysWhoAmI, msgTo, msgId, msgData )
if MSG = 0 then
   Ok = 0
Else do

   If msgId \= "SYS_ETAT" Then
      Avant = SetQueue( SysVars.SysQueue )
   Else
      Avant = SetQueue( SysVars.SysEtatQueue )
   
   if Avant \= "" then do

      If Translate(msgData) \= SysVars.SysLEnd Then 
         Call QueueData MSG
      Else
         Call PushData MSG
      
      Apres = SetQueue( Avant )
      Ok    = 1

   End
   Else Ok = 0

End
Signal PostMessageRetour

PostMessageError:
Apres = SetQueue( Avant )
Ok    = 0

PostMessageRetour:
Signal On Syntax Name StandardSyntax
Call SysOut "PostMessage returns: "Ok
return Ok
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   PostMessageTop
-------------------------------------------------------------------------------------- */
PostMessageTop: PROCEDURE EXPOSE SysVars. LockedProcess CtrlAttn
Trace Off
Parse Arg msgTo, msgId, msgData

Signal On Syntax Name PostMessageError
Call SysOut "PostMessage: "msgTo","msgId","msgData

if msgTo = '' then msgTo = SysVars.SysMonName

MSG = FormatMessage( SysVars.SysWhoAmI, msgTo, msgId, msgData )
if MSG = 0 then
   Ok = 0
Else do

   If msgId \= "SYS_ETAT" Then
      Avant = SetQueue( SysVars.SysQueue )
   Else
      Avant = SetQueue( SysVars.SysEtatQueue )
   
   if Avant \= "" then do

      Call PushData MSG
      
      Apres = SetQueue( Avant )
      Ok    = 1

   End
   Else Ok = 0

End
Signal PostMessageRetour

PostMessageError:
Apres = SetQueue( Avant )
Ok    = 0

PostMessageRetour:
Signal On Syntax Name StandardSyntax
Call SysOut "PostMessage returns: "Ok
return Ok
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   PushData
-------------------------------------------------------------------------------------- */
PushData:
Trace Off
Parse Arg aData

Call SysOut "PushData("GetQueueName()"): "aData

Push aData

Call SysOut "PushData ends"
return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   PullData
-------------------------------------------------------------------------------------- */
PullData:
Trace off
Parse Arg bWait

Call SysOut "PullData("GetQueueName()"): "bWait

if bWait = "" | DataType( bWait ) = CHAR then bWait = 0
pdWait = bWait

Do Forever
   If Queued() > 0 Then Do
      Parse Pull aData
      Leave
   End
   If pdWait = 0 Then Leave
   Call monSleep pdWait
   pdWait = 0
End

Call SysOut "PullData returns: "aData
return aData
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   QueueData
-------------------------------------------------------------------------------------- */
QueueData:
Trace Off
Parse Arg aData

Call SysOut "QueueData("GetQueueName()"): "aData

QUEUE aData

Call SysOut "QueueData ends"
return
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   CreateProcess
-------------------------------------------------------------------------------------- */
CreateProcess:
Trace Off
Arg cpProcess

if cpProcess \= "" then do

   CreatedProcess    = 1
   SysVars.SysWhoAmI = cpProcess'_'AllocQueue()
   ParentQ           = SetQueue( FormatQueue( SysVars.SysWhoAmI ))
   Ok                = SendMessage( , "SYS_BEGINPROCESS" )

   if Ok \= "" then Do

      CreatedProcess = 1
      Ok             = 1

   End
   Else Ok = 0

End
Else Ok = 0

if Ok = 0 then CreatedProcess = 0
return Ok
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FreeProcess
-------------------------------------------------------------------------------------- */
FreeProcess:
Trace Off

if CreatedProcess = 1 then
   Ok = PostMessage( , "SYS_ENDPROCESS" )

CreatedProcess = 0
Tm             = SetQueue( ParentQ )
Tm             = FreeQueue( FormatQueue( SysVars.SysWhoAmI ))
Tm             = FreeQueue( SysVars.SysSendMessageQueue )

return Ok
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   Display
-------------------------------------------------------------------------------------- */
Display:
Trace Off
Parse arg pData

if CreatedProcess = 1 then
   Ok = PostMessage( , "SYS_DISPLAY", pData )
Else
   Ok = 0

return Ok
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   WaitMessage
-------------------------------------------------------------------------------------- */
WaitMessage: PROCEDURE EXPOSE SysVars. CreatedProcess IAmMonitor CtrlAttn isCreating WaitingIdle LockedProcess CtrlAttn
Trace Off
Arg pQueue, pTimeOut, NoPost

Call   On Halt   Name CallNoHalt

Call SysOut "WaitMessage: "pQueue","pTimeOut","NoPost

WMqueue        = ""
Ok             = 1
tWaitingIdle   = Time('E')

if NoPost = "" then NoPost = 0
if DataType( NoPost ) = CHAR then NoPost = 1

if CreatedProcess = 1 | isCreating = 1 then do

   if pTimeOut = "" | DataType( pTimeOut ) \= "NUM" | pTimeOut < 0 then pTimeOut = 0

   if pQueue \= '' then do
      if pQueue \= GetQueueName() then do

         WMqueue = SetQueue( pQueue )
         if WMqueue = '' then Ok = 0

      End
      Else pQueue = ''
   End

   if Ok = 1 & IAmMonitor = 0 then do

      if NoPost = 0 then
         Ok = PostMessage( , "SYS_BEGINLOOP", GetQueueName())

      MSG = WaitMessageLoop(pTimeOut)

      if NoPost = 0 then
         Ok = PostMessage( , "SYS_ENDLOOP" )

   End
   else
   if Ok = 1 then do

      MSG = WaitMessageLoop(pTimeOut)

   End

   if WMqueue \= '' then do

      tm = SetQueue( WMQueue )

   End

End
Else
   Ok = 0

if Ok       = 0         then MSG       = "!ERROR!"
if MSG      = '!STOP!'  then CtrlAttn  = 1
if CtrlAttn = 1         then MSG       = SysVars.SysLHalt

if Left( MSG, 8 ) = "SYS_ACK~" then
   Parse Var MSG dummy"~"MSG

tWaitingIdle = Time('E') - tWaitingIdle
WaitingIdle  = WaitingIdle + tWaitingIdle

Call SysOut "WaitMessage returns: "MSG
Return MSG
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   WaitMessageLoop
-------------------------------------------------------------------------------------- */
WaitMessageLoop:
Trace Off
Arg wmlpTimeOut

Call   On Halt   Name CallNoHalt

if wmlpTimeOut = "" | DataType( wmlpTimeOut ) \= "NUM" | wmlpTimeOut < 0 then wmlpTimeOut = 0
Call SysOut "WaitMessageLoop: "wmlpTimeOut

wmlTimeStart = Time('E')
do Forever

   if CtrlAttn = 1 then Leave

   if Queued() > 0 then do
      smRC = PullData()
      Leave
   End

   Call monSleep SysVars.SysTSleep
   if wmlpTimeOut > 0 then do

      wmlTimeElapsed = Time('E') - wmlTimeStart
      smRC        = SysVars.SysLTimeOut
      if wmlTimeElapsed >= wmlpTimeOut then Leave

   End

End

if CtrlAttn = 1 then smRC = SysVars.SysLHalt
Call SysOut "WaitMessageLoop returns: "smRC
return smRC
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   SendMessage
-------------------------------------------------------------------------------------- */
SendMessage:
Trace Off
Parse Arg msgTo, msgId, msgData

Call SysOut "SendMessage: "msgTo","msgId","msgData

if SysVars.SysSendMessageQueue = "" then
   SysVars.SysSendMessageQueue = AllocQueue()

smRc = PostMessage( msgTo, msgId':'SysVars.SysSendMessageQueue, msgData )

if smRc > 0 then do
   If IAmMonitor = 0 then
      smRc = WaitMessage( SysVars.SysSendMessageQueue, 0, 0 )
   Else
      smRc = "OK"
End
Else smRc = ""

Call SysOut "SendMessage returns: "smRC
return smRC
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   ProcStartTimer
-------------------------------------------------------------------------------------- */
ProcStartTimer:
Trace Off
Arg Delai

Call SysOut "StartTimer: "Delai

if DataType( Delai, "N" ) \= 1 then Delai = 0
else
if Delai < 0 then Delai = -Delai

Ok = PostMessage( , "SYS_BEGINWAIT", Delai )

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   ProcStopTimer
-------------------------------------------------------------------------------------- */
ProcStopTimer:
Trace Off

Call SysOut "StopTimer: "

Ok = PostMessage( , "SYS_ENDWAIT", 0 )

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   GetProfileString
-------------------------------------------------------------------------------------- */
GetProfileString: PROCEDURE EXPOSE SysVars. LockedProcess CtrlAttn
Trace Off

Parse Arg gpsFile, gpsSection, gpsKey, gpsDefault

gpsSection = Translate(Strip(gpsSection))
gpsKey = Translate(Strip(gpsKey))

Call   On Halt   Name CallNoHalt

Call SysOut "GetProfileString: "gpsFile","gpsSection","gpsKey","gpsDefault

if Strip( gpsFile ) = '' then
   gpsFile = 'ini/'SysVars.SysMonName'.ini'
else
   gpsFile = gpsFile

gpsBuffer = gpsDefault

if Strip( gpsSection ) = '' then gpsBuffer = gpsDefault
else
if Strip( gpsKey ) = '' then gpsBuffer = gpsDefault
else do

   If Stream( gpsFile, 'c', 'query exists' ) = "" then return gpsBuffer

   Tm = Stream( gpsFile, 'c', 'open read' )
   if Tm = 'READY:' then do

      gpsFS = 0
      Do while Lines( gpsFile ) > 0

         xgpsLine = LineIn( gpsFile )
         gpsLine  = Translate( Strip( xgpsLine ))

         Select

            When gpsLine = '' then
               Iterate

            When Left( gpsLine, 1 ) =  '#' then
               Iterate

            When Left( gpsLine, 1 ) =  ';' then
               Iterate

            When Left( gpsLine, 4 ) =  'REM ' then
               Iterate

            When Left( gpsLine, 1 ) = '[' then do

               gpsFS = 0
               gpsP  = Pos( ']', gpsLine )
               if  gpsP = 0 then Iterate

               xS = Substr( gpsLine, 2, gpsP - 2 )

               if xS = gpsSection then
                  gpsFS = 1

            End

            When gpsFS = 1 then do

               gpsP  = Pos( '=', gpsLine )
               if gpsP = 0 then Iterate

               xK = Left( gpsLine, gpsP - 1 )
               if xK = gpsKey then do
                  gpsLine = Strip( xgpsLine )
                  gpsBuffer = Right( gpsLine, Length( gpsLine ) - gpsP )
                  Leave

               End

            End

            OtherWise
               nop
         End

      End

      Tm = Stream( gpsFile, 'c', 'close' )

   End
   Else gpsBuffer = gpsDefault

End
Call SysOut "GetProfileString returns: "gpsBuffer
return gpsBuffer
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   WriteProfileString
-------------------------------------------------------------------------------------- */
WriteProfileString: PROCEDURE EXPOSE SysVars. LockedProcess CtrlAttn
Trace Off
Parse Arg gpsFile, gpsSection, gpsKey, gpsDefault

Call   On Halt   Name CallNoHalt

Call SysOut "WriteProfileString: "gpsFile","gpsSection","gpsKey","gpsDefault

if Strip( gpsFile ) = '' then
   gpsFile = 'ini/'SysVars.SysMonName'.ini'
else
   gpsFile = Strip( gpsFile )

tmpFile   = 'ini/wpstring.'FormatQueue( SysVars.SysWhoAmI )
gpsBuffer = 0

if Strip( gpsSection ) = '' then gpsBuffer = 0
else
if Strip( gpsKey ) = '' then gpsBuffer = 0
else do

   If Stream( gpsFile, 'c', 'query exists' ) = "" then Do

      Tm = LineOut( gpsFile, '['gpsSection']' )
      Tm = LineOut( gpsFile, gpsKey'='gpsDefault )
      Tm = Stream( gpsFile, 'c', 'close' )
      Return 1

   End

   ligCur = 0
   Tm = Stream( gpsFile, 'c', 'open read' )
   if Tm = 'READY:' then do

      gpsFS = 0
      Do while Lines( gpsFile ) > 0

         ligCur   = ligCur + 1
         xgpsLine = LineIn( gpsFile )
         gpsLine  = Translate( Strip( xgpsLine ))

         Select

            When gpsLine = '' then
               Iterate

            When Left( gpsLine, 1 ) =  '#' then
               Iterate

            When Left( gpsLine, 1 ) =  ';' then
               Iterate

            When Left( gpsLine, 4 ) =  'REM ' then
               Iterate

            When Left( gpsLine, 1 ) = '[' then do

               gpsFS = 0
               gpsP  = Pos( ']', gpsLine )
               if  gpsP = 0 then Iterate

               xS = Substr( gpsLine, 2, gpsP - 2 )

               if xS = gpsSection then
                  gpsFS = 1
                  gpsLi = ligCur + 1

            End

            When gpsFS = 1 then do

               gpsP  = Pos( '=', gpsLine )
               if gpsP = 0 then Iterate

               xK = Left( gpsLine, gpsP - 1 )
               if xK = gpsKey then do

                  gpsFS = 2
                  gpsLi = ligCur
                  Leave

               End

            End

            OtherWise
               nop
         End

      End

      Tm        = Stream( gpsFile, 'c', 'close' )
      gpsBuffer = 1

      Select

         When gpsFS > 0 then do

            Tm = Stream( gpsFile, 'c', 'open' )
            Tm = Stream( tmpFile, 'c', 'open write' )

            gpsPos = 0
            Do while Lines( gpsFile ) > 0

               gpsPos  = gpsPos + 1
               gpsLine = LineIn( gpsFile )

               if gpsPos = gpsLi then do
                  Tm = LineOut( tmpFile, gpsKey'='gpsDefault )
                  if gpsFS = 2 then Iterate
               End

               Tm = LineOut( tmpFile, gpsLine )

            End

            Tm = Stream( gpsFile, 'c', 'close' )
            Tm = Stream( tmpFile, 'c', 'close' )

            Address SYSTEM 'cp 'tmpFile' 'gpsFile
            Address SYSTEM 'rm 'tmpFile

         End

         OtherWise

            Tm = Stream( gpsFile, 'c', 'open' )
            Do While Lines( gpsFile ) > 0
               gpsLine = LineIn( gpsFile )
            end /* do */
            Tm = LineOut( gpsFile, '['gpsSection']' )
            Tm = LineOut( gpsFile, gpsKey'='gpsDefault )
            Tm = Stream( gpsFile, 'c', 'close' )

      End

   End
   Else gpsBuffer = 0

End
Call SysOut "WriteProfileString returns: "gpsBuffer
return gpsBuffer
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   MkSpace
-------------------------------------------------------------------------------------- */
MkSpace:
Trace Off
Parse arg mArg1, mArg2

mkBuffer = mArg1
do Forever

   if Length( mkBuffer ) < mArg2 then
      mkBuffer = mkBuffer' '
   Else
      Leave

End

return mkBuffer
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   Lock
-------------------------------------------------------------------------------------- */
Lock:
Trace Off
Parse arg lkRsrc, lkMode, lkWait

Call SysOut "Lock: "lkRsrc","lkMode","lkWait

lkRsrc = Translate( Strip( lkRsrc ))
lkMode = Translate( Strip( lkMode ))
lkWait = Translate( Strip( lkWait ))
lkRc   = ""

if lkWait = "" then
   lkWait = "WAIT"
else
if lkWait \= "WAIT" & lkWait \= "NOWAIT" then
   lkWait = "WAIT"

if lkRsrc \= "" then
   if lkMode = "S" | lkMode = "X" then
      lkRc = SendMessage( , "SYS_LOCK", lkRsrc':'lkMode':'lkWait )

Call SysOut "Lock returns: "lkRc
return lkRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   UnLock
-------------------------------------------------------------------------------------- */
UnLock:
Trace Off
Parse arg lkRsrc

Call SysOut "UnLock: "lkRsrc

lkRsrc = Translate( Strip( lkRsrc ))
lkRc   = ""

if lkRsrc \= "" then
   lkRc = SendMessage( , "SYS_UNLOCK", lkRsrc )

Call SysOut "UnLock returns: "lkRc
return lkRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   Sysout
-------------------------------------------------------------------------------------- */
Sysout:
Trace Off
Parse arg soMsg, psoSay

If SysVars.SysWriteSysout = "YES" Then Do
   If SysVars.SysInSysout = 0 Then Do
      SysVars.SysInSysout = 1
      if SysVars.SysRacine = "" then
         SysVars.SysRacine = GetProfileString( , "LOG", "RACINE", "" )
      if SysVars.SysRacine \= "" then do
         soQ    = SysVars.FicName"_"SysVars.PID
         soName = SysVars.SysRacine"/"soQ".sysout"
         Rc     = LineOut( soName, Time('L')": "soMsg )
      End

      SysVars.SysInSysout = 0
   End
End
If psoSay = "1" then
   Say soMsg

If SysVars.soSay = "YES" then
   Say Time('L')": ("SysVars.SysWhoAmI")"soMsg

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   SysAddService
-------------------------------------------------------------------------------------- */
SysAddService:
Trace Off
Parse arg asName

Call   On Halt   Name CallNoHalt

asName = Translate( Strip( asName ))
asRc   = ""

if asName \= "" then do
   tmRc = Lock( "ADD_SERVICE", "X" )

   asRc = Lock( asName, "X", "NOWAIT" )
   if asRc = SysVars.SysLCommit then
      asRc = 1
   Else
      asRc = 0

   tmRc = UnLock( "ADD_SERVICE" )
End

return asRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   SysRemoveService
-------------------------------------------------------------------------------------- */
SysRemoveService:
Trace Off
Parse arg asName

asName = Translate( Strip( asName ))
asRc   = ""

if asName \= "" then do
   asRc = UnLock( asName )
   asRc = 1
End

return asRc
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   SysFindService

   Att: DeadLock si AddService (Process1) avec FindService (Process2) ...
-------------------------------------------------------------------------------------- */
SysFindService:
Trace Off
Parse arg fasName

tmRc = Lock( "ADD_SERVICE", "X" )
tmRc = Lock( "FIND_SERVICE", "X" )

fasRc = SysAddService( fasName )
if fasRc = 1 then do
   fasRc = SysRemoveService( fasName )
   fasRc = 1
End
Else
   fasRc = 0

tmRc = UnLock( "FIND_SERVICE" )
tmRc = UnLock( "ADD_SERVICE" )
return fasRc
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   DdeInit
-------------------------------------------------------------------------------------- */
DdeInit:
Trace Off
DdeIniName  = "ini/dde.ini"
DdeService  = GetProfileString( DdeIniName, "SERVICE", "NAME", "" )

if DdeService = "" then
   DdeRc = 0
Else
if SysFindService( DdeService ) = 0 then
   DdeRc = 0
Else
   DdeRc = 1
return DdeRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   DdeRegisterService
-------------------------------------------------------------------------------------- */
DdeRegisterService:
Trace Off
Arg aSrv
Parse var SysVars.SysWhoAmI aP"_"aQ

Rc = SendMessage( "DDE",,"REGISTER_SERVICE:"aQ":"aSrv )
if Rc = SysVars.SysLCommit then
   DdeRc = 1
Else
   DdeRc = 0
return DdeRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   DdeRegisterItem
-------------------------------------------------------------------------------------- */
DdeRegisterItem:
Trace Off
Arg aItem

Rc = SendMessage( "DDE",,"REGISTER_ATOM:"aItem )
if Rc = SysVars.SysLCommit then
   DdeRc = 1
Else
   DdeRc = 0
return DdeRc
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   monSleep
      Periode: Dur\E9e d'attente exprim\E9e en seconde
-------------------------------------------------------------------------------------- */
monSleep:
Trace Off
Arg Periode
Signal On Syntax Name SleepEnd

If Periode > 0 Then Do
   t1 = Time('E')
   Call SysSleep Periode
   t2 = Time('E') - t1
   SysVars.SysElapsedIdle = SysVars.SysElapsedIdle + t2
End

SleepEnd:
Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SleepEx
      Periode: Dur\E9e d'attente exprim\E9e en seconde
-------------------------------------------------------------------------------------- */
SleepEx:
Trace Off
Arg Periode
Signal On Syntax Name SleepExEnd

slexRC = 0
If Periode > 0 Then Do
   Call ProcStartTimer Periode
   Do Forever
      If Queued() > 0 Then Leave
      Call Sleep SysVars.SysSleepexDelay
   End
   If Queued() > 0 Then Do
      Parse Pull slexQ
      if slexQ \= "SYS_BEGINWAIT~COMMIT" & slexQ \= "" then Do
         Push slexQ
      End
   End
   slexRC = SysSleep(Periode)
End

SleepExEnd:
Call ProcStopTimer
Return slexRC
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SysSleep
      Periode: Dur\E9e d'attente exprim\E9e en seconde
      La fonction se r\E9veille si une donn\E9e est dans la file d'attente
-------------------------------------------------------------------------------------- */
SysSleep:
Trace Off
Arg dwT

ssDelay=dwT
if ssDelay <= 0 then ssDelay = 1
Do Forever
   Call Sleep SysVars.SysSleepDelay
   If Queued() > 0 Then Do
      Leave
   End
   ssDelay = ssDelay - SysVars.SysSleepDelay
   If ssDelay <= 0 Then Leave
End
Return ssDelay
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SysCls
      Efacer l'\E9cran
-------------------------------------------------------------------------------------- */
SysCls:
Trace Off

Address SYSTEM 'clear'
Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SysFileTree
      Lister les fichiers d'un r\E9pertoire
-------------------------------------------------------------------------------------- */
SysFileTree:
Trace Off
Parse arg vPath, vStem, vX

FicT = '/tmp/sysfiletree.'FormatQueue( SysVars.SysWhoAmI )

Address SYSTEM 'ls 'vPath' > 'FicT
nbFic = 0
Do While Lines(FicT) > 0
   L = LineIn(FicT)
   Do nF = 1 to Words(L)
      nbFic = nbFic + 1
      sF = Value(vStem)"."nbFic" = '"Word(L, nF)"'"
      Interpret sF
   End
End
sF = Value(vStem)'.0 = 'nbFic
Interpret sF

Return 0
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   getWSOCKIP
       R\E9cup\E9rer l'adresse IP de la tour de controle
-------------------------------------------------------------------------------------- */
getWSOCKIP:
Trace Off
If Stream(SysVars.fileWSOCK, 'c', 'query exists') \= "" Then Do
   wsockIPP = Strip(LineIn(SysVars.fileWSOCK))
   Parse Var wsockIPP xx"'"wsockIP" "wsockPort"'"
   xxtmp = Stream(SysVars.fileWSOCK, 'c', 'Close')
End
Else Do
   if wsockIPP = "" Then
      wsockIPP = Value("WSOCKIP",,Share)
      
   If wsockIPP \= "" Then 
      Parse Var wsockIPP wsockIP":"wsockPort
   Else Do
      wsockIP   = "127.0.0.1"
      wsockPort = "80"
   End
End
return

/* -----------------------------------------------------------------------------------
   ThrowLog
       Envoyer un message sur la console de supervision
-------------------------------------------------------------------------------------- */
ThrowLog:
Trace Off
Parse Arg tllpMsg

If gblCaption = "" | gblCaption = "GBLCAPTION" Then gblCaption = ProcessName

'wsock 'wsockIP' 'wsockPort' "WFD 'Sysvars.SysWhoAmI';'gblCaption';'tllpMsg'"'Devnull
Return

/* -----------------------------------------------------------------------------------
   ThrowLogM
       Envoyer un message sur la console de supervision
-------------------------------------------------------------------------------------- */
ThrowLogM:
Trace Off
Parse Arg tlProcessName, tlpMsg

'wsock 'wsockIP' 'wsockPort' "WFD 'tlProcessName';Internal;'tlpMsg'"'Devnull

Return

/* -----------------------------------------------------------------------------------
   ThrowLogD
       Envoyer un message sur la console de supervision
-------------------------------------------------------------------------------------- */
ThrowLogD:
Trace Off
Parse Arg tllpMsg

'wsock 'wsockIP' 'wsockPort' "'tllpMsg'"'Devnull
Return
/*************************************************************************************
                           BOUCLE des outils de MONITOR
                           ============================
****************************************************************************************/
SysMainLoop:
nbMsg       = 0
SetTrace    = 0
TimeToCheck = 0
WaitingIdle = 0
nbIdle      = 0
nbIdleMax   = 5
MonitorStat = 0

Call Sysout "Entering SysMainLoop ..."
do FOREVER

   CtrlAttn    = 0
   LoopTimeAll = Time('E')

   Call getWSOCKIP

   Call monSleep SysVars.SysLoopBreak
   nbMsg = QUEUED()

   if nbMsg > 0 then do

      xSysGetCmd = PullData()
      xSysGetCmd = Strip( xSysGetCmd )

      Call Sysout "SysMainLoop pulled "GetQueueName()": "xSysGetCmd

      if Left( xSysGetCmd, 1 ) = '~' then do
         GetCmd = Right( xSysGetCmd, Length( xSysGetCmd ) - 1 )
         msgId  = ''
      End
      Else
         Parse var xSysGetCmd msgId'~'GetCmd

      msgCmd   = Strip( GetCmd )
      msgId    = Translate( Strip( msgId ))

      if Pos( ":", msgId ) > 0 then
         Parse Var msgId msgId":"AnswerQueue
      Else
         AnswerQueue = ""

      smsgCmd  = Translate( Strip( GetCmd ))
      smsgId   = Translate( Strip( msgId ))

      if IsSysCmd() = 0 then do

         if SysVars.SysTimerEnabled = 0 | EndForce = 1 then do

            WaitingIdle = 0

            if MonitorStat = 1 then
               Rc = PostMessage( , "SYS_ETAT", "A" )

            If Translate(msgCmd) = SysVars.SysLEnd Then msgCmd = SysVars.SysLEnd
            Call SysOut "Calling main with "msgCmd

            LoopTime = Time('E')

            if SetTrace = 1 then Trace I
            
            Call Main msgCmd
            
            Trace Off
            trTime         = Time('E') - LoopTime
            SysVars.SysElapsedCmd  = SysVars.SysElapsedCmd + trTime
            if SysVars.SysElapsedCmd = 0 then SysVars.SysElapsedCmd = SysVars.SysElapsedCmd + 0.01

            if MonitorStat = 1 then
               Rc = PostMessage( , "SYS_ETAT", "I" )

            if WaitingIdle > 0 then
               SysVars.SysElapsedCmd = SysVars.SysElapsedCmd - WaitingIdle

            if AnswerQueue \= "" then do

               if AnswerQueue = "ANSWEREQUEUE" then AnswerQueue = ""
               Rc             = PostMessage( , "SYS_ANSWERE", AnswerQueue":"msgResult )

            End

         End
         Else Do
            Call QueueData xSysGetCmd
            smsgId = ""
            smsgCmd = ""
         End
      End

      if smsgId = "SYS_STOP" then
         EndForce = 1
      Else
      if smsgId = "SYS_INIT" then
         ProcessInitialized = 1
      Else
      if smsgCmd = SysVars.SysLEnd then
         EndForce = 1

   End
   Else do

      if SysVars.SysTimerEnabled = 0 then do

         WaitingIdle = 0
         GetCmd      = SysVars.SysLIdle
         msgCmd      = SysVars.SysLIdle
         msgId       = ""

         if MonitorStat = 1 then
            Rc = PostMessage( , "SYS_ETAT", "N" )
            
         LoopTime = Time('E')
         Call SysOut "Calling main with "msgCmd

         if SetTrace = 1 then Trace I
         
         Call Main msgCmd
         
         Trace Off
         
         trTime         = Time('E') - LoopTime
         SysVars.SysElapsedCmd  = SysVars.SysElapsedCmd + trTime
         if SysVars.SysElapsedCmd = 0 then SysVars.SysElapsedCmd = SysVars.SysElapsedCmd + 0.01

         if MonitorStat = 1 then
            Rc = PostMessage( , "SYS_ETAT", "I" )

      End

      Do TimeToWait
         Call monSleep SysVars.SysLoopDelay
         if Queued() > 0 then Leave
      End

   End

   if CtrlAttn = 1 then Leave
   if EndForce > 0 then Leave

   TimeToCheck             = TimeToCheck + 1
   SysVars.SysElapsedAll   = SysVars.SysElapsedAll + ( Time('E') - LoopTimeAll )
   SysVars.SysElapsedIdle  = SysVars.SysElapsedIdle + WaitingIdle
   WaitingIdle             = 0

   if SysVars.SysElapsedAll < ( SysVars.SysElapsedIdle + SysVars.SysElapsedCmd ) then
      SysVars.SysElapsedAll = SysVars.SysElapsedIdle + SysVars.SysElapsedCmd + 0.01

   if TimeToCheck = 10 then Do
      if SysVars.SysDetectMon = "YES" Then /* JSM 13.03.2003 Here */
         if Stream( SysVars.SysProcessLockName, 'c', 'query exists' ) = "" then
            Leave

      TimeToCheck = 0
   End

   Avant          = SetQueue( CpuQueue )
   SysVars.PTIME  = 0
   Ln             = SysVars.SysWhoAmI"~"SysVars.PTIME":"SysVars.SysElapsedCmd":"SysVars.SysElapsedIdle":"SysVars.SysElapsedAll
   Call QueueData Ln
   Apres = SetQueue( Avant )

   if CtrlAttn = 1 then Call StandardHalt

End
Call Sysout "Leaving SysMainLoop ..."
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   IsSysCmd
-------------------------------------------------------------------------------------- */
IsSysCmd:

Select

   When msgId = "SYS_ENDWAIT" then do

      SysVars.SysTimerEnabled   = 0
      iscOk                     = 1

   End

   When msgId = "SYS_BEGINWAIT" then do

      SysVars.SysTimerEnabled   = 1
      iscOk                     = 1

   End

   When msgId = "SYS_BEGINSTAT" then do

      MonitorStat       = 1
      iscOk             = 1

   End

   When msgId = "SYS_ENDSTAT" then do

      MonitorStat       = 0
      iscOk             = 1

   End

   When msgId = "SYS_STOPFORCE" then do

      EndForce = 1
      iscOk    = 1

   End

   When msgId = "SYS_STOP" then do

      EndForce = 1
      msgCmd   = SysVars.SysLEnd
      iscOk    = 0

   End

   When msgId = "SYS_LOCKPROCESS" then do

      LockedProcess  = 1
      iscOk          = 1

   End

   When msgId = "SYS_UNLOCKPROCESS" then do

      LockedProcess  = 0
      iscOk          = 1

   End

   When msgId = "SYS_ACK" then do

      iscOk    = 0

   End

   When msgId = "USR_MSG" then

      Select

         When Translate( msgCmd ) = "DUMPVARIABLES" then DO

            if SysVars.SysRacine = "" then
               SysVars.SysRacine = GetProfileString( , "LOG", "RACINE", "" )
            if SysVars.SysRacine \= "" then do
               soQ    = FormatQueue( SysVars.SysWhoAmI )
               soName = SysVars.SysRacine"/"soQ".dumpvariables"
               Call SysDumpVariables soName
            End

         End

         When Translate( msgCmd ) = "WAIT" then do

            Call ProcStartTimer
            IscOk = 1

         End

         When Translate( msgCmd ) = "WAKE" then do

            Call ProcStopTimer
            IscOk = 1

         End

         When Translate( msgCmd ) = "TRACE ON" then do

            SetTrace = 1
            IscOk    = 1

         End

         When Translate( msgCmd ) = "TRACE OFF" then do

            SetTrace = 0
            IscOk    = 1

         End

         When Translate( Left( msgCmd, 5 )) = "EXEC " then do

            IscOk    = 1
            Parse Var msgCmd "EXEC "tmpCmd
            Interpret tmpCmd

         End

         OtherWise

            IscOk = 0
   End

   OtherWise

      iscOk = 0

End

if iscOk = 1 then Call ProcUserHook

Return iscOk
/* =================================================================================== */
/*************************************************************************************
                                 MONITOR
                           ===================
****************************************************************************************/

/* -----------------------------------------------------------------------------------
   MainLoop
-------------------------------------------------------------------------------------- */
MonSysMainLoop:
if ProcessInitialized = 0 then do

   nbLineDbg = 0
   Call SysCls
   if SetQueue( SysVars.SysQueue ) = "" then do

      Say "ERROR SETTING QUEUE IN MAINLOOP !"
      Return

   End

   Address SYSTEM '#rm log/* 1>/dev/null 2>&1'
   Address SYSTEM '#rm dbg/* 1>/dev/null 2>&1'
   Address SYSTEM 'rm cmd/.rexx 1>/dev/null 2>&1' /* */

   ProcessInitialized   = 1
   LocalProcess.0       = 0
   MessageReSend.0      = 0
   LockOwner.0          = 0
   DoChainLock          = 1
   LockRemoved          = 0
   LockAsked.0          = 0
   TimeToResend         = Time('E') + SysVars.SysLoopResend
   SysCancelTime        = 20
   SysStopAsked         = 0
   SysStopRedo          = 0
   SysNbWait            = 0

   SysHookProc.0        = 0
   nbSysHookProc        = 0
   HookProcess          = 0
   LockProcess          = 0

   msgResult            = SysVars.SysLCommit
   ForceTimeOut         = Time('E') + SysVars.SysLoopForceTO

End

Call ThrowLogM ProcessName, "UP"

Do ForEver

   CtrlAttn = 0
   SysCmd   = WaitMessage( , SysVars.SysLoopWM )
   Call getWSOCKIP

   if Time('E') >= ForceTimeOut then do

      ForceTimeOut = Time('E') + SysVars.SysLoopForceTO
      Call PushData SysVars.SysLTimeOut

   End

   if SysCmd = SysVars.SysLTimeOut then do

      if TimeToResend < Time('E') & MessageReSend.0 > 0 then do

         Rc             = ReSendMessage()
         TimeToResend   = Time('E') + SysVars.SysLoopResend

      End

      if LockAsked.0 > 0 & LockRemoved = 1 then
         Call TryLockAsked

      if SysNbWait > 0 then
         Call TryWakeUp

      Call QueryTime

   End
   Else
   if SysCmd = SysVars.SysLHalt then
      SysStopAsked = 1
   Else do

      smlRc = TrtSysCmd( SysCmd )
      Call LinkHookProc
      /*SysStopRedo = 0*/

   End

   if CtrlAttn = 1 | SysStopAsked = 1 then do
      Call ThrowLogM ProcessName, "SHUTTING DOWN"
      Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Arrêt du process demandé ..."
      Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Il y a actuellement "NumberProcess" process actifs"
      Call StopAllProcess
      SysVars.SysStopping  = 1
      SysStopAsked         = 0
   End

   if SysVars.SysStopping = 1 then do
      if NumberProcess <= 1 then Do
         Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Il n'y a plus de process actif ..."
         Leave
      End

      SysStopRedo = SysStopRedo + 1
      if SysStopRedo > SysVars.SysStopRedoT / 2 then do
         Call StopAllProcess
         Call Sleep SysVars.SysLoopDelay
      end

      if SysStopRedo > SysVars.SysStopRedoT then
         SysStopAsked = 2

   End

   if SysStopAsked = 2 then  Do
      Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Arrêt du moniteur forcé !"
      Leave
   End

End

Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Fin de la boucle principale"
Call ThrowLogM ProcessName, "DOWN"
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   SendToAll
-------------------------------------------------------------------------------------- */
SendToAll:
Parse arg staId, staMsg
Do stamI = 1 to LocalProcess.0

   stamQ = LocalProcess.stamI

   if stamQ \= "" then
      Rc = LinkMessage( stamQ, staId, staMsg )

End
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   StopAllProcess
-------------------------------------------------------------------------------------- */
StopAllProcess:

Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Purge des queues system"
Address SYSTEM 'rxqueue 'SysVars.SysEtatQueue' /clear'
Address SYSTEM 'rxqueue 'CpuQueue' /clear'
Address SYSTEM 'rxqueue 'SysVars.SysQueue' /clear'
Do mI = 1 to LocalProcess.0

   mQ = LocalProcess.mI

   if mQ \= "" then do
      do kI = 1 to LocalProcess.mQ.0
         tQ = LocalProcess.mQ.kI
         if tQ \= "" then do
            Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Purge de "tQ
            Address SYSTEM 'rxqueue 'tQ' /clear'
         End
      End
      Rc = CancelLoopForProcess( mQ )
      Rc = LinkMessage( mQ, "SYS_STOP", SysVars.SysLEnd )
      Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "Arrêt demandé pour "mQ

   End

End
SysVars.SysStopping = 1
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   TrtSysCmd
-------------------------------------------------------------------------------------- */
TrtSysCmd:
Parse arg TheCmd

if Left( TheCmd, 1 ) = '{' then do

   Parse var TheCmd '{'ReSendNum'}'TheCmd
   ReSendNum = ReSendNum + 1

End
Else
   ReSendNum = 0

Parse var TheCmd msgId'~'msgFrom'~'msgTo'~'msgDate'~'msgTime'#'msgData


if Pos( ':', msgId ) > 0 then do
   Parse var msgId msgId':'AnswerQueue
End
Else AnswerQueue = ""


if MsgLevelDisplayDebug = "YES" then
   Say "*DBG* Id="msgId", From="msgFrom", To="msgTo", Data="msgData

if LogFileNameDbg \= "" then
   Rc = LineOut( LogFileNameDbg, "*DBG* Id="msgId", From="msgFrom", To="msgTo", Data="msgData )

if msgTo = SysVars.SysMonName then
   Call TrtSysMonCmd
else do

   if AnswerQueue \= "" then AnswerQueue = ":"AnswerQueue

   if FindProcess( msgTo ) > 0 then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )
      dJ = FindSubProcess( Qn )
      Call Printf Pn"("Dj")", MsgLLevelDisplaySend, "TO "msgTo": "msgData

      Tm = LinkMessage( msgTo, msgId''AnswerQueue, msgData )
      if Tm = 0 then Call Printf SysVars.SysMonName, "ERROR", "Impossible de rediriger le message."

   End
   Else Do
      Call AddReSendMessage TheCmd, ReSendNum
      if ReSendNum = 0 then
         Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "PUSHED To="msgTo", Data="msgData
   End

   AnswerQueue = ""
End

If AnswerQueue \= "" then
   Rc = LinkMessageQ( AnswerQueue, "SYS_ACK", msgResult )

msgResult = SysVars.SysLCommit
Return 1
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   TrtSysMonCmd
-------------------------------------------------------------------------------------- */
TrtSysMonCmd:

Select

   When msgId = "SYS_BEGINPROCESS" then do

      dI = AddProcess( msgFrom )
      Call Printf SysVars.SysMonName, "ADD", FormatQueue( msgFrom, 'X' )

      Rc          = LinkMessageQ( FormatQueue( msgFrom ), "SYS_INIT", SysVars.SysLInit )
      HookProcess = 1

   End

   When msgId = "SYS_ENDPROCESS" then do

      Call RemoveProcess msgFrom
      Call RemoveLockPro FormatQueue( msgFrom )

      Call Printf SysVars.SysMonName, "REMOVE", FormatQueue( msgFrom, 'X' )
      HookProcess = 1
      LockProcess = 1

   End

   When msgId = "SYS_BEGINWAIT" then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )
      dJ = FindSubProcess( Qn )

      SysNbWait                  = SysNbWait + 1
      LocalProcess.Pn.dJ.Wait    = 1
      if msgData > 0 then
         LocalProcess.Pn.dJ.WaitT   = Time('E') + msgData

      Rc = LinkMessageQ( Qn, "SYS_BEGINWAIT", SysVars.SysLCommit )
      Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "WAIT "FormatQueue( msgFrom, 'X' )"("Dj")"
      HookProcess = 1

   End

   When msgId = "SYS_ENDWAIT" then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )
      dJ = FindSubProcess( Qn )

      SysNbWait                  = SysNbWait - 1
      LocalProcess.Pn.dJ.Wait    = 0
      LocalProcess.Pn.dJ.WaitT   = 0

      Rc = LinkMessageQ( Qn, "SYS_ENDWAIT", SysVars.SysLCommit )
      Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "WAKE "FormatQueue( msgFrom, 'X' )"("Dj")"
      HookProcess = 1

   End

   When msgId = "SYS_DISPLAY" then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )

      dJ = FindSubProcess( Qn )
      Call Printf Pn"("dJ")", MsgLLevelDisplayDebug, msgData

   End

   When msgId = "SYS_ETAT" then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )
      dJ = FindSubProcess( Qn )

      LocalProcess.Pn.dJ.Etat = msgData
      if FindHookProc( Qn ) = 0 then
         HookProcess = 1

   End

   When msgId = "SYS_BEGINLOOP" then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )
      dJ = FindSubProcess( Qn )

      LocalProcess.Pn.dJ.Loop = msgData
      if FindHookProc( Qn ) = 0 then
         HookProcess = 1

   End

   When msgId = "SYS_ENDLOOP" then do

      Pn = FormatQueue( msgFrom, 'X' )
      Qn = FormatQueue( msgFrom )
      dJ = FindSubProcess( Qn )

      LocalProcess.Pn.dJ.Loop = ''
      if FindHookProc( Qn ) = 0 then
         HookProcess = 1

   End

   When msgId = "SYS_BEGINSTAT" then
      Call SendToAll "SYS_BEGINSTAT", ""

   When msgId = "SYS_ENDSTAT" then
      Call SendToAll "SYS_ENDSTAT", ""

   When msgId = "SYS_STOP" then do

      If Translate( msgData ) = "FORCE" then
         SysStopAsked = 2
      Else
         SysStopAsked = 1

   End

   /* La cmd END ne parvient pas a un process en wait - si waitmessage - donc
      il faut tuer la boucle */
   When msgId = "SYS_KILLPROCESS" then do

      if msgData \= "" then do
         Rc = CancelLoopForProcess( msgData )
         Rc = LinkMessage( msgData, "SYS_STOP", SysVars.SysLEnd )

      End

   End

   When msgId = "SYS_ANSWERE" then do

      Parse Var msgData Qn":"msgData
      Rc = LinkMessageQ( Qn, "SYS_ACK", msgData )

   End

   When msgId = "SYS_LOCK" then do

      Parse var msgData lkRsrc":"lkMode":"lkWait

      Pn = FormatQueue( msgFrom, "X" )
      Qn = FormatQueue( msgFrom )
      Rc = AddLock( Qn, lkRsrc, lkMode )
      if Rc > 0 then
         Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "LOCK: "Pn":"msgData
      Else
      if Rc = 0 then do
         if lkWait = "WAIT" then do
            Rc = CheckDeadLock( Qn, lkRsrc )
            if Rc = 0 then do
               Call AddLockAsked Qn, AnswerQueue, lkRsrc, lkMode
               Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "ASK LOCK: "Pn":"Qn":"msgData
               AnswerQueue = ""
            End
            Else
               msgResult = SysVars.SysLDeadLock
         End
         Else
            msgResult = SysVars.SysLBusy
      End
      LockProcess = 1

   End

   When msgId = "SYS_UNLOCK" then do

      Pn = FormatQueue( msgFrom, "X" )
      Qn = FormatQueue( msgFrom )
      Call RemoveLock Qn, msgData
      LockProcess = 1

   End

   When msgId = "SYS_HOOK" then do

      Pn    = FormatQueue( msgFrom, "X" )
      Qn    = FormatQueue( msgFrom )
      hData = Translate( Strip( msgData ))

      Select

         When Left( hData, 4 ) = "ADD:" then do

            Parse var hData "ADD:"What
            Call AddHookProc Qn, What

         End

         When hData = "REMOVE" then do

            If RemoveHookProc( Qn ) = 0 then
               Nop

         End

         When Left( hData, 5 ) = "LIST:" then do

            Parse var hData "LIST:"What":"Qn

            Select

               When What = "PROCESS" then
                  Call FillProcess Qn

               When What = "LOCKS" then
                  Call FillLocks Qn

               OtherWise

                  Nop

            End

         End

         OtherWise
            Call Printf Pn, "ERROR", "INVALID HOOK COMMAND"

      End

   End

   When msgId = "USR_MSG" then do

      msgData = Translate( msgData )

      Select

         When msgData = SysVars.SysLEnd then

            SysStopAsked = 1

         When msgData = "LOCK" then

            LockedProcess = 1

         When msgData = "UNLOCK" then

            LockedProcess = 0

         When msgData = "TRUNCLOG" then

            if LogFileName \= "" then do

               Status = Stream( LogFileName, 'c', 'close' )
               Address SYSTEM 'cat 'LogFileName' >> 'LogFileName'.trc '
               Address SYSTEM 'rm 'LogFileName' 1>/dev/null 2>&1'
               Status = Stream( LogFileName, 'c', 'open write' )

            End

         When msgData = "DUMPVARIABLES" then DO

            if SysVars.SysRacine = "" then
               SysVars.SysRacine = GetProfileString( , "LOG", "RACINE", "" )
            if SysVars.SysRacine \= "" then do
               soQ    = FormatQueue( SysVars.SysWhoAmI )
               soName = SysVars.SysRacine"/"soQ".dumpvariables"
               Call SysDumpVariables soName
            End

         End

         OtherWise

            Call Printf SysVars.SysMonName, "WARNING", "IGNORED "msgId"/"msgData

      End

   End

   OtherWise

      Call Printf SysVars.SysMonName, "WARNING", "IGNORED "msgId"/"msgData

End
Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   MonitorStartQueueSys
-------------------------------------------------------------------------------------- */
MonitorStartQueueSys:
msqsRC = Value("Rxstack_run",,Share)
If msqsRC \= "NO" & SysVars.FirstInstance = 1 Then Do
   Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Démarrage du service de gestion des files d'attente ..."
   Address SYSTEM 'echo "rxstack -d" | at now'
   Address SYSTEM 'sleep 1'
End
Else
   Rc = 0
Return Rc
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   MonitorStopQueueSys
-------------------------------------------------------------------------------------- */
MonitorStopQueueSys:
msqsRC = Value("Rxstack_run",,Share)
If msqsRC \= "NO" & SysVars.FirstInstance = 1 Then Do
   If TheFirstQueue = SysVars.SysQueue Then Do
      Call Printf SysVars.SysMonName, MsgLLevelDisplayOther, "Arrêt du service de gestion des files d'attente ..."
      Address SYSTEM 'rxstack -k'
      Address SYSTEM 'ps > /tmp/monitor.ps.list'
      Do While Lines('/tmp/monitor.ps.list') > 0
         a=LineIn('/tmp/monitor.ps.list')
         if pos('rxstack',a) > 0 then do
            b=word(a,1)
            'kill 'strip(b)
            leave
         end
      end
   End
   Else
      Rc = 2
End
Return Rc
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   MonitorPrologue
-------------------------------------------------------------------------------------- */
MonitorPrologue:
If MonitorStartQueueSys() = 0 Then
   TheFirstQueue = AllocQueue( SysVars.SysQueue )
Else
   TheFirstQueue = ""
/***********************************************
if TheFirstQueue \= SysVars.SysQueue then do
   Say "Monitor déjà actif !"
   do I = 1 to LocalQueue.0
      if LocalQueue.I \= "" then
         Call RxQueue 'Delete', LocalQueue.I
   End
   exit
End
********/

/* Libells */
MsgLLevelDisplaySyst  = "LEVEL_SYSTEM"
MsgLLevelDisplayRecv  = "LEVEL_RECEIVE"
MsgLLevelDisplaySend  = "LEVEL_SEND"
MsgLLevelDisplayOther = "LEVEL_OTHERS"
MsgLLevelDisplayDebug = "LEVEL_DEBUG"

/* Rechercher dans .INI */
MsgLevelDisplaySyst  = GetProfileString( , "DISPLAY", MsgLLevelDisplaySyst, "YES" )
MsgLevelDisplayRecv  = GetProfileString( , "DISPLAY", MsgLLevelDisplayRecv, "YES" )
MsgLevelDisplaySend  = GetProfileString( , "DISPLAY", MsgLLevelDisplaySend, "YES" )
MsgLevelDisplayOther = GetProfileString( , "DISPLAY", MsgLLevelDisplayOther, "YES" )
MsgLevelDisplayDebug = GetProfileString( , "DISPLAY", MsgLLevelDisplayDebug, "NO" )
LogFileName          = GetProfileString( , "LOG", "FileName", "" )
LogFileNameDbg       = GetProfileString( , "LOG", "FileNameDebug", "" )

if SysVars.FirstInstance then do
    if AllocQueue( CpuQueue ) = "" then do
       Say "Impossible de creer "CpuQueue" !"
       do I = 1 to LocalQueue.0
          if LocalQueue.I \= "" then
             Call RxQueue 'Delete', LocalQueue.I
       End
       Exit
    End

    if AllocQueue( SysVars.SysEtatQueue ) = "" then do
       Say "Impossible de creer "SysVars.SysEtatQueue" !"
       do I = 1 to LocalQueue.0
          if LocalQueue.I \= "" then
             Call RxQueue 'Delete', LocalQueue.I
       End
       Exit
    End
End

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   Printf
-------------------------------------------------------------------------------------- */
Printf:
Parse arg pfFrom, pfLevel, pfInfos

pfDisplay = 0
Select

   When pfLevel = MsgLLevelDisplaySyst then do
      pfSign = "$"
      if MsgLevelDisplaySyst = 'YES' then
         pfDisplay = 1
   End

   When pfLevel = MsgLLevelDisplayRecv then do
      pfSign = "<"
      if MsgLevelDisplayRecv = 'YES' then
         pfDisplay = 1
   End

   When pfLevel = MsgLLevelDisplaySend then do
      pfSign = ">"
      if MsgLevelDisplaySend = 'YES' then
         pfDisplay = 1
   End

   When pfLevel = "ERROR" then do
      pfSign    = "!"
      pfDisplay = 1
   End

   When pfLevel = "WARNING" then do
      pfSign    = "?"
      pfDisplay = 1
   End

   When pfLevel = "ADD" then do
      pfSign    = "+"
      pfDisplay = 1
      pfFrom    = pfInfos
      pfInfos   = ""
   End

   When pfLevel = "REMOVE" then do
      pfSign    = "-"
      pfDisplay = 1
      pfFrom    = pfInfos
      pfInfos   = ""
   End

   OtherWise
      pfSign = "%"
      if MsgLevelDisplayOther = 'YES' then
         pfDisplay = 1

End

if pfDisplay = 1 then do

   D        = Date('S')
   T        = Left( Time('L'), 11 )
   zFrom    = MkSpace( pfFrom, 30 )
   zLevel   = MkSpace( pfLevel, 15 )

   ZoneInfo = D'-'T' 'pfSign''zFrom''pfInfos
   Say ZoneInfo
   If wsockIP \= "" Then 'wsock 'wsockIP' 'wsockPort' "WFMVM 'ZoneInfo'" > /dev/null 2>&1'
   Call SysLog ZoneInfo
End
Return
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   SysLog
-------------------------------------------------------------------------------------- */
SysLog:
Parse Arg LogData

if LogFileName \= "" then do

   Status = LineOut( LogFileName, logData )

End
Return


/* -----------------------------------------------------------------------------------
   LinkMessage
-------------------------------------------------------------------------------------- */
LinkMessage:
Parse Arg PmsgTo, PmsgId, PmsgData

if PmsgTo = SysVars.SysMonName then return 1

Pn   = FormatQueue( PmsgTo, 'X' )
lmI  = FindProcess( Pn )

If Translate(PmsgData) = SysVars.SysLEnd Then SysVars.SysPushQueue = 1

if lmI > 0 then do

   do lmJ = 1 to LocalProcess.Pn.0

      Qn = LocalProcess.Pn.lmJ
      Rc = LinkMessageQ( Qn, PmsgId, PmsgData )

   End

End

Return 1
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   LinkMessageQ
-------------------------------------------------------------------------------------- */
LinkMessageQ:
Parse Arg lQmsgTo, lQmsgId, lQmsgData

lQlmOk = LinkMessageAs( lQmsgTo, lQmsgId'~'lQmsgData )

Return lQlmOk
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   LinkMessageAs
-------------------------------------------------------------------------------------- */
LinkMessageAs:
Parse Arg QmsgTo, QmsgData

Signal On Syntax Name LinkMessageAsError

QlmOk = 0
Avant = SetQueue( QmsgTo )
if Avant \= "" then do

   If SysVars.SysPushQueue = 1 Then
      Call PushData QmsgData
   Else
      Call QueueData QmsgData
      
   QlmOk = 1
   Tm    = SetQueue( Avant )

End
Signal LinkMessageAsRetour

LinkMessageAsError:
QlmOk = 0
Tm    = SetQueue( Avant )

LinkMessageAsRetour:
Signal On Syntax Name StandardSyntax
SysVars.SysPushQueue = 0
Return QlmOk
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   AddProcess
-------------------------------------------------------------------------------------- */
AddProcess:
Arg pArg1

Pn = FormatQueue( pArg1, 'X' )
Qn = FormatQueue( pArg1 )

I = FindProcess( Pn )
if I = 0 then do

   I                          = LocalProcess.0
   I                          = I + 1
   LocalProcess.0             = I
   LocalProcess.I             = Pn
   LocalProcess.Pn.0          = 1
   LocalProcess.Pn.1          = Qn
   LocalProcess.Pn.1.Loop     = ''
   LocalProcess.Pn.1.Lock     = 0
   LocalProcess.Pn.1.Wait     = 0
   LocalProcess.Pn.1.WaitT    = 0
   LocalProcess.Pn.1.TAll     = 0
   LocalProcess.Pn.1.TCmd     = 0
   LocalProcess.Pn.1.TIdle    = 0
   LocalProcess.Pn.1.TElapsed = 0
   LocalProcess.Pn.1.Etat     = "?"

End
Else do

   J                          = LocalProcess.Pn.0
   J                          = J + 1
   LocalProcess.Pn.0          = J
   LocalProcess.Pn.J          = Qn
   LocalProcess.Pn.J.Loop     = ''
   LocalProcess.Pn.J.Lock     = 0
   LocalProcess.Pn.J.Wait     = 0
   LocalProcess.Pn.J.WaitT    = 0
   LocalProcess.Pn.J.TAll     = 0
   LocalProcess.Pn.J.TCmd     = 0
   LocalProcess.Pn.J.TIdle    = 0
   LocalProcess.Pn.1.TElapsed = 0
   LocalProcess.Pn.J.Etat     = "?"

End
NumberProcess = NumberProcess + 1

Return I
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindProcess
-------------------------------------------------------------------------------------- */
FindProcess:
Arg fpPn

do fpI = 1 to LocalProcess.0

   if LocalProcess.fpI = fpPn then Return fpI

End
Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindSubProcess
-------------------------------------------------------------------------------------- */
FindSubProcess:
Arg fpQn

do fpI = 1 to LocalProcess.0
   fpQ = LocalProcess.fpI
   if fpQ = "" then Iterate

   do fpJ = 1 to LocalProcess.fpQ.0

      if LocalProcess.fpQ.fpJ = fpQn then Return fpJ

   End

End
Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   RemoveProcess
-------------------------------------------------------------------------------------- */
RemoveProcess:
Arg pArg1

Pn = FormatQueue( pArg1, 'X' )
Qn = FormatQueue( pArg1 )

rpI = FindProcess( Pn )
if rpI > 0 then do

   if LocalProcess.Pn.0 > 1 then do

      do rpJ = 1 to LocalProcess.Pn.0

         if LocalProcess.Pn.rpJ = Qn then do

            LocalProcess.Pn.rpJ = ""
            Leave

         End

      End
      NumberProcess = NumberProcess - 1

   End
   Else do

      LocalProcess.rpI        = ""
      LocalProcess.Pn.0       = 0
      LocalProcess.Pn.1       = ""
      NumberProcess           = NumberProcess - 1

   End
End
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   CancelLoopForProcess
-------------------------------------------------------------------------------------- */
CancelLoopForProcess:
Arg clP
Signal On Syntax Name CancelLoopProcessRetour

do clI = 1 to LocalProcess.clP.0

   if LocalProcess.clP.clI.Loop \= "" then do
      Rc                         = CancelLoop( LocalProcess.clP.clI.Loop )
      LocalProcess.clP.clI.Loop  = ""
      HookProcess                = 1
   End

End

CancelLoopProcessRetour:
Signal On Syntax Name StandardSyntax

Return 1
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   CancelLoop
-------------------------------------------------------------------------------------- */
CancelLoop:
Arg clQ
Signal On Syntax Name CancelLoopRetour

clAvant = SetQueue( clQ )
if clAvant \= "" then do

   Call PushData '!STOP!'
   Tm = SetQueue( clAvant )

End

CancelLoopRetour:
Signal On Syntax Name StandardSyntax

Return 1
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   AddReSendMessage
-------------------------------------------------------------------------------------- */
AddReSendMessage:
Parse Arg rsmCmd, rsmNum

if rsmNum < SysVars.SysCancelTime then Do
   rsmI                 = MessageReSend.0 + 1
   MessageReSend.0      = rsmI
   MessageReSend.rsmI   = rsmCmd
   MessageReSend.rsmI.1 = rsmNum
End

Return
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   ReSendMessage
-------------------------------------------------------------------------------------- */
ReSendMessage:

ReSendMessageLoop:
do rsmJ = 1 to MessageReSend.0

   if MessageReSend.rsmJ \= "" then Do

      Call QueueData "{"MessageReSend.rsmJ.1"}"MessageReSend.rsmJ
      MessageReSend.rsmJ   = ""

   End

End
Drop MessageReSend.
MessageReSend.0 = 0

Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindLockX
-------------------------------------------------------------------------------------- */
FindLockX:
Parse Arg flkXRsrc, flkXWho

do flkXI = 1 to LockOwner.0

   do flkXJ = 1 to LockOwner.flkXI.0

      if LockOwner.flkXI.flkXJ = flkXRsrc & LockOwner.flkXI.flkXJ.0 = "X" then do

         if LockOwner.flkXI = flkXWho then
            Return 2
         Else
            Return 1

      End

   End

End
Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindLockOwner
-------------------------------------------------------------------------------------- */
FindLockOwner:
Parse Arg flkoWho

do flkoI = 1 to LockOwner.0

   if LockOwner.flkoI = flkoWho then
      Return flkoI

End
Return 0
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   FindLockPoste
-------------------------------------------------------------------------------------- */
FindLockPoste:
Parse Arg flkpP, flkpRsrc

do flkpI = 1 to LockOwner.flkpP.0

   if LockOwner.flkpP.flkpI = flkpRsrc then
      return flkpI

End
Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindLock
-------------------------------------------------------------------------------------- */
FindLock:
Parse Arg flkRsrc

do flkI = 1 to LockOwner.0

   if FindLockPoste( flkI, flkRsrc ) > 0 then
      Return flkI

End
Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindProprio
-------------------------------------------------------------------------------------- */
FindProprio:
Parse Arg fdlRsrc

do fdlI = 1 to LockOwner.0
   do fdlJ = 1 to LockOwner.fdlI.0
      if LockOwner.fdlI.fdlJ = fdlRsrc & LockOwner.fdlI.fdlJ = "X" then
         Return dlOwner

   End
End
Return ""
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   CheckDeadLock
-------------------------------------------------------------------------------------- */
CheckDeadLock:
Parse Arg dlWho, dlRsrc

Return 0

/* Constituer une liste de lock en wait */
lkw.0    = 1
lkw.1.0  = dlRsrc
lkw.1.1  = FindProprio( dlRsrc )

do dlI = 1 to LockAsked.0
   dlJ         = dlI + 1
   lkw.dlJ.0   = LockAsked.dlI.2
   lkw.dlJ.1   = FindProprio( dlRsrc )
End

Return 0
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   AddLock
-------------------------------------------------------------------------------------- */
AddLock:
Parse Arg lkWho, lkRsrc, lkMode

if lkWho = "" | lkRsrc = "" | lkMode = "" | SysVars.SysStopping = 1 then return -1

lpR = FindLock( lkRsrc )

if lpR = 0 then do

   lpO = FindLockOwner( lkWho )

   if lpO = 0 then do

      lpO               = LockOwner.0 + 1
      LockOwner.0       = lpO
      LockOwner.lpO     = lkWho
      LockOwner.lpO.0   = 1
      LockOwner.lpO.1   = lkRsrc
      LockOwner.lpO.1.0 = lkMode
      LockOwner.lpO.1.1 = 1

      Return 1

   End
   Else do

      lpI                  = LockOwner.lpO.0 + 1
      LockOwner.lpO.0      = lpI
      LockOwner.lpO.lpI    = lkRsrc
      LockOwner.lpO.lpI.0  = lkMode
      LockOwner.lpO.lpI.1  = 1

      Return 1

   End

End
Else do

   lpX = FindLockX( lkRsrc, lkWho )        /* Un verrou type 'X' posé ? */
   if lpX = 1 then Return 0

   lpO = FindLockOwner( lkWho )

   if lpO = 0 then do

      lpP = FindLockPoste( lpR, lkRsrc )

      if LockOwner.lpR.lpP.0 = "X" then do

         Return 0

      End
      Else do

         if lkMode = "S" then do

            lpO               = LockOwner.0 + 1
            LockOwner.0       = lpO
            LockOwner.lpO     = lkWho
            LockOwner.lpO.0   = 1
            LockOwner.lpO.1   = lkRsrc
            LockOwner.lpO.1.0 = lkMode
            LockOwner.lpO.1.1 = 1

            Return 1

         End
         Else Do

            Return 0

         End

      End

   End
   Else do

      lpP = FindLockPoste( lpO, lkRsrc )

      if lpP = 0 then do

         lkI                  = LockOwner.lpO.0 + 1
         LockOwner.lpO.0      = lkI
         LockOwner.lpO.lkI    = lkRsrc
         LockOwner.lpO.lkI.0  = lkMode
         LockOwner.lpO.lkI.1  = 1

         Return 1

      End
      Else do

         lkI                  = LockOwner.lpO.lpP.1 + 1
         LockOwner.lpO.lpP.1  = lkI

         if lkMode = "X" then
            LockOwner.lpO.lpP.0  = lkMode

         Return 1

      End

   End

End

Return lkRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   RemoveLock
-------------------------------------------------------------------------------------- */
RemoveLock:
Parse Arg lkWho, lkRsrc

if lkWho = "" | lkRsrc = "" then return


lkO = FindLockOwner( lkWho )
if lkO > 0 then do

   lkP = FindLockPoste( lkO, lkRsrc )

   if lkP > 0 then do

      LockOwner.lkO.lkP.1  = LockOwner.lkO.lkP.1 - 1

      if LockOwner.lkO.lkP.1 = 0 then do

         Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "UNLOCK: "lkWho":"lkRsrc

         LockRemoved       = 1
         LockOwner.lkO.lkP = ""

      End

      if DoChainLock = 1 & LockRemoved = 1 then
         Call ChainLock

   End

End

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   ChainLock
-------------------------------------------------------------------------------------- */
ChainLock:

if SysVars.SysStopping = 1 then return

lrI = 0
do lkI = 1 to LockOwner.0

   lrI      = lrI + 1
   lr.lrI   = LockOwner.lrI

   if LockOwner.lkI.0 > 0 then do

      lrJ = 0
      do lkJ = 1 to LockOwner.lkI.0

         if LockOwner.lkI.lkJ \= "" then do

            lrJ            = lrJ + 1
            lr.lrI.lrJ     = LockOwner.lkI.lkJ
            lr.lrI.lrJ.0   = LockOwner.lkI.lkJ.0
            lr.lrI.lrJ.1   = LockOwner.lkI.lkJ.1

         End

      End

      lr.lrI.0 = lrJ

   End
   Else
      lr.lrI.0    = 0

End

Drop LockOwner.

do lkI = 1 to lrI

   LockOwner.lkI     = lr.lkI
   LockOwner.lkI.0   = lr.lkI.0

   do lkJ = 1 to lr.lkI.0

      LockOwner.lkI.lkJ    = lr.lkI.lkJ
      LockOwner.lkI.lkJ.0  = lr.lkI.lkJ.0
      LockOwner.lkI.lkJ.1  = lr.lkI.lkJ.1

   End

End

LockOwner.0 = lrI
Drop lr.

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   RemoveLockPro
-------------------------------------------------------------------------------------- */
RemoveLockPro:
Parse Arg lkWho

if SysVars.SysStopping = 1 then return
if lkWho = "" then return

DoChainLock = 0

Nothing = 1
lpO = FindLockOwner( lkWho )
if lpO > 0 then
   do rlI = 1 to LockOwner.lpO.0

      if LockOwner.lpO.rlI \= "" 0 then
         do rlJ = 1 to LockOwner.lpO.rlI.1
            Call RemoveLock lkWho, LockOwner.lpO.rlI
            Nothing = 0
         End

   End

do rlI = 1 to LockAsked.0

   if LockAsked.rlI.0 = lkWho then
      LockAsked.rlI.0 = ""

End

DoChainLock = 1
Call ChainLock

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   AddLockAsked
-------------------------------------------------------------------------------------- */
AddLockAsked:

Parse arg alWho, alTo, alRsrc, alMode

alI               = LockAsked.0 + 1
LockAsked.0       = alI
LockAsked.alI.0   = alWho
LockAsked.alI.1   = alTo
LockAsked.alI.2   = alRsrc
LockAsked.alI.3   = alMode

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   TryLockAsked
-------------------------------------------------------------------------------------- */
TryLockAsked:

if SysVars.SysStopping = 1 then return

do laI = 1 to LockAsked.0

   if LockAsked.laI.0 \= "" then do

      Rc = AddLock( LockAsked.laI.0, LockAsked.laI.2, LockAsked.laI.3 )
      if Rc > 0 then do

         Rc                = LinkMessageQ( LockAsked.laI.1, "SYS_ACK", SysVars.SysLCommit )
         LockAsked.laI.0   = ""
         Call Printf SysVars.SysMonName, MsgLLevelDisplaySyst, "LOCK: "LockAsked.laI.2":"LockAsked.laI.3

      End

   End

End
LockRemoved = 0
LockProcess = 1

Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   TryWakeUp
-------------------------------------------------------------------------------------- */
TryWakeUp:

Do twuI = 1 to LocalProcess.0

   twuPn = LocalProcess.twuI
   if twuPn \= "" then
      do twuJ = 1 to LocalProcess.twuPn.0

         if LocalProcess.twuPn.twuJ \= "" then
            if LocalProcess.twuPn.twuJ.Wait = 1 then
               if LocalProcess.twuPn.twuJ.WaitT > 0 & LocalProcess.twuPn.twuJ.WaitT <= Time('E') then do

                  twuQn = LocalProcess.twuPn.twuJ
                  twuMs = FormatMessage( twuPn"_"twuQn, SysVars.SysMonName, "SYS_ENDWAIT", 0 )
                  Call PushData twuMs

               End

      End

End

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   AddHookProc
-------------------------------------------------------------------------------------- */
AddHookProc:
Arg hpQn, hpWhat

hpI               = SysHookProc.0 + 1
SysHookProc.0     = hpI
SysHookProc.hpI   = hpQn
SysHookProc.hpI.1 = hpWhat
nbSysHookProc     = nbSysHookProc + 1

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FindHookProc
-------------------------------------------------------------------------------------- */
FindHookProc:
Arg fhpQn

do fhpI = 1 to SysHookProc.0

   if SysHookProc.fhpI = fhpQn then
      Return fhpI

End
Return 0
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   RemoveHookProc
-------------------------------------------------------------------------------------- */
RemoveHookProc:
Arg hpQn

hpI = FindHookProc( hpQn )
if hpI > 0 then do

   SysHookProc.hpI = ""
   nbSysHookProc   = nbSysHookProc - 1
   Return nbSysHookProc

End
Return -1
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   LinkHookProc
-------------------------------------------------------------------------------------- */
LinkHookProc:

if SysVars.SysStopping = 1 then Return

do hpI = 1 to SysHookProc.0

   if SysHookProc.hpI \= "" then do

      if SysHookProc.hpI.1 = "PROCESS" & HookProcess = 1 then
         lhRc = LinkMessageQ( SysHookProc.hpI, "MSG_HOOK", msgId )
      Else
      if SysHookProc.hpI.1 = "LOCKS" & LockProcess = 1 then
         lhRc = LinkMessageQ( SysHookProc.hpI, "MSG_HOOK", msgId )

   End

End

HookProcess = 0
LockProcess = 0
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FillProcess
-------------------------------------------------------------------------------------- */
FillProcess:
Arg aQueue
if SysVars.SysStopping = 1 then Return

Signal On Syntax Name FillProcessPass

msgResult   = "ERROR"
Avant       = SetQueue( aQueue )

do I = 1 to LocalProcess.0

   Pn = LocalProcess.I
   if Pn = "" then Iterate

   Call QueueData Pn":"LocalProcess.Pn.0

   do J = 1 to LocalProcess.Pn.0

      if LocalProcess.Pn.J = "" then
         Call QueueData " "
      Else do
         Ln = LocalProcess.Pn.J
         Ln = Ln":"LocalProcess.Pn.J.Loop
         Ln = Ln":"LocalProcess.Pn.J.Lock
         Ln = Ln":"LocalProcess.Pn.J.Wait
         Ln = Ln":"LocalProcess.Pn.J.WaitT
         Ln = Ln":"LocalProcess.Pn.J.TAll
         Ln = Ln":"LocalProcess.Pn.J.TCmd
         Ln = Ln":"LocalProcess.Pn.J.TIdle
         Ln = Ln":"LocalProcess.Pn.J.TElapsed
         Ln = Ln":"LocalProcess.Pn.J.Etat

         Call QueueData Ln
      End

   End

End

msgResult = "DONE"
FillProcessPass:
Rc = SetQueue( Avant )

Signal On Syntax  Name StandardSyntax
Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FillLocks
-------------------------------------------------------------------------------------- */
FillLocks:
Arg aQueue
if SysVars.SysStopping = 1 then Return

Signal On Syntax Name FillLocksPass

msgResult   = "ERROR"
Avant       = SetQueue( aQueue )

do I = 1 to LockOwner.0

   Pn = LockOwner.I
   if LockOwner.I.0 = 0 then Iterate
   Call QueueData Pn":"LockOwner.I.0

   do J = 1 to LockOwner.I.0

      Ln = LockOwner.I.J
      Ln = Ln":"LockOwner.I.J.0
      Ln = Ln":"LockOwner.I.J.1
      Call QueueData Ln

   End

End

Do I = 1 to LockAsked.0

   if LockAsked.I.0 \= "" then do

      Call QueueData "(#)"LockAsked.I.0":"1
      Call QueueData LockAsked.I.2":"LockAsked.I.3

   End

End

msgResult = "DONE"
FillLocksPass:
Rc = SetQueue( Avant )

Signal On Syntax  Name StandardSyntax
Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   QueryTime
-------------------------------------------------------------------------------------- */
QueryTime:
if SysVars.SysStopping = 1 then Return

Signal On Syntax Name QueryTimeError

Avant = SetQueue( CpuQueue )

nbQ = Queued()

do nbQ
   Ln = PullData()
   Parse var Ln pFrom"~"pAll":"pCmd":"pIdle":"pElapsed

   Pn = FormatQueue( pFrom, 'X' )
   Qn = FormatQueue( pFrom )
   dJ = FindSubProcess( Qn )

   if Pos( '.', pAll  ) = 0 then pAll  = pAll'.0000'
   if Pos( '.', pCmd  ) = 0 then pCmd  = pCmd'.0000'
   if Pos( '.', pIdle ) = 0 then pIdle = pIdle'.0000'
   if Pos( '.', pElapsed ) = 0 then pElapsed = pElapsed'.0000'

   pAll     = Format( pAll,, 2 )
   pCmd     = Format( pCmd,, 2 )
   pIdle    = Format( pIdle,, 2 )
   pElapsed = Format( pElapsed,, 2 )

   LocalProcess.Pn.dJ.TAll       = pAll
   LocalProcess.Pn.dJ.TCmd       = pCmd
   LocalProcess.Pn.dJ.TIdle      = pIdle
   LocalProcess.Pn.dJ.TElapsed   = pElapsed

   HookProcess = 1
End

QueryTimeError:
Apres = SetQueue( Avant )

Signal On Syntax  Name StandardSyntax
Return
/* =================================================================================== */
Return
ProcUserHook:
Return

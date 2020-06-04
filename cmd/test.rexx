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

address SYSTEM 'echo $PPID |rxqueue'
Pull SysVars.PID

share                   = "ENVIRONMENT"
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
SysVars.soSay           = "0"
SysVars.SysElapsedIdle  = 0
SysVars.SysElapsedCmd   = 0
SysVars.SysElapsedAll   = 0
SysVars.SysRacine       = ""
SysVars.fileWSOCK       = "/etc/xchglistener.ini"
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
TimeToWait              = 1
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
    Address SYSTEM 'ps -eaf | grep 'psProcessName' | grep -v grep > /tmp/monitor.ps.list'
    Do While Lines('/tmp/monitor.ps.list') > 0
        a=LineIn('/tmp/monitor.ps.list')
         if pos(psProcessName,a) > 0 then do
                tempFirstInstance = 0
                leave
         end
    end
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
         soQ    = FormatQueue( SysVars.SysWhoAmI )
         soName = SysVars.SysRacine"/"soQ
         Rc     = LineOut( soName, Time('L')": "soMsg )
      End

      SysVars.SysInSysout = 0
   End
End
If psoSay = "1" then
   Say soMsg

If SysVars.soSay = "1" then
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
Main:
if ProcessInitialized = 0 then do
   nbW = 0
End

Select
   When msgCmd = Sysvars.SysLInit then do
      Call SysOut "Reçu : INIT"
      Rc = Lock( "TOTO", "X" )
   End
   When msgCmd = Sysvars.SysLIdle then do
      nbW = nbW + 1
      if nbW > 10 then do
         Call Test
         nbW = 0
      End
   End
   When msgCmd = Sysvars.SysLEnd then do
      Call SysOut "Reçu : END"
   End
   OtherWise
      nop
End
Return

Test:
tm = Display( 'Salut !' )
return
Return
ProcUserHook:
Return

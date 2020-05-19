/*************************************************************************************
                           PROLOGUE des outils de MONITOR
                           ==============================
Ce produit est un graticiel (houla...)
Ecrit et maintenu par Julien de Sainte Marie
mailto:julien.desaintemarie@unilog.fr

****************************************************************************************/
Parse Arg ProcessName" "Reste
Trace Off

ProcessName = Strip( ProcessName )
if ProcessName = "" then do
   Parse Source A1 A2 FullProcessName Reste

   ProcessName = FileSpec( "name", FullProcessName )
   Parse var ProcessName ProcessName"."Reste
End

ProcessName = Translate(Strip( ProcessName ))

/*
if Length( ProcessName ) > 8 then do
   Say "Nom du process trop grand (8 car. maximum) !"
   Return
End
*/

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
FirstInstance           = 0
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

if strip(rep)   = "" then rep   = "/home/monitor"
if strip(drive) = "" then drive = ""

curdir         = DIRECTORY()
PipDir         = drive""rep
newdir         = DIRECTORY(PipDir)

if IAmMonitor = 0 then do
   if SysVars.SysDetectMon = "YES" Then /* JSM 13.03.2003 Here */
      if Stream( SysVars.SysProcessLockName, 'c', 'query exists' ) = "" then do
         Say "Le moniteur n'est pas actif !"
         Exit
      End
End
Else do
   Address SYSTEM 'rm lock/* 1>/dev/null 2>&1'
End

if Stream( ProcessLockName, 'c', 'query exists' ) = "" then do
   FirstInstance  = 1
   Rc             = Stream( ProcessLockName, 'c', 'open write' )
   Rc             = Stream( ProcessLockName, 'c', 'close' )
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
      Call RxQueue 'Delete', LocalQueue.I
End

if FirstInstance  = 1 then
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
Trace Off
Arg pArg1
Call SysOut "AllocQueue: "pArg1

oldQ = rxQueue('get')
Parse Var oldQ oldQ"@"ResteX


if pArg1 = '' then do

   aqNom          = RxQueue( "create" )
   Parse Var aqNom aqNom"@"ResteX
   I              = LocalQueue.0
   I              = I + 1
   LocalQueue.I   = aqNom
   LocalQueue.0   = I

End
Else do

   aqNom = RxQueue( 'create', pArg1 )
   Parse Var aqNom aqNom"@"ResteX
   if aqNom = pArg1 then do

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
      fqOk         = RxQueue( "delete", fqArg1 )
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
Trace Off
Arg Arg1
Signal On Syntax Name SetQueueSyntax
Call SysOut "SetQueue: "Arg1

if Arg1 = "" then Signal SetQueueSyntax

Nom = RxQueue( "set", arg1 )
Signal SetQueueRetour

SetQueueSyntax:
Nom = ""

SetQueueRetour:
Signal On Syntax Name StandardSyntax
Call SysOut "SetQueue returns: "Nom
return Nom
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
   PushData
-------------------------------------------------------------------------------------- */
PushData:
Trace Off
Parse Arg aData

Call SysOut "PushData("RxQueue('get')"): "aData

Push aData

Call SysOut "PushData ends"
return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   PullData
-------------------------------------------------------------------------------------- */
PullData:
Trace Off
Parse Arg bWait

Call SysOut "PullData("RxQueue('get')"): "bWait

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

Call SysOut "QueueData("RxQueue('get')"): "aData

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
      if pQueue \= RxQueue( 'get' ) then do

         WMqueue = SetQueue( pQueue )
         if WMqueue = '' then Ok = 0

      End
      Else pQueue = ''
   End

   if Ok = 1 & IAmMonitor = 0 then do

      if NoPost = 0 then
         Ok = PostMessage( , "SYS_BEGINLOOP", RxQueue( 'get' ))

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
      Periode: Durée d'attente exprimée en seconde
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
      Periode: Durée d'attente exprimée en seconde
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
      Call Sleep 0.2
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
      Periode: Durée d'attente exprimée en seconde
      La fonction se réveille si une donnée est dans la file d'attente
-------------------------------------------------------------------------------------- */
SysSleep:
Trace Off
Arg dwT

ssDelay=dwT
if ssDelay <= 0 then ssDelay = 1
Do Forever
   Call Sleep 1
   If Queued() > 0 Then Do
      Leave
   End
   ssDelay = ssDelay - 1
   If ssDelay = 0 Then Leave
End
Return ssDelay
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SysCls
      Efacer l'écran
-------------------------------------------------------------------------------------- */
SysCls:
Trace Off

Address SYSTEM 'clear'
Return
/* =================================================================================== */

/* -----------------------------------------------------------------------------------
   SysFileTree
      Lister les fichiers d'un répertoire
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
       Récupérer l'adresse IP de la tour de controle
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

      Call Sysout "SysMainLoop pulled "rxqueue('get')": "xSysGetCmd

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
/* 
                                          Analyse des journaux TSM 
*/
Main:

/* Plusieurs images du process sont actives ? */
If FirstInstance = 0 then do
   tm = Display("Une autre instance du process est déjà active. Fin de cette instance.")
   EndForce = 1
   Return
End

Address System "hostname" WITH OUTPUT STEM localserv.
lserveur = localserv.1
if Pos(".", lserveur) \= 0 Then do
   Parse var lserveur lserveur"."reste2
End

Select
   /* Fin du process demandée */
   When msgCmd = SysVars.SysLEnd then do
      Nop
   End
   /* Initialisation du process demandée */
   When msgCmd = SysVars.SysLInit then do
      tm = Display("Initialisation du process en cours.")
      
      tmpfile = "/tmp/scandal.log"
      
      tm = Display("Initialisation du process terminée.")
   End
   /* Rien à faire ... */
   When msgCmd = SysVars.SysLIdle then do
      Nop
   End
   When Translate(Word(msgCmd,1)) = "ARCH" then do
      Call doArch
   End
   When Translate(Word(msgCmd,1)) = "SCAN" then do
      ViewdoScan = 0
      Call doScan
   End
   When Translate(Word(msgCmd,1)) = "STAT" then do
      Call doStat
   End
   OtherWise
      Nop
End
Return

/*****************************************************************************************************************
      ARCHIVEr un journal TSM
      -----------------------
      
syntaxe    : arch host=wwwww,file1=xxxxx,file2=yyyyy,target=zzzzz
objectif   : trouver, trier et archiver tout ou partie du(des) journal(ux)
             généré(s) pendant la sauvegarde d'un hôte défini.

*****************************************************************************************************************/
doArch:
/* Trace I */
/* Récupération des arguments ***********************************************************************************/
Parse Var msgCmd x_cmd" "x_host","x_file1","x_file2","x_target
Parse Var x_host p_host"="hostname
Parse Var x_file1 p_file1"="serverfile
Parse Var x_file2 p_file2"="clientfile
say clientfile
Parse var x_target p_target"="targetdirS

/* déclaration et construction de variables *********************************************************************/
serveur=hostname
if Pos(".", serveur) \= 0 Then do
   Parse var serveur serveur"."reste
End
targetdirC = targetdirS
Address System "date +'%A'" WITH OUTPUT STEM purgersi.
ladate_u = date('u')
ladate_s = date('s')

/*****************************************************************************************************************
 Traitement du fichier serverfile, si il existe.
*****************************************************************************************************************/
if serverfile \= "" then do
   tsf       = serverfile
   do while Pos("/",tsf) \= 0 
      parse var tsf asup"/"tsf
   End
   tsfName = ""
   
   if tsf  = "" then do
      Address System "rsh "hostname" ls -tr "serverfile" | grep -v zip" WITH OUTPUT STEM liste.
      
      nbfile   = liste.0
      lastfile = liste.nbfile
      tsfName  = lastfile
      
      Parse Var tsfName "start_backup_prod_"ladate".log"
      
      /* on récupère donc [ladate] au format ddmmyyyy */
      /* on utilise SUBSTR(string, start, [length], [pad]) */
      annee       = SUBSTR(ladate, 5, 4)
      mois        = SUBSTR(ladate, 3, 2)
      jour        = SUBSTR(ladate, 1, 2)
      ladate_u    = mois"/"jour"/"SUBSTR(annee, 3, 2)   /* format mm/dd/yy */
      ladate_s    = annee""mois""jour                   /* format aaaammdd */
   End
   
   tsfWhole    = serverfile""tsfName
   rControl = 0
   If rControl    = 0 Then do
      Parse Var tsfName tsfVorName"."extension
      targetfile    = targetdirS"/"serveur"_backup_prod_"date('s')
      Address System "rsh "hostname" cat "tsfWhole" > "targetfile
      /* Tm   = Display("Action=archive,host="hostname",file="tsfWhole",target="targetdirS)    */
      todsp    = VerifArchive( targetfile, "ARRET DES PROCESS ORACLE APPLICATIONS SUR PRD" )
      Tm   = Display("#>BILAN ARCHIVAGE de "tsfName" depuis "serveur)
      Tm   = Display(">  ETAT : "todsp)
   End
End

/*****************************************************************************************************************
 Traitement du fichier clientfile, si il existe.
*****************************************************************************************************************/
if clientfile \= "" then do
   tcfName = clientfile
   do while Pos("/",tcfName) \= 0 
      parse var tcfName asup"/"tcfName
   End
   
   Parse Var tcfName tcfVorName"."extension
   targetfile2   = targetdirC"/"serveur"_"tcfVorName"_"date('s')
   
   /***************************************************************************************************************
   la partie ci-dessous est a améliorer car, POUR SIGTSM, elle ne fonctionne que si la sauvegarde se finie
   avant minuit... + Vérifier aussi que dans la recherche, la date apparaisse en début de ligne et pas en fin...
   ***************************************************************************************************************/
   
   Address System "rsh "hostname" grep '"ladate_u"   ' "clientfile" > "targetfile2
   todsp    = VerifArchive( targetfile2, "SCHEDULEREC QUERY BEGIN" )
   Tm      = Display("#>BILAN ARCHIVAGE de "tcfName" depuis "serveur)
   Tm      = Display(">  ETAT : "todsp)
End
return

/******************************************************************************************************************
      SCANner le log d'une sauvegarde TSM
      -----------------------------------
      
syntaxe    : scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]
objectif   : fournir un bilan rapide du journal de sauvegarde à analyser.
        + sauvegarde réussie = oui/non
        + durée de la sauvegarde
        + volumetrie sauvegardée
        + afficher message : "plus d'infos, tappez : stats hostname ddmmyyyy"
*******************************************************************************************************************/
doScan:
/* trace i */
Select
   When Pos("host=", msgCmd) = 0 Then Do
      Tm = DISPLAY("ERREUR SYNTAXE : commande doit être :")
      Tm = DISPLAY("q scandal scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]")
      return
   End
   When Pos("inputdir=", msgCmd) = 0 Then Do
      Tm = DISPLAY("ERREUR SYNTAXE : commande doit être :")
      Tm = DISPLAY("q scandal scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]")
      return      
   End
   When Pos("partnames=", msgCmd) = 0 Then Do
      Tm = DISPLAY("ERREUR SYNTAXE : commande doit être :")
      Tm = DISPLAY("q scandal scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]")
      return
   End
   OtherWise
      Nop
End
If Pos(",date", msgCmd) \= 0 then Do
   Parse Var msgCmd x_cmd" "x_host","y_indir","z_pname","w_date
   Parse Var w_date p_wdate"="cejour
   say cejour
   If length(cejour) >= 4 then Do
      jj = SUBSTR(cejour, 1, 2)
      mm = SUBSTR(cejour, 3, 2)
      Select
         When length(cejour) = 8 then Do
            aaaa = SUBSTR(cejour, 5, 4)
         End
         When length(cejour) = 4 then Do
            aaaa = SUBSTR(date('s'), 1, 4)
         End
         OtherWise Do
         Tm = DISPLAY("ERREUR SYNTAXE : date="w_date" erronée. Doit être : date=jjmm[aaaa]")
         return
         End
      End
      jour_S = aaaa""mm""jj
      jour_E = jj"/"mm"/"aaaa
   End
   Else Do
      Tm = DISPLAY("ERREUR SYNTAXE : date="w_date" erronée. Doit être : date=jjmm[aaaa]")
      return
   End
End
Else Do
   Parse Var msgCmd x_cmd" "x_host","y_indir","z_pname
   jour_S = date('s')
   jour_E = date('e')
End
Parse Var x_host p_host"="hostname
Parse Var y_indir p_indir"="inputdir
Parse Var z_pname p_pname"="partnames
Select
   When Length(hostname) = 0 Then Do
      Tm = DISPLAY("ERREUR SYNTAXE : commande doit être :")
      Tm = DISPLAY("q scandal scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]")
      return
   End
   When Length(inputdir) = 0 Then Do
      Tm = DISPLAY("ERREUR SYNTAXE : commande doit être :")
      Tm = DISPLAY("q scandal scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]")
      return      
   End
   When Length(partnames) = 0 Then Do
      Tm = DISPLAY("ERREUR SYNTAXE : commande doit être :")
      Tm = DISPLAY("q scandal scan host=xxxxx,inputdir=yyyyy,partnames=zzzzz[,date=jjmm[aaaa]]")
      return
   End
   OtherWise
      Nop
End


/* déclaration et construction de variables & Initialisation de variables *****************************************/
serveur    = hostname
if Pos(".", serveur) \= 0 then
   Parse var serveur serveur"."reste


info.serveur.1    = ""                                            /* on y stockera info si sauvegarde ok = Y/N    */
info.serveur.2    = ""                                            /* on y stockera info sur durée de sauvegarde   */
info.serveur.3    = ""                                            /* on y stockera info sur volume sauvegardé     */

maxtest.serveur.111 = 0
test.serveur.111 = 0
maxtest.serveur.112 = 0
test.serveur.112 = 0
maxtest.serveur.113 = 0
test.serveur.113 = 0
maxtest.serveur.211 = 0
test.serveur.211 = 0
maxtest.serveur.212 = 0 
test.serveur.212 = 0     

Address System "hostname | cut -f '1' -d '.'" WITH OUTPUT STEM lserv.
localserv = lserv.1

entete = ">  "

fichiers = strip(partnames)
i = 0
Do while length(fichiers) > 0
   i = i + 1
   Parse var fichiers fich.i" "fichiers
End

Cas1 = "dsmsched"
Cas2 = "backup_prod"


Do n=1 to i
   Address System "ls "inputdir"/"serveur"*"fich.n"*"jour_S WITH OUTPUT STEM cefich.
   msg.serveur.020 = "FICHIER ARCHIVE A ANALYSER : //"localserv""cefich.1
   If ViewdoScan = 1 then Do
      Td = DISPLAY("# "msg.serveur.020)
      say entete""msg.serveur.020
   End
   If cefich.0 \= 0 Then Do
      Select
         /* DEBUT - CAS 1 = ANALYSER FICHIER LOG DE TYPE 'dsmsched'            */
         When fich.n = Cas1 Then Do
         /* RECHERCHE 11   : LA SAUVEGARDE A-T'ELLE REUSSIE ?
            test111        : trouver ligne :
                           : mm/dd/yy"   "heure" "Scheduled event '[baratin]' completed successfully.
            position111    : n'importe où
            RESULTATs      : si réussie   : test111 = 1
                           : si échouée   : test111 = 0
         */
            Address System "grep 'Scheduled event' "cefich.1" | grep -ch 'completed successfully'" WITH OUTPUT STEM test111.
            If test111.1 \= 0 then Do
               test.serveur.111 = 1
               msg.serveur.111 = "OK : Présence phrase 'Scheduled event '...' completed successfully'."
            End
            Else Do
               test.serveur.111 = 0
               msg.serveur.111 = "KO : Présence phrase 'Scheduled event '...' completed successfully'."
            End
            maxtest.serveur.111 = 1
            If ViewdoScan = 1 then Do
               Tm = DISPLAY(entete""msg.serveur.111)
               say entete""msg.serveur.111
            End
/*DEBUG-  say "test."serveur".111 = "test.serveur.111    */
            
         /* test112        : trouver ligne avec "Waiting to be contacted by the server."
            etape1121      : définir sa position si elle existe.
                           : si existe pas   : valeur = 0
                           : si existe   : poursuivre
            etape1122      : trouver le nombre de ligne du fichier.
            etape1123      : comparer à la position définie à l'etape1_1_2_1
            RESULTATS      : si =         : test112 = 2
                           : si \=        : test112 = 1
         */
            Address System "grep -nh 'Waiting to be contacted by the server' "cefich.1" | cut -f '1' -d :" WITH OUTPUT STEM etape1121.
            If etape1121.0 \= 0 then Do
               etape.serveur.1121 = etape1121.1
/*DEBUG-     say "etape."serveur".1121 = "etape.serveur.1121   */
               If test1121 \= 0 then Do
                  Address System "wc -l "cefich.1" | awk '{print $1}'" WITH OUTPUT STEM etape1122.
                  if etape1122.0 \= 0 then etape.serveur.1122 = etape1122.1     
/*DEBUG-        say "etape."serveur".1122 = "etape.serveur.1122   */
                  If etape.serveur.1122 = etape.serveur.1121 then Do
                     test.serveur.112 = 2   /* phrase existe en dernière ligne       */
                     msg.serveur.112 = "OK : Présence phrase 'Waiting to be contacted by the server'."
                  End
                  Else Do
                     test.serveur.112 = 1
                     msg.serveur.112 = "PB : Présence phrase 'Waiting to be contacted by the server' mais pas en dernière ligne."
                  End
               End
            End
            Else Do
               test.serveur.112 = 0
               msg.serveur.112 = "KO : Présence phrase 'Waiting to be contacted by the server'."
            End
            maxtest.serveur.112 = 2
            If ViewdoScan = 1 then Do
               Tm = DISPLAY(entete""msg.serveur.112)    
               say entete""msg.serveur.112
            End  
/*DEBUG-  say "test."serveur".112 = "test.serveur.112    */
                   
         /* test113        : vérifier si tous volumes qui devaient être sauvegardés l'ont été,
                           : et définir (comme ça en passant) la durée de sauvegarde pour chaque
                           : volume.                           
            etape1131      : obtenir la liste des volumes à sauvegarder par       
                           : grep "Incremental backup of volume" [fichier] | awk '{print $7}'
                           : compter nombre de volume à sauvegarder = etape1131.0
            etape1132      : pour chaque '/--' si on a une ligne "Successful incremental backup of '/--'" ?
                           : avec la commande 
                           : "grep "etape1131.x" "cefich.1" | grep 'Successful incremental backup of ' | awk '{print $2}'"
                           : stocker l'heure de fin de sauvegarde,
                           : x variant de 1 à etape1131.0
            etape1133      : compter nombre de volume sauvegardés
            RESULTATs      : si etape1132 = etape1131.0   : test113 = 1 & msg113 = ""
                           : si etape1132 \= etape1131.0   : test113 = 0 & msg113 = rapport volumes non sauvegardés / volumes a sauvegarder        
         */
            Address System "grep 'Incremental backup of volume' "cefich.1" | awk '{print $7}'" WITH OUTPUT STEM etape1131.
/*DEBUG-  say "etape1131.0 = "etape1131.0       */
            If etape1131.0 \= 0 then Do
               etape.serveur.1131   = etape1131.0
               etape.serveur.1133   = 0
               Do x=1 to etape1131.0
                  complemt = "'{print $2}'"
                  Address System 'grep "Successful incremental backup of 'etape1131.x'" 'cefich.1' | awk 'complemt WITH OUTPUT STEM etape1132.
                  if etape1132.0 \= 0 then etape.serveur.1133  = etape.serveur.1133 + 1
                  else etape1132.1    = "HS"
                  etape.serveur.1131.x = etape1131.x"_HrEnd-"etape1132.1
               End
               If etape.serveur.1133 = etape.serveur.1131 then Do
                  test.serveur.113  = 1
                  msg.serveur.113 = "OK : "etape.serveur.1133" volumes sauvegardés / "etape.serveur.1131
               End
               Else Do
                     test.serveur.113 = 0
                     msg.serveur.113 = "KO : "etape.serveur.1133" volumes sauvegardés / "etape.serveur.1131
               End
               maxtest.serveur.113 = 1
               If ViewdoScan = 1 then Do
                  Tm = DISPLAY(entete""msg.serveur.113)
                  say entete""msg.serveur.113
               End
/*DEBUG-     say "test."serveur".113 = "test.serveur.113       */ 
            
         /* RECHERCHE 12   : DUREE DE LA SAUVEGARDE
            test121        : (durée sauvegarde complète) trouver ligne 
                           : mm/dd/yy"   "heure" "Elapsed processing time:           "hh:mm:ss                
            position121    : n'importe où
            RESULTATs      : test121 = hh:mm:ss    = durée de la sauvegarde                     
         */
               Address System "grep 'Elapsed processing time:' "cefich.1" | awk '{print $6}'" WITH OUTPUT STEM test121.
               If test121.0 \= "" then Do
                  test.serveur.121 = test121.1
                  Parse var test.serveur.121 hh.serveur.121":"mm.serveur.121":"ss.serveur.121
                  stest.serveur.121 = hh.serveur.121*3600 + mm.serveur.121*60 + ss.serveur.121
                  stest.serveur.121.MAXI = 6*3600
                  If stest.serveur.121.MAXI >= stest.serveur.121 then Do
                     msg.serveur.121 = "OK : Durée de la sauvegarde = "test.serveur.121" < à fenêtre de 6h00."
                  End
                  Else Do
                     msg.serveur.121 = "PB : Durée de la sauvegarde = "test.serveur.121" > à fenêtre de 6h00."
                  End
               End
               Else Do
                  test.serveur.121 = "n/d"
                  msg.serveur.121 = "PB : Durée de la sauvegarde = non définissable."
               End
               If ViewdoScan = 1 then Do
                  Tm = DISPLAY(entete""msg.serveur.121)
                  say entete""msg.serveur.121
               End
/*DEBUG-     say "DUREE de sauvegarde : test."serveur".121 = "test.serveur.121       */
                      
         /* test122        : (durée sauvegarde de chaque volume)
            etape1221      : trouver heure début de sauvegarde du premier volume, avec la commande
                           : grep -h "ANS1898I" [fichier] | head -n 1 | tail -n 1 | awk '{print$2}'
                           : stocker dans etape1131.1
            s/etape1222    : convertir les heures en secondes
            s/etape1223    : faire la différence avec les heures de fin
            RESULTATs      : test122.x = etape1131.x+1 - etape1131.x
         */
               Address System "grep -h 'ANS1898I' "cefich.1" | head -n 1 | tail -n 1 | awk '{print$2}'" WITH OUTPUT STEM etape1221.
/*DEBUG-     say "heure debut de sauvegarde des environnements = "etape1221.1       */
               Hr.serveur.Start = etape1221.1
               Parse var Hr.serveur.Start hh.serveur.122":"mm.serveur.122":"ss.serveur.122
               sHr.serveur.Start = hh.serveur.122*3600 + mm.serveur.122*60 + ss.serveur.122
               Do x=1 to etape.serveur.1131
/*DEBUG-        say "Heure de fin de sauvegarde de volume "etape1131.x     */
                  If Pos("HS", etape.serveur.1131.x) = 0 then Do
                     Parse var etape.serveur.1131.x etape.serveur.1131.x"_HrEnd-"etape.serveur.1222
                     parse var etape.serveur.1222 hh.serveur.122":"mm.serveur.122":"ss.serveur.122
                     setape.serveur.1222    = hh.serveur.122*3600 + mm.serveur.122*60 + ss.serveur.122
                     setape.serveur.1223    = setape.serveur.1222 - sHr.serveur.Start
                     sHr.serveur.Start      = setape.serveur.1222
                     H.serveur.122          = setape.serveur.1223%3600
                     forM.serveur.122       = setape.serveur.1223//3600
                     M.serveur.122          = forM.serveur.122%60
                     S.serveur.122          = forM.serveur.122//60
                     etape.serveur.1223.x   = BonFormat(H.serveur.122,2)":"BonFormat(M.serveur.122,2)":"BonFormat(S.serveur.122,2)
                     Ajout122 = "OK : "
                  End
                  Else Do
                     Parse var etape.serveur.1131.x etape.serveur.1131.x"_HrEnd-"etape.serveur.1222
                     etape.serveur.1223.x = "n/a"
                     Ajout122 = "KO = "
                  End
                  msg.serveur.122.x = Ajout122"Durée de sauvegarde du volume "etape.serveur.1131.x" = "etape.serveur.1223.x
                  If ViewdoScan = 1 then Do
                     Tm = DISPLAY(entete""msg.serveur.122.x)
                     say entete""msg.serveur.122.x
                  End
               End
         
         /* RECHERCHE 13      : volumetrie sauvegardée                                     
               test131        : trouver ligne mm/dd/yy"   "heure" "Total number of bytes transferred:    "volumetrie
               position131    : n'importe où
               RESULTATs      : test131 = volumetrie (ex:146,5 GB) = volumetrie sauvegardée
         */
               Address System "grep 'Total number of bytes transferred:' "cefich.1" | awk '{print $8" "$9}'" WITH OUTPUT STEM test131.
               if test131.0 \= 0 then test.serveur.131 = test131.1
/*DEBUG-     say "test."serveur".131 = "test.serveur.131          */
            End
            Else Do
               /* 
               On prend ici, en compte le cas où la commande :
               "grep 'Incremental backup of volume' "cefich.1" | awk '{print $7}'"
               ne renvoie aucunes correspondances. On défini d'une autre façon les variables :
                  test113
                  test121
                  test131
               */
               /*
                  RECHERCHE 11   : LA SAUVEGARDE A-T'ELLE REUSSIE :
                     test111     : défini plus haut
                     test112     : défini plus haut 
                     test113     : ZERO volumes à sauvegarder."   
                                 : pas utile ici      : test113 = -1
               */
               test.serveur.113 = -1
               maxtest.serveur.113 = -1
/*DEBUG-     say "test."serveur".113 inutile ici donc = "test.serveur.113      */
               
               /* DEBUT MODIFICATION "M1a" : ESAU - DATE : 17.09.2004
                  MOTIF : bug si présence de plusieurs séquences infructueuses :
                        "Querying server for next scheduled event."
                        .... (pas d'information de durée de sauvegarde) ...
                        "Waiting to be contacted by the server."
                        avant la présence d'une séquence contenant les informations de durée de sauvegarde.
                  ISSUE : si on ne peut pas déterminer de durée de sauvegarde
                        m1in  : fichier d'entrée = cefich.1
                        m1out : fichier de sortie temporaire = m1out[x]
                        m1_1  : on cherche la position de la ligne "Querying server for next scheduled event."
                        tant que m1_1 > 0, alors
                           m1a2  : on cherche le nb de ligne du fichier
                           m1a2  : on en déduit le nouveau nb de ligne fichier (à créer) = m1a2 - m1a1
                           m1a3  : on copie les m1_2 dernières lignes du fichier [m1ain] dans un fichier temporaire [m1aout]
                                 : on recherche la durée de sauvegarde : cf. test121
                           si durée non trouvée, on boucle
                           si durée déterminée, on quite  (m1a1 = 0)                      
               */
               m1a1 = 1
               m1acount = 1
               m1ain = cefich.1
               Do while m1a1 > 0
                  Address System "grep -nh 'Querying server for next scheduled event.' "m1ain" | cut -f '1' -d :" WITH OUTPUT STEM m1a1.
                  If m1a1.0 > 0 then Do
                     m1a1 = m1a1.1
                     say "m1a1 : "m1a1
                     Address System "wc -l "m1ain" | awk '{print $1}'" WITH OUTPUT STEM m1a2.
                     m1a2 = m1a2.1
                     say "m1a2 : "m1a2
                     m1a2 = m1a2 - m1a1
                     say "m1a2 : "m1a2
                     m1aout = inputdir"/"M1_OUT""m1acount
                     Address System "tail -n "m1a2" "m1ain" > "m1aout
                     /*
                     RECHERCHE 12   : DUREE DE LA SAUVEGARDE
                        test121     : faire la différence entre heure début et heure fin
                        etape121    : trouver phrase "SCHEDULEREC QUERY BEGIN" par la commande
                                    : grep "SCHEDULEREC QUERY BEGIN" [fichier] | awk '{print $2}'
                                    : renvoi normalement deux valeurs
                        RESULTATs   : si etape121.0 = 2  : test121 = etape121.2 - etape121.1
                                                         : test121 = "PB"
                     */
                     Address System "grep 'SCHEDULEREC QUERY BEGIN' "m1aout" | awk '{print $2}'" WITH OUTPUT STEM etape121.
                     etape.serveur.121 = etape121.0
                     say "etape.serveur.121 = "etape.serveur.121
/*DEBUG-     say "etape."serveur".121 = "etape.serveur.121           */
                     If etape.serveur.121 = 2 then Do
                        Do y=1 to etape.serveur.121
                           Parse var etape121.y hh.serveur.121":"mm.serveur.121":"ss.serveur.121
                           setape.serveur.121.y = hh.serveur.121*3600 + mm.serveur.121*60 + ss.serveur.121
                        End
                        stest.serveur.121 = setape.serveur.121.2 - setape.serveur.121.1
                        H.serveur.121     = stest.serveur.121%3600
                        forM.serveur.121  = stest.serveur.121//3600
                        M.serveur.121     = forM.serveur.121%60
                        S.serveur.121     = forM.serveur.121//60
                        test.serveur.121  = BonFormat(H.serveur.121,2)":"BonFormat(M.serveur.121,2)":"BonFormat(S.serveur.121,2)
                        stest.serveur.121.MAXI = 2*3600
                        If stest.serveur.121.MAXI >= stest.serveur.121 Then Do
                           msg.serveur.121 = "OK : Durée de sauvegarde = "test.serveur.121" < à fenêtre de 2h00."
                           m1a1 = 0
                        End
                        Else Do
                           msg.serveur.121 = "PB : Durée de sauvegarde = "test.serveur.121" > à fenêtre de 2h00."
                           m1a1 = 0
                        End
/*DEBUG-        say "   : Durée de sauvegarde : test."serveur".121 = "test.serveur.121          */
                     End
                     If etape.serveur.121 < 2 Then Do
                        test.serveur.121 = "n/d"
                        msg.serveur.121 = "PB : Durée de sauvegarde = non définissable."
                        m1a1 = 0
                     End
                     If etape.serveur.121 > 2 then Do
                     m1a1 = 1
                     End
                     m1ain = m1aout
                     say m1ain
                     m1acount = m1acount + 1
                  End
                  Else Do
                     test.serveur.121 = "n/d"
                     msg.serveur.121 = "PB : Durée de sauvegarde = non définissable."
                     m1a1 = 0
                  End
               End
               If ViewdoScan = 1 then Do
                  Tm = DISPLAY(entete""msg.serveur.121)
                  say entete""msg.serveur.121
                  nettoyage = inputdir"/"M1_OUT"*"
                  Address System "rm -f "nettoyage
               End
               /* FIN MODIFICATION "M1" : ESAU */

               /*                                   
                  RECHERCHE 13   : VOLUMETRIE SAUVEGARDEE
                     test131     : pas définissable   : test131 = -1
               */
               test.serveur.131 = -1
/*DEBUG-     say "test131 indéfinissable ici donc = "test.serveur.131      */
            End
         End
         
         /* DEBUT - CAS 2 = ANALYSER FICHIER LOG DE TYPE 'backup_prod'     */
         When fich.n = Cas2 Then Do
         /* RECHERCHE 21   : LA SAUVEGARDE A-T'ELLE REUSSIE                                    
               test211     : trouver ligne "Sauvegarde completee avec succes"
               position211 : dernière ligne
         */
               Address System "grep 'Sauvegarde completee avec succes' "cefich.1 WITH OUTPUT STEM test211.
               If test211.0 \= 0 then Do
                  test.serveur.211 = 1
                  msg.serveur.211 = "OK : Présence phrase 'Sauvegarde completee avec succes'."
               End
               Else Do
                  test.serveur.211 = 0
                  msg.serveur.211 = "KO : Présence phrase 'Sauvegarde completee avec succes'."
               End
               maxtest.serveur.211 = 1
               If ViewdoScan = 1 then Do
                  Tm = DISPLAY(entete""msg.serveur.211)
                  say entete""msg.serveur.211
               End
/*DEBUG-     say "test."serveur".211 = "test.serveur.211          */
                                          
         /*    test212     : compter le nombre d'erreurs, par la commande
                           : grep -i -p -c -h " error " [fichier] 2> /dev/null   
         */
               Address System "grep -i -p -c -h ' error ' "cefich.1" 2> /dev/null" WITH OUTPUT STEM test212.
               If test212.1 = 0 then Do
                  test.serveur.212 = 1
                  msg.serveur.212 = "OK : "test212.1" message(s) 'error' trouvé(s)."
               End
               Else Do
                  test.serveur.212 = 0
                  msg.serveur.212 = "KO : "test212.1" message(s) 'error' trouvé(s)."
               End
               maxtest.serveur.212 = 1
               If ViewdoScan = 1 then Do
                  Tm = DISPLAY(entete""msg.serveur.212)
                  say entete""msg.serveur.212
               End
/*DEBUG-     say "test."serveur".212 = "test.serveur.212          */
               
         /*    test213     : lister les erreurs 'si on veut avoir la liste plus tard', par la commande
                           : grep -n -i -p -h " error " [fichier] 2> /dev/null
         */
               Address System "grep -n -i -p -h ' error ' "cefich.1" 2> /dev/null" WITH OUTPUT STEM test213.
               If test213.0 \= 0 then Do
                  Do z=1 to test213.0
                     msg.serveur.213.z = test213.z
                     If ViewdoScan = 1 then Do
                        Tm = DISPLAY(entete""msg.serveur.213.z)
                        say entete""msg.serveur.213.z
                     End
                  End
               End
               Else Do
                  test.serveur.213 = 0
               End
/*DEBUG-     say "test."serveur".213 = "test.serveur.213       */
         End
         OtherWise do
            Nop
         End
         
      End
   End
End

/* DEBUT BILAN DE SAUVEGARDE */

/*DEBUG-  say "maxtest.serveur.111 = "maxtest.serveur.111      */
/*DEBUG-  say "test.serveur.111 = "test.serveur.111            */
/*DEBUG-  say "maxtest.serveur.112 = "maxtest.serveur.112      */
/*DEBUG-  say "test.serveur.112 = "test.serveur.112            */
/*DEBUG-  say "maxtest.serveur.113 = "maxtest.serveur.113      */
/*DEBUG-  say "test.serveur.113 = "test.serveur.113            */
/*DEBUG-  say "maxtest.serveur.211 = "maxtest.serveur.211      */
/*DEBUG-  say "test.serveur.211 = "test.serveur.211            */
/*DEBUG-  say "maxtest.serveur.212 = "maxtest.serveur.212      */
/*DEBUG-  say "test.serveur.212 = "test.serveur.212            */

/* 1.1 SAUVEGARDE REUSSIE ?                                    */
maxtest.serveur.ALL = maxtest.serveur.111 + maxtest.serveur.112 + maxtest.serveur.113 + maxtest.serveur.211 + maxtest.serveur.212
test.serveur.ALL = test.serveur.111 + test.serveur.112 + test.serveur.113 + test.serveur.211 + test.serveur.212
info.serveur.4    = "; (+)INFOS : q scandal stat "serveur" [jjmm[aaaa]]"
If test.serveur.ALL = maxtest.serveur.ALL then Do
   info.serveur.1 = "ETAT : OK "
End
Else Do
   info.serveur.1 = "ETAT : PB "
   Select
      When serveur = "sigmineur" then Do
         If test.serveur.111 = 0 then Do
          info.serveur.1 = "ETAT : KO "
         End
         If test.serveur.112 = 0 then Do
            info.serveur.1 = "ETAT : KO "
         End
      End
      When serveur = "sigtsm" then Do
         If test.serveur.211 = 0 then Do
            info.serveur.1 = "ETAT : KO "
         End
      End
   End
End
   
/* 1.2 DUREE DE SAUVEGARDE ?                                     */
info.serveur.2 = "; DUREE : "test.serveur.121"  "
If Pos("PB", msg.serveur.121) \= 0 then Do
   if Pos("KO", info.serveur.1) = 0 then info.serveur.1 = "ETAT : PB "
End

/* 1.3 VOLUMETRIE SAUVEGARDEE ?   */
if test.serveur.131 < 0 then info.serveur.3 = "; VOLUMETRIE : n/a "
else info.serveur.3 = "; VOLUMETRIE : "test.serveur.131"  "

/* AFFICHAGE DU BILAN                                             */
msgBilandoScan1 = "#>BILAN ANALYSE SAUVEGARDE de ["serveur"] depuis fichier(s) archive(s) du "jour_E
msgBilandoScan2 = entete""info.serveur.1""info.serveur.2""info.serveur.3""info.serveur.4
Tm   = Display(msgBilandoScan1)
Tm   = Display(msgBilandoScan2)
say msgBilandoScan1
say msgBilandoScan2
/* FIN BILAN DE SAUVEGARDE */

return
/************************************************************************************************
      STATistique d'une sauvegarde depuis les archives des fichiers log
      -----------------------------------------------------------------
      
      
syntaxe : stat hostname [jjmm[aaaa]]

*/
doStat:
Parse Var msgCmd x_cmd" "options
options = strip(options)
i = 0
Do while length(options) > 0
   i = i + 1
   Parse var options option.i" "options
End
Select
   When i=2 then Do
      If length(option.2) >= 4 then Do
         jj = SUBSTR(option.2, 1, 2)
         mm = SUBSTR(option.2, 3, 2)
         Select
            When length(option.2) = 8 then Do
               aaaa = SUBSTR(option.2, 5, 4)
            End
            When length(option.2) = 4 then Do
               aaaa = SUBSTR(date('s'), 1, 4)
            End
            OtherWise Do
            Tm = DISPLAY("ERREUR SYNTAXE : date="option.2" erronée. Doit être : date=jjmm[aaaa]")
            return
            End
         End
         jour_S = jj""mm""aaaa
         jour_E = jj"/"mm"/"aaaa
      End
      Else Do
         Tm = DISPLAY("ERREUR SYNTAXE : date="option.2" erronée. Doit être : date=jjmm[aaaa]")
         return
      End
   End
   When i=1 then Do
      jour_S = SUBSTR(date('s'), 7, 2)""SUBSTR(date('s'), 5, 2)""SUBSTR(date('s'), 1, 4)
      jour_E = date('e')
   End
   OtherWise Do
      Tm = DISPLAY("ERREUR SYNTAXE : "msgCmd" erronée. Doit être : stat hostname [jjmm[aaaa]]")
      return
   End
End
   

FichNoeEnv = "/home/exploit/scripts/noe/project_noe/ini/EXPL-TSM-ANA.env"
If Stream(FichNoeEnv, 'c', 'query exists' ) \= "" then Do
   Address System "grep 'scan' "FichNoeEnv" | grep "option.1 WITH OUTPUT STEM redoStat.
   if redoStat.0 \= "" then do
      Parse var redoStat.1 baratin'"q scandal 'msgCmd'"'
      msgCmd = msgCmd",date="jour_S
      say msgCmd
      ViewdoScan = 1
      Call doScan
   End
End
Else
   say "Le fichier "FichNoeEnv" est introuvable."

return

/*************************************************************************************
FONCTIONS
**************************************************************************************/

/* ---- VerifArchive : DEBUT ---- */
VerifArchive:
Parse Arg tgf, cetxt
tgfName = tgf
do while Pos("/",tgfName) \= 0 
      parse var tgfName asup"/"tgfName
   End
If Stream(tgf, 'c', 'query exists' ) = "" then
   todsp    = "Nok -- Archivage : Nok | Nom : n/a  | Contenu : n/a"
Else Do
   /* Fichier créé       : OK                     */
   /* RECHERCHE : SAVOIR SI FICHIER EST VIDE OU NON               */
   If Stream(tgf, 'c', 'query size' ) = 0 then
      /* Fichier créé       : OK                     */
      /* Fichier nonvide    : KO                     */
      todsp    = "Nok -- Archivage : Ok  | Nom : "tgfName"  | Contenu : VIDE"
   Else Do
      /* Fichier créé       : OK                     */
      /* Fichier nonvide    : OK                     */
      /* RECHERCHE : SAVOIR SI FICHIER CONTIENT DES INFORMATIONS DE BACKUP      */
      Address System "grep -c '"cetxt"' "tgf WITH OUTPUT STEM test.
      if test.1 > 0 then
          /* Fichier créé       : OK                  */
          /* Fichier nonvide       : OK                  */
          /* Fichier type sauvegarde   : OK                  */
      todsp    = "Ok  -- Archivage : Ok  | Nom : "tgfName"  | Contenu : INFOS BACKUP"
      Else
          /* Fichier créé       : OK                  */
          /* Fichier nonvide       : OK                  */
          /* Fichier type sauvegarde   : KO                  */
      todsp    = "Nok -- Archivage : Ok  | Nom : "tgfName"  | Contenu : INVALIDE"
   End
End

return todsp
/* ---- VerifArchive : FIN ---- */


/*
BesoinArchive:
Parse Arg ladate_u, fic1, fic2, ladate_s
   Address System "grep -hc '"ladate_u"   ' "targetdirS"/"hostname"*_Arch_*" WITH OUTPUT STEM Controla.
   sommeA = 0
   Do indexA = 1 to Controla.0
      sommeA = sommeA + controla.indexA
   End
   Address System "find "targetdirS" -name '"hostname"*"ladate"_Arch*' | grep -c ''" WITH OUTPUT STEM Controlb.
   rControl = sommeA + Controlb.1
return barch
*/

/* ---- BonFormat : DEBUT ---- */
BonFormat:
Arg lechiffre, nbcaract
   do while length(lechiffre) << nbcaract then
      if length(lechiffre) >= nbcaract then leave
      lechiffre = "0"lechiffre
   end
return lechiffre
/* ---- BonFormat : FIN ---- */Return
ProcUserHook:
Return

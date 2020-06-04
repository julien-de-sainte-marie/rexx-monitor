/*************************************************************************************
                                 SendMessage
                           ======================
                           
!!! La fonction "PostMessage()" ne fait pas un "Queue()" mais un "Push()" !!!
                           
****************************************************************************************/
Parse Arg ArgAll
ArgAll = Strip( ArgAll )

if ArgAll = '/help' | ArgAll = '-help' | ArgAll = '--help' | ArgAll = "" then do
   Say "Syntaxe:  Q [To[, Id,] ]Data | -help"
   Say "-------"
   Say ""
   Say "To     : Destinataire. Si omis, le moniteur sera destinataire."
   Say "Id     : Identifiant de message. Si omis, USR_MSG sera utilis."
   Say "Data   : Donnes  fournir (pas de conversion en majuscules)."
   Say ""
   Say ""
   Say "Exemples:"
   Say "--------"
   Say ""
   Say "Q COMMAND,USR_CMD,dir                -> "
   Say "Q COMMAND,,list des process          -> COMMAND,USR_MSG,list des ..."
   Say "Q END                                -> MONITOR,USR_MSG,END"
   Say "Q MAIL SEND MSG TO TOTO message      -> MAIL,USR_MSG,SEND MSG TO TOTO ..."
   Exit
End

SysLockPath             = "./lock/"
SysQueue                = "PIPE"
SysSendMessageQueue     = ""
SysMonName              = "MONITOR"
SysLTimeOut             = "!TIMEOUT!"
SysLHalt                = "!HALT!"
SysLIdle                = "!IDLE!"
SysLInit                = "!INIT!"
SysLEnd                 = "END"
SysWhoAmI               = ""
SysStopping             = 0
SysTimerEnabled         = 0
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
ProcessLockName         = SysLockPath""ProcessName".LOK"
EndForce                = 0

CreatedProcess          = 1
SysWhoAmI               = "SYSTEM_"RxQueue('get')

b = Pos( ' ', ArgAll )
v = Pos( ',', ArgAll )

if (v > 0 & v < b) | b = 0 then
   Parse Var ArgAll pTo","pId","pData
Else do
   Parse Var ArgAll pTo" "pData
   pId = ""
End

pTo   = Translate( Strip( pTo ))
pId   = Translate( Strip( pId ))
pData = Strip( pData )

if pTo \= "" & pId = "" & pData = "" then do
   pData = pTo
   pTo   = SysMonName
End

if pTo = "" then do
   pTo = SysMonName
end  /* Do */

if pId = "" then do
   pId = "USR_MSG"
end  /* Do */

Ok = PostMessage( pTo, pId, pData )
if Ok = 0 then
   Say "ERR: To="pTo", Id="pId", Data="pData

do I = 1 to LocalQueue.0
   if LocalQueue.I \= "" then do
      fqOk         = RxQueue( "delete", LocalQueue.I )
      LocalQueue.I = ""
   End
End
Call RxQueue 'delete', RxQueue('get')

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
Arg pArg1

if pArg1 = '' then do

   aqNom          = RxQueue( "create" )
   I              = LocalQueue.0
   I              = I + 1
   LocalQueue.I   = aqNom
   LocalQueue.0   = I

End
Else do

   aqNom = RxQueue( 'create', pArg1 )
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

return aqNom
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FreeQueue
-------------------------------------------------------------------------------------- */
FreeQueue:
Arg fqArg1

fqOk = 9
do I = 1 to LocalQueue.0
   if LocalQueue.I = fqArg1 then do
      LocalQueue.I = ""
      fqOk         = RxQueue( "delete", fqArg1 )
      Leave
   End
End

return fqOk
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FormatQueue
-------------------------------------------------------------------------------------- */
FormatQueue: PROCEDURE
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
SetQueue: PROCEDURE
Arg Arg1
Signal On Syntax Name SetQueueSyntax

if Arg1 = "" then Signal SetQueueSyntax

Nom = RxQueue( "set", arg1 )
Signal SetQueueRetour

SetQueueSyntax:
Nom = ""

SetQueueRetour:
Signal On Syntax Name StandardSyntax
return Nom
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   FormatMessage
-------------------------------------------------------------------------------------- */
FormatMessage: PROCEDURE
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
GetDate: PROCEDURE
Dd = Date('S')
Dj = Left( Dd, 4 )'/'Substr( Dd, 5, 2 )'/'Right( Dd, 2 )
return Dj
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   GetTime
-------------------------------------------------------------------------------------- */
GetTime: PROCEDURE
Hj = Left( Time('L'), 11 )
return Hj
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   PostMessage
-------------------------------------------------------------------------------------- */
PostMessage: PROCEDURE EXPOSE SysQueue SysWhoAmI SysMonName
Parse Arg msgTo, msgId, msgData

Signal On Syntax Name PostMessageError

if msgTo = '' then msgTo = SysMonName

MSG = FormatMessage( SysWhoAmI, msgTo, msgId, msgData )
if MSG = 0 then
   Ok = 0
Else do

   Avant = SetQueue( SysQueue )
   if Avant \= "" then do

      Push MSG
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
return Ok
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   CreateProcess
-------------------------------------------------------------------------------------- */
CreateProcess:
Arg cpProcess

if cpProcess \= "" then do

   CreatedProcess = 1
   SysWhoAmI      = cpProcess'_'AllocQueue()
   ParentQ        = SetQueue( FormatQueue( SysWhoAmI ))
   Ok             = SendMessage( , "SYS_BEGINPROCESS" )

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

if CreatedProcess = 1 then
   Ok = PostMessage( , "SYS_ENDPROCESS" )

CreatedProcess = 0
Tm             = SetQueue( ParentQ )
Tm             = FreeQueue( FormatQueue( SysWhoAmI ))

return Ok
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   Display
-------------------------------------------------------------------------------------- */
Display:
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
WaitMessage: PROCEDURE EXPOSE SysQueue SysWhoAmI CreatedProcess SysMonName IAmMonitor SysLTimeOut SysLHalt CtrlAttn isCreating WaitingIdle
Arg pQueue, pTimeOut, NoPost

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

      MSG = WaitMessageLoop()

      if NoPost = 0 then
         Ok = PostMessage( , "SYS_ENDLOOP" )

   End
   else
   if Ok = 1 then do

      MSG = WaitMessageLoop()

   End

   if WMqueue \= '' then do

      tm = SetQueue( WMQueue )

   End

End
Else
   Ok = 0

if Ok = 0 then MSG = "!ERROR!"
if MSG = SysLHalt then CtrlAttn = 1
if MSG = '!STOP!' then CtrlAttn = 1

if Left( MSG, 8 ) = "SYS_ACK~" then
   Parse Var MSG dummy"~"MSG

tWaitingIdle = Time('E') - tWaitingIdle
WaitingIdle  = WaitingIdle + tWaitingIdle

Return MSG
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   WaitMessageLoop
-------------------------------------------------------------------------------------- */
WaitMessageLoop:
Signal On Halt Name WaitMessageLoopStop

TimeStart = Time('E')
do Forever

   if Queued() > 0 then do
      smRC = LineIn( "QUEUE:" )
      Leave
   End

   Call SysSleep 0.1
   if pTimeOut > 0 then do

      TimeElapsed = Time('E') - TimeStart
      smRC        = SysLTimeOut
      if TimeElapsed >= pTimeOut then Leave

   End

End
return smRC

WaitMessageLoopStop:
Signal On Halt Name StandardHalt

smRC = SysLHalt
return smRC
/* =================================================================================== */


/* -----------------------------------------------------------------------------------
   SendMessage
-------------------------------------------------------------------------------------- */
SendMessage:
Parse Arg msgTo, msgId, msgData

if SysSendMessageQueue = "" then
   SysSendMessageQueue = AllocQueue()

smRc = PostMessage( msgTo, msgId':'SysSendMessageQueue, msgData )

if smRc > 0 then do
   If IAmMonitor = 0 then
      smRc = WaitMessage( SysSendMessageQueue, 0, 0 )
   Else
      smRc = "OK"
End
Else smRc = ""

return smRC
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   ProcStartTimer
-------------------------------------------------------------------------------------- */
ProcStartTimer:
Arg Delai

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

Ok = PostMessage( , "SYS_ENDWAIT", 0 )

Return
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   GetProfileString
-------------------------------------------------------------------------------------- */
GetProfileString: PROCEDURE EXPOSE SysMonName
Arg gpsFile, gpsSection, gpsKey, gpsDefault

if Strip( gpsFile ) = '' then
   gpsFile = 'ini/'SysMonName'.ini'
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

return gpsBuffer
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   WriteProfileString
-------------------------------------------------------------------------------------- */
WriteProfileString: PROCEDURE EXPOSE SysMonName
Arg gpsFile, gpsSection, gpsKey, gpsDefault

if Strip( gpsFile ) = '' then
   gpsFile = 'ini/'SysMonName'.ini'
else
   gpsFile = Strip( gpsFile )

tmpFile   = SysTempFileName( 'ini/temp????' )
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

            'Copy 'tmpFile' 'gpsFile' 0>nul 1>nul 2>nul'
            'Del 'tmpFile' 0>nul 1>nul 2>nul'

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

return gpsBuffer
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   MkSpace
-------------------------------------------------------------------------------------- */
MkSpace:
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
Parse arg lkRsrc, lkMode

lkRsrc = Translate( Strip( lkRsrc ))
lkMode = Translate( Strip( lkMode ))
lkRc   = ""

if lkRsrc \= "" then
   if lkMode = "S" | lkMode = "X" then
      lkRc = SendMessage( , "SYS_LOCK", lkRsrc':'lkMode )

return lkRc
/* =================================================================================== */



/* -----------------------------------------------------------------------------------
   UnLock
-------------------------------------------------------------------------------------- */
UnLock:
Parse arg lkRsrc

lkRsrc = Translate( Strip( lkRsrc ))
lkRc   = ""

if lkRsrc \= "" then
   lkRc = SendMessage( , "SYS_UNLOCK", lkRsrc )

return lkRc
/* =================================================================================== */


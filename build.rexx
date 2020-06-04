/* */
Trace Off 
Parse Arg pProcess" "Reste

Do While Queued() > 0
   Parse Pull L
End

address SYSTEM 'echo $PPID |rxqueue'
pull Pid

if pProcess = "" then
   Say "NOM OBLIGATOIRE (all pour tous)"
else
if Pos( "all", pProcess ) > 0 then do

   Rc = SysFileTree( "pgm", "Liste", "F" )
   Do flI = 1 to Liste.0
      Nom = Liste.flI
      Call OneBuild Nom
   end /* do */

end  /* Do */
else
   Call OneBuild pProcess

Address SYSTEM 'rm cmd/.rexx 1>/dev/null 2>&1'

Return

OneBuild:
Parse Arg Process
Say "BUILDING : " Process
Line  = Translate( Strip( LineIn( "pgm/"Process )))
Rc    = Stream( "pgm/"Process, "c", "close" )

if Pos( "ISPF_LOCAL", Line ) > 0 then
   NoCompil = 1
Else 
if Pos( "ISPF", Line ) > 0 then
   NoCompil = 2
Else
   NoCompil = 0

Address SYSTEM "cat skl/prologue.skl skl/api.skl skl/loop.skl pgm/"Process" > cmd/"Process".tmp"
Address SYSTEM "echo Return >> cmd/"Process".tmp"
Address SYSTEM "echo ProcUserHook: >> cmd/"Process".tmp"
Address SYSTEM "echo Return >> cmd/"Process".tmp"

Select 
   When NoCompil = 0 Then Do
      Address SYSTEM "rm cmd/"Process".rexx 1>/dev/null 2>&1"
      Address SYSTEM "mv cmd/"Process".tmp cmd/"Process".rexx"
   End
   When NoCompil = 1 Then Do
      Address SYSTEM "cat cmd/"Process".tmp skl/ispexec.skl > cmd/"Process".rexx"
      Address SYSTEM "rm cmd/"Process".tmp 1>/dev/null 2>&1"
   End
   When NoCompil = 2 Then Do
      Address SYSTEM "rm cmd/"Process".isp 1>/dev/null 2>&1"
      Address SYSTEM "mv cmd/"Process".tmp cmd/"Process".isp"
   End 
End

Return

SysFileTree:
Parse arg vPath, vStem, vX

Address SYSTEM 'ls 'vPath' | rxqueue'
nbFic = 0
Do While Queued() > 0
   Parse Pull L
   Do nF = 1 to Words(L)
      nbFic = nbFic + 1
      sF = Value(vStem)"."nbFic" = '"Word(L, nF)"'"
      Interpret sF
   End
End
sF = Value(vStem)'.0 = 'nbFic

Interpret sF

Return 0

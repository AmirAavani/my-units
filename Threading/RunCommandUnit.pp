unit RunCommandUnit;
{Imported from
https://wiki.freepascal.org/Executing_External_Programs#Runcommand_extensions
}
//{$mode objfpc}{$H+}
{$mode delphi}

interface
uses
  process;

function RunCommandTimeout(const ExeName: AnsiString;
    const Commands: array of TProcessString;
    out Outputstring: AnsiString;
    out ErrorString: AnsiString;
    Options: TProcessOptions = [];
    SWOptions: TShowWindowOptions = swoNone;
    Timeout: Integer = 60): Boolean;
function RunCommand(const ExeName: AnsiString;
    const Commands: array of TProcessString;
    out Outputstring: AnsiString): Boolean;

implementation

uses
    classes, sysutils, dateutils;

type
{ TProcessTimeout }
  TProcessTimeout = class(TProcess)
  private
    TimeoutPeriod: TTime;
    TimedOut : Boolean;
    Started : TDateTime;
     procedure LocalnIdleSleep(Sender, Context : TObject;status:TRunCommandEventCode;
         const Message:string);
  end;


procedure TProcessTimeout.LocalnIdleSleep(Sender, Context: TObject;
    Status: TRunCommandEventCode; const Message:string);
begin
  if Status = RunCommandIdle then
   begin
     if (Now - Started) > TimeoutPeriod then
        begin
          TimedOut := True;
          Terminate(255);
          Exit;
        end;

     Sleep(RunCommandSleepTime);
   end;
end;

function RunCommandTimeout(const ExeName: AnsiString;
  const Commands: array of TProcessString;
  out Outputstring: AnsiString;
  out ErrorString: AnsiString;
  Options: TProcessOptions; SWOptions: TShowWindowOptions; Timeout: Integer
  ): Boolean;
Var
   p : TProcessTimeout;
   i,
   exitstatus : integer;

begin
 p := TProcessTimeout.create(nil);
 p.OnRunCommandEvent:= p.LocalnIdleSleep;
 p.timeoutperiod:=timeout/SecsPerDay;
 if Options<>[] then
   P.Options:=Options - [poRunSuspended,poWaitOnExit];
 p.options:=p.options+[poRunIdle]; // needed to run the RUNIDLE event. See User Changes 3.2.0

 P.ShowWindow:=SwOptions;
 p.Executable:=exename;
 if high(commands)>=0 then
  for i:=low(commands) to high(commands) do
    p.Parameters.add(commands[i]);
 p.timedout:=false;
 p.started:=now;
 try
   // the core loop of runcommand() variants, originally based on the "large output" scenario in the wiki, but continously expanded over 5 years.
   Result:= p.RunCommandLoop(Outputstring, ErrorString, exitstatus)=0;
   if p.TimedOut then
     Result := False;

 finally
   p.free;

 end;

 Result := exitstatus <> 0;
end;

function RunCommand(const ExeName: AnsiString;
  const Commands: array of TProcessString; out Outputstring: AnsiString
  ): Boolean;
begin
  Result := process.RunCommand(ExeName, Commands, Outputstring, [], swoNone);

end;

{
// example use:

var s : string;
begin
 for s in FileList do
   begin
      if not RunCommandTimeout('someexe',['-v',s,'--output','dest\'+s],err,[],swoNone,60) then
         begin
           // failed to run or timeout. e.g. movefile() it to a "failed" dir.
        end
     else
       begin
        // ok, move file to "done" directory.
       end;
   end;
end;
}

end.


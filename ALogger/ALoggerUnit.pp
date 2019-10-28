unit ALoggerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TALogger }

  TALogger = class(TObject)
  private
  public
    constructor Create;
    destructor Destroy; override;

  end;

procedure DebugLn(Msg: AnsiString);
procedure WriteLn(Msg: AnsiString);
procedure FatalLn(Msg: AnsiString);

implementation

uses ParameterManagerUnit;

procedure DebugLn(Msg: AnsiString);
begin
  if RunTimeParameterManager.BoolValueOrDefault['Debug', True] then
  begin
    System.Writeln(Format('%s] %s', [DateTimeToStr(Now), Msg]));
    Flush(Output);
  end;
end;

procedure WriteLn(Msg: AnsiString);
begin
  System.Writeln(Format('%s-%d] %s', [DateTimeToStr(Now), GetThreadID, Msg]));

  Flush(Output);
end;

procedure FatalLn(Msg: AnsiString);
begin
  WriteLn('Fatal:' + Msg);
  Halt(1);

end;

{ TALogger }

constructor TALogger.Create;
begin
  inherited Create;

end;

destructor TALogger.Destroy;
begin
  inherited Destroy;
end;

end.


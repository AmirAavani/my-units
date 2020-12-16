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

procedure DebugLn(Msg: AnsiString; Verbosity: Integer = 0);
procedure FMTDebugLn(Fmt: AnsiString; const Args : array of const; Verbosity: Integer = 0);
procedure DebugLnEveryN(N: Integer; Msg: AnsiString; Verbosity: Integer = 0);
procedure FMTDebugLnEveryN(N: Integer; Fmt: AnsiString; const Args : array of const; Verbosity: Integer = 0);
procedure FatalLn(Msg: AnsiString);
procedure FmtFatalLn(Fmt: AnsiString; const Args: array of const);

implementation

uses
  ParameterManagerUnit, StringUnit, WideStringUnit, SyncUnit, lnfodwrf, fgl;

var
  Mutex4LineInfo: TMutex;

procedure GetParentLineInfo(var Filename: AnsiString; var LineNumber: Integer);
var
  i: Integer;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: CodePointer;
  Func, Source: ShortString;
  Parts: TStringList;

begin
  Filename:= 'UNKOWN';
  LineNumber := -1;

  Mutex4LineInfo.Lock();

  bp := get_caller_frame(get_frame);
  if bp = nil then
  begin
    Mutex4LineInfo.Unlock();
    Exit;
  end;

  CallerAddress := get_caller_addr(bp);
  CallerFrame := get_caller_frame(bp);
  if (CallerAddress = nil) or (CallerFrame = nil) then
    Halt(1);
  Func := ''; Source := '';
  if not GetLineInfo(CodePtrUInt(CallerAddress), Func, Source, LineNumber) then
    Halt(2);

  Parts := Split(Source, '/');
   if Parts.Count <> 0 then
     Filename := Parts[Parts.Count - 1];
   Parts.Free;
  Mutex4LineInfo.Unlock();

end;

var
  MutexWriteLn: TMutex;

procedure _WriteLn(Message: AnsiString);
begin
  MutexWriteLn.Lock;

  System.Writeln(Message);
  Flush(Output);

  MutexWriteLn.Unlock;

end;

procedure _DebugLn(Filename: AnsiString; LineNumber: Integer;
  Fmt: AnsiString; const Args: array of const; Verbosity: Integer);
begin
  if RunTimeParameterManager.ValueByName['--Debug'].AsIntegerOrDefault(-1) < Verbosity then
     Exit;

  if (Filename <> 'UNKNOWN') and (LineNumber <> -1) then
    _Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber,
      Format(Fmt, Args)]))
  else
    _Writeln(Format('%d-%s] %s', [ThreadID, DateTimeToStr(Now), Format(Fmt, Args)]));

end;


procedure DebugLn(Msg: AnsiString; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLn(Filename, LineNumber, '%s', [Msg], Verbosity);
end;

procedure FMTDebugLn(Fmt: AnsiString; const Args: array of const;
  Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLn(Filename, LineNumber, Fmt, Args, Verbosity);

end;

type
  TLineInfoIntegerMap = specialize TFPGMap<AnsiString, Integer>;

var
  Counters: TLineInfoIntegerMap;
  Mutex4Counters: TMutex;

procedure _DebugLnEveryN(Filename: AnsiString; LineNumber: Integer; N: Integer; Fmt: AnsiString; const Args: array of const; Verbosity: Integer; Depth: Integer);
var
  LineInfo: AnsiString;
  Index: Integer;
  b: Boolean;

begin
  if RunTimeParameterManager.ValueByName['--Debug'].AsIntegerOrDefault(-1) < Verbosity then
    Exit;
  LineInfo := Format('%s:%d', [Filename, LineNumber]);

  Mutex4Counters.Lock;
  if not Counters.Find(LineInfo, Index) then
  begin
    Counters.Add(LineInfo, 0);
    Counters.Find(LineInfo, Index);

  end;

  b := Counters.Data[Index] mod N = 0;
  Counters.Data[Index] := Counters.Data[Index] + 1;
  Mutex4Counters.Unlock;

  if b then
    _Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber,
    Format(Fmt, Args)]));

end;

procedure DebugLnEveryN(N: Integer; Msg: AnsiString; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLnEveryN(Filename, LineNumber, N, '%s', [Msg], Verbosity, 2);

end;

procedure FMTDebugLnEveryN(N: Integer; Fmt: AnsiString;
  const Args: array of const; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLnEveryN(Filename, LineNumber, N, Fmt, Args, Verbosity, 2);

end;

procedure _FatalLn(Msg: AnsiString; Depth: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  Filename := 'UNKNOWN';
  LineNumber := -1;
  GetParentLineInfo(Filename, LineNumber);
  if (Filename <> 'UNKNOWN') and (LineNumber <> -1) then
    _Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber, Msg]))
  else
    _Writeln(Format('%d-%s] %s', [ThreadID, DateTimeToStr(Now), Msg]));

  Halt(1);

end;

procedure FatalLn(Msg: AnsiString);
begin
  _FatalLn(Msg, 2);
end;


procedure FmtFatalLn(Fmt: AnsiString; const Args: array of const);
begin
  _FatalLn(Format(Fmt, Args), 2);

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

initialization
  Mutex4LineInfo := TMutex.Create;
  Mutex4Counters := TMutex.Create;
  MutexWriteLn := TMutex.Create;
  Counters := TLineInfoIntegerMap.Create;
  Counters.Sorted := True;

finalization
  Counters.Free;
  Mutex4LineInfo.Free;
  Mutex4Counters.Free;
  MutexWriteLn.Free;

end.


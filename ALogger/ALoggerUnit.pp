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

procedure DebugLn(constref Msg: AnsiString; Verbosity: Integer = 0);
procedure FMTDebugLn(constref Fmt: AnsiString; constref Args: array of const ; Verbosity: Integer = 0);
procedure DebugLnEveryN(N: Integer; constref Msg: AnsiString; Verbosity: Integer = 0);
procedure FMTDebugLnEveryN(N: Integer; constref Fmt: AnsiString; constref Args : array of const; Verbosity: Integer = 0);
procedure FatalLn(constref Msg: AnsiString);
procedure FmtFatalLn(constref Fmt: AnsiString; const Args: array of const);
procedure FmtFatalLnIFFalse(Value: Boolean; constref Fmt: AnsiString; constref Args: array of const);

implementation

uses
  ParameterManagerUnit, StringUnit, WideStringUnit, SyncUnit, OnceUnit, lnfodwrf,
  GenericCollectionUnit;

var
  Mutex4LineInfo: TMutex;
  PrintOnce: TOnce;

procedure GetParentLineInfo(var Filename: AnsiString; var LineNumber: Integer);
var
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

  Func := ''; Source := '';
  CallerAddress := get_caller_addr(bp);
  if CallerAddress = nil then
  begin
    PrintOnce.Run
  end
  else if not GetLineInfo(CodePtrUInt(CallerAddress), Func, Source, LineNumber) then
  begin
    PrintOnce.Run;

  end;

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
  System.Writeln(StdErr, Message);
  Flush(StdErr);

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


procedure DebugLn(constref Msg: AnsiString; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLn(Filename, LineNumber, '%s', [Msg], Verbosity);
end;

procedure FMTDebugLn(constref Fmt: AnsiString; constref Args: array of const;
  Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLn(Filename, LineNumber, Fmt, Args, Verbosity);

end;

type
  TLineInfoIntegerMap = specialize TMap<AnsiString, Integer>;

var
  Counters: TLineInfoIntegerMap;
  Mutex4Counters: TMutex;

procedure _DebugLnEveryN(Filename: AnsiString; LineNumber: Integer; N: Integer; Fmt: AnsiString; const Args: array of const; Verbosity: Integer; Depth: Integer);
var
  LineInfo: AnsiString;
  Value: Integer;
  b: Boolean;

begin
  if RunTimeParameterManager.ValueByName['--Debug'].AsIntegerOrDefault(-1) < Verbosity then
    Exit;
  LineInfo := Format('%s:%d', [Filename, LineNumber]);

  Mutex4Counters.Lock;

  if not Counters.TryGetData(LineInfo, Value) then
  begin
    Counters.Add(LineInfo, 0);
    Value := 0;

  end;

  b := Value mod N = 0;
  Counters.AddOrUpdateData(LineInfo, Value + 1);
  Mutex4Counters.Unlock;

  if b then
  begin
    _Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber,
    Format(Fmt, Args)]));

  end;

end;

procedure DebugLnEveryN(N: Integer; constref Msg: AnsiString; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLnEveryN(Filename, LineNumber, N, '%s', [Msg], Verbosity, 2);

end;

procedure FMTDebugLnEveryN(N: Integer; constref Fmt: AnsiString;
  constref  Args: array of const; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _DebugLnEveryN(Filename, LineNumber, N, Fmt, Args, Verbosity, 2);

end;

procedure _FatalLn(FileName: AnsiString; LineNumber: Integer; Msg: AnsiString);
begin
  _Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber, Msg]));

  Halt(1);

end;

procedure FatalLn(constref Msg: AnsiString);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _FatalLn(Filename, LineNumber, Msg);

end;


procedure FmtFatalLn(constref Fmt: AnsiString; const Args: array of const);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber);
  _FatalLn(Filename, LineNumber, Format(Fmt, Args));

end;

procedure FmtFatalLnIFFalse(Value: Boolean; constref Fmt: AnsiString;
  constref Args: array of const);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  if Value then
    Exit;

  GetParentLineInfo(Filename, LineNumber);
  _FatalLn(Filename, LineNumber, Format(Fmt, Args));

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

procedure PrintError(Arguments: TPtrArray);
begin
  WriteLn(StdErr, 'Please make sure the code is compiled with -g');

end;

initialization
  Mutex4LineInfo := TMutex.Create;
  Mutex4Counters := TMutex.Create;
  MutexWriteLn := TMutex.Create;
  Counters := TLineInfoIntegerMap.Create;
  PrintOnce := TOnce.Create(@PrintError, nil);

finalization
  Counters.Free;
  Mutex4LineInfo.Free;
  Mutex4Counters.Free;
  MutexWriteLn.Free;
  PrintOnce.Free;

end.


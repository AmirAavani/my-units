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
procedure DebugLnEveryN(N: Integer; Msg: AnsiString; Verbosity: Integer = 0);
procedure FatalLn(Msg: AnsiString);

implementation

uses
  ParameterManagerUnit, StringUnit, WideStringUnit, SyncUnit, lnfodwrf, fgl;

var
  Mutex4LineInfo: TMutex;

procedure GetParentLineInfo(var Filename: AnsiString; var LineNumber: Integer; Index: Integer);
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

procedure DebugLn(Msg: AnsiString; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
   // if Verbosity <= RunTimeParameterManager.ValueByName['--Debug'].AsIntegerOrDefault(-1)  then
  begin
    Filename := 'UNKNOWN';
    LineNumber := -1;
    GetParentLineInfo(Filename, LineNumber, 1);
    System.Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber, Msg]));

    Flush(Output);

  end;
end;

type
  TLineInfoIntegerMap = specialize TFPGMap<AnsiString, Integer>;

var
  Counters: TLineInfoIntegerMap;
  Mutex4DebugLn: TMutex;

procedure DebugLnEveryN(N: Integer; Msg: AnsiString; Verbosity: Integer);
var
  Filename: AnsiString;
  LineNumber: Integer;
  LineInfo: AnsiString;
  Index: Integer;
  b: Boolean;

begin
  if RunTimeParameterManager.ValueByName['--Debug'].AsIntegerOrDefault(-1) < Verbosity then
    Exit;
  GetParentLineInfo(Filename, LineNumber, 1);
  LineInfo := Format('%s:%d', [Filename, LineNumber]);

  Mutex4DebugLn.Lock;
  if not Counters.Find(LineInfo, Index) then
  begin
    Counters.Add(LineInfo, 0);
    Counters.Find(LineInfo, Index);

  end;

  b := Counters.Data[Index] mod N = 0;
  Counters.Data[Index] := Counters.Data[Index] + 1;
  Mutex4DebugLn.Unlock;

  if b then
  begin
    System.Writeln(Format('%d-%s-%s:%d] %s', [ThreadID, DateTimeToStr(Now), Filename, LineNumber, Msg]));
    Flush(Output);

  end;

end;

procedure FatalLn(Msg: AnsiString);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  GetParentLineInfo(Filename, LineNumber, 1);
  System.Writeln(Format('%s-%s:%d] %s', [DateTimeToStr(Now), Filename, LineNumber, Msg]));
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

initialization
  Mutex4LineInfo := TMutex.Create;
  Mutex4DebugLn := TMutex.Create;
  Counters := TLineInfoIntegerMap.Create;
  Counters.Sorted := True;

finalization
  Counters.Free;
  Mutex4LineInfo.Free;
  Mutex4DebugLn.Free;

end.


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
procedure FatalLn(Msg: AnsiString);

implementation

uses
  ParameterManagerUnit, StringUnit, WideStringUnit, SyncUnit, lnfodwrf;

procedure GetParentLineInfo(var Filename: AnsiString; var LineNumber: Integer);
var
  i: Integer;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  Func, Source: ShortString;
  Parts: TStringList;

begin
  Filename:= 'UNKOWN';
  LineNumber := -1;

  bp := get_frame;

  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    i := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       if not GetLineInfo(ptruint(CallerAddress), Func, Source, LineNumber) then
         Break;

       Inc(i);
       if (i = 2) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
   end;

   Parts := Split(Source, '/');
   if Parts.Count <> 0 then
     Filename := Parts[Parts.Count - 1];
   Parts.Free;

end;

var
  Mutex: TMutex;

procedure DebugLn(Msg: AnsiString);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  if RunTimeParameterManager.ValueByName['--Debug'].AsBooleanOrDefault(True) then
  begin
    Mutex.Lock;

    GetParentLineInfo(Filename, LineNumber);
    System.Writeln(Format('%s-%s:%d] %s', [DateTimeToStr(Now), Filename, LineNumber, Msg]));

    Flush(Output);
    Mutex.Unlock;

  end;
end;

procedure FatalLn(Msg: AnsiString);
var
  Filename: AnsiString;
  LineNumber: Integer;

begin
  Mutex.Lock;

  GetParentLineInfo(Filename, LineNumber);
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
  Mutex := TMutex.Create;

finalization
  Mutex.Free;

end.


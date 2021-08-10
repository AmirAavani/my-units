program WorkerPool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, WorkerPoolUnit, ThreadPoolUnit, QueueUnit, HeapUnit, ALoggerUnit,
  WideStringUnit, ThreadSafeStackUnit, GenericCollectionUnit
  { you can add units after this };

function WorkerFunction01(
  Request: WorkerPoolUnit.TRequest;
  Response: WorkerPoolUnit.TResponse): Boolean;
var
  S: AnsiString;

begin
  S := PAnsiString(Request)^;
  PAnsiString(Response)^ := 'x' + S;
  FMTDebugLn('Request: %s', [S]);
  Sleep(Random(10));
  Result := True;

end;

procedure Test1;
var
  wp: TWorkerPool;
  i: Integer;
  Request: TRequest;
  Response: TResponse;
  PStr: PAnsiString;
  AllResponses: specialize TCollection<TResponse>;
  AllDone: Boolean;

begin
  AllResponses :=  (specialize TCollection<TResponse>).Create;

  wp := TWorkerPool.CreateWithDefaultOptions(@WorkerFunction01);

  for i := 0 to 10000 do
  begin
    New(PStr);
    PStr^ := IntToStr(i);
    Request := PStr;
    New(PStr);
    PStr^ := '';
    Response := PStr;

    AllResponses.Add(Response);
    wp.ServeRequest(Request, Response);

  end;

  AllDone := False;
  while not AllDone do
  begin
    AllDone := True;

    for i := 0 to AllResponses.Count - 1 do
    begin
      if AllResponses[i] = nil then
        Continue;

      if PAnsiString(AllResponses[i])^ <> '' then
      begin
        if PAnsiString(AllResponses[i])^ <> 'x' + IntToStr(i) then
        begin
          FmtFatalLn('Something went wrong! Expected %s Recieved: %s',
            ['x' + IntToStr(i) ,  PAnsiString(AllResponses[i])^]);
        end;
        FMTDebugLnEveryN(100, '%d is done', [i]);
        Dispose(PAnsiString(AllResponses[i]));
        AllResponses[i] := nil;

        Continue;
      end;
      FMTDebugLn('Request %d has not been served yet!', [i]);

      AllDone := False;
      Break;

    end;
    Sleep(1000);

  end;

  FMTDebugLn('All Done!', []);
  wp.Free;

end;

begin
  Test1;

end.


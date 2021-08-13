program WorkerPool;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, sysutils, WorkerPoolUnit, ThreadPoolUnit, QueueUnit, HeapUnit,
  ALoggerUnit, WideStringUnit, ThreadSafeStackUnit, ResponseUnit, RequestUnit,
  GenericCollectionUnit;

function WorkerFunction01(
  Request: TRequestForTest1;
  Response: TResponseForTest1): Boolean;
var
  S: AnsiString;

begin
  S := Request.Text;
  Response.Text := 'x' + S;
  Sleep(Random(100) + 1);

  Result := True;

end;

procedure Test01;
type
  TWorkerPoolForTest1 = specialize TWorkerPool<TRequestForTest1, TResponseForTest1>;

var
  wp: TWorkerPoolForTest1;
  i: Integer;
  Request: TRequestForTest1;
  Response: TResponseForTest1;
  AllResponses: specialize TCollection<TResponseForTest1>;
  AllDone: Boolean;

begin
  AllResponses :=  (specialize TCollection<TResponseForTest1>).Create;

  wp := TWorkerPoolForTest1.CreateWithDefaultOptions(@WorkerFunction01);

  for i := 0 to 10000 do
  begin
    Request := TRequestForTest1.Create;
    Request.Text := IntToStr(i);
    Response := TResponseForTest1.Create;

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

      if AllResponses[i].Text <> '' then
      begin
        if AllResponses[i].Text <> 'x' + IntToStr(i) then
        begin
          FmtFatalLn('Something went wrong! Expected %s Recieved: %s',
            ['x' + IntToStr(i) ,  PAnsiString(AllResponses[i])^]);
        end;
        AllResponses[i].Free;
        AllResponses[i] := nil;

        Continue;
      end;
      // WriteLn('Not Done ', i);
      FMTDebugLn('Request %d has not been served yet!', [i]);

      AllDone := False;
      Break;

    end;
    Sleep(1000);

  end;

  WriteLn('AllDone: ', AllDone);
  wp.Free;
  AllResponses.Free;
end;

begin
  Test01;

  Sleep(1000);


end.


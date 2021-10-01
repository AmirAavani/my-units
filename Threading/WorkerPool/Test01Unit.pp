unit Test01Unit;

{$mode ObjFPC}{$H+}

interface

procedure Test01;

implementation
uses
  SysUtils, WorkerPoolUnit, RequestUnit, ResponseUnit, GenericCollectionUnit,
  ALoggerUnit;

function WorkerFunction01(
  Request: TRequestForTest01;
  Response: TResponseForTest01): Boolean;
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
  TWorkerPoolForTest1 = specialize TWorkerPool<TRequestForTest01, TResponseForTest01>;

var
  wp: TWorkerPoolForTest1;
  i: Integer;
  Request: TRequestForTest01;
  Response: TResponseForTest01;
  AllResponses: specialize TCollection<TResponseForTest01>;
  AllDone: Boolean;

begin
  AllResponses :=  (specialize TCollection<TResponseForTest01>).Create;

  wp := TWorkerPoolForTest1.CreateWithDefaultOptions(@WorkerFunction01);

  for i := 0 to 10000 do
  begin
    Request := TRequestForTest01.Create;
    Request.Text := IntToStr(i);
    Response := TResponseForTest01.Create;

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

end.


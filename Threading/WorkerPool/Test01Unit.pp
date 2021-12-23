unit Test01Unit;

{$mode ObjFPC}{$H+}

interface

procedure Test01;

implementation
uses
  SysUtils, WorkerPoolUnit, GenericCollectionUnit,
  ALoggerUnit, pb.DataUnit, Pb.RequestUnit, pb.ResponseUnit, SourcerUnit;

function WorkerFunction01(
  Data: TDataForTest01): Boolean;
var
  S: AnsiString;

begin
  S := Data.Request.Text;
  Data.Response.Text := 'x' + S;
  Sleep(Random(100) + 1);

  Result := True;

end;

procedure Test01;
type
  TWorkerPoolForTest1 = specialize TWorkerPool<TDataForTest01>;

var
  wp: TWorkerPoolForTest1;
  i: Integer;
  Data: TDataForTest01;
  AllData: specialize TObjectCollection<TDataForTest01>;
  AllDone: Boolean;

begin
  AllData :=  (specialize TObjectCollection<TDataForTest01>).Create;

  wp := TWorkerPoolForTest1.CreateWithDefaultOptions;

  for i := 0 to 10000 do
  begin
    Data := TDataForTest01.Create;
    Data.Request.Text := IntToStr(i);
    Data.Response := TResponseForTest01.Create;

    AllData.Add(Data);

  end;
  wp.SetSource((specialize TSoucrerFromCollection<TDataForTest01>).Create(AllData));
//  wp.AddWorker(TWorkerPool.TWorkerFunction.);
  AllDone := False;
  while not AllDone do
  begin
    AllDone := True;

    for i := 0 to AllData.Count - 1 do
    begin
      if AllData[i].Response = nil then
        Continue;

      if AllData[i].Response.Text <> '' then
      begin
        if AllData[i].Response.Text <> 'x' + IntToStr(i) then
        begin
          FmtFatalLn('Something went wrong! Expected %s Recieved: %s',
            ['x' + IntToStr(i) ,  PAnsiString(AllData[i].Response)^]);
        end;
        AllData[i].Free;

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

end;

end.


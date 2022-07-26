unit Test01Unit;

{$mode ObjFPC}{$H+}

interface

procedure Test01;

implementation
uses
  SysUtils, WorkerPoolUnit, GenericCollectionUnit,
  ALoggerUnit, SourcerUnit, DataUnit, ProtoHelperUnit;

function WorkerFunction01(
  Request: TBaseMessage): TBaseMessage;
var
  Response: TDataForTest01;
  S: AnsiString;

begin
  Response := TDataForTest01.Create;
  S := (Request as TDataForTest01).Text;
  Response.Text := 'x' + S;

  Sleep(Random(100) + 1);
  Result := Response;

end;

procedure Test01;
type
  TRequestForTest01s = specialize TObjectCollection<TBaseMessage>;
  TSourcerForTest01 = specialize TSoucrerFromCollection<TBaseMessage>;
  TWorkerPoolForTest01 = specialize TWorkerPool<TBaseMessage>;
{
var
  wp: TWorkerPoolForTest01;
  i: Integer;
  Request: TDataForTest01;
  Requests: TRequestForTest01s;
}
begin
{  wp := TWorkerPoolForTest01.CreateWithDefaultOptions;

  Requests := TRequestForTest01s.Create;
  for i := 0 to 1000 do
  begin
    Request := TDataForTest01.Create;
    Request.Text := IntToStr(i);
    Requests.Add(Request);

  end;

  wp.SetSource(TSourcerForTest01.Create(Requests));
  wp.AddWorker(nil);
//    TWorkerPool.TPoolWorkerByFunc.Create(@WorkerFunction01));

  wp.Run;
  FMTDebugLn('1', []);
  Sleep(1);
  FMTDebugLn('2', []);
  Wp.WaitToBeDone;
  FMTDebugLn('3', []);

  wp.Free;
}
end;

end.


unit Test02Unit;

{$mode ObjFPC}{$H+}

interface

procedure Test02;

implementation
uses
  SysUtils, WorkerPoolUnit, RequestUnit, ResponseUnit, GenericCollectionUnit,
  ALoggerUnit, SyncUnit;

function WorkerFunction02(
  Request: TRequestForTest02;
  Response: TResponseForTest02): Boolean;
begin
  Response.Text := IntToStr(Request.Number);

  Result := True;

end;

var
  wgForTest02: TWaitGroup;

procedure FirstCallbackFor02(Request: TRequestForTest02; Response: TResponseForTest02);
begin
  if IntToStr(Request.Number) <> Response.Text then
  begin
    FmtFatalLn('%d -> %s', [Request.Number, Response.Text]);

  end;

  wgForTest02.Done(1);
  Request.Free;
  Response.Free;

end;

procedure SecondCallbackFor02(Request: TRequestForTest02; Response: TResponseForTest02);
begin
  if IntToStr(Request.Number) <> Response.Text then
  begin
    FmtFatalLn('%d -> %s', [Request.Number, Response.Text]);

  end;

  wgForTest02.Done(1);
  Request.Free;
  Response.Free;

end;

procedure Test02;
type
  TMyWorkerPool = specialize TWorkerPool<TRequestForTest02, TResponseForTest02>;

var
  wp: TMyWorkerPool;
  i: Integer;
  Request: TRequestForTest02;
  Response: TResponseForTest02;
  Options: TWorkerPoolOptions;

begin
  Options := TMyWorkerPool.GetDefaultOptions;
  Options.FreeRequests := False;
  wp := TMyWorkerPool.CreateWithOption(@WorkerFunction02, Options);

  wgForTest02 := TWaitGroup.Create;
  for i := 0 to 10000 do
  begin
    wgForTest02.Add(1);

    Request := TRequestForTest02.Create;
    Request.Number := i; //(i + 1) * (Random(1000) + 1);
    Response := TResponseForTest02.Create;

    wp.ServeRequest(Request, Response, @FirstCallbackFor02);

  end;

  FMTDebugLn('wg: %d', [wgForTest02.GetValue]);
  wgForTest02.Wait;
  FMTDebugLn('The first set of requests is done', []);

  for i := 0 to 10000 do
  begin
    wgForTest02.Add(1);

    Request := TRequestForTest02.Create;
    Request.Number := (i + 1) * (Random(1000) + 1);
    Response := TResponseForTest02.Create;

    wp.ServeRequest(Request, Response, @SecondCallbackFor02);

  end;

  FMTDebugLn('wg: %d', [wgForTest02.GetValue]);
  wgForTest02.Wait;
  FMTDebugLn('The second set of requests is done', []);

  for i := 0 to 10000 do
  begin
    wgForTest02.Add(1);

    Request := TRequestForTest02.Create;
    Request.Number := i;
    Response := TResponseForTest02.Create;

    wp.ServeRequest(Request, Response, @FirstCallbackFor02);

    wgForTest02.Add(1);

    Request := TRequestForTest02.Create;
    Request.Number := (i + 1) * (Random(1000) + 1);
    Response := TResponseForTest02.Create;

    wp.ServeRequest(Request, Response, @SecondCallbackFor02);

  end;

  FMTDebugLn('wg: %d', [wgForTest02.GetValue]);
  wgForTest02.Wait;

  wgForTest02.Free;

  wp.Free;

end;


end.


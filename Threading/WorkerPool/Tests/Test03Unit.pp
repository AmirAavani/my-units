unit Test03Unit;

{$mode ObjFPC}{$H+}
interface

//procedure Test03;

implementation
uses
  SysUtils, WorkerPoolUnit, GenericCollectionUnit,
  ALoggerUnit, SyncUnit;
{
type
  TMyWorkerPool = specialize TWorkerPool<TRequestForTest03, TResponseForTest03>;

  { TTest03Worker }

  TTest03Worker = class(TMyWorkerPool.TBaseWorker)
  protected
    procedure Serve(ARequest: TMyWorkerPool.TRequestClass;
      AResponse: TMyWorkerPool.TResponseClass;
      Callback: TMyWorkerPool.TCallbackProcedure); override;

  public

  end;

procedure Test03;
var
  wp: TMyWorkerPool;
  Options: TWorkerPoolOptions;

begin
  Options := TMyWorkerPool.GetDefaultOptions;
  wp := TMyWorkerPool.CreateWithOption(
    TTest03Worker.Create, Options);


  wp.Free;

end;

{ TTest03Worker }

procedure TTest03Worker.Serve(ARequest: TMyWorkerPool.TRequestClass;
  AResponse: TMyWorkerPool.TResponseClass;
  Callback: TMyWorkerPool.TCallbackProcedure);
begin

end;

}
end.


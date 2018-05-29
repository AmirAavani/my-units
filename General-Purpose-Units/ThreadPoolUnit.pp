unit ThreadPoolUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, QueueUnit;

type

  { TThreadPool }

  generic TThreadPool<T> = class(specialize TFPGList<TThread>)
  protected
    Queue: TThreadSafeQueue;

  public
    constructor Create;
    destructor Destroy; override;


  end;

implementation

{ TThreadPool }

constructor TThreadPool.Create;
var
  i: Integer;

begin
  inherited Create;

end;

destructor TThreadPool.Destroy;
begin
  Queue.Free;

end;

end.


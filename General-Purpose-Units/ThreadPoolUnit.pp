unit ThreadPoolUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, QueueUnit;

type

  { TThreadPool }

  TThreadPool = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TThreadPool }

constructor TThreadPool.Create;
begin
  inherited Create;

end;

destructor TThreadPool.Destroy;
begin
  inherited;

end;

end.


unit ThreadPoolUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, QueueUnit;

type

  { TAbstractRunner }

  TAbstractRunner = class(TObject)
  private
  public
    procedure Execute; virtual; abstract;

  end;

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


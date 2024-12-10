unit MapReduce.OutputMappersUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, MapReduce.NodeUnit, ProtoHelperUnit;

type

  { TBaseOutputMapper }

  TBaseOutputMapper = class(TMapper)
  protected
    function Save(
      constref InputKey: AnsiString;
      InputValue: TBaseMessage): Boolean; virtual; abstract;

  public
    constructor Create;

    function Process(constref InputKey: AnsiString; InputValue: TBaseMessage;
      Sender: TNode): Boolean; override;

  end;


  { TOutputStreamMapper }

  TOutputStreamMapper = class(TBaseOutputMapper)
  protected
    OutputStream: TStream;

    function Save(
      constref InputKey: AnsiString;
      InputValue: TBaseMessage): Boolean; override;

  public
    constructor Create(constref Stream: TStream);
    destructor Destroy; override;

  end;


  { TOutputWriteLnMapper }

  TOutputWriteLnMapper = class(TBaseOutputMapper)
  protected
    Buffer: AnsiString;
    Position: Integer;

    function Save(
      constref InputKey: AnsiString;
      InputValue: TBaseMessage): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TBaseOutputMapper }

constructor TBaseOutputMapper.Create;
begin
  inherited Create;

end;

function TBaseOutputMapper.Process(constref InputKey: AnsiString;
  InputValue: TBaseMessage; Sender: TNode): Boolean;
begin
  WriteLn(Format('%d -> Process', [ThreadID]));
  Result := Save(InputKey, InputValue);
  WriteLn(Format('%d -> Process', [ThreadID]));
end;

{ TOutputStreamMapper }

function TOutputStreamMapper.Save(constref InputKey: AnsiString;
  InputValue: TBaseMessage): Boolean;
begin
  raise Exception.Create('');
end;

constructor TOutputStreamMapper.Create(constref Stream: TStream);
begin
  OutputStream := Stream;
end;

destructor TOutputStreamMapper.Destroy;
begin
  inherited Destroy;
end;

{ TOutputWriteLnMapper }

function TOutputWriteLnMapper.Save(constref InputKey: AnsiString;
  InputValue: TBaseMessage): Boolean;
begin
  if InputValue <> nil then
    WriteLn(Format('%s -> %s', [InputKey, InputValue.ToJSON]))
  else
    WriteLn(Format('%s -> {}', [InputKey]));
  Result := True;
end;

constructor TOutputWriteLnMapper.Create;
begin

end;

destructor TOutputWriteLnMapper.Destroy;
begin
  inherited Destroy;
end;

end.


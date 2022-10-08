unit Pipeline.DataLineUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, PairUnit, GenericCollectionUnit, Mapper.OptionUnit, Mapper.TypesUnit;

type
  { TBaseChannel }

  TBaseChannel = class(TObject)
  protected
    FOptions: TChannelOptions;
    function Delete: TKeyValue; virtual; abstract;

  public
    constructor Create(Options: TChannelOptions);

    procedure AddData(kv: TKeyValue); virtual; abstract;

    class function CreateChannel(Option: TChannelOptions): TBaseChannel;
  end;

  { TDataLine }

  TDataLine = class(specialize TObjectCollection<TBaseChannel>)
  protected
    SharderFunction: TSharderFunction;
    FOptions: TChannelOptions;

  public
    constructor Create(_Count: Integer; const Options: TChannelOptions;
      _SharderFunc: TSharderFunction = nil);

    procedure Send(Key: AnsiString; Value: Pointer);
    procedure Send(Key: AnsiString; Value: Pointer; Shard: Integer);
    function Recieve: TKeyValue;
    function Recieve(Shard: Integer): TKeyValue;

  end;



implementation
uses
  ALoggerUnit, SyncUnit, ElfHashUnit;

type

  { TInMemoryChannel }

  TInMemoryChannel = class(TBaseChannel)
  protected
    Mutex: TSimpleRWSync;
    Stack: specialize TCollection<TKeyValue>;

  protected
    function Delete: TKeyValue; override;

  public
    constructor Create(Options: TChannelOptions);
    destructor Destroy; override;

    procedure AddData(kv: TKeyValue); override;

  end;

{ TDataLine }

constructor TDataLine.Create(_Count: Integer; const Options: TChannelOptions;
  _SharderFunc: TSharderFunction);
var
  i: Integer;

begin
  inherited Create;

  SharderFunction := _SharderFunc;
  FOptions := Options;
  Count := _Count;
  for i := 0 to Count - 1 do
    Self[i] := TBaseChannel.CreateChannel(Options);

end;

procedure TDataLine.Send(Key: AnsiString; Value: Pointer);
var
  ShardID: UInt32;

begin
  ShardID := ElfHash(Key) mod Count;
  Self[ShardID]
end;

procedure TDataLine.Send(Key: AnsiString; Value: Pointer; Shard: Integer);
begin

end;

function TDataLine.Recieve: TKeyValue;
begin

end;

function TDataLine.Recieve(Shard: Integer): TKeyValue;
begin

end;


{ TBaseChannel }

constructor TBaseChannel.Create(Options: TChannelOptions);
begin
  inherited Create;

  FOptions := Options;
end;

class function TBaseChannel.CreateChannel(Option: TChannelOptions
  ): TBaseChannel;
begin
  case Option.ChannelType of
    ctInMemoryChannel:
      Result := TInMemoryChannel.Create(Option)
    else
      FmtFatalLn('Unknown ChannelType: %d', [Ord(Option.ChannelType)]);
  end;

end;

{ TInMemoryChannel }

function TInMemoryChannel.Delete: TKeyValue;
begin
  Mutex.Beginwrite;

  Result := Stack.Last;
  Stack.Delete(Stack.Count - 1);

  Mutex.Endwrite;
 end;

constructor TInMemoryChannel.Create(Options: TChannelOptions);
begin
  inherited Create(Options);
  Mutex := TSimpleRWSync.Create;
  Stack := (specialize TCollection<TKeyValue>).Create;

end;

destructor TInMemoryChannel.Destroy;
begin
  Mutex.Free;
  Stack.Free;

  inherited Destroy;

end;

procedure TInMemoryChannel.AddData(kv: TKeyValue);
begin
  Mutex.Beginwrite;

  Stack.Add(kv);

  Mutex.Endwrite;
end;

end.


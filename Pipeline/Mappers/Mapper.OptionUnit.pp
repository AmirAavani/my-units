unit Mapper.OptionUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;
type
  TSharderFunction = function (constref Key: AnsiString): UInt64;


  { TChannelOptions }

  TChannelOptions = class(TObject)
  public type
    TChannelType = (ctInMemoryChannel);

  private
    FChannelType: TChannelType;
    FSharder: TSharderFunction;

  public
    property ChannelType: TChannelType read FChannelType;
    property Sharder: TSharderFunction read FSharder;

    function SetChannelType(_ChannelType: TChannelType): TChannelOptions;

  end;

  { TMappingOptions }

  TMappingOptions = class(TObject)
  private
    FCapacity: Integer;
    FNumShards: Integer;
    FSharder: TSharderFunction;
    FThreadCount: Integer;
    FChannelOptions: TChannelOptions;

  public
    property NumShards: Integer read FNumShards;
    property Sharder: TSharderFunction read FSharder;
    property ThreadCount: Integer read FThreadCount;
    property ChannelOptions: TChannelOptions read FChannelOptions;
    property Capacity: Integer read FCapacity;

  public
    constructor Create;
    destructor Destroy; override;
    class function NewOptions: TMappingOptions;

    function SetNumShards(_NumShards: Integer): TMappingOptions;
    function SetSharder(_Sharder: TSharderFunction): TMappingOptions;
    function SetThreadCount(_ThreadCount: Integer): TMappingOptions;
    function SetChannelOptions(_ChannelOptions: TChannelOptions): TMappingOptions;
    function SetCapactiy(_Capacity: Integer): TMappingOptions;
  end;

implementation
uses
  ElfHashUnit;

function DefaultSharder(constref Key: AnsiString): UInt64;
begin
  Result := ElfHash(Key);

end;

{ TChannelOptions }

function TChannelOptions.SetChannelType(_ChannelType: TChannelType
  ): TChannelOptions;
begin
  Result := Self;
  Result.FChannelType := _ChannelType;

end;

{ TMappingOptions }

constructor TMappingOptions.Create;
begin
  inherited Create;

  FNumShards := 1;
  FSharder := @DefaultSharder;
  FThreadCount := 1;
  FChannelOptions := TChannelOptions.Create;
  FCapacity := 64;

end;

destructor TMappingOptions.Destroy;
begin
  FChannelOptions.Free;

  inherited Destroy;
end;

class function TMappingOptions.NewOptions: TMappingOptions;
begin
  Result := TMappingOptions.Create;

end;

function TMappingOptions.SetNumShards(_NumShards: Integer): TMappingOptions;
begin
  Result := Self;
  Result.FNumShards := _NumShards;

end;

function TMappingOptions.SetSharder(_Sharder: TSharderFunction): TMappingOptions;
begin
  Result := Self;
  Result.FSharder := _Sharder;
  Result.FChannelOptions.FSharder := _Sharder;

end;

function TMappingOptions.SetThreadCount(_ThreadCount: Integer): TMappingOptions;
begin
  Result := Self;
  Result.FThreadCount := _ThreadCount;

end;

function TMappingOptions.SetChannelOptions(_ChannelOptions: TChannelOptions
  ): TMappingOptions;
begin
  Result := Self;
  Result.FChannelOptions := _ChannelOptions;

end;

function TMappingOptions.SetCapactiy(_Capacity: Integer): TMappingOptions;
begin
  Result := Self;
  Result.FCapacity := _Capacity;

end;

end.


unit DataLineUnit;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, BaseMapperUnit, PairUnit, GenericCollectionUnit,
  Mapper.OptionUnit;

type

  { TBaseChannel }

  TBaseChannel = class(TObject)
  public type
    TKeyValuePair = specialize TPair<AnsiString, Pointer>;

  protected
    FOptions: TChannelOptions;
    function Delete: TKeyValuePair; virtual; abstract;

  public
    constructor Create(Options: TChannelOptions);

    procedure AddData(kv: TKeyValuePair); virtual; abstract;

    class function CreateChannel(Option: TChannelOptions): TBaseChannel;
  end;

  { TDataPoint }

  TDataPoint = class(TObject)
  private type
    TOutputChannels = specialize TObjectCollection<TBaseChannel>;

  private
    Name: AnsiString;
    Mapper: TBaseMapper;
    Options: TMappingOptions;
    FParent: TDataPoint;
    WaitToBeDone, FreeOnTerminate: Boolean;
    Channels: TOutputChannels;

  protected
    property Parent: TDataPoint read FParent;

  public
    constructor Create(_Parent: TDataPoint; constref AName: AnsiString;
      AMapper: TBaseMapper; _Options: TMappingOptions);
    destructor Destroy; override;

    function Map(AName: AnsiString; AMapper: TBaseMapper; _Options: TMappingOptions): TDataPoint;
    function Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;
    procedure Summary;
    function Wait: Boolean;

    class function Start: TDataPoint;
  end;

  { TDataLine }

  TDataLine = class(TObject)
  private
    FName: AnsiString;
    FMapper: TBaseMapper;
    FOptions: TMappingOptions;
    FParent: TDataLine;
    WaitToBeDone, FreeOnTerminate: Boolean;

  protected
    property Name: AnsiString read FName;
    property Options: TMappingOptions read FOptions;
    property Parent: TDataLine read FParent;

    function DoRun: Boolean;

    constructor CreateWithParent(_Parent: TDataLine);
  public
    destructor Destroy; override;

    function Map(AName: AnsiString; AMapper: TBaseMapper; _Options: TMappingOptions): TDataLine;

    function Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;

    procedure Summary;
    function Wait: Boolean;
  end;

implementation

uses
  ALoggerUnit;

type

  { TInMemoryChannel }

  TInMemoryChannel = class(TBaseChannel)
  protected
    function Delete: TKeyValuePair; override;

  public
    constructor Create(Options: TChannelOptions);

    procedure AddData(kv: TKeyValuePair); override;

  end;

{ TInMemoryChannel }

function TInMemoryChannel.Delete: TKeyValuePair;
begin

end;

constructor TInMemoryChannel.Create(Options: TChannelOptions);
begin

end;

procedure TInMemoryChannel.AddData(kv: TKeyValuePair);
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

{ TDataPoint }

constructor TDataPoint.Create(_Parent: TDataPoint; constref AName: AnsiString;
  AMapper: TBaseMapper; _Options: TMappingOptions);
var
  i: Integer;

begin
  inherited Create;

  FParent := _Parent;
  Name := AName;
  Mapper := AMapper;
  Options := _Options;
  Channels := TOutputChannels.Create;

  Channels.Count := Options.NumShards;
  for i := 0 to Channels.Count - 1 do
    Channels[i] := TBaseChannel.CreateChannel(Options.ChannelOptions);

end;

destructor TDataPoint.Destroy;
begin
  Mapper.Free;
  Options.Free;

  inherited Destroy;
end;

function TDataPoint.Map(AName: AnsiString; AMapper: TBaseMapper;
  _Options: TMappingOptions): TDataPoint;
begin
  Result := TDataPoint.Create(Self, AName, AMapper, _Options);

end;

function TDataPoint.Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;
begin
  Result := True;

  FreeOnTerminate := AFreeOnTerminate;
  WaitToBeDone := AWaitToBeDone;

  if WaitToBeDone then
  begin


    if FreeOnTerminate then
      Self.Free;
  end;

end;

procedure TDataPoint.Summary;
var
  Data: AnsiString;

begin
  Data := '';
  {
  Data := Format('Name: %s Mapper: %s ShardCount: %d ThreadCount: %d',
    [Name, Mapper.ClassName, Options.NumShards,
    Options.ThreadCount]);
  }
  if Parent <> nil then
    Parent.Summary;
  WriteLn(Data);

end;

function TDataPoint.Wait: Boolean;
begin
  Result := WaitToBeDone;

  if FreeOnTerminate then
    Self.Free;

end;

class function TDataPoint.Start: TDataPoint;
begin
  Result := TDataPoint.Create(nil, '', nil, TMappingOptions.Create);
  // Result.Options := TMappingOptions.Create.SetNumShards(1).SetThreadCount(1);

end;

{ TDataLine }

function TDataLine.DoRun: Boolean;
begin
  if Parent <> nil then
    if not Parent.DoRun then
      Exit(False);

end;

constructor TDataLine.CreateWithParent(_Parent: TDataLine);
begin
  inherited Create;

  FParent := _Parent;

end;

destructor TDataLine.Destroy;
begin
  FMapper.Free;
  FOptions.Free;

  FParent.Free;

  inherited Destroy;
end;

function TDataLine.Map(AName: AnsiString; AMapper: TBaseMapper;
  _Options: TMappingOptions): TDataLine;
begin
  Result := TDataLine.CreateWithParent(Self);
  Result.FName := AName;
  Result.FMapper := AMapper;
  Result.FOptions := _Options;

end;

function TDataLine.Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;
begin
  Result := True;

  FreeOnTerminate := AFreeOnTerminate;
  WaitToBeDone := AWaitToBeDone;

  if WaitToBeDone then
  begin


    if FreeOnTerminate then
      Self.Free;
  end;


end;

procedure TDataLine.Summary;
var
  Data: AnsiString;

begin
  Data := Format('Name: %s Mapper: %s ShardCount: %d ThreadCount: %d',
    [Name, FMapper.ClassName, Options.NumShards,
    Options.ThreadCount]);

  if Parent <> nil then
    Parent.Summary;
  WriteLn(Data);

end;

function TDataLine.Wait: Boolean;
begin
  Result := WaitToBeDone;

  if FreeOnTerminate then
    Self.Free;

end;

end.


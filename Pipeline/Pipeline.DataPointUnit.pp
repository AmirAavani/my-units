unit Pipeline.DataPointUnit;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, Mapper.BaseMapperUnit, PairUnit,
  Mapper.OptionUnit, Pipeline.DataLineUnit, SyncUnit, GenericCollectionUnit;

type

  { TDataPoint }

  TDataPoint = class(TObject)
  private type
    TDataPoints = specialize TCollection<TDataPoint>;

  private
    Name: AnsiString;
    Options: TMappingOptions;
    FParent: TDataPoint;
    FChildren: TDataPoints;
    WaitToBeDone, FreeOnTerminate: Boolean;
    WaitGroup: TWaitGroup;
    Output: TDataLine;

  protected
    property Parent: TDataPoint read FParent;
    property Children: TDataPoints read FChildren;

  public
    constructor Create(_Parent: TDataPoint; constref AName: AnsiString;
      _Options: TMappingOptions);
    destructor Destroy; override;

    function Map(AName: AnsiString; AMapper: TBaseMapper; _Options: TMappingOptions): TDataPoint;
    function Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;
    procedure Summary;
    function Wait: Boolean;

    class function Start: TDataPoint;
  end;

implementation

uses
  ALoggerUnit;


{ TDataPoint }

constructor TDataPoint.Create(_Parent: TDataPoint; constref AName: AnsiString;
  _Options: TMappingOptions);
var
  i: Integer;

begin
  inherited Create;

  FParent := _Parent;
  Name := AName;
  Options := _Options;
  Output := TDataLine.Create(
    Options.NumShards,
   Options.ChannelOptions,
   Options.Sharder
  );
  WaitGroup := TWaitGroup.Create;
  FChildren := TDataPoints.Create;

end;

destructor TDataPoint.Destroy;
begin
  Options.Free;
  Output.Free;
  WaitGroup.Free;
  FChildren.Free;

  if FParent <> nil then
    FParent.Free;

  inherited Destroy;
end;

function TDataPoint.Map(AName: AnsiString; AMapper: TBaseMapper;
  _Options: TMappingOptions): TDataPoint;
begin
  Result := TDataPoint.Create(Self, AName, _Options);
  Self.Children.Add(Result);


end;

function TDataPoint.Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;
  function GetDepth(const dp: TDataPoint): Integer;
  begin
    if dp = nil then
      Exit(0);

    Result := 1 + GetDepth(dp.Parent);

  end;

  function GetFirst: TDataPoint;
  begin
    Result := Self;

    while Result.Parent <> nil do
      Result := Result.Parent;

  end;

  function RunMapper(Parent, Child: TDataPoint): Boolean;
  begin
    if Parent = nil then
      Exit(True);

  end;

  procedure RecRun(Root: TDataPoint);
  var
    Child: TDataPoint;

  begin
    for Child in Root.Children do
    begin
      // Child.Mapper.Map();
    end;


  end;

var
  s: TDataPoint;

begin
  Result := True;

  FreeOnTerminate := AFreeOnTerminate;
  WaitToBeDone := AWaitToBeDone;

  s := Self;
  while s.Parent <> nil do
    s := s.Parent;

  RecRun(s);
  if WaitToBeDone then
  begin
    WaitGroup.Wait;
    if FreeOnTerminate then
      Self.Free;
  end;

end;

procedure TDataPoint.Summary;
var
  Data: AnsiString;

begin
  Data := '';

  Data := Format('Name: %s ShardCount: %d ThreadCount: %d',
    [Name, Options.NumShards,
    Options.ThreadCount]);
  if Parent <> nil then
    Parent.Summary;
  WriteLn(Data);

end;

function TDataPoint.Wait: Boolean;
begin
  Result := not WaitToBeDone;
  if not WaitToBeDone then
    WaitGroup.Wait;

  if Result and FreeOnTerminate then
    Self.Free;

end;

class function TDataPoint.Start: TDataPoint;
begin
  Result := TDataPoint.Create(nil, '', TMappingOptions.Create);

end;

end.


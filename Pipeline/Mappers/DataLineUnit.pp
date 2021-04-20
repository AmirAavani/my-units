unit DataLineUnit;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, BaseMapperUnit, SourcerUnit, GenericCollectionUnit,
  MapperOptionUnit;

type
  { TDataLine }

  TDataLine = class(TObject)
  private
    FName: AnsiString;
    FMapper: TBaseMapper;
    FOptions: TMappingOptions;
    FSourcer: TBaseSourcer;
    FParent: TDataLine;
    WaitToBeDone, FreeOnTerminate: Boolean;

  protected
    property Sourcer: TBaseSourcer read FSourcer;
    property Name: AnsiString read FName;
    property Options: TMappingOptions read FOptions;
    property Parent: TDataLine read FParent;

    constructor CreateWithParent(_Parent: TDataLine);
  public
    class function Create(Src: TBaseSourcer): TDataLine;
    destructor Destroy; override;

    function Map(AName: AnsiString; AMapper: TBaseMapper; _Options: TMappingOptions): TDataLine;

    function Run(AWaitToBeDone, AFreeOnTerminate: Boolean): Boolean;

    procedure Report;
    function Wait: Boolean;

  end;

implementation

{ TDataLine }

constructor TDataLine.CreateWithParent(_Parent: TDataLine);
begin
  inherited Create;

  FParent := _Parent;

end;

class function TDataLine.Create(Src: TBaseSourcer): TDataLine;
begin
  Result := TDataLine.CreateWithParent(nil);
  Result.FSourcer := Src;

end;

destructor TDataLine.Destroy;
begin
  FMapper.Free;
  FOptions.Free;
  Sourcer.Free;

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

procedure TDataLine.Report;
var
  Data: AnsiString;

begin
  if FSourcer <> nil then
    Data := Format('Sourcer: %s (%s)', [FSourcer.ClassName, FSourcer.ToString])
  else
    Data := Format('Name: %s Mapper: %s ShardCount: %d ThreadCount: %d',
      [Name, FMapper.ClassName, Options.NumShards,
      Options.ThreadCount]);

  if Parent <> nil then
    Parent.Report;
  WriteLn(Data);

end;

function TDataLine.Wait: Boolean;
begin
  Result := WaitToBeDone;

  if FreeOnTerminate then
    Self.Free;

end;

end.


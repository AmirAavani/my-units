unit DataLineUnit;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, BaseMapperUnit, SourceUnit, GenericCollectionUnit,
  MapperOptionUnit;

type
  { TDataLine }

  TDataLine = class(TObject)
  private type
    TMappingInfo = record
      Name: AnsiString;
      Mapper: TBaseMapper;
      Options: TMappingOptions;

    end;

    TMappingInfoCollection = specialize TCollection<TMappingInfo>;

  private
    FMappingInfoCollection: TMappingInfoCollection;
    FSource: TBaseSource;
    function GetMapper(Index: Integer): TBaseMapper;

  protected
    property Source: TBaseSource read FSource;
    property MappingInfoCollection: TMappingInfoCollection read FMappingInfoCollection;
    property Mapper[Index: Integer]: TBaseMapper read GetMapper;

  public
    constructor Create(Src: TBaseSource);
    destructor Destroy; override;

    function Map(AName: AnsiString; AMapper: TBaseMapper; Options: TMappingOptions): TDataLine;

    function Run(WaitToBeDone, FreeOnTerminate: Boolean): Boolean;

    function Report: AnsiString;

  end;

implementation

{ TDataLine }

function TDataLine.GetMapper(Index: Integer): TBaseMapper;
begin
  Result := FMappingInfoCollection[Index].Mapper;

end;

constructor TDataLine.Create(Src: TBaseSource);
begin
  inherited Create;

  FSource := Src;
  FMappingInfoCollection := TMappingInfoCollection.Create;

end;

destructor TDataLine.Destroy;
var
  i: Integer;

begin
  for i := 0 to FMappingInfoCollection.Count - 1 do
    Mapper[i].Free;
  FMappingInfoCollection.Free;
  Source.Free;

  inherited Destroy;
end;

function TDataLine.Map(AName: AnsiString; AMapper: TBaseMapper;
  Options: TMappingOptions): TDataLine;

  function CreateMappingInfo(AName: AnsiString; AMapper: TBaseMapper;
    Options: TMappingOptions): TMappingInfo;
  begin
    Result.Name := AName;
    Result.Mapper := AMapper;
    Result.Options := Options;

  end;

begin
  Result := Self;
  Result.MappingInfoCollection.Add(CreateMappingInfo(AName, AMapper, Options));

end;

function TDataLine.Run(WaitToBeDone, FreeOnTerminate: Boolean): Boolean;
begin
  Result := True;

  if FreeOnTerminate then
    Self.Free;

end;

function TDataLine.Report: AnsiString;
var
  i: Integer;
  Lines: TStringList;

begin
  Lines := TStringList.Create;

  for i := 0 to MappingInfoCollection.Count - 1 do
  begin
//    Lines.Add(Format
  end;
end;

end.


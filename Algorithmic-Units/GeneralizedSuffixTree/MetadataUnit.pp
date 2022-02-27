unit MetadataUnit;
{$Mode objfpc}

interface

uses
 classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TDocMetaData = class;

  // message DocMetaData
  { TDocMetaData }
  TDocMetaData = class(TBaseMessage)
  // Forward Declarations.

  private
    FDocId: UInt32;

  public
    function GetDocId: UInt32;

  public
    // uint32 doc_id = 1;
    property DocId: UInt32 read FDocId write FDocId;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TDocMetaData;

  end;



implementation

function TDocMetaData.GetDocId: UInt32;
begin
  if Self = nil then
    Exit(0);

  Result := FDocId; 

end;


constructor TDocMetaData.Create;
begin
  inherited Create;


end;


destructor TDocMetaData.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TDocMetaData.Clear;
begin

  inherited;
end;

procedure TDocMetaData.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveUint32(Stream, DocId, 1);

end;


function TDocMetaData.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      DocId := LoadUint32(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TDocMetaData.DeepCopy: TDocMetaData;
begin
  if Self = nil then
    Exit(nil);

  Result := TDocMetaData.Create;

  Result.DocId := Self.DocId;

end;



end.

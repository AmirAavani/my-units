unit Pb.RequestUnit;
{$Mode objfpc}

interface

uses
 classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TRequestForTest01 = class;
  TRequestForTest02 = class;

  // message RequestForTest01
  { TRequestForTest01 }
  TRequestForTest01 = class(TBaseMessage)
  // Forward Declarations.

  private
    FText: AnsiString;

  public
    function GetText: AnsiString;

  public
    // string text = 1;
    property Text: AnsiString read FText write FText;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TRequestForTest01;

  end;

  // message RequestForTest02
  { TRequestForTest02 }
  TRequestForTest02 = class(TBaseMessage)
  // Forward Declarations.

  private
    FNumber: Int32;

  public
    function GetNumber: Int32;

  public
    // int32 number = 1;
    property Number: Int32 read FNumber write FNumber;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TRequestForTest02;

  end;



implementation

function TRequestForTest01.GetText: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FText; 

end;


constructor TRequestForTest01.Create;
begin
  inherited Create;


end;


destructor TRequestForTest01.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TRequestForTest01.Clear;
begin

  inherited;
end;

procedure TRequestForTest01.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Text, 1);

end;


function TRequestForTest01.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Text := LoadString(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TRequestForTest01.DeepCopy: TRequestForTest01;
begin
  if Self = nil then
    Exit(nil);

  Result := TRequestForTest01.Create;

  Result.Text := Self.Text;

end;

function TRequestForTest02.GetNumber: Int32;
begin
  if Self = nil then
    Exit(0);

  Result := FNumber; 

end;


constructor TRequestForTest02.Create;
begin
  inherited Create;


end;


destructor TRequestForTest02.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TRequestForTest02.Clear;
begin

  inherited;
end;

procedure TRequestForTest02.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveInt32(Stream, Number, 1);

end;


function TRequestForTest02.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Number := LoadInt32(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TRequestForTest02.DeepCopy: TRequestForTest02;
begin
  if Self = nil then
    Exit(nil);

  Result := TRequestForTest02.Create;

  Result.Number := Self.Number;

end;



end.

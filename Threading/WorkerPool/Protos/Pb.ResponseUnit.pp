unit Pb.ResponseUnit;
{$Mode objfpc}

interface

uses
 classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TResponseForTest01 = class;
  TResponseForTest02 = class;

  // message ResponseForTest01
  { TResponseForTest01 }
  TResponseForTest01 = class(TBaseMessage)
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
    function DeepCopy: TResponseForTest01;

  end;

  // message ResponseForTest02
  { TResponseForTest02 }
  TResponseForTest02 = class(TBaseMessage)
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
    function DeepCopy: TResponseForTest02;

  end;



implementation

function TResponseForTest01.GetText: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FText; 

end;


constructor TResponseForTest01.Create;
begin
  inherited Create;


end;


destructor TResponseForTest01.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TResponseForTest01.Clear;
begin

  inherited;
end;

procedure TResponseForTest01.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Text, 1);

end;


function TResponseForTest01.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
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

function TResponseForTest01.DeepCopy: TResponseForTest01;
begin
  if Self = nil then
    Exit(nil);

  Result := TResponseForTest01.Create;

  Result.Text := Self.Text;

end;

function TResponseForTest02.GetText: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FText; 

end;


constructor TResponseForTest02.Create;
begin
  inherited Create;


end;


destructor TResponseForTest02.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TResponseForTest02.Clear;
begin

  inherited;
end;

procedure TResponseForTest02.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Text, 1);

end;


function TResponseForTest02.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
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

function TResponseForTest02.DeepCopy: TResponseForTest02;
begin
  if Self = nil then
    Exit(nil);

  Result := TResponseForTest02.Create;

  Result.Text := Self.Text;

end;



end.

unit SimpleTypesUnit;
{$Mode objfpc}

interface

uses
 classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TUInt64Message = class;
  TInt64Message = class;
  TStringMessage = class;
  TDoubleMessage = class;
  TFloatMessage = class;

  // message UInt64Message
  { TUInt64Message }
  TUInt64Message = class(TBaseMessage)
  // Forward Declarations.

  private
    FData: UInt64;

  public
    function GetData: UInt64;

  public
    // uint64 data = 1;
    property Data: UInt64 read FData write FData;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TUInt64Message;

  end;

  // message Int64Message
  { TInt64Message }
  TInt64Message = class(TBaseMessage)
  // Forward Declarations.

  private
    FData: Int64;

  public
    function GetData: Int64;

  public
    // int64 data = 1;
    property Data: Int64 read FData write FData;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TInt64Message;

  end;

  // message StringMessage
  { TStringMessage }
  TStringMessage = class(TBaseMessage)
  // Forward Declarations.

  private
    FData: AnsiString;

  public
    function GetData: AnsiString;

  public
    // string data = 1;
    property Data: AnsiString read FData write FData;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TStringMessage;

  end;

  // message DoubleMessage
  { TDoubleMessage }
  TDoubleMessage = class(TBaseMessage)
  // Forward Declarations.

  private
    FData: Double;

  public
    function GetData: Double;

  public
    // double data = 1;
    property Data: Double read FData write FData;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TDoubleMessage;

  end;

  // message FloatMessage
  { TFloatMessage }
  TFloatMessage = class(TBaseMessage)
  // Forward Declarations.

  private
    FData: Single;

  public
    function GetData: Single;

  public
    // float data = 1;
    property Data: Single read FData write FData;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TFloatMessage;

  end;



implementation

function TUInt64Message.GetData: UInt64;
begin
  if Self = nil then
    Exit(0);

  Result := FData; 

end;


constructor TUInt64Message.Create;
begin
  inherited Create;


end;


destructor TUInt64Message.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TUInt64Message.Clear;
begin

  inherited;
end;

procedure TUInt64Message.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveUint64(Stream, Data, 1);

end;


function TUInt64Message.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Data := LoadUint64(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TUInt64Message.DeepCopy: TUInt64Message;
begin
  if Self = nil then
    Exit(nil);

  Result := TUInt64Message.Create;

  Result.Data := Self.Data;

end;

function TInt64Message.GetData: Int64;
begin
  if Self = nil then
    Exit(0);

  Result := FData; 

end;


constructor TInt64Message.Create;
begin
  inherited Create;


end;


destructor TInt64Message.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TInt64Message.Clear;
begin

  inherited;
end;

procedure TInt64Message.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveInt64(Stream, Data, 1);

end;


function TInt64Message.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Data := LoadInt64(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TInt64Message.DeepCopy: TInt64Message;
begin
  if Self = nil then
    Exit(nil);

  Result := TInt64Message.Create;

  Result.Data := Self.Data;

end;

function TStringMessage.GetData: AnsiString;
begin
  if Self = nil then
    Exit('');

  Result := FData; 

end;


constructor TStringMessage.Create;
begin
  inherited Create;


end;


destructor TStringMessage.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TStringMessage.Clear;
begin

  inherited;
end;

procedure TStringMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveString(Stream, Data, 1);

end;


function TStringMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Data := LoadString(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TStringMessage.DeepCopy: TStringMessage;
begin
  if Self = nil then
    Exit(nil);

  Result := TStringMessage.Create;

  Result.Data := Self.Data;

end;

function TDoubleMessage.GetData: Double;
begin
  if Self = nil then
    Exit(0);

  Result := FData; 

end;


constructor TDoubleMessage.Create;
begin
  inherited Create;


end;


destructor TDoubleMessage.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TDoubleMessage.Clear;
begin

  inherited;
end;

procedure TDoubleMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveDouble(Stream, Data, 1);

end;


function TDoubleMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Data := LoadDouble(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TDoubleMessage.DeepCopy: TDoubleMessage;
begin
  if Self = nil then
    Exit(nil);

  Result := TDoubleMessage.Create;

  Result.Data := Self.Data;

end;

function TFloatMessage.GetData: Single;
begin
  if Self = nil then
    Exit(0);

  Result := FData; 

end;


constructor TFloatMessage.Create;
begin
  inherited Create;


end;


destructor TFloatMessage.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TFloatMessage.Clear;
begin

  inherited;
end;

procedure TFloatMessage.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveFloat(Stream, Data, 1);

end;


function TFloatMessage.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
      Data := LoadFloat(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TFloatMessage.DeepCopy: TFloatMessage;
begin
  if Self = nil then
    Exit(nil);

  Result := TFloatMessage.Create;

  Result.Data := Self.Data;

end;



end.

unit SessionDataUnit;
{$Mode objfpc}
interface

uses 
    classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit;

type


  TNameValue = Class(TBaseMessage)
  // Declarations for string name = 1;
  private
    FName: AnsiString;
  public
    property Name: AnsiString read FName write FName;

  // Declarations for string value = 2;
  private
    FValue: AnsiString;
  public
    property Value: AnsiString read FValue write FValue;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
    function ToString(const Indent: AnsiString): AnsiString;

  public 
    constructor Create;
    constructor Create(aName: AnsiString; aValue: AnsiString);
    destructor Destroy; override;
    function ToString: AnsiString; override;
 
  end;

  TSessionInfo = Class(TBaseMessage)
  // Declarations for string session_ID = 1;
  private
    FSessionID: AnsiString;
  public
    property SessionID: AnsiString read FSessionID write FSessionID;

  // Declarations for int32 creation_timestamp = 2;
  private
    FCreationTimestamp: Int32;
  public
    property CreationTimestamp: Int32 read FCreationTimestamp write FCreationTimestamp;

  // Declarations for int32 last_activity_timestamp = 3;
  private
    FLastActivityTimestamp: Int32;
  public
    property LastActivityTimestamp: Int32 read FLastActivityTimestamp write FLastActivityTimestamp;

  // Methods for repeated NameValue name_values = 4;
  private
    FNameValues: specialize TObjectList<TNameValue>;

    // Getter Functions
    function GetNameValues(Index: Integer): TNameValue;
    function GetAllNameValues: specialize TObjectList<TNameValue>;
    function GetOrCreateAllNameValues: specialize TObjectList<TNameValue>;

  public
    property NameValues[Index: Integer]: TNameValue read GetNameValues;
    property ConstAllNameValues: specialize TObjectList<TNameValue> read GetAllNameValues;
    property AllNameValues: specialize TObjectList<TNameValue> read GetOrCreateAllNameValues;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;
    function ToString(const Indent: AnsiString): AnsiString;

  public 
    constructor Create;
    constructor Create(aSessionID: AnsiString; aCreationTimestamp: Int32; aLastActivityTimestamp: Int32; aNameValues: specialize TObjectList<TNameValue>);
    destructor Destroy; override;
    function ToString: AnsiString; override;
 
  end;


implementation

uses strutils;

 { TNameValue }

constructor TNameValue.Create;
begin
  inherited Create;

  FName := '';
  FValue := '';
end;

constructor TNameValue.Create(aName: AnsiString; aValue: AnsiString);
begin
  inherited Create;

  FName := aName; 
  FValue := aValue; 

end;

destructor TNameValue.Destroy;
begin

  inherited;
end;

function TNameValue.ToString: AnsiString;
begin
  Exit(Self.ToString(''))
end;

function TNameValue.ToString(const Indent: AnsiString): AnsiString;
begin
  Result := '';

  if FName <> '' then
  begin
    Result += Indent + Format('name: %s ', [FName]);
    Result += sLineBreak;
  end;

  if FValue <> '' then
  begin
    Result += Indent + Format('value: %s ', [FValue]);
    Result += sLineBreak;
  end;


end;

procedure TNameValue.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveAnsiString(Stream, FName, 1);

  SaveAnsiString(Stream, FValue, 2);

end;

function TNameValue.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  Result := True;

  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1: FName := LoadAnsiString(Stream);


    2: FValue := LoadAnsiString(Stream);


    end;
  end;

  Result := StartPos + Len = Stream.Position;
end;


 { TSessionInfo }

// Methods for repeated NameValue name_values = 4;
// Getter Functions
function TSessionInfo.GetNameValues(Index: Integer): TNameValue;
begin
  Result := FNameValues[Index];end;

function TSessionInfo.GetAllNameValues: specialize TObjectList<TNameValue>;
begin
  if Self = nil then
    Exit(nil);
  if FNameValues = nil then
    Exit(nil);
  Result := FNameValues; 
end;

function TSessionInfo.GetOrCreateAllNameValues: specialize TObjectList<TNameValue>;

begin
  if Self = nil then
    Exit(nil);
  if Self.FNameValues = nil then
    FNameValues := specialize TObjectList<TNameValue>.Create;
  Result := FNameValues; 
end;


constructor TSessionInfo.Create;
begin
  inherited Create;

  FSessionID := '';
  FCreationTimestamp := 0;
  FLastActivityTimestamp := 0;
end;

constructor TSessionInfo.Create(aSessionID: AnsiString; aCreationTimestamp: Int32; aLastActivityTimestamp: Int32; aNameValues: specialize TObjectList<TNameValue>);
begin
  inherited Create;

  FSessionID := aSessionID; 
  FCreationTimestamp := aCreationTimestamp; 
  FLastActivityTimestamp := aLastActivityTimestamp; 
  FNameValues := aNameValues; 

end;

destructor TSessionInfo.Destroy;
begin
  FNameValues.Free;

  inherited;
end;

function TSessionInfo.ToString: AnsiString;
begin
  Exit(Self.ToString(''))
end;

function TSessionInfo.ToString(const Indent: AnsiString): AnsiString;
var
  BaseMessage: TBaseMessage;

begin
  Result := '';

  if FSessionID <> '' then
  begin
    Result += Indent + Format('session_ID: %s ', [FSessionID]);
    Result += sLineBreak;
  end;

  if FCreationTimestamp <> 0 then
  begin
    Result += Indent + Format('creation_timestamp: %d ', [FCreationTimestamp]);
    Result += sLineBreak;
  end;

  if FLastActivityTimestamp <> 0 then
  begin
    Result += Indent + Format('last_activity_timestamp: %d ', [FLastActivityTimestamp]);
    Result += sLineBreak;
  end;

    if FNameValues <> nil then
    begin
      Result += 'name_values  = ';
      for BaseMessage in FNameValues do
        Result += Format('[%s%s]', [Indent, BaseMessage.ToString]);
      Result += sLineBreak;
    end;

end;

procedure TSessionInfo.SaveToStream(Stream: TProtoStreamWriter);
var
  SizeNode: TLinkListNode;
  BaseMessage: TBaseMessage;

begin
  SaveAnsiString(Stream, FSessionID, 1);

  SaveInt32(Stream, FCreationTimestamp, 2);

  SaveInt32(Stream, FLastActivityTimestamp, 3);

  if FNameValues <> nil then
    for BaseMessage in FNameValues do
      SaveMessage(Stream, BaseMessage, 4);


end;

function TSessionInfo.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

  BaseMessage: TBaseMessage;

begin
  Result := True;

  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1: FSessionID := LoadAnsiString(Stream);


    2: FCreationTimestamp := LoadInt32(Stream);


    3: FLastActivityTimestamp := LoadInt32(Stream);


    4:
    begin
      if WireType <> 2 then
        Exit(False);
      GetOrCreateAllNameValues.Add(TNameValue.Create);
      if not LoadMessage(Stream, FNameValues.Last) then
        Exit(False);
    end;



    end;
  end;

  Result := StartPos + Len = Stream.Position;
end;

end.

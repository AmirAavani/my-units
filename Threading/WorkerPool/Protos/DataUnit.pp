unit DataUnit;
{$Mode objfpc}

interface

uses
 RequestUnit, ResponseUnit, classes, fgl, sysutils, ProtoHelperUnit, ProtoHelperListsUnit, ProtoStreamUnit, GenericCollectionUnit;

type
  TDataForTest01 = class;
  TDataForTest02 = class;

  // message DataForTest01
  { TDataForTest01 }
  TDataForTest01 = class(TBaseMessage)
  // Forward Declarations.

  private
    FRequest: RequestUnit.TRequestForTest01;

  public
    function GetRequest: RequestUnit.TRequestForTest01;
    function GetOrCreateRequest: RequestUnit.TRequestForTest01;

  public
    // request.RequestForTest01 request = 1;
    property Request: RequestUnit.TRequestForTest01 read FRequest write FRequest;
    property ConstRequest: RequestUnit.TRequestForTest01 read GetRequest;
    property MutableRequest: RequestUnit.TRequestForTest01 read GetOrCreateRequest;

  private
    FResponse: ResponseUnit.TResponseForTest01;

  public
    function GetResponse: ResponseUnit.TResponseForTest01;
    function GetOrCreateResponse: ResponseUnit.TResponseForTest01;

  public
    // response.ResponseForTest01 response = 2;
    property Response: ResponseUnit.TResponseForTest01 read FResponse write FResponse;
    property ConstResponse: ResponseUnit.TResponseForTest01 read GetResponse;
    property MutableResponse: ResponseUnit.TResponseForTest01 read GetOrCreateResponse;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TDataForTest01;

  end;

  // message DataForTest02
  { TDataForTest02 }
  TDataForTest02 = class(TBaseMessage)
  // Forward Declarations.

  private
    FRequest: RequestUnit.TRequestForTest02;

  public
    function GetRequest: RequestUnit.TRequestForTest02;
    function GetOrCreateRequest: RequestUnit.TRequestForTest02;

  public
    // request.RequestForTest02 request = 1;
    property Request: RequestUnit.TRequestForTest02 read FRequest write FRequest;
    property ConstRequest: RequestUnit.TRequestForTest02 read GetRequest;
    property MutableRequest: RequestUnit.TRequestForTest02 read GetOrCreateRequest;

  private
    FResponse: ResponseUnit.TResponseForTest02;

  public
    function GetResponse: ResponseUnit.TResponseForTest02;
    function GetOrCreateResponse: ResponseUnit.TResponseForTest02;

  public
    // response.ResponseForTest02 response = 2;
    property Response: ResponseUnit.TResponseForTest02 read FResponse write FResponse;
    property ConstResponse: ResponseUnit.TResponseForTest02 read GetResponse;
    property MutableResponse: ResponseUnit.TResponseForTest02 read GetOrCreateResponse;

  protected 
    procedure SaveToStream(Stream: TProtoStreamWriter); override;
    function LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;

  public // functions
    function DeepCopy: TDataForTest02;

  end;



implementation

function TDataForTest01.GetRequest: RequestUnit.TRequestForTest01;
begin
  if Self = nil then
    Exit(nil);

  Result := FRequest; 

end;

function TDataForTest01.GetOrCreateRequest: RequestUnit.TRequestForTest01;
begin
  if Self = nil then
    Exit(nil);

  if Self.FRequest = nil then
    FRequest := RequestUnit.TRequestForTest01.Create;

  Result := FRequest; 

end;


function TDataForTest01.GetResponse: ResponseUnit.TResponseForTest01;
begin
  if Self = nil then
    Exit(nil);

  Result := FResponse; 

end;

function TDataForTest01.GetOrCreateResponse: ResponseUnit.TResponseForTest01;
begin
  if Self = nil then
    Exit(nil);

  if Self.FResponse = nil then
    FResponse := ResponseUnit.TResponseForTest01.Create;

  Result := FResponse; 

end;


constructor TDataForTest01.Create;
begin
  inherited Create;


end;


destructor TDataForTest01.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TDataForTest01.Clear;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);

  inherited;
end;

procedure TDataForTest01.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveMessage(Stream, Request, 1);

  SaveMessage(Stream, Response, 2);

end;


function TDataForTest01.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableRequest) then
        Exit(False);
    end;
    2:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableResponse) then
        Exit(False);
    end;

    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TDataForTest01.DeepCopy: TDataForTest01;
begin
  if Self = nil then
    Exit(nil);

  Result := TDataForTest01.Create;

  Result.Request := Self.Request.DeepCopy;

  Result.Response := Self.Response.DeepCopy;


end;

function TDataForTest02.GetRequest: RequestUnit.TRequestForTest02;
begin
  if Self = nil then
    Exit(nil);

  Result := FRequest; 

end;

function TDataForTest02.GetOrCreateRequest: RequestUnit.TRequestForTest02;
begin
  if Self = nil then
    Exit(nil);

  if Self.FRequest = nil then
    FRequest := RequestUnit.TRequestForTest02.Create;

  Result := FRequest; 

end;


function TDataForTest02.GetResponse: ResponseUnit.TResponseForTest02;
begin
  if Self = nil then
    Exit(nil);

  Result := FResponse; 

end;

function TDataForTest02.GetOrCreateResponse: ResponseUnit.TResponseForTest02;
begin
  if Self = nil then
    Exit(nil);

  if Self.FResponse = nil then
    FResponse := ResponseUnit.TResponseForTest02.Create;

  Result := FResponse; 

end;


constructor TDataForTest02.Create;
begin
  inherited Create;


end;


destructor TDataForTest02.Destroy;
begin
  Self.Clear;

  inherited;
end;

procedure TDataForTest02.Clear;
begin
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);

  inherited;
end;

procedure TDataForTest02.SaveToStream(Stream: TProtoStreamWriter);
begin
  SaveMessage(Stream, Request, 1);

  SaveMessage(Stream, Response, 2);

end;


function TDataForTest02.LoadFromStream(Stream: TProtoStreamReader; Len: Integer): Boolean;
var
  StartPos, FieldNumber, WireType: Integer;

begin
  StartPos := Stream.Position;
  while Stream.Position < StartPos + Len do
  begin
    Stream.ReadTag(FieldNumber, WireType);

    case FieldNumber of
    1:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableRequest) then
        Exit(False);
    end;
    2:
    begin
      if WireType <> 2 then
        Exit(False);
      if not LoadMessage(Stream, MutableResponse) then
        Exit(False);
    end;

    end;
  end;

  Result := StartPos + Len = Stream.Position;

end;

function TDataForTest02.DeepCopy: TDataForTest02;
begin
  if Self = nil then
    Exit(nil);

  Result := TDataForTest02.Create;

  Result.Request := Self.Request.DeepCopy;

  Result.Response := Self.Response.DeepCopy;


end;



end.

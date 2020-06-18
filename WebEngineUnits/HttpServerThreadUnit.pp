unit HttpServerThreadUnit;

{$mode objfpc}{$H+}{$I-}

interface

uses
  Classes, SysUtils, fphttpapp, fphttpserver, fgl;

type

  { THTTPServerRequest }

  THTTPServerRequest = class(TObject)
  public
  type
    TMethodEnum = (meGet = 1, mePost);

  private
    FMethod: TMethodEnum;
    FOriginalRequest: TFPHTTPConnectionRequest;
    FUserAgent: AnsiString;
    FParams: specialize TFPGMap<AnsiString, AnsiString>;


    constructor Create(const ARequest: TFPHTTPConnectionRequest);
    function GetContent: AnsiString;
    function GetFieldCount: Integer;
    function GetFieldNames(Index: Integer): AnsiString;
    function GetFieldValues(Index: Integer): AnsiString;
    function GetHeaderLine: AnsiString;

    function GetParamsCount: Integer;
    function GetParamNameByIndex(Index: Integer): AnsiString;
    function GetParamValueByIndex(Index: Integer): AnsiString;
    function GetParamValueByName(Name: AnsiString): AnsiString;
    function GetPathInfo: AnsiString;
    function GetQueryString: AnsiString;

  public
    property Method: TMethodEnum read FMethod;
    property PathInfo: AnsiString read GetPathInfo;
    property QueryString: AnsiString read GetQueryString;
    property UserAgent: AnsiString read FUserAgent;
    property HeaderLine: AnsiString read GetHeaderLine;
    property Content: AnsiString read GetContent;

    property FieldCount: Integer read GetFieldCount;
    property FieldNames[Index: Integer]: AnsiString read GetFieldNames;
    property FieldValues[Index: Integer]: AnsiString read GetFieldValues;
    property ParamsCount: Integer read GetParamsCount;
    property Params: specialize TFPGMap<AnsiString, AnsiString> read FParams;
    property ParamNameByIndex[Index: Integer]: AnsiString read GetParamNameByIndex;
    property ParamValueByIndex[Index: Integer]: AnsiString read GetParamValueByIndex;
    property ParamValueByName[Name: AnsiString]: AnsiString read GetParamValueByName;
    property OriginalRequest: TFPHTTPConnectionRequest read FOriginalRequest;

    destructor Destroy; override;

  end;

  { ENoOpenTag }

  ENoOpenTag = class(Exception)
  public
    constructor Create;
  end;

  { TAttribute }

  TAttribute = class(TObject)
    Name: AnsiString;
    Value: Variant;

    constructor Create(aName: AnsiString; aValue: Variant);
  end;

  { TBaseNode }

  TBaseNode = class(TObject)
  private
    FAttributes: specialize TFPGList<TAttribute>;
    FParentNode: TBaseNode;

    function GetAttributeByIndex(Index: Integer): TAttribute;
    function GetAttributeByName(aName: AnsiString): AnsiString;

  public
    property Attributes: specialize TFPGList<TAttribute> read FAttributes;
    property Attribute[Index: Integer]: TAttribute read GetAttributeByIndex;
    property AttributeByName[aName: AnsiString]: AnsiString read GetAttributeByName;

    constructor Create(PNode: TBaseNode);
    destructor Destroy; override;

    procedure AddAttribute(Name: AnsiString; Value: Variant);
  end;

{ TNode }

  TNode = class(TBaseNode)
  private
    FTagName: AnsiString;
    FChildren: specialize TFPGList<TBaseNode>;

  public
    property TagName: AnsiString read FTagName;

   constructor Create(aTagName: AnsiString; PNode: TBaseNode);
   destructor Destroy; override;


  end;

  { THTTPServerResponse }

  THTTPServerResponse = class(TObject)
  private
    FOutputStream: TStringStream;
    FRootNode: TNode;
    FOriginalResponse: TFPHTTPConnectionResponse;

    constructor Create(const AResponse: TFPHTTPConnectionResponse);

  public
    property OriginalResponse: TFPHTTPConnectionResponse read FOriginalResponse;
    property OutputStream: TStringStream read FOutputStream;

    procedure OpenTag(TagName: AnsiString); virtual;
    procedure AddAttribute(aName: AnsiString; aValue: Variant);
    procedure CloseTag;

    procedure WriteLn(Lines: array of AnsiString);
    procedure WriteLn(Line: AnsiString);

    destructor Destroy; override;

  end;

  TBasePageHandler = class;

  { THTTPServerThread }

  THTTPServerThread = class(TThread)
  private type
    TPageHandlers = specialize TFPGList<TBasePageHandler>;

  private
    FPageNotFoundHandler: TBasePageHandler;
    PageHandlers: TPageHandlers;

  protected
    Server: TFPHTTPServer;
    procedure DefaultRequestHandler(Sender: TObject;
      ARequest: THTTPServerRequest;
      AResponse : THTTPServerResponse); virtual;

    procedure HandleRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse : TFPHTTPConnectionResponse);


  public
    constructor Create(APort: Word; PageNotFoundHandler: TBasePageHandler);
    destructor Destroy; override;

    procedure RegisterPageHandler(const PageHandler: TBasePageHandler);
    procedure Start;
  end;


  { TBasePageHandler }

  TBasePageHandler = class(TObject)
  private
    FName: AnsiString;
    FServingPath: AnsiString;

  public
    property Name: AnsiString read FName;
    property ServingPath: AnsiString read FServingPath;

    constructor Create(aName: AnsiString; aServingPath: AnsiString);

    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; virtual;
    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; virtual; abstract;
  end;


implementation
uses
  DefaultPageHandlerUnit, httpprotocol;

{ TBasePageHandler }

constructor TBasePageHandler.Create(aName: AnsiString; aServingPath: AnsiString
  );
begin
  inherited Create;

  FName := aName;
  FServingPath := aServingPath;

end;

function TBasePageHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  Result := False;

end;

{ TNode }

constructor TNode.Create(aTagName: AnsiString; PNode: TBaseNode);
begin
  inherited Create(PNode);

  FTagName := aTagName;
  FChildren := nil;
end;

destructor TNode.Destroy;
var
  Child: TBaseNode;

begin
  if FChildren <> nil then
  begin
    for Child in FChildren do
      Child.Free;
    FChildren.Free;

  end;
  inherited Destroy;
end;

{ TBaseNode }

function TBaseNode.GetAttributeByIndex(Index: Integer): TAttribute;
begin
  if FAttributes = nil then
    Exit(nil);
  Result := FAttributes[Index];

end;

function TBaseNode.GetAttributeByName(aName: AnsiString): AnsiString;
var
  Attr: TAttribute;

begin
  if FAttributes = nil then
    Exit('');
  for Attr in FAttributes do
    if Attr.Name = aName then
      Exit(Attr.Value);

  Result := '';

end;

constructor TBaseNode.Create(PNode: TBaseNode);
begin
  inherited Create;

  FAttributes := nil;
  FParentNode := PNode;
end;

destructor TBaseNode.Destroy;
var
  Attr: TAttribute;

begin
  for Attr in FAttributes do
    Attr.Free;
  FAttributes.Free;

  inherited Destroy;
end;

procedure TBaseNode.AddAttribute(Name: AnsiString; Value: Variant);
begin
  if FAttributes = nil then
  begin
    FAttributes := (specialize TFPGList<TAttribute>).Create;
    Self.AddAttribute(Name, Value);
    Exit;
  end;

  FAttributes.Add(TAttribute.Create(Name, Value));

end;

{ TAttribute }

constructor TAttribute.Create(aName: AnsiString; aValue: Variant);
begin
  inherited Create;

  Name := aName;
  Value:= aValue;
end;


{ ENoOpenTag }

constructor ENoOpenTag.Create;
begin
  inherited Create('');

end;

{ THTTPServerResponse }

constructor THTTPServerResponse.Create(
  const AResponse: TFPHTTPConnectionResponse);
begin
  inherited Create;

  FOriginalResponse := AResponse;
  FRootNode := TNode.Create('HTML', nil);
  FOutputStream := TStringStream.Create('');

end;

procedure THTTPServerResponse.OpenTag(TagName: AnsiString);
begin
  //FTags.Add(TTagInfo.Create(TagName));

end;

procedure THTTPServerResponse.AddAttribute(aName: AnsiString; aValue: Variant);
begin

end;

procedure THTTPServerResponse.CloseTag;
{var
  Tag: TTagInfo;
 }
begin
{  if FTags.Count = 0 then
    raise ENoOpenTag.Create;
  Tag:= FTags[FTags.Count - 1];
  FTags.Delete(FTags.Count - 1);

  Self.WriteLn(Tag.ToString);
}
end;

procedure THTTPServerResponse.WriteLn(Lines: array of AnsiString);
var
  Line: AnsiString;

begin
  for Line in Lines do
    Self.WriteLn(Line);
end;

procedure THTTPServerResponse.WriteLn(Line: AnsiString);
begin
  OutputStream.WriteString(Line);
  OutputStream.WriteString(sLineBreak);

end;

destructor THTTPServerResponse.Destroy;
begin
  OriginalResponse.Content := OutputStream.DataString;
  OutputStream.Free;

  inherited Destroy;
end;

{ THTTPServerRequest }

constructor THTTPServerRequest.Create(const ARequest: TFPHTTPConnectionRequest);

  procedure FillGetRequest;
  var
    StrList: TStringList;
    NameValue: AnsiString;
    i: Integer;

  begin
    StrList := TStringList.Create;
    StrList.Delimiter := '&';
    StrList.DelimitedText := ARequest.QueryString;

    for i := 0 to StrList.Count - 1 do
    begin
      NameValue := StrList[i];

      if Pos('=', NameValue) <> 0 then
        FParams[Copy(NameValue, 1, Pos('=', NameValue) - 1)] :=
          Copy(NameValue, Pos('=', NameValue) + 1, Length(NameValue))
      else
        FParams[NameValue] := '';
    end;

    StrList.Free;
  end;

  procedure FillPostRequest;
  var
    StrList: TStringList;
    NameValue: AnsiString;
    i: Integer;

  begin
    StrList := TStringList.Create;
    StrList.Delimiter := '&';
    StrList.DelimitedText := ARequest.Content;

    for i := 0 to StrList.Count - 1 do
    begin
      NameValue := StrList[i];
      if Pos('=', NameValue) <> 0 then
        FParams[Copy(NameValue, 1, Pos('=', NameValue) - 1)] :=
          Copy(NameValue, Pos('=', NameValue) + 1, Length(NameValue))
      else
        FParams[NameValue] := '';
    end;

    StrList.Free;
  end;

begin
  inherited Create;

  FOriginalRequest := ARequest;
  FParams := (specialize TFPGMap<AnsiString, AnsiString>).Create;
  FParams.Sorted := True;

  if OriginalRequest.Method = 'GET' then
  begin
    Self.FMethod := meGet;
    FillGetRequest;
  end
  else if ARequest.Method = 'POST' then
  begin
    Self.FMethod := mePost;
    FillPostRequest
  end
  else
    raise Exception.Create('NIY: No Support for ' + ARequest.Method);

  Self.FUserAgent := ARequest.UserAgent;

end;

function THTTPServerRequest.GetContent: AnsiString;
begin
  Result := OriginalRequest.Content;
end;

function THTTPServerRequest.GetFieldCount: Integer;
begin
  Result := OriginalRequest.FieldCount;

end;

function THTTPServerRequest.GetFieldNames(Index: Integer): AnsiString;
begin
  Result := OriginalRequest.FieldNames[Index];
end;

function THTTPServerRequest.GetFieldValues(Index: Integer): AnsiString;
begin
  Result := OriginalRequest.FieldValues[Index];
end;

function THTTPServerRequest.GetHeaderLine: AnsiString;
begin
  Result := FOriginalRequest.HeaderLine;

end;

function THTTPServerRequest.GetParamsCount: Integer;
begin
  Result := FParams.Count;
end;

function THTTPServerRequest.GetParamNameByIndex(Index: Integer): AnsiString;
begin
  Result := FParams.Keys[Index];

end;

function THTTPServerRequest.GetParamValueByIndex(Index: Integer): AnsiString;
begin
  Result := FParams.Data[Index];

end;

function THTTPServerRequest.GetParamValueByName(Name: AnsiString): AnsiString;
begin
  if FParams.IndexOf(Name) < 0 then
    Exit('');
  Result := FParams[Name];

end;

function THTTPServerRequest.GetPathInfo: AnsiString;
begin
  Result := OriginalRequest.PathInfo;
end;

function THTTPServerRequest.GetQueryString: AnsiString;
begin
  Result := OriginalRequest.QueryString;

end;

destructor THTTPServerRequest.Destroy;
begin
  FParams.Free;

  inherited Destroy;
end;

procedure THTTPServerThread.DefaultRequestHandler(Sender: TObject;
  ARequest: THTTPServerRequest; AResponse: THTTPServerResponse);
var
  PageHandler: TBasePageHandler;

begin
  for PageHandler in PageHandlers do
    if PageHandler.WouldHandleRequest(ARequest) then
      if PageHandler.Execute(Self, ARequest, AResponse) then
        Exit;

  FPageNotFoundHandler.Execute(Self, ARequest, AResponse);
end;

procedure THTTPServerThread.HandleRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  Request: THTTPServerRequest;
  Response: THTTPServerResponse;

begin
  Request := THTTPServerRequest.Create(ARequest);
  Response := THTTPServerResponse.Create(AResponse);

  Self.DefaultRequestHandler(Sender, Request, Response);

  Request.Free;
  Response.Free;

end;

constructor THTTPServerThread.Create(APort: Word;
  PageNotFoundHandler: TBasePageHandler);
begin
  inherited Create(True);

  Server := TFPHttpServer.Create(nil);
  Server.Port := APort;
  Server.OnRequest := @Self.HandleRequest;
  Server.Threaded := True;
  Self.FreeOnTerminate := False;

  FPageNotFoundHandler := PageNotFoundHandler;
  PageHandlers := TPageHandlers.Create;
end;

destructor THTTPServerThread.Destroy;
var
  PageHandler: TBasePageHandler;

begin
  for PageHandler in PageHandlers do
    PageHandler.Free;

  Server.Free;

end;

procedure THTTPServerThread.RegisterPageHandler(
  const PageHandler: TBasePageHandler);
begin
  PageHandlers.Add(PageHandler);
end;

procedure THTTPServerThread.Start;
begin
  Server.Active := True;

end;

end.

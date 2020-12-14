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
    function GetCookieByName(const aName: AnsiString): AnsiString;
    function GetFieldCount: Integer;
    function GetFieldNames(Index: Integer): AnsiString;
    function GetFieldValues(Index: Integer): AnsiString;
    function GetHeaderLine: AnsiString;

    function GetParamsCount: Integer;
    function GetParamNameByIndex(Index: Integer): AnsiString;
    function GetParamValueByIndex(Index: Integer): AnsiString;
    function GetParamValueByName(Name: AnsiString): AnsiString;
    function GetParamValueOrDefaultByName(Name, DefaultValue: AnsiString): AnsiString;
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
    property ParamValueOrDefaultByName[Name, DefaultValue: AnsiString]: AnsiString read GetParamValueOrDefaultByName;
    property OriginalRequest: TFPHTTPConnectionRequest read FOriginalRequest;
    property CookieByName[const aName: AnsiString]: AnsiString read GetCookieByName;

    destructor Destroy; override;

  end;

  { THTTPServerResponse }

  THTTPServerResponse = class(TObject)
  private
    FOutputStream: TStringStream;
    FOriginalResponse: TFPHTTPConnectionResponse;

    constructor Create(const AResponse: TFPHTTPConnectionResponse);

  public
    property OriginalResponse: TFPHTTPConnectionResponse read FOriginalResponse;
    property OutputStream: TStringStream read FOutputStream;

    procedure WriteLn(Lines: array of AnsiString);
    procedure WriteLn(Line: AnsiString);

    destructor Destroy; override;

    procedure Redirect(TargetPage: AnsiString);
    procedure AddCookie(Name, Value, Path: AnsiString; Domain: AnsiString = '';
      MaxAge: Integer = 46800);
  end;

  TBasePageHandler = class;

  { THTTPServerThread }

  THTTPServerThread = class(TObject)
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
  ALoggerUnit, DefaultPageHandlerUnit, WebUtilsUnit, CookieUnit, StringUnit, httpprotocol,
  HTTPDefs;

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
  Result := ARequest.PathInfo = ServingPath;

end;


{ THTTPServerResponse }

constructor THTTPServerResponse.Create(
  const AResponse: TFPHTTPConnectionResponse);
begin
  inherited Create;

  FOriginalResponse := AResponse;
  FOutputStream := TStringStream.Create('');

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

procedure THTTPServerResponse.Redirect(TargetPage: AnsiString);
begin
  OriginalResponse.SendRedirect(TargetPage);

end;

procedure THTTPServerResponse.AddCookie(Name, Value, Path: AnsiString;
  Domain: AnsiString; MaxAge: Integer);
var
  Cookie: HTTPDefs.TCookie;

begin
  Cookie := Self.OriginalResponse.Cookies.Add;
  Cookie.Name := Name;
  Cookie.Value := Value;
  Cookie.Domain := Domain;
  Cookie.Expires := TimeStampToDateTime(
    MSecsToTimeStamp(
      1000 * (DateTimeToTimeStamp(Now).Time + MaxAge)));

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

    for i := 0 to FParams.Count - 1 do
      FParams.Data[i] := NormalizeGetString(FParams.Data[i]);

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
        FParams[NormalizePostString(Copy(NameValue, 1, Pos('=', NameValue) - 1))] :=
          NormalizePostString(Copy(NameValue, Pos('=', NameValue) + 1, Length(NameValue)))
      else
        FParams[NormalizePostString(NameValue)] := '';
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

function THTTPServerRequest.GetCookieByName(const aName: AnsiString
  ): AnsiString;
var
  Lines: TStringList;
  Line: AnsiString;
  p: Integer;

begin
  Lines := Split(OriginalRequest.Cookie, ';');
  Result := '';

  for Line in Lines do
  begin
    p := Pos('=', Line);
    if p = 0 then
      Continue;
    if Copy(Line, 1, p - 1) = aName then
    begin
      Result := Copy(Line, p + 1, Length(Line));
      Break;
    end;

  end;

  Lines.Free;
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

function THTTPServerRequest.GetParamValueOrDefaultByName(Name, DefaultValue: AnsiString
  ): AnsiString;
begin
  if FParams.IndexOf(Name) < 0 then
    Exit(DefaultValue);
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
  inherited Create;

  Server := TFPHttpServer.Create(nil);
  Server.Port := APort;
  Server.OnRequest := @Self.HandleRequest;
  Server.Threaded := True;

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

unit HttpServerThreadUnit;

{$mode objfpc}{$H+}{$I-}

interface

uses
  Classes, SysUtils, fphttpapp, fphttpserver, GenericCollectionUnit;

type

  { THTTPServerRequest }

  THTTPServerRequest = class(TObject)
  public
  type
    TMethodEnum = (meGet = 1, mePost);
    TParams = specialize TMap<AnsiString, AnsiString>;
  private
    FMethod: TMethodEnum;
    FOriginalRequest: TFPHTTPConnectionRequest;
    FUserAgent: AnsiString;
    FParams: TParams;


    constructor Create(const ARequest: TFPHTTPConnectionRequest);
    function GetContent: AnsiString;
    function GetCookieByName(constref aName: AnsiString): AnsiString;
    function GetFieldCount: Integer;
    function GetFieldNames(Index: Integer): AnsiString;
    function GetFieldValues(Index: Integer): AnsiString;
    function GetHeaderLine: AnsiString;

    function GetParamsCount: Integer;
    function GetParamNameByIndex(Index: Integer): AnsiString;
    function GetParamValueByIndex(Index: Integer): AnsiString;
    function GetParamValueByName(constref Name: AnsiString): AnsiString;
    function GetParamValueOrDefaultByName(constref Name, DefaultValue: AnsiString): AnsiString;
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
    property Params: specialize TMap<AnsiString, AnsiString> read FParams;
    property ParamNameByIndex[Index: Integer]: AnsiString read GetParamNameByIndex;
    property ParamValueByIndex[Index: Integer]: AnsiString read GetParamValueByIndex;
    property ParamValueByName[constref Name: AnsiString]: AnsiString read GetParamValueByName;
    property ParamValueOrDefaultByName[constref Name, DefaultValue: AnsiString]: AnsiString read GetParamValueOrDefaultByName;
    property OriginalRequest: TFPHTTPConnectionRequest read FOriginalRequest;
    property CookieByName[constref aName: AnsiString]: AnsiString read GetCookieByName;

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

    procedure WriteLn(constref Lines: array of AnsiString);
    procedure WriteLn(constref Line: AnsiString);

    destructor Destroy; override;

    procedure Redirect(constref TargetPage: AnsiString);
    procedure AddCookie(constref Name, Value, Path: AnsiString;
      constref Domain: AnsiString = '';
      MaxAge: Integer = 46800);
    procedure DeleteCookie(constref Name, Path: AnsiString; constref Domain: AnsiString = '');
  end;

  TBasePageHandler = class;

  { THTTPServerThread }

  THTTPServerThread = class(TObject)
  private type
    TPageHandlers = specialize TObjectCollection<TBasePageHandler>;

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
    procedure Stop;
  end;


  { TBasePageHandler }

  TBasePageHandler = class(TObject)
  private
    FName: AnsiString;
    FServingPaths: TStringList;
    function GetServingPath: AnsiString;

  public
    property Name: AnsiString read FName;
    property ServingPath: AnsiString read GetServingPath;
    property ServingPaths: TStringList read FServingPaths;

    constructor Create(constref aName: AnsiString; constref aServingPath: AnsiString);
    constructor Create(constref aName: AnsiString; TheServingPaths: array of AnsiString);
    destructor Destroy; override;

    function WouldHandleRequest(ARequest: THTTPServerRequest): Boolean; virtual;
    function Execute(Sender: THTTPServerThread; TheRequest: THTTPServerRequest;
      TheResponse : THTTPServerResponse): Boolean; virtual; abstract;
  end;


implementation
uses
  ALoggerUnit, DefaultPageHandlerUnit, WebUtilsUnit, CookieUnit, StringUnit, httpprotocol,
  HTTPDefs;

{ TBasePageHandler }

function TBasePageHandler.GetServingPath: AnsiString;
begin
  Result := FServingPaths[0];
end;

constructor TBasePageHandler.Create(constref aName: AnsiString;
  constref aServingPath: AnsiString);
begin
  inherited Create;

  FServingPaths := TStringList.Create;
  FName := aName;
  FServingPaths.Add(aServingPath);

end;

constructor TBasePageHandler.Create(constref aName: AnsiString;
  TheServingPaths: array of AnsiString);
var
  Path: AnsiString;

begin
  inherited Create;

  FServingPaths := TStringList.Create;
  FName := aName;
  for Path in TheServingPaths do
    FServingPaths.Add(Path);

end;

destructor TBasePageHandler.Destroy;
begin
  FServingPaths.Free;

  inherited Destroy;
end;

function TBasePageHandler.WouldHandleRequest(ARequest: THTTPServerRequest
  ): Boolean;
begin
  FMTDebugLn('Path: "%s" "%s"', [ARequest.PathInfo, JoinStrings(FServingPaths, '**')]);
  Result := 0 <= FServingPaths.IndexOf(ARequest.PathInfo);

end;


{ THTTPServerResponse }

constructor THTTPServerResponse.Create(
  const AResponse: TFPHTTPConnectionResponse);
begin
  inherited Create;

  FOriginalResponse := AResponse;
  FOutputStream := TStringStream.Create('');

end;

procedure THTTPServerResponse.WriteLn(constref Lines: array of AnsiString);
var
  Line: AnsiString;

begin
  for Line in Lines do
    Self.WriteLn(Line);
end;

procedure THTTPServerResponse.WriteLn(constref Line: AnsiString);
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

procedure THTTPServerResponse.Redirect(constref TargetPage: AnsiString);
begin
  OriginalResponse.SendRedirect(TargetPage);

end;

procedure THTTPServerResponse.AddCookie(constref Name, Value, Path: AnsiString;
  constref Domain: AnsiString; MaxAge: Integer);
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

procedure THTTPServerResponse.DeleteCookie(constref Name, Path: AnsiString;
  constref Domain: AnsiString);
var
  Cookie: HTTPDefs.TCookie;

begin
  Cookie := Self.OriginalResponse.Cookies.Add;
  Cookie.Name := Name;
  Cookie.Path := Domain;
  Cookie.Domain := Domain;
  Cookie.Expire;


end;

const
  PostLineBreak = #$0D#$0A;

{ THTTPServerRequest }

constructor THTTPServerRequest.Create(const ARequest: TFPHTTPConnectionRequest);

  function MaybeSkip(var SourcePtr: PChar; Pattern: PChar): Boolean;
  var
    Count: Integer;

  begin
    Count := 0;

    while Pattern^ <> #0 do
    begin
      if SourcePtr^ <> Pattern^ then
        Break;
      Inc(Count);
      Inc(Pattern);
      Inc(SourcePtr);

    end;

    Result := Pattern^ = #0;
    if not Result then
      Dec(SourcePtr, Count);

  end;

  procedure FillGetRequest;
  var
    StrList: TStringList;
    NameValue: AnsiString;
    i, Index: Integer;
    Name, Value: AnsiString;

  begin
    StrList := TStringList.Create;
    StrList.Delimiter := '&';
    StrList.DelimitedText := ARequest.QueryString;

    for i := 0 to StrList.Count - 1 do
    begin
      NameValue := StrList[i];

      Index := Pos('=', NameValue);
      if Index <> 0 then
      begin
        Name :=  Copy(NameValue, 1, Index - 1);
        Value := Copy(NameValue, Index + 1, Length(NameValue));

      end
      else
      begin
        Name := NameValue;;
        Value := '';

      end;
      Name := NormalizeGetString(Name);
      Value := NormalizeGetString(Value);

      FParams.Add(Name, Value);

    end;

    StrList.Free;

  end;

  procedure FillPostRequest;

    procedure FillURLEncodedRequest(constref Content: AnsiString);
    var
      StrList: TStringList;
      NameValue: AnsiString;
      Name, Value: AnsiString;
      i: Integer;

    begin
      StrList := TStringList.Create;
      StrList.Delimiter := '&';
      StrList.DelimitedText := ARequest.Content;

      for i := 0 to StrList.Count - 1 do
      begin
        NameValue := StrList[i];
        if Pos('=', NameValue) <> 0 then
        begin
          Name := NormalizePostString(Copy(NameValue, 1, Pos('=', NameValue) - 1));
          Value := NormalizePostString(Copy(NameValue, Length(Name) + 2, Length(NameValue)));

        end
        else
        begin
          Name := NormalizePostString(NameValue);
          Value := '';

        end;

        if FParams.ContainsKey(Name) then
          FParams[Name] := Value
        else
          FParams.Add(Name, Value);

      end;

      StrList.Free;

    end;

    function MaybeFillFormDataRequest(constref Content: AnsiString): Boolean;
    const
      ContentDisposition = 'Content-Disposition: form-data; name="';

    var
      ContentPtr: PChar;
      BoundaryString: AnsiString;
      Name, Value, Data: AnsiString;
      Names, Values: TStringList;
      StrList: TStringList;
      i: Integer;

    begin
      FMTDebugLn('Content: %s', [Content]);
      ContentPtr := PChar(Content);

      BoundaryString := '';
      while not IsPrefix(PostLineBreak, ContentPtr) and (ContentPtr^ <> #0) do
      begin
        BoundaryString += ContentPtr^;
        Inc(ContentPtr);

      end;
      if ContentPtr^ = #0 then
        Exit(False);
      if not IsPrefix('--', BoundaryString) then
        Exit(False);
      if not MaybeSkip(ContentPtr, PostLineBreak) then
        Exit(False);

      StrList := Split(Content, BoundaryString);
      if (StrList.Count = 0) or (StrList[0] <> '') then
      begin
        StrList.Free;
        Exit(False);

      end;

      Result := StrList[StrList.Count - 1] = '--' + PostLineBreak;
      Names := TStringList.Create; Values := TStringList.Create;

      for i := 1 to StrList.Count - 2 do
      begin
        Data := StrList[i];

        ContentPtr := PChar(Data);
        if not MaybeSkip(ContentPtr, PostLineBreak) or not MaybeSkip(ContentPtr, PChar(ContentDisposition)) then
        begin
          Result := False;
          Break;

        end;

        Name := '';
        while not IsPrefix(PChar('"' + PostLineBreak), ContentPtr) and (ContentPtr^<> #0) do
        begin
          Name += ContentPtr^;
          Inc(ContentPtr);

        end;
        if not MaybeSkip(ContentPtr, '"') then
        begin
          Result := False;
          Break;

        end;
        if not MaybeSkip(ContentPtr, PostLineBreak + PostLineBreak) then
        begin
          Result := False;
          Break;

        end;
        Value := AnsiString(ContentPtr);
        Value := Copy(Value, 1, Length(Value) - Length(PostLineBreak));

        Names.Add(Name);
        Values.Add(Value);

      end;

      StrList.Free;
      if not Result then
      begin
        Names.Free;
        Values.Free;
        Exit;

      end;

      for i := 0 to Names.Count - 1 do
      begin
        FParams.Add(Names[i], Values[i]);

      end;
      Names.Free;
      Values.Free;

    end;

  begin
    if not MaybeFillFormDataRequest(ARequest.Content) then
      FillURLEncodedRequest(ARequest.Content);
  end;

begin
  inherited Create;

  FOriginalRequest := ARequest;
  FParams := TParams.Create;

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

function THTTPServerRequest.GetCookieByName(constref aName: AnsiString
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
    if TrimLeft(Copy(Line, 1, p - 1)) = aName then
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
var
  it: TParams.TPairEnumerator;

begin
  it := FParams.GetEnumerator;
  while 0 <= Index do
    if not it.MoveNext then
      Exit;
  Result := it.Current.Key;

end;

function THTTPServerRequest.GetParamValueByIndex(Index: Integer): AnsiString;
var
  it: TParams.TPairEnumerator;

begin
  it := FParams.GetEnumerator;
  while 0 <= Index do
    if not it.MoveNext then
      Exit;
  Result := it.Current.Value;

end;

function THTTPServerRequest.GetParamValueByName(constref Name: AnsiString): AnsiString;
begin
  Result := FParams.Find(Name);

end;

function THTTPServerRequest.GetParamValueOrDefaultByName(constref Name, DefaultValue: AnsiString
  ): AnsiString;
begin
  if not FParams.TryGetData(Name, Result) then
    Exit(DefaultValue);

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
  begin
    if PageHandler.WouldHandleRequest(ARequest) then
    begin
      if PageHandler.Execute(Self, ARequest, AResponse) then
        Exit;
    end;
  end;

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
begin
  FPageNotFoundHandler.Free;
  PageHandlers.Free;
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

procedure THTTPServerThread.Stop;
begin
  Server.Active := False;

end;

end.

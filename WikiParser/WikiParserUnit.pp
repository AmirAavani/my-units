unit WikiParserUnit;

{$mode ObjFPC}{$H+}
{$modeswitch ADVANCEDRECORDS}


interface

uses
  Classes, SysUtils, Laz2_DOM, WikiDocUnit;

type

  { EBaseWikiParser }

  EBaseWikiParser = class(Exception)
  public
    constructor Create(constref msg: AnsiString);
  end;

function ParseWiki(Node: TDOMNode): TWikiPage;

implementation
uses
  ALoggerUnit, WideStringUnit, GenericCollectionUnit,
  StringUnit;

type
  TTokenType = (ttNone,
  ttText, ttLessThan, ttGreaterThan,
  ttBeginTag, ttEndTag,
  ttIdentifier, ttEqualSign, ttString, ttNewLine,
  ttBeginTemplate, ttEndTemplate,
  ttBeginComment, ttEndComment,
  ttOpenHeadingSection,
  ttCloseHeadingSection,
  ttOpenBracket, ttCloseBracket,
  ttOpenCurlyBrace, ttCloseCurlyBrace,
  ttOpenHyperLink, ttCloseHyperLink,
  ttBeginTable, ttEndTable,
  ttTableCaption,
  ttBeginTableRow,
  ttTableCellSeparator,
  ttSingleQuote, ttStylerItalic, ttStylerBold,
  ttDoubleQuote,
  ttMinus, ttBar,
  ttBulletedList, ttNumberedList,
  ttSlash, ttEOF);

  { TToken }

  TToken = record
    TokenType: TTokenType;
    TokenStart, TokenLast: PWideChar;
    // Text: WideString;

    function IsEmpty: Boolean;
    function IsSame(constref OtherToken: TToken): Boolean;
    function Text: WideString;
  end;

  { TWikiTokenizer }

  TWikiTokenizer = class(TObject)
  private
    Current, Prev: PWideChar;
    LastToken: TToken;

    //function GetCurrentToken: TToken;
    function GetNextChar: WideChar;
    procedure GetNextToken;
    procedure _GetNextToken(var Result: TToken);

    procedure Rewind;
  public
    //property NextToken: TToken read GetNextToken;
    // property CurrentToken: TToken read GetCurrentToken;
    constructor Create(constref Data: WideString);

  end;

  { TTokens }

  TTokens = class(specialize TCollection<TToken>)
  end;

  { EInvalidToken }

  EInvalidToken = class(Exception)
  public
    constructor Create(Visited, Expected: TTokenType);
    constructor Create(constref Visited: AnsiString);

  end;

  { EInvalidEntity }

  EInvalidEntity = class(EBaseWikiParser)
  private
    FToken: TToken;

  public
    property Token: TToken read FToken;
    constructor Create(_Token: TToken);
  end;

  { TWikiParser }

  TWikiParser = class(TObject)
  private
    FTokenizer: TWikiTokenizer;
    property Tokenizer: TWikiTokenizer read FTokenizer;


    function IsDone(constref Token: TToken; EndTokens: TTokens): Boolean;
    function ParseEntity(EndTokens: TTokens): TBaseWikiNode;
    function ParseTag(constref Token: TToken; EndTokens: TTokens): TTagEntity;
    function ParseTextEntity(constref Token: TToken; EndTokens: TTokens): TTextWikiEntity;
    function ParseHyperLink(EndTokens: TTokens): THyperLinkEntity;
    function ParseTemplate(EndTokens: TTokens): TTemplate;
    function ParseComment(EndTokens: TTokens): TCommentWikiEntry;
    function ParseTable(EndTokens: TTokens): TTable;
    function ParseHeadingSection(constref Token: TToken; EndTokens: TTokens): THeadingSection;
    function ParseSeparator(constref Token: TToken): TSeparatorWikiEntry;
    function ParseCaption(EndTokens: TTokens): TBaseWikiNode;
    function ParseSeparatorOrHyperLink(constref Token: TToken; EndTokens: TTokens): TBaseWikiNode;

    function ParseEntityWithSingleQuoteToken(constref AToken: TToken; EndTokens: TTokens): TTextWikiEntity;

    // TODO: Implement these two functions correctly.
    function ParseBulletList(constref Token: TToken; EndTokens: TTokens): TBaseWikiNode;
    function ParseNumberedList(constref Token: TToken; EndTokens: TTokens): TBaseWikiNode;

    function ParseUntilNil(Current: TBaseWikiNode; EndTokens: TTokens): TBaseWikiNode;
  public
    constructor Create(_Tokenizer: TWikiTokenizer);
    destructor Destroy; override;

    function ParseDoc: TNodes;

  end;

type
  TSetOfTokenType = set of TTokenType;
  PObject= ^TObject;

var
  it: Integer;


procedure FreeObjects(Objs: array of PObject);
var
  i: Integer;

begin
  for i := 0 to High(Objs) do
    if Objs[i]^ <> nil then
      FreeAndNil((Objs[i])^);

end;

function IsUnAcceptable(TokenType: TTokenType;
  setOfAcceptableTokenType: TSetOfTokenType): Boolean;
begin
  if TokenType = ttEOF then
    Exit(False);

  Result := not (TokenType in setOfAcceptableTokenType);

end;


function TokenTypeToString(TokenType: TTokenType): AnsiString;
begin
  WriteStr(Result, TokenType);

end;

function TokenTypeToString(Token: TToken): AnsiString;
begin
  Result := TokenTypeToString(Token.TokenType);

end;

function HasPrefix(Current: PWideChar; constref StrToProbe: AnsiString): Boolean;
var
  i: Integer;

begin
  Result := False;
  for i := 1 to Length(StrToProbe) do
  begin
    if Current^ <> StrToProbe[i] then
      Exit;
    Inc(Current);

  end;

  Result := True;
end;

const
  SizeOfWideChar = SizeOf(WideChar);

function MakeToken(Start, Last: PWideChar; TokenType: TTokenType): TToken;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Start <> nil then
  begin
    Result.TokenStart:= Start;
    Result.TokenLast := Last;

  end;
  if (Result.TokenStart = nil) xor (Result.TokenStart = nil) then
    WriteLn('Somethign is not right!');

  Result.TokenType := TokenType;

end;

function MakeTokens(TokenTypes: array of TTokenType; EndTokens: TTokens): TTokens;
var
  TokenType: TTokenType;

begin
  for TokenType in TokenTypes do
  begin
    EndTokens.Add(MakeToken(nil, nil, TokenType));
  end;

  Result := EndTokens; 
end;


function ParseContent(constref Content: WideString): TNodes; forward;

const
  TitlePrefixesToBeSkipped: array of AnsiString = (
  'مدیاویکی:',
  'ویکی‌پدیا:',
  'پرونده:',
  'درگاه:',
  'رده:',
  'الگو:'
  );
  TitleSuffixesToBeSkipped: array of AnsiString = (
  );

function ShouldBeSkipped(TitleNode: TTextWikiEntity): Boolean;
var
  Prefix, Suffix: AnsiString;

begin
  for Prefix in TitlePrefixesToBeSkipped do
    if IsPrefix(Prefix, WriteAsUTF8(TitleNode.Content)) then
    begin
      Exit(True);
    end;
  for Suffix in TitleSuffixesToBeSkipped do
    if IsSuffix(Suffix, WriteAsUTF8(TitleNode.Content)) then
    begin
      Exit(True);
    end;

  Result := False;
end;

function ParseWikiNode(Node: TDOMNode; var WikiPage: TWikiPage): Boolean;
var
  WikiEntry: TBaseWikiNode;
  Child: TDOMNode;

begin
  Result := True;
  WikiEntry := nil;

  if Node.NodeName = 'title' then
  begin
    WikiEntry := TTextWikiEntity.Create(ReadWideStringFromString(Node.TextContent));
    WikiPage.Title := WikiEntry as TTextWikiEntity;
    if ShouldBeSkipped(WikiPage.Title) then
    begin
      ALoggerUnit.GetLogger.FMTDebugLnEveryN(1000,
        'Skipping %s',
        [WriteAsUTF8(WikiPage.Title.Content)]);
      Exit(False);

    end;


  end
  else if Node.NodeName = 'ns' then
  begin
    WikiEntry := TTextWikiEntity.Create(
      ReadWideStringFromString(Node.TextContent));
    WikiPage.NS := WikiEntry as TTextWikiEntity;

  end
  else if Node.NodeName = 'id' then
  begin
    WikiEntry := TTextWikiEntity.Create(ReadWideStringFromString(Node.TextContent));
    WikiPage.ID := WikiEntry as TTextWikiEntity;

  end
  else if Node.NodeName = 'redirect' then
  begin
    WikiEntry := TTextWikiEntity.Create(ReadWideStringFromString(Node.TextContent));
    WikiPage.Redirect := WikiEntry as TTextWikiEntity;

  end
  else if Node.NodeName = 'revision' then
  begin
    Child := Node.FirstChild;
    while Child <> nil do
    begin
      if Child.NodeName = 'text' then
        break;
      Child := Child.NextSibling;

    end;

    if Child <> nil then
    begin
      try
        WikiPage.Content := ParseContent(ReadWideStringFromString(Child.TextContent));

      except
        on e: EBaseWikiParser do
        begin
          Exit(False);
        end;
        on e: EInvalidToken do
        begin
          Exit(False);
        end;
      end;
      Result := True;
      Exit;

    end;
  end
  else
  begin
    Result := False;
    ALoggerUnit.FmtFatalLnIFFalse(False, 'NodeName: %s', [Node.NodeName]);

  end;

end;

function ParseWiki(Node: TDOMNode): TWikiPage;
var
  Child: TDOMNode;

begin
  FmtFatalLnIFFalse(Node.NodeName = 'page', 'NodeName: "%s"', [Node.NodeName]);

  Result := TWikiPage.Create;
  Child := Node.FirstChild;

  while Child <> nil do
  begin
    if not ParseWikiNode(Child, Result) then
    begin
      FreeAndNil(Result);
      Break;
    end;

    Child := Child.NextSibling;
  end;
end;

function TToken.IsEmpty: Boolean;
begin
  Result := Self.TokenStart = nil;

end;


function TToken.IsSame(constref OtherToken: TToken): Boolean;
var
  P1, P2: PWideChar;

begin
  if Self.TokenType <> OtherToken.TokenType then
    Exit(False);

  if Self.TokenLast - Self.TokenStart <> OtherToken.TokenLast - OtherToken.TokenStart then
    Exit(False);

  P1 := Self.TokenStart;
  P2 := OtherToken.TokenStart;
  while True do
  begin
    if P1^ <> P2^ then
      Exit(False);

    if P1 = Self.TokenLast then
      Break;

    Inc(P1);
    Inc(P2);

  end;
  Result := True;
end;

function TToken.Text: WideString;
begin
  if (Self.TokenStart = nil) then
    Exit(EmptyWideStr);

  SetLength(Result, Self.TokenLast - Self.TokenStart + 1);
  Move(
    Self.TokenStart^,
    Result[1],
    SizeOfWideChar * (Self.TokenLast - Self.TokenStart + 1)
  );
end;

{ EInvalidToken }

constructor EInvalidToken.Create(Visited, Expected: TTokenType);
begin
  inherited Create(Format('Visited: %s Expected: %s', [
    TokenTypeToString(Visited),
    TokenTypeToString(Expected)]));
end;

constructor EInvalidToken.Create(constref Visited: AnsiString);
begin
  inherited Create(Visited);

end;

{ EInvalidEntity }


constructor EInvalidEntity.Create(_Token: TToken);
begin
  inherited Create('');
  FToken := _Token;
end;

function ParseContent(constref Content: WideString): TNodes;
var
  Parser: TWikiParser;

begin
  if Content = '' then
    Exit(nil);

  Parser := TWikiParser.Create(TWikiTokenizer.Create(Content));
  try
    Result := Parser.ParseDoc;

  except
    on e: EBaseWikiParser do
    begin
      FreeAndNil(Parser);
      raise;
    end;
    on e: EInvalidToken do
    begin
      FreeAndNil(Parser);
      raise;

    end;
  end;
  Parser.Free;

end;

{ TWikiParser }

function TWikiParser.IsDone(constref Token: TToken; EndTokens: TTokens): Boolean;
var
  i: Integer;
  Current: TToken;

begin
  if EndTokens = nil then
    Exit(False);

  Result := False;
  i := 0;
  while i < EndTokens.Count do
  begin
    Current := EndTokens[i];
    Inc(i);
    if Token.TokenType <> Current.TokenType then
      Continue;
    if Current.IsEmpty then
    begin
      Result := True;
      Break;

    end;

    if Token.IsSame(Current) then
    begin
      Result := True;
      Break;

    end;

  end;

  if Result then
    Exit;

  if Token.TokenType in [
    ttCloseHyperLink,
    ttCloseHeadingSection,
    ttEndTemplate,
    ttEndTag,
    ttEndTable
    ] then
  begin
    Result := True;
  end;

end;

function ToXML(Obj: TObject): AnsiString;
begin
  Result := (Obj as TBaseWikiNode).ToXML('');

end;

function TWikiParser.ParseEntity(EndTokens: TTokens): TBaseWikiNode;
var
  Token: TToken;

begin
  if EndTokens = nil then
    FmtFatalLnIFFalse(False, 'done', []);

  Result := nil;

  Token := Tokenizer.LastToken;
  ALoggerUnit.GetLogger.FMTDebugLn('it: %d Token: %s  %d', [
    it,
    Token.Text,
    Token.TokenType], 4);
  if IsDone(Token, EndTokens) then
  begin
    {
    if Token.TokenType = ttEOF then
      WriteLn;
      }
    ALoggerUnit.GetLogger.FMTDebugLn('Token: %s  %d', [
      Token.Text,
      Token.TokenType], 4);
    Exit(nil);

  end;

  try
    case Token.TokenType of
    ttBeginTag:
      Result := ParseTag(Token, EndTokens);
    ttText, ttSingleQuote, ttNewLine:
      Result := ParseTextEntity(Token, EndTokens);
    ttBulletedList:
      Result := ParseBulletList(Token, EndTokens);
    ttNumberedList:
      Result := ParseNumberedList(Token, EndTokens);
    ttString, ttDoubleQuote:
      Result := ParseTextEntity(Token, EndTokens);
    ttBeginTemplate:
      Result := ParseTemplate(EndTokens);
    ttBeginComment:
      Result := ParseComment(EndTokens);
    ttOpenHyperLink:
      Result := ParseHyperLink(EndTokens);
    ttBeginTable:
      Result := ParseTable(EndTokens);
    ttOpenBracket:
      Result := ParseSeparatorOrHyperLink(Token, EndTokens);
    ttOpenHeadingSection:
      Result := ParseHeadingSection(Token, EndTokens);
    ttBar, ttEqualSign, ttMinus, ttLessThan, ttGreaterThan,
    ttBeginTableRow, ttTableCaption, ttEndTable, ttOpenCurlyBrace,
    ttCloseCurlyBrace, ttSlash,
    ttCloseBracket, ttTableCellSeparator:
      Result := ParseSeparator(Token);
    else
      ALoggerUnit.GetLogger.FMTDebugLn(
        '+Unrecognized Token: %s %s',
        [
        TokenTypeToString(Token.TokenType),
        Token.Text]);
      FreeAndNil(Result);
      raise EInvalidEntity.Create(Token);
    end;
  except
    on e: EBaseWikiParser do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;

  {
  ALoggerUnit.GetLogger.FMTDebugLn(
   'Result: %s', [Result.ToXML('')]);
  }
end;

function TWikiParser.ParseTag(constref Token: TToken; EndTokens: TTokens
  ): TTagEntity;
var
  TagName: WideString;
  Parameters: TNodes;
  Param, Child: TBaseWikiNode;

begin
  Result := nil;
  if IsSuffix(WideString('/>'), Token.Text) then
    Exit(TTagEntity.Create(Copy(Token.Text, 2, Length(Token.Text) - 3), nil));

  TagName := Token.Text;
  EndTokens:= MakeTokens(
    [ttGreaterThan, ttEndTag, ttOpenHeadingSection],
    EndTokens);
  Tokenizer.GetNextToken;

  Child := nil;
  Parameters := TNodes.Create;
  Result := TTagEntity.Create(TagName, Parameters);
  try
    Param := ParseEntity(EndTokens);
    if Param <> nil then
      Parameters.Add(Param);
    ParseUntilNil(Param, EndTokens);

    if Tokenizer.LastToken.TokenType in [ttGreaterThan] then
    begin
      Tokenizer.GetNextToken;
      EndTokens.Pop(3);

    end else // if Tokenizer.LastToken.TokenType in [ttEndTzag] then
    begin
      Tokenizer.GetNextToken;
      EndTokens.Pop(3);
      Exit;

    end;

    EndTokens.Add(MakeToken(nil, nil, ttEndTag));
    EndTokens.Add(MakeToken(nil, nil, ttOpenHeadingSection));
    while not (Tokenizer.LastToken.TokenType in [ttEndTag, ttEOF]) do
    begin
      Child := nil;
      Child := ParseEntity(EndTokens);
      if Child <> nil then
        Result.AddChild(Child);
      if ParseUntilNil(Child, EndTokens) = nil then
        Break;
      if Tokenizer.LastToken.TokenType in [ttEOF, ttEndTag] then
        Break;
      Tokenizer.GetNextToken;

    end;

    if Tokenizer.LastToken.TokenType = ttEndTag then
      Tokenizer.GetNextToken;
  except
    on EInvalidEntity do
    begin
      EndTokens.Pop(2);
      FreeObjects([@Result, @Child]);
      raise;

    end;
  end;
  EndTokens.Pop(2);


end;

const
  WideStringSpace = WideString(' ');

function TWikiParser.ParseTextEntity(constref Token: TToken; EndTokens: TTokens
  ): TTextWikiEntity;
var
  Text: WideString;

begin
  FmtFatalLnIFFalse(
    Token.TokenType in [ttText, ttSingleQuote, ttDoubleQuote, ttNewLine],
    'TokenType: %s',
    [Token.Text]
    );
  EndTokens := MakeTokens([
	  ttOpenHeadingSection,
	  ttBeginTable,
	  ttBeginTag,
	  ttBeginTemplate
	  ],
          EndTokens
	 );

  if Token.TokenType = ttSingleQuote then
  begin
    Result := ParseEntityWithSingleQuoteToken(Token, EndTokens);
    EndTokens.Pop(4);
    // ALoggerUnit.GetLogger.FMTDebugLn('Result: %X', [Result]);
    // ALoggerUnit.GetLogger.FMTDebugLn('TStyleTextNode: %X', [Result], 16);
    Exit;

  end;

  Text := Tokenizer.LastToken.Text;
  while not (Tokenizer.LastToken.TokenType  in [
	  ttOpenHeadingSection,
	  ttBeginTable,
	  ttBeginTag,
	  ttBeginTemplate]) do
  begin
    Tokenizer.GetNextToken;
    if IsDone(Tokenizer.LastToken, EndTokens) then
    begin
      Break;
    end;

    if Tokenizer.LastToken.TokenType <> ttText then
      Break;

    Text += WideStringSpace;
    Text += Tokenizer.LastToken.Text;

  end;
  Result := TTextWikiEntity.Create(Text);
  EndTokens.Pop(4);

end;

function TWikiParser.ParseHyperLink(EndTokens: TTokens
  ): THyperLinkEntity;
var
  Current: TBaseWikiNode;
  Text, Link: TBaseWikiNode;
  Parameters: TNodes;


begin
  Tokenizer.GetNextToken;
  EndTokens := MakeTokens([
    ttCloseHyperLink,
    ttBar,
    ttOpenHeadingSection,
    ttNewLine], EndTokens);

  Result := nil;
  Parameters := TNodes.Create;
  while not (Tokenizer.LastToken.TokenType in [ttEOF]) do
  begin

    try
      Current := Self.ParseEntity(EndTokens);

    except on EInvalidEntity do
    begin
      FreeObjects([@Result, @Parameters]);
      EndTokens.Pop(4);
      raise
    end;

    end;

    if Current <> nil then
    begin
     Parameters.Add(Current);

    end;

    Current := ParseUntilNil(Current, EndTokens);
    if Tokenizer.LastToken.TokenType = ttCloseHyperLink then
    begin
      Tokenizer.GetNextToken;
      Break;

    end;
    if Tokenizer.LastToken.TokenType = ttBar then
    begin
      Tokenizer.GetNextToken;
      Continue;
    end;

    FreeObjects([@Result, @Parameters]);
    EndTokens.Pop(4);
    raise EInvalidEntity.Create(MakeToken(nil, nil, ttNone));
  end;

  Text := nil;
  if Parameters.Count <> 0 then
  begin
    Text := Parameters.Last;
    Parameters.Delete(Parameters.Count - 1);

  end;

  Link := nil;
  if Parameters.Count <> 0 then
  begin
    Link := Parameters.First;
    Parameters.Delete(0);

  end;
  Result := THyperLinkEntity.Create(Link, Text, Parameters);

  EndTokens.Pop(4);

end;

function TWikiParser.ParseTemplate(EndTokens: TTokens
  ): TTemplate;
var
  Current: TBaseWikiNode;
  Name: TTextWikiEntity;
  Parameters: TNodes;

begin
  Tokenizer.GetNextToken;
  Result := nil;
  EndTokens.Add(MakeToken(nil, nil, ttEndTemplate));
  EndTokens.Add(MakeToken(nil, nil, ttBar));
  EndTokens.Add(MakeToken(nil, nil, ttTableCellSeparator));
  EndTokens.Add(MakeToken(nil, nil, ttOpenHeadingSection));

  Current := Self.ParseEntity(EndTokens);
  if not (Current is TTextWikiEntity) then
  begin
    Current.Free;
    raise EInvalidEntity.Create(MakeToken(nil, nil, ttNone));

  end;
  Name := Current as TTextWikiEntity;
  // ALoggerUnit.GetLogger.FMTDebugLn('Template: %s', [Name.ToXML('')], 3);
  Parameters := TNodes.Create;

  while IsUnAcceptable(Tokenizer.LastToken.TokenType,
    [ttBar, ttTableCellSeparator,
      ttNewLine, ttEndTemplate, ttOpenHeadingSection]) do
  begin
    if Tokenizer.LastToken.TokenType in [ttEndTemplate, ttOpenHeadingSection, ttEOF] then
      Break;
    Current := Current.LastNode;
    try
      Current.Next := ParseEntity(EndTokens);
    except
      on e: EInvalidEntity do
      begin
        FreeObjects([@Parameters, @Name]);
        EndTokens.Pop(4);
        raise;
      end;
    end;
    Current := Current.Next;
    if Current = nil then
      Break;

  end;

  try
    while IsUnacceptable(Tokenizer.LastToken.TokenType, [ttEndTemplate,
      ttOpenHeadingSection]) do
    begin
      Tokenizer.GetNextToken;
      Current := ParseEntity(EndTokens);
      Parameters.Add(Current);

      while Current <> nil do
      begin
        Current := Current.LastNode;
        if Tokenizer.LastToken.TokenType in [ttEndTemplate, ttEOF] then
          Break;
        Current.Next := ParseEntity(EndTokens);

        Current := Current.Next;
        if Current = nil then
          Break;

      end;

    end;
  except on e: EInvalidEntity do
  begin
    // ALoggerUnit.GetLogger.FMTDebugLn('Name: %s', [name.ToXML('')]);
    FreeObjects([@Parameters, @Name]);
    EndTokens.Pop(4);
    raise;
  end;
  end;

  EndTokens.Pop(4);

  if Tokenizer.LastToken.TokenType = ttEndTemplate then
    Tokenizer.GetNextToken;
  Result := TTemplate.Create(Name, Parameters);

end;

function TWikiParser.ParseComment(EndTokens: TTokens): TCommentWikiEntry;
var
  Current: TBaseWikiNode;

begin
  EndTokens.Add(MakeToken(nil, nil, ttEndComment));
  Result := TCommentWikiEntry.Create('');
  Current := Result;

  try
    while Current <> nil do
    begin
      Tokenizer.GetNextToken;
      Current.Next := ParseEntity(EndTokens);
      Current := Current.Next;

    end;

  except on e: EInvalidEntity do
  begin
    Result.Free;
    EndTokens.Pop(1);
    raise;

  end;
  end;
  EndTokens.Pop(1);

end;

function TWikiParser.ParseTable(EndTokens: TTokens): TTable;
var
  Current: TBaseWikiNode;

begin
  Tokenizer.GetNextToken;
  Result := TTable.Create;
  EndTokens := MakeTokens([ttEndTable, ttOpenHeadingSection], EndTokens);
  Current := Result;

  while (Current <> nil) and (Tokenizer.LastToken.TokenType <> ttEOF) do
  begin
    try
      Current := ParseEntity(EndTokens);
      if Current = nil then
        Break;
      Current.Free;

    except
      on EInvalidEntity do
      begin
        EndTokens.Pop(2);
        FreeAndNil(Result);
        raise;
      end;

    end;
  end;

  EndTokens.Pop(2);

end;

function TWikiParser.ParseHeadingSection(constref Token: TToken;
  EndTokens: TTokens): THeadingSection;
var
  Current: TBaseWikiNode;
  Ch: PWideChar;

begin
  Tokenizer.GetNextToken;
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  Ch := Token.TokenStart;
  while Ch <= Token.TokenLast do
  begin
    EndTokens.Add(
      MakeToken(
        Token.TokenStart,
        Ch,
        ttCloseHeadingSection
      )
    );
    Inc(Ch);

  end;

  try
    Result := nil;
    Current := ParseEntity(EndTokens);
    Result := THeadingSection.Create(
      Length(Token.Text),
      Current);

    Current := ParseUntilNil(Current, EndTokens);
  except
    on EInvalidEntity do
    begin
      EndTokens.Pop(1);
      FreeAndNil(Result);
      raise;
    end;

  end;

  if Tokenizer.LastToken.TokenType <> ttCloseHeadingSection then
  begin
    FreeAndNil(Result);
    raise EInvalidToken.Create(WriteAsUTF8(Tokenizer.LastToken.Text));

  end;
  EndTokens.Pop(2 + Token.TokenLast - Token.TokenStart);
  Tokenizer.GetNextToken;

end;

function TWikiParser.ParseSeparator(constref Token: TToken
  ): TSeparatorWikiEntry;
begin
  Result := TSeparatorWikiEntry.Create(Token.Text);
  Tokenizer.GetNextToken;

end;

function TWikiParser.ParseCaption(EndTokens: TTokens): TBaseWikiNode;
var
  Current: TBaseWikiNode;

begin
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  Result := TTextWikiEntity.Create('');
  Current := Result.LastNode;
  while (Current <> nil) and (Tokenizer.LastToken.TokenType = ttEOF) do
  begin
    Current.Next := ParseEntity(EndTokens);
    Current := Current.Next.LastNode;

  end;
end;

function TWikiParser.ParseSeparatorOrHyperLink(constref Token: TToken;
  EndTokens: TTokens): TBaseWikiNode;
  function IsAnInternetProtocol(constref Text: AnsiString): Boolean;
  begin
    Result := StringUnit.IsPrefix('http://', AnsiString(Text)) or
              StringUnit.IsPrefix('https://', AnsiString(Text)) or
              StringUnit.IsPrefix('ftp://', AnsiString(Text));

  end;

var
  Link, Text: TBaseWikiNode;

begin
  Tokenizer.GetNextToken;

  if not IsAnInternetProtocol(LowerCase(WriteAsUTF8(Tokenizer.LastToken.Text))) then
  begin
    Exit(TSeparatorWikiEntry.Create(Token.Text));

  end;
  Link := TTextWikiEntity.Create(Tokenizer.LastToken.Text);

  Tokenizer.GetNextToken;
  EndTokens.Add(MakeToken(nil, nil, ttCloseBracket));

  Text := nil;
  try
    Text := ParseEntity(EndTokens);
    Text := ParseUntilNil(Text, EndTokens);

  except
    on e: EBaseWikiParser do
    begin
      Link.Free;
      EndTokens.Pop;
      Text.Free;
      Exit(nil);
    end;
  end;
  EndTokens.Pop;
  Result := THyperLinkEntity.Create(Link, Text, nil);

end;

function TWikiParser.ParseBulletList(constref Token: TToken; EndTokens: TTokens
  ): TBaseWikiNode;
begin
  Tokenizer.GetNextToken;
  Result := TTextWikiEntity.Create(Token.Text);
  if EndTokens.Count = EndTokens.Count + 1 then
    Exit;

  Exit;
  // TODO: Implement this
                       {
  Tokenizer.NextToken;
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));

  Result := TBulletedListEntity.Create;
  Current := ParseEntity(EndTokens);

  while Current <> nil do
  begin
    Result.Children.Add(Current);
    CurPosition:= Self.Tokenizer.Current;
    if Self.Tokenizer.LastToken.TokenType <> ttBulletedList then
    begin
      Self.Tokenizer.Current := CurPosition;
      Break;
    end;
    Current := ParseEntity(EndTokens);

  end;
  EndTokens.Pop;

  FMTDebugLn('Result: %s', [Result.ToXML('')], 1);
  }
end;

function TWikiParser.ParseNumberedList(constref Token: TToken;
  EndTokens: TTokens): TBaseWikiNode;
begin
  Tokenizer.GetNextToken;
  Result := TTextWikiEntity.Create(Token.Text);
  Exit;
{
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));

  Result := TNumberedListEntity.Create;
  Current := ParseEntity(EndTokens);

  while Current <> nil do
  begin
    Result.Children.Add(Current);
    Current := ParseEntity(EndTokens);

  end;
  EndTokens.Pop;
}
end;

function TWikiParser.ParseUntilNil(Current: TBaseWikiNode; EndTokens: TTokens): TBaseWikiNode;
begin
  Result := Current;
  Current := Result.LastNode;
  while Current <> nil do
  begin
    Current.Next := ParseEntity(EndTokens);
    Current := Current.Next.LastNode;

  end;

end;

function TWikiParser.ParseEntityWithSingleQuoteToken(constref AToken: TToken;
  EndTokens: TTokens): TTextWikiEntity;

  function IsAnEndToken(CurrentToken: TToken): Boolean;
  begin
    Result := False;

    if CurrentToken.TokenType in [ttNewLine, ttEOF] then
      Exit(True);
    if CurrentToken.Text = AToken.Text then
      Exit(True);
  end;

var
  InitialCount: Integer;
  Child: TBaseWikiNode;

begin
  Tokenizer.GetNextToken;
  InitialCount := Length(AToken.Text);
  if InitialCount = 1 then
  begin
    Exit(TTextWikiEntity.Create(AToken.Text));

  end;

  Result := TTextStyler.CreateStyler(AToken.Text);
  if Result = nil then
    raise EInvalidEntity.Create(MakeToken(nil, nil, ttNone));

  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  EndTokens.Add(MakeToken(nil, nil, ttOpenHeadingSection));
  if InitialCount = 2 then
    EndTokens.Add(MakeToken(AToken.TokenStart, AToken.TokenStart + 1, ttSingleQuote))
  else if InitialCount = 3 then
    EndTokens.Add(MakeToken(AToken.TokenStart, AToken.TokenStart + 2, ttSingleQuote))
  else if InitialCount = 4 then
    EndTokens.Add(MakeToken(AToken.TokenStart, AToken.TokenStart + 3, ttSingleQuote))
  else if (InitialCount = 5) or (InitialCount = 6) then
  begin
    EndTokens.Add(MakeToken(AToken.TokenStart, AToken.TokenStart + 4, ttSingleQuote))

  end;


  while True do
  begin
    try
      Child := Self.ParseEntity(EndTokens);
      if Child <> nil then
        Result.Children.Add(Child)
      else
        Break;
    except on e: EBaseWikiParser do
    begin
      Result.Free;
      EndTokens.Pop(3);
      raise;
    end;
    end;

  end;
  if Tokenizer.LastToken.TokenType = ttSingleQuote then
    Tokenizer.GetNextToken;

  EndTokens.Pop(3);

end;

constructor TWikiParser.Create(_Tokenizer: TWikiTokenizer);
begin
  inherited Create;

  FTokenizer := _Tokenizer;

end;

destructor TWikiParser.Destroy;
begin
  FTokenizer.Free;

  inherited Destroy;
end;

function TWikiParser.ParseDoc: TNodes;
var
  Next: TBaseWikiNode;
  EndTokens: TTokens;

begin
  try
    EndTokens := TTokens.Create;
    Result := TNodes.Create;
    EndTokens.Add(MakeToken(nil, nil, ttEOF));
    Tokenizer.GetNextToken;
    Next := Self.ParseEntity(EndTokens);
    while Next <> nil do
    begin
      Result.Add(Next);
      Next := Self.ParseEntity(EndTokens);

    end;
    EndTokens.Pop;
    EndTokens.Free;

  except
    on e: EInvalidToken do
    begin
      FreeAndNil(EndTokens);
      FreeAndNil(Result);
      raise
    end;
    on e: EInvalidEntity do
    begin
      FreeAndNil(EndTokens);
      FreeAndNil(Result);
      raise
    end
  end;

end;

{ EBaseWikiParser }

constructor EBaseWikiParser.Create(constref msg: AnsiString);
begin
  // ALoggerUnit.GetLogger.FMTDebugLn('it: %d', [it]);
  inherited Create(msg);

end;

{ TWikiTokenizer }

function TWikiTokenizer.GetNextChar: WideChar;
begin
  Result := Current^;
  Inc(Current);

end;

{
function TWikiTokenizer.GetCurrentToken: TToken;
var
  Pos: PWideChar;

begin
  Pos := Self.Current;
  Result := GetNextToken;
  Self.Current := Pos;

end;
}

procedure TWikiTokenizer.GetNextToken;
begin
  Prev := Current;

  _GetNextToken(LastToken);
  ALoggerUnit.GetLogger.FMTDebugLn('%d Token: %s  %d',
    [it, LastToken.Text, LastToken.TokenType],
    4);
  Inc(it);

end;

constructor TWikiTokenizer.Create(constref Data: WideString);
begin
  inherited Create;

  Current := @(Data[1]);
  LastToken.TokenType := ttNone;
end;

function MaybeGroupSamePatternToken(
  var Current: PWideChar;
  TargetWChar: WideChar;
  ThenTokenType: TTokenType): TToken;
begin
  Result.TokenStart := Current;
  while Current^ = TargetWChar do
  begin
    Result.TokenLast := Current;
    Inc(Current);

  end;

  Result.TokenType := ThenTokenType;
end;

function ScanTillToken(
  var Current: PWideChar;
  StopPattern: AnsiString;
  TokenType: TTokenType;
  UntilNewLine: Boolean = True): TToken;
var
  Start: PWideChar;

begin
  Start := Current;

  while not HasPrefix(Current, StopPattern) do
  begin
    if UntilNewLine and (Current^ in [#10, #13]) then
      Break;
    if Current^ = #0 then
      Break;

    Inc(Current);

  end;
  if Current^ = #0 then
  begin
    Result := MakeToken(Start, Current - 1, TokenType);
  end
  else if Current^ in [#10, #13] then
  begin
    Result := MakeToken(Start, Current - 1, TokenType);
    Inc(Current);
  end
  else
  begin
    Result := MakeToken(Start, Current - 1, TokenType);
    Inc(Current, Length(StopPattern));

  end;

end;

const
  SingleQuote = #$27;

procedure TWikiTokenizer._GetNextToken(var Result: TToken);

  function GetNext(Current: PWideChar; Delta: Integer = 1): WideChar;
  begin
    Inc(Current, Delta);

    Result := Current^;
  end;

var
  Start: PWideChar;
  Status: Integer;
  PrevToken: TToken;

begin
  while Current^ = ' ' do
  begin
    Inc(Current);

  end;

  Start := Current;
  case Current^ of
    #0:
    begin
      Result := MakeToken(nil, nil, ttEOF);

    end;
    '<':
    begin
      if GetNext(Current) = ' ' then
      begin
        Inc(Current);
        Result := MakeToken(Start, Current - 1, ttLessThan);
        Exit;
      end;
      Status := 0;
      Inc(Current);
      if Current^ = '/' then
      begin
        Status := 1;
        Inc(Current);

      end
      else if HasPrefix(Current, '!--') then
      begin
        Status := 2;
        Inc(Current, 3);

      end;

      if Status <> 2 then
      begin
        while Ord(Current^) in [
           Ord('a')..Ord('z'),
           Ord('A')..Ord('Z'),
           Ord('0')..Ord('9')] do
        begin
          Inc(Current);

        end;

        if Status = 0 then
          Result := MakeToken(Start + 1, Current - 1, ttBeginTag)
        else if Status = 1 then
        begin
          if Current^ = '>' then
            Inc(Current)
          else
          begin
            Result := MakeToken(Start, Current, ttBeginTag);
            raise EInvalidToken.Create(AnsiString(Result.Text));

          end;

          Result := MakeToken(Start + 2, Current - 2, ttEndTag)
        end
      end
      else // if Status = 2 then
      begin
        Result := MakeToken(Start, Current, ttBeginComment);

      end;
    end;
    SingleQuote:
    begin
      Result := MaybeGroupSamePatternToken(Current, SingleQuote, ttSingleQuote);

    end;
    '"':
    begin
      Result := MakeToken(Start, Current, ttDoubleQuote);
      Inc(Current, 1);

    end;
    '=':
    begin
      PrevToken.TokenType := LastToken.TokenType;
      Result := MaybeGroupSamePatternToken(Current, '=', ttEqualSign);

      if Length(Result.Text) = 1 then
      begin
        Result.TokenType := ttEqualSign;

      end
      else if 2 <= Length(Result.Text) then
      begin
        if PrevToken.TokenType = ttNewLine then
           Result.TokenType := ttOpenHeadingSection
        else
        begin
           Result.TokenType := ttCloseHeadingSection;

        end;

      end;

    end;
    '[':
    begin
      if GetNext(Current) = '[' then
      begin
        Result := MakeToken(Start, Current + 1, ttOpenHyperLink);
        Inc(Current, 2);
      end
      else
      begin
        Result := MakeToken(Start, Current, ttOpenBracket);
        Inc(Current);

      end

    end;
    ']':
    begin
      if GetNext(Current) = ']' then
      begin
        Result := MakeToken(Start, Current + 1, ttCloseHyperLink);
        Inc(Current, 2);
      end
      else
      begin
        Result := MakeToken(Start, Current, ttCloseBracket);
        Inc(Current);

      end

    end;
    '{':
    begin
      if GetNext(Current) = '|' then
      begin
        Result := MakeToken(Start, Current + 1, ttBeginTable);
        Inc(Current, 2);
        Exit;
      end;
      Result := MaybeGroupSamePatternToken(Current, '{', ttOpenCurlyBrace);
      if Length(Result.Text) = 1 then
      begin
        Result.TokenType := ttOpenCurlyBrace;
      end
      else if Length(Result.Text) = 2 then
      begin
        Result.TokenType := ttBeginTemplate;
      end;
    end;
    '}':
    begin
      Result := MaybeGroupSamePatternToken(Current, '}', ttCloseCurlyBrace);
      if Length(Result.Text) >= 2 then
      begin
        Result.TokenType := ttEndTemplate;
        Current -= Length(Result.Text);
        Inc(Current, 2);

      end;
    end;
    '/':
    begin
      if GetNext(Current) = '>' then
      begin
        Inc(Current, 2);
        Result := MakeToken(Start, Current, ttEndTag);
        Exit;

      end;
      Result := MakeToken(Start, Current, ttSlash);
      Inc(Current);
    end;
    '>':
    begin
      Result := MakeToken(Start, Current, ttGreaterThan);
      Inc(Current);

    end;
    '-':
    begin
      Result := MakeToken(Start, Current, ttMinus);
      Inc(Current);

    end;
    '|':
    begin
      if GetNext(Current) = '+' then
      begin
        Result := MakeToken(Start, Current + 1, ttTableCaption);
        Inc(Current, 2);

      end
      else if GetNext(Current) = '-' then
      begin
        Result := MakeToken(Start, Current + 1, ttBeginTableRow);
        Inc(Current, 2);

      end
      else if GetNext(Current) = '|' then
      begin
        Result := MakeToken(Start, Current + 1, ttTableCellSeparator);
        Inc(Current, 2);

      end
      else if (GetNext(Current) = '}') and (GetNext(Current, 2) <> '}') then
      begin
        Result := MakeToken(Start, Current + 1, ttEndTable);
        Inc(Current, 2);

      end
      else
      begin
        Result := MakeToken(Start, Current, ttBar);
        Inc(Current);

      end;
    end;
    '*':
    begin
      Result := MaybeGroupSamePatternToken(Current, '*', ttBulletedList);
      if Current^ <> ' ' then
        Result.TokenType := ttText;

    end;
    '#':
    begin
      Result := MaybeGroupSamePatternToken(Current, '#', ttNumberedList);
      if Current^ <> ' ' then
        Result.TokenType := ttText;

    end;
    #10, #13:
    begin
      Result := MakeToken(Start, Current, ttNewLine);
      Inc(Current);

    end;
    else
    begin

      while not (Ord(Current^) in [
        Ord('<'), Ord('>'), 0, 10, 13, 32, Ord('='), Ord('{'),
        Ord('}'), Ord('"'), Ord(''''), Ord('|'),
        Ord('['), Ord(']')]) do
      begin
        Inc(Current);
      end;

      Result := MakeToken(Start, Current - 1, ttText);
    end;

  end;
end;

procedure TWikiTokenizer.Rewind;
begin
  Self.Current := Self.Prev;

end;

end.


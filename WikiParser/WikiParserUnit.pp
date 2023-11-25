unit WikiParserUnit;

{$mode ObjFPC}{$H+}

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
  ALoggerUnit, WideStringUnit, ParameterManagerUnit, GenericCollectionUnit,
  StringUnit;

type
  TTokenType = (ttNone, ttHeading, ttText, ttLessThan, ttGreaterThan,
  ttBeginTag, ttEndTag,
  ttIdentifier, ttEqualSign, ttString, ttNewLine,
  ttBeginTemplate, ttEndTemplate,
  ttComment,
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
  TToken = record
    TokenType: TTokenType;
    Text: WideString;

  end;

  { TWikiTokenizer }

  TWikiTokenizer = class(TObject)
  private
    Data: WideString;
    Current, Prev: PWideChar;
    LastToken: TToken;

    //function GetCurrentToken: TToken;
    function GetNextChar: WideChar;
    function GetNextToken: TToken;
    function _GetNextToken: TToken;

    procedure Rewind;
  public
    property NextToken: TToken read GetNextToken;
    // property CurrentToken: TToken read GetCurrentToken;
    constructor Create(constref _Data: WideString);

  end;

  { TTokens }

  TTokens = class(specialize TCollection<TToken>)
  end;

  { EInvalidToken }

  EInvalidToken = class(EBaseWikiParser)
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


    function IsDone(Token: TToken; EndTokens: TTokens): Boolean;
    function ParseEntity(EndTokens: TTokens): TBaseWikiNode;
    function ParseTag(Token: TToken; EndTokens: TTokens): TTagEntity;
    function ParseTextEntity(Token: TToken; EndTokens: TTokens): TTextWikiEntity;
    function ParseHyperLink(EndTokens: TTokens): THyperLinkEntity;
    function ParseTemplate(EndTokens: TTokens): TTemplate;
    function ParseComment(EndTokens: TTokens): TCommentWikiEntry;
    function ParseTable(EndTokens: TTokens): TTable;
    function ParseHeadingSection(Token: TToken; EndTokens: TTokens): THeadingSection;
    function ParseSeparator(Token: TToken): TSeparatorWikiEntry;
    function ParseCaption(EndTokens: TTokens): TBaseWikiNode;
    function ParseSeparatorOrHyperLink(Token: TToken; EndTokens: TTokens): TBaseWikiNode;

    function ParseEntityWithSingleQuoteToken(AToken: TToken; EndTokens: TTokens): TTextWikiEntity;

    // TODO: Implement these two functions correctly.
    function ParseBulletList(Token: TToken; EndTokens: TTokens): TBaseWikiNode;
    function ParseNumberedList(Token: TToken; EndTokens: TTokens): TBaseWikiNode;

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
    if Current^ = #0 then
      Break;

  end;

  Result := True;
end;

const
  SizeOfWideChar = SizeOf(WideChar);

function MakeToken(Start, Last: PWideChar; TokenType: TTokenType): TToken;
begin
  Result.Text := '';
  if Start <> nil then
  begin
    SetLength(Result.Text, Last - Start + 1);
    Move(Start^, Result.Text[1], SizeOfWideChar * (Last - Start + 1));

  end;
  Result.TokenType := TokenType;

end;

function MakeTokens(TokenTypes: array of TTokenType; EndTokens: TTokens): TTokens;
var
  TokenType: TTokenType;

begin
  for TokenType in TokenTypes do
    EndTokens.Add(MakeToken(nil, nil, TokenType));

  Result := EndTokens; 
end;


function ParseContent(Content: WideString): TNodes; forward;

const
  TitlePrefixesToBeSkipped: array of AnsiString = (
  'مدیاویکی:',
  'ویکی‌پدیا:',
  'پرونده:',
  'درگاه:',
<<<<<<< HEAD
  'رده'
=======
  'رده:',
  'الگو:'
>>>>>>> bf15520 (...)
  );
  TitleSuffixesToBeSkipped: array of AnsiString = (
  );

function CanNotParse(TitleNode: TTextWikiEntity): Boolean;
var
  Prefix, Suffix: AnsiString;

begin
  for Prefix in TitlePrefixesToBeSkipped do
    if IsPrefix(Prefix, WriteAsUTF8(TitleNode.Content)) then
      Exit(True);
  for Suffix in TitleSuffixesToBeSkipped do
    if IsSuffix(Suffix, WriteAsUTF8(TitleNode.Content)) then
      Exit(True);

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
    if CanNotParse(WikiPage.Title) then
    begin
      FMTDebugLn('Skipping %s', [WriteAsUTF8(WikiPage.Title.Content)]);
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
      end;
      Result := True;
      Exit;

    end;
  end
  else
  begin
    Result := False;
    FmtFatalLn('NodeName: %s', [Node.NodeName]);

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

function ParseContent(Content: WideString): TNodes;
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
  end;
  Parser.Free;

end;

{ TWikiParser }

function TWikiParser.IsDone(Token: TToken; EndTokens: TTokens): Boolean;
var
  EndToken: TToken;

begin
  if EndTokens = nil then
    Exit(False);

  Result := True;
  for EndToken in EndTokens do
  begin
    if Token.TokenType <> EndToken.TokenType then
      Continue;
    if EndToken.Text = '' then
      Exit;

    if Token.Text = EndToken.Text then
      Exit;
  end;

  if Token.TokenType in [
    ttCloseHyperLink,
    ttCloseHeadingSection,
    ttEndTemplate,
    ttEndTag,
    ttEndTable
    ] then
    Exit;
  Result := False;

end;

function ToXML(Obj: TObject): AnsiString;
begin
  Result := (Obj as TBaseWikiNode).ToXML('');

end;

function TWikiParser.ParseEntity(EndTokens: TTokens
  ): TBaseWikiNode;
var
  Token: TToken;

begin
  if EndTokens = nil then
    FmtFatalLn('done', []);

  Result := nil;

  Token := Tokenizer.LastToken;
  FMTDebugLn('it: %d Token: %s  %s', [
    it,
    Token.Text,
    TokenTypeToString(Token.TokenType)], 4);
  if IsDone(Token, EndTokens) then
  begin
    {
    if Token.TokenType = ttEOF then
      WriteLn;
      }
    FMTDebugLn('Token: %s  %s', [
      Token.Text,
      TokenTypeToString(Token.TokenType)], 4);
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
    ttComment:
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
      FMTDebugLn('+Unrecognized Token: %s %s', [
        TokenTypeToString(Token.TokenType), Token.Text]);
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

  if ParameterManagerUnit.GetRunTimeParameterManager.ValueByName['--Debug'].AsInteger and 2 <> 0 then
    FMTDebugLn('Result: %s', [Result.ToXML('')], 2);

end;

function TWikiParser.ParseTag(Token: TToken; EndTokens: TTokens): TTagEntity;
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
  Tokenizer.NextToken;

  Child := nil;
  Parameters := TNodes.Create;
  Result := TTagEntity.Create(TagName, Parameters);
  try
    Param := ParseEntity(EndTokens);
    if Param = nil then
      Parameters.Add(Param);
    ParseUntilNil(Param, EndTokens);

    if Tokenizer.LastToken.TokenType in [ttGreaterThan] then
    begin
      Tokenizer.NextToken;
      EndTokens.Pop(3);

    end else // if Tokenizer.LastToken.TokenType in [ttEndTzag] then
    begin
      Tokenizer.NextToken;
      EndTokens.Pop(3);
      Exit;

    end;

    EndTokens.Add(MakeToken(nil, nil, ttEndTag));
    EndTokens.Add(MakeToken(nil, nil, ttOpenHeadingSection));
    while not (Tokenizer.LastToken.TokenType in [ttEndTag, ttEOF]) do
    begin
      Child := nil;
      Child := ParseEntity(EndTokens);
      ParseUntilNil(Child, EndTokens);
      if Child = nil then
        Break;
      Result.AddChild(Child);
      if Tokenizer.LastToken.TokenType in [ttEOF, ttEndTag] then
        Break;
      Tokenizer.NextToken;

    end;

    if Tokenizer.LastToken.TokenType = ttEndTag then
      Tokenizer.NextToken;
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

procedure DumpEndTokens(EndTokens: TTokens);
var
  i: Integer;

begin
  FMTDebugLn('EndTokens.Count: %d', [EndTokens.Count]);

  for i := 0 to EndTokens.Count - 1 do
  begin
    FMTDebugLn('EndTokens[%d]: %s', [i, TokenTypeToString(EndTokens[i].TokenType)]);
  end;
end;

function TWikiParser.ParseTextEntity(Token: TToken; EndTokens: TTokens
  ): TTextWikiEntity;
var
  NextNode: TBaseWikiNode;

begin
  FmtFatalLnIFFalse(
    Token.TokenType in [ttText, ttSingleQuote, ttDoubleQuote, ttNewLine],
    'TokenType: %s',
    [TokenTypeToString(Token)]
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
    // FMTDebugLn('Result: %s', [Result.ToXML('')]);
    // FMTDebugLn('TStyleTextNode: %s', [Result.ToXML('')], 16);
    Exit;

  end;

  Result := TTextWikiEntity.Create(Token.Text);
  if not (Tokenizer.LastToken.TokenType  in [
	  ttOpenHeadingSection,
	  ttBeginTable,
	  ttBeginTag,
	  ttBeginTemplate]) then
    Tokenizer.NextToken;

  try
    NextNode := ParseEntity(EndTokens);

    except on e: EInvalidEntity do
    begin
      EndTokens.Pop(4);
      FreeAndNil(Result);
      if e.Token.TokenType in [ttEndTag, ttEndTemplate, ttEndTable] then
      begin
        Exit(nil);
      end;
      raise
    end;

  end;

  if (NextNode = nil) or not (NextNode is TTextWikiEntity) then
  begin
    if NextNode <> nil then
      Result.AddChild(NextNode);
    EndTokens.Pop(4);
    Exit;

  end;

  Result.Children.Add(NextNode);
  // FMTDebugLn('Result: %s', [Result.ToXML('')]);
  Result.Flatten;
  //FMTDebugLn('Flatten.Result: %s', [Result.ToXML('')]);

  FMTDebugLn('TextNode: %s', [Result.ToXML('')], 16);
  EndTokens.Pop(4);
end;

function TWikiParser.ParseHyperLink(EndTokens: TTokens
  ): THyperLinkEntity;
var
  Current: TBaseWikiNode;
  Text, Link: TBaseWikiNode;
  Parameters: TNodes;


begin
  Tokenizer.NextToken;
  EndTokens := MakeTokens([ttCloseHyperLink,
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
      EndTokens.Pop(3);
      raise
    end;

    end;

    if Current = nil then
      raise EInvalidEntity.Create(MakeToken(nil, nil, ttNone));

    Parameters.Add(Current);
    Current := ParseUntilNil(Current, EndTokens);
    if Tokenizer.LastToken.TokenType = ttCloseHyperLink then
    begin
      Tokenizer.NextToken;
      Break;

    end;
    if Tokenizer.LastToken.TokenType = ttBar then
    begin
      Tokenizer.NextToken;
      Continue;
    end;

    FreeObjects([@Result, @Parameters]);
    EndTokens.Pop(3);
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

  EndTokens.Pop(3);

end;

function TWikiParser.ParseTemplate(EndTokens: TTokens
  ): TTemplate;
var
  Current: TBaseWikiNode;
  Name: TTextWikiEntity;
  Parameters: TNodes;

begin
  Tokenizer.NextToken;
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
      Tokenizer.NextToken;
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
    FMTDebugLn('Name: %s', [name.ToXML('')]);
    FreeObjects([@Parameters, @Name]);
    EndTokens.Pop(4);
    raise;
  end;
  end;

  EndTokens.Pop(4);

  if Tokenizer.LastToken.TokenType = ttEndTemplate then
    Tokenizer.NextToken;
  Result := TTemplate.Create(Name, Parameters);

end;

function TWikiParser.ParseComment(EndTokens: TTokens): TCommentWikiEntry;
begin
  Result := TCommentWikiEntry.Create(Tokenizer.LastToken.Text);
  Tokenizer.NextToken;
end;

function TWikiParser.ParseTable(EndTokens: TTokens): TTable;
var
  Current: TBaseWikiNode;

begin
  Tokenizer.NextToken;
  Result := TTable.Create;
  EndTokens.Add(MakeToken(nil, nil, ttEndTable));
  Current := Result;

  while (Current <> nil) and (Tokenizer.LastToken.TokenType <> ttEOF) do
  begin
    try
      Current := Current.LastNode;
      Current.Next := ParseEntity(EndTokens);
      Current := Current.Next;

    except
      on EInvalidEntity do
      begin
        EndTokens.Pop(1);
        FreeAndNil(Result);
        raise;
      end;

    end;
  end;

  EndTokens.Pop;

end;

function TWikiParser.ParseHeadingSection(Token: TToken; EndTokens: TTokens
  ): THeadingSection;
var
  Current: TBaseWikiNode;
  i: Integer;

begin
  Tokenizer.NextToken;
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  for i := 1 to Length(Token.Text) do
  begin
    EndTokens.Add(
      MakeToken(
        @Token.Text[i],
        @Token.Text[Length(Token.Text) + 1 - i],
        ttCloseHeadingSection
      )
    );
  end;

  try
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
    FMTDebugLn('Result: %s', [Result.ToXML('')], 64);
    FreeAndNil(Result);
    raise EInvalidToken.Create(WriteAsUTF8(Tokenizer.LastToken.Text));

  end;
  EndTokens.Pop(1 + Length(Token.Text));
  Tokenizer.NextToken;

end;

function TWikiParser.ParseSeparator(Token: TToken): TSeparatorWikiEntry;
begin
  Result := TSeparatorWikiEntry.Create(Token.Text);
  Tokenizer.NextToken;

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

function TWikiParser.ParseSeparatorOrHyperLink(Token: TToken; EndTokens: TTokens
  ): TBaseWikiNode;
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

  Tokenizer.NextToken;
  EndTokens.Add(MakeToken(nil, nil, ttCloseBracket));

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

function TWikiParser.ParseBulletList(Token: TToken; EndTokens: TTokens
  ): TBaseWikiNode;
begin
  Tokenizer.NextToken;
  Result := TTextWikiEntity.Create(Token.Text);
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

function TWikiParser.ParseNumberedList(Token: TToken; EndTokens: TTokens
  ): TBaseWikiNode;
begin
  Tokenizer.NextToken;
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

function TWikiParser.ParseEntityWithSingleQuoteToken(AToken: TToken;
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
  Tokenizer.NextToken;
  InitialCount := Length(AToken.Text);
  if InitialCount = 1 then
    Exit(TTextWikiEntity.Create(AToken.Text));

  Result := TTextStyler.CreateStyler(AToken.Text);
  if Result = nil then
    raise EInvalidEntity.Create(MakeToken(nil, nil, ttNone));

  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  EndTokens.Add(MakeToken(nil, nil, ttOpenHeadingSection));
  if InitialCount = 2 then
    EndTokens.Add(MakeToken(@AToken.Text[1], @AToken.Text[2], ttSingleQuote))
  else if InitialCount = 3 then
    EndTokens.Add(MakeToken(@AToken.Text[1], @AToken.Text[3], ttSingleQuote))
  else if InitialCount = 4 then
    EndTokens.Add(MakeToken(@AToken.Text[1], @AToken.Text[4], ttSingleQuote))
  else if (InitialCount = 5) or (InitialCount = 6) then
  begin
    EndTokens.Add(MakeToken(@AToken.Text[1], @AToken.Text[5], ttSingleQuote))

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
      raise;
    end;
    end;

  end;
  if Tokenizer.LastToken.TokenType = ttSingleQuote then
    Tokenizer.NextToken;

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
    Tokenizer.NextToken;
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
  FMTDebugLn('it: %d', [it]);
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

function TWikiTokenizer.GetNextToken: TToken;
begin
  Prev := Current;

  Result := _GetNextToken;
  LastToken := Result;
  FMTDebugLn('%d Token: %s  %s',
    [it, Result.Text, TokenTypeToString(Result.TokenType)],
    4);
  Inc(it);

end;

constructor TWikiTokenizer.Create(constref _Data: WideString);
begin
  inherited Create;

  Data := _Data;
  Current := @(Data[1]);
  LastToken.TokenType := ttNone;
end;

function MaybeGroupSamePatternToken(
  var Current: PWideChar;
  TargetWChar: WideChar;
  ThenTokenType: TTokenType): TToken;
begin
  Result.Text := '';

  while Current^ = TargetWChar do
  begin
    Result.Text += Current^;
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

function TWikiTokenizer._GetNextToken: TToken;

  function GetNext(Current: PWideChar; Delta: Integer = 1): WideChar;
  begin
    Inc(Current, Delta);

    Result := Current^;
  end;

var
  Start: PWideChar;
  Status: Integer;

begin
  while Current^ = ' ' do
  begin
    Inc(Current);

  end;

  Start := Current;
  case Current^ of
    #0:
    begin
      Result.Text := '';
      Result.TokenType:= ttEOF;

    end;
    '<':
    begin
      if GetNext(Current) = ' ' then
      begin
        Inc(Current);
        Exit(MakeToken(Start, Current - 1, ttLessThan));
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
            SetLength(Result.Text, (Current - Start + 1));
            Move(Start^, Result.Text[1], SizeOfWideChar * (Current - Start + 1));
            raise EInvalidToken.Create(AnsiString(Result.Text));

          end;

          Result := MakeToken(Start + 2, Current - 2, ttEndTag)
        end
      end
      else // if Status = 2 then
      begin
        while not HasPrefix(Current, '-->') do
        begin
          Inc(Current);
        end;
        Result := MakeToken(Start + 4, Current, ttComment);
        Inc(Current, 3);

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
      Result := MaybeGroupSamePatternToken(Current, '=', ttEqualSign);

      if Length(Result.Text) = 1 then
      begin
        Result.TokenType := ttEqualSign;

      end
      else if 2 <= Length(Result.Text) then
      begin
        if LastToken.TokenType = ttNewLine then
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
        Exit(MakeToken(Start, Current, ttEndTag));

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


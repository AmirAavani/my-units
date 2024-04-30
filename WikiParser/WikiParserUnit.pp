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

function ParseWikiNode(constref Data: AnsiString; var WikiPage: TWikiPage): Boolean;

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
  ttSlash,
  ttAmpersand, ttApostrophe,
  ttEOF);

  { TToken }

  TToken = record
    TokenType: TTokenType;
    Start, Last: PWideChar;
    // Text: WideString;

    function IsEmpty: Boolean;
    function IsSame(constref OtherToken: TToken): Boolean;
    function Text: WideString;
    function Length: Integer;

    function HasSuffix(constref w: WideString): Boolean;
    function HasTheSameText(constref Token: TToken): Boolean;
  end;

  { TWikiTokenizer }

  TWikiTokenizer = class(TObject)
  private
    Current, Prev: PWideChar;
    LastToken: TToken;

    //function GetCurrentToken: TToken;
    // function GetNextChar: WideChar;
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

const
  SingleQuote = #$27;


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

const
  SizeOfWideChar = SizeOf(WideChar);

function MakeToken(Start, Last: PWideChar; TokenType: TTokenType): TToken;
begin
  Result.Start:= Start;
  Result.Last := Last;
  if (Result.Start = nil) xor (Result.Last = nil) then
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

function ShouldBeSkipped(Title: WideString): Boolean;
var
  Prefix, Suffix: AnsiString;

begin
  for Prefix in TitlePrefixesToBeSkipped do
    if IsPrefix(Prefix, WriteAsUTF8(Title)) then
    begin
      Exit(True);
    end;
  for Suffix in TitleSuffixesToBeSkipped do
    if IsSuffix(Suffix, WriteAsUTF8(Title)) then
    begin
      Exit(True);
    end;

  Result := False;
end;

function IsPrefix(Str, SubStr: PChar): Boolean;
begin
  while SubStr^ <> #0 do
  begin
    if Str^ <> SubStr^ then
      Exit(False);
    Inc(Str);
    Inc(SubStr);

  end;
  Result := True;

end;

function GetEndOfTag(Ch, LastCh: PChar; constref TagName: AnsiString): PChar;
begin
  Result := LastCh;
  while Ch <= LastCh do
  begin
    if Ch^ <> '<' then
    begin
      Inc(Ch);
      Continue;

    end;
    Inc(Ch);

    if Ch^ <> '/' then
    begin
      Inc(Ch);
      Continue;

    end;
    Inc(Ch);

    if IsPrefix(Ch, @TagName[1]) and ((Ch + Length(TagName))^ = '>') then
    begin
      Exit(Ch - 3);

    end;

  end;
end;

const
  TitleTag: AnsiString = 'title';
  TextTag: AnsiString = 'text';
  NSTag: AnsiString = 'ns';
  IDTag: AnsiString = 'id';
  RedirectTag: AnsiString = 'redirect';
  PageTag: AnsiString = 'page';

function ParseWikiNode(constref Data: AnsiString; var WikiPage: TWikiPage): Boolean;
var
  TagName: AnsiString;
  Ch: PChar;
  LastCh: PChar;
  LastOfTag: PChar;

begin
  Result := False;
  Ch := @Data[1];
  LastCh := Ch + Length(Data) - 1;
  WikiPage := TWikiPage.Create;

  while Ch <= LastCh do
  begin
    if Ch^ <> '<' then
    begin
      Inc(Ch);
      Continue;

    end;
    Inc(Ch);
    if (IsPrefix(Ch, @TitleTag[1])) and ((Ch + Length(TitleTag))^ = '>') then
    begin
      Inc(Ch, Length(TitleTag));

      while Ch^ <> '>' do
      begin
        Inc(Ch);
      end;
      Inc(Ch);
      LastOfTag := GetEndOfTag(Ch, LastCh, TitleTag);
      WikiPage.Title := ReadWideStringFromACharArray(Ch, LastOfTag - Ch + 1);
      Ch := LastOfTag + Length(TitleTag) + 4;

      if ShouldBeSkipped(WikiPage.Title) then
      begin
        ALoggerUnit.GetLogger.FMTDebugLnEveryN(1000,
          'Skipping %s',
          [WriteAsUTF8(WikiPage.Title)]);
        Exit(False);

      end;

    end
    else if IsPrefix(Ch, @'text'[1]) then
    begin
      Inc(Ch, 4);

      while Ch^ <> '>' do
      begin
        Inc(Ch);
      end;
      Inc(Ch);
      LastOfTag := GetEndOfTag(Ch, LastCh, '</text>');
      WikiPage.RawData := ReadWideStringFromACharArray(Ch, LastOfTag - Ch + 1);

      WikiPage.Content := ParseContent(WikiPage.RawData);

      Result := True;
      Ch := LastOfTag + Length(TextTag) + 4;

    end
    else if IsPrefix(Ch, @NSTag[1]) and ((Ch + Length(NSTag))^ = '>') then
    begin
      Inc(Ch, Length(NSTag) + 1);
      LastOfTag := GetEndOfTag(Ch, LastCh, NSTag);
      WikiPage.NS := ReadWideStringFromACharArray(Ch, LastOfTag - Ch + 1);
      Ch := LastOfTag + Length(NSTag) + 4;

    end
    else if IsPrefix(Ch, @IDTag[1]) and ((Ch + Length(IDTag))^ = '>') and
      (WikiPage.ID <> '') then
    begin
      Inc(Ch, Length(IDTag) + 1);
      LastOfTag := GetEndOfTag(Ch, LastCh, IDTag);
      WikiPage.ID := ReadWideStringFromACharArray(Ch, LastOfTag - Ch + 1);
      Ch := LastOfTag + Length(IDTag) + 4;

    end
    else if IsPrefix(Ch, @RedirectTag[1])  and ((Ch + Length(RedirectTag))^ = '>')  then
    begin
      WikiPage.Redirect := 'YES';
    end
    else
    begin
      TagName := '<';
      while not (Ch^ in [' ', #10]) do
      begin
        TagName += Ch^;
        Inc(Ch);

      end;
      if TagName = '<' + PageTag + '>' then
        continue
      else if StringUnit.IsPrefix('<id>', TagName) then
      else
        ALoggerUnit.GetLogger.FMTDebugLn('NodeName: %s', [TagName]);


    end

{  if Node.NodeName = 'title' then
  begin
    WikiPage.Title := ReadWideStringFromString(Node.TextContent);
    if ShouldBeSkipped(WikiPage.Title) then
    begin
      ALoggerUnit.GetLogger.FMTDebugLnEveryN(1000,
        'Skipping %s',
        [WriteAsUTF8(WikiPage.Title)]);
      Exit(False);

    end;


  end
  else if Node.NodeName = 'ns' then
  begin
    WikiPage.NS := Node.TextContent;

  end
  else if Node.NodeName = 'id' then
  begin
    WikiPage.ID := Node.TextContent;

  end
  else if Node.NodeName = 'redirect' then
  begin
    if Node.Attributes.GetNamedItem('title') <> nil then
      WikiPage.Redirect := Node.Attributes.GetNamedItem('title').NodeValue;

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
        WikiPage.RawData := ReadWideStringFromString(Child.TextContent);
        WikiPage.Content := ParseContent(WikiPage.RawData);

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
    // ALoggerUnit.FmtFatalLnIFFalse(False, 'NodeName: %s', [Node.NodeName]);

  end;
  }

  end;
end;

function ParseWiki(Start, Last: PChar): TWikiPage;
var
  Child: TDOMNode;

begin
  Result := TWikiPage.Create;
  //Child := Node.FirstChild;

  while Child <> nil do
  begin
    //if not ParseWikiNode(Child, Result) then
    begin
      FreeAndNil(Result);
      Break;
    end;

    Child := Child.NextSibling;
  end;
end;

function TToken.IsEmpty: Boolean;
begin
  Result := Self.Start = nil;

end;


function TToken.IsSame(constref OtherToken: TToken): Boolean;
var
  P1, P2: PWideChar;

begin
  if Self.TokenType <> OtherToken.TokenType then
    Exit(False);

  if Self.Last - Self.Start <> OtherToken.Last - OtherToken.Start then
    Exit(False);

  P1 := Self.Start;
  P2 := OtherToken.Start;
  while True do
  begin
    if P1^ <> P2^ then
      Exit(False);

    if P1 = Self.Last then
      Break;

    Inc(P1);
    Inc(P2);

  end;
  Result := True;
end;

function TToken.Length: Integer;
begin
  Result := Self.Last - Self.Start + 1;
end;

function TToken.HasSuffix(constref w: WideString): Boolean;
var
  pw, pt: PWideChar;
  i: Integer;

begin
  pw := @(w[System.Length(w)]);
  pt := Self.Last;

  i := Self.Length;
  if  System.Length(w) < i then
    i := System.Length(w);

  while i <> 0 do
  begin
    if pw <> pt then
      Exit(False);
    Dec(pw);
    Dec(pt);
    Dec(i);

  end;

  Result := True;


end;

function TToken.HasTheSameText(constref Token: TToken): Boolean;
var
  i: Integer;
  pc1, pc2: PWideChar;

begin
  if Self.Length <> Token.Length then
    Exit(False);


  pc1 := Self.Start;
  pc2 := Token.Start;
  for i := 1 to Self.Length do
  begin
    if pc1 <> pc2 then
      Exit(False);
    Inc(pc1);
    Inc(pc2);

  end;
  Result := True;;

end;

function TToken.Text: WideString;
begin
  if (Self.Start = nil) then
    Exit(EmptyWideStr);

  SetLength(Result, Self.Last - Self.Start + 1);
  Move(
    Self.Start^,
    Result[1],
    SizeOfWideChar * (Self.Last - Self.Start + 1)
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
  Parameters: TNodes;
  Param, Child: TBaseWikiNode;

begin
  Result := nil;
  if Token.HasSuffix(WideString('/>')) then
  begin
    Tokenizer.GetNextToken;
    Exit(TTagEntity.Create(Copy(Token.Text, 2, Token.Length - 3), nil));

  end;

  EndTokens:= MakeTokens(
    [ttGreaterThan, ttEndTag, ttOpenHeadingSection],
    EndTokens);
  Tokenizer.GetNextToken;

  Child := nil;
  Parameters := TNodes.Create;
  Result := TTagEntity.Create(Token.Text, Parameters);
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
    while not (Tokenizer.LastToken.TokenType in
      [ttOpenHeadingSection, ttEndTag, ttEOF]) do
    begin
      Child := nil;
      Child := ParseEntity(EndTokens);
      if Child = nil then
        Break;
      Result.AddChild(Child);
      Child := nil;
      if ParseUntilNil(Result.Children.Last, EndTokens) = nil then
        Break;
      if Tokenizer.LastToken.TokenType in
        [ttOpenHeadingSection, ttEOF, ttEndTag] then
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
begin
  if not (Token.TokenType in [ttText, ttSingleQuote, ttDoubleQuote, ttNewLine]) then
    ALoggerUnit.GetLogger.FmtFatalLn('TokenType: %s', [Token.Text]);

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

  Result := TTextWikiEntity.Create(Token.Start, Token.Last);
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

    Result.Content.Add(
      WikiDocUnit.TTokens.MakePair(
        Tokenizer.LastToken.Start,
        Tokenizer.LastToken.Last
      )
    )

  end;
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
    ttOpenHeadingSection], EndTokens);

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

const COMMENTWideString = WideString('COMMENT:');

function TWikiParser.ParseComment(EndTokens: TTokens): TCommentWikiEntry;
var
  Current: TBaseWikiNode;

begin
  EndTokens.Add(MakeToken(nil, nil, ttEndComment));
  Result := TCommentWikiEntry.Create(
    @COMMENTWideString[1],
    @COMMENTWideString[Length(COMMENTWideString)]);
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
  Ch := Token.Start;
  while Ch <= Token.Last do
  begin
    EndTokens.Add(
      MakeToken(
        Token.Start,
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
      Token.Length,
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
  EndTokens.Pop(2 + Token.Last - Token.Start);
  Tokenizer.GetNextToken;

end;

function TWikiParser.ParseSeparator(constref Token: TToken
  ): TSeparatorWikiEntry;
begin
  Result := TSeparatorWikiEntry.Create(Token.Start, Token.Last);
  Tokenizer.GetNextToken;

end;

function TWikiParser.ParseCaption(EndTokens: TTokens): TBaseWikiNode;
var
  Current: TBaseWikiNode;

begin
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  Result := TTextWikiEntity.Create(nil, nil);
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

  if (Token.Length < 6) or not IsAnInternetProtocol(
    LowerCase(WriteAsUTF8(Tokenizer.LastToken.Text))) then
  begin
    Exit(TSeparatorWikiEntry.Create(Token.Start, Token.Last));

  end;
  Link := TTextWikiEntity.Create(
    Tokenizer.LastToken.Start,
    Tokenizer.LastToken.Last);

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
  Result := TTextWikiEntity.Create(Token.Start, Token.Last);
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
  Result := TTextWikiEntity.Create(Token.Start, Token.Last);
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

var
  InitialCount: Integer;
  Child: TBaseWikiNode;

begin
  Tokenizer.GetNextToken;
  InitialCount := AToken.Length;
  if InitialCount = 1 then
  begin
    Exit(TTextWikiEntity.Create(AToken.Start, AToken.Last));

  end;

  Result := TTextStyler.CreateStyler(AToken.Text);
  if Result = nil then
    raise EInvalidEntity.Create(MakeToken(nil, nil, ttNone));

  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  EndTokens.Add(MakeToken(nil, nil, ttOpenHeadingSection));
  if InitialCount = 2 then
    EndTokens.Add(MakeToken(AToken.Start, AToken.Start + 1, ttSingleQuote))
  else if InitialCount = 3 then
    EndTokens.Add(MakeToken(AToken.Start, AToken.Start + 2, ttSingleQuote))
  else if InitialCount = 4 then
    EndTokens.Add(MakeToken(AToken.Start, AToken.Start + 3, ttSingleQuote))
  else if (InitialCount = 5) or (InitialCount = 6) then
  begin
    EndTokens.Add(MakeToken(AToken.Start, AToken.Start + 4, ttSingleQuote))

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
  Result.Start := Current;
  while Current^ = TargetWChar do
  begin
    Result.Last := Current;
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

procedure TWikiTokenizer._GetNextToken(var Result: TToken);

  function GetNext(Current: PWideChar; Delta: Integer = 1): WideChar;
  begin
    Result := (Current + 1)^;
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

      if Result.Length = 1 then
      begin
        Result.TokenType := ttEqualSign;

      end
      else if 2 <= Result.Length then
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
      if Result.Length = 1 then
      begin
        Result.TokenType := ttOpenCurlyBrace;
      end
      else if Result.Length = 2 then
      begin
        Result.TokenType := ttBeginTemplate;
      end;
    end;
    '}':
    begin
      Result := MaybeGroupSamePatternToken(Current, '}', ttCloseCurlyBrace);
      if Result.Length >= 2 then
      begin
        Result.TokenType := ttEndTemplate;
        Current -= Result.Length;
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


unit WikiParserUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, GenericCollectionUnit, WikiDocUnit,
  StringUnit;

type

  EBaseWikiParser = class(Exception);

function ParseWiki(Node: TDOMNode): TWikiPage;

implementation
uses
  ALoggerUnit, WideStringUnit;

type
  TTokenType = (ttNone, ttHeading, ttText, ttLessThan, ttGreaterThan,
  ttStartTag, ttEndTag,
  ttIdentifier, ttEqualSign, ttString, ttNewLine,
  ttBeginTemplate, ttEndTemplate,
  ttComment,
  ttOpenBracket, ttCloseBracket,
  ttOpenCurlyBrace, ttCloseCurlyBrace,
  ttOpenHyperLink, ttCloseHyperLink,
  ttBeginTable, ttEndTable,
  ttTableCaption,
  ttBeginTableRow,
  ttTableCellSeparator,
  ttMinus, ttBar,
  ttSlash, ttEOF);
  TToken = record
    TokenType: TTokenType;
    Text: WideString;

  end;

  { TWikiTokenizer }

  TWikiTokenizer = class(TObject)
  private
    Data: WideString;
    Current: PWideChar;
    LastToken: TToken;

    function GetNextChar: WideChar;
    function GetNextToken: TToken;
    function _GetNextToken: TToken;

  public
    property NextToken: TToken read GetNextToken;
    constructor Create(constref _Data: WideString);

  end;

  { TTokens }

  TTokens = class(specialize TCollection<TToken>)
  end;

  EInvalidToken = class(EBaseWikiParser);

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
    function ParseHyperLink(EndTokens: TTokens): THyperLinkEntity;
    function ParseTemplate(Token: TToken; EndTokens: TTokens): TTemplate;
    function ParseTable(Token: TToken; EndTokens: TTokens): TTable;
    function ParseCaption(EndTokens: TTokens): TTextWikiEntity;

  public
    constructor Create(_Tokenizer: TWikiTokenizer);
    destructor Destroy; override;

    function ParseDoc: TNodes;

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

function ParseContent(Content: WideString): TNodes; forward;

function ParseWikiNode(Node: TDOMNode; WikiPage: TWikiPage): Boolean;
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

{ EInvalidEntity }

constructor EInvalidEntity.Create(_Token: TToken);
begin
  inherited Create('');

  FToken := _Token;
end;

function TokenTypeToString(TokenType: TTokenType): AnsiString;
begin
  WriteStr(Result, TokenType);

end;

function TokenTypeToString(Token: TToken): AnsiString;
begin
  Result := TokenTypeToString(Token.TokenType);

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
      Parser.Free;
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

  Result := False;

end;

function TWikiParser.ParseEntity(EndTokens: TTokens): TBaseWikiNode;
var
  Token: TToken;

begin
  if EndTokens = nil then
    FmtFatalLn('done', []);

  Result := nil;

  Token := Tokenizer.GetNextToken;
  FMTDebugLn('Token: %s  %s', [Token.Text, TokenTypeToString(Token.TokenType)], 4);
  if IsDone(Token, EndTokens) then
  begin
    FMTDebugLn('Token: %s  %s', [Token.Text, TokenTypeToString(Token.TokenType)], 4);
    Exit(nil);

  end;

  case Token.TokenType of
  ttStartTag:
    Result := ParseTag(Token, EndTokens);
  ttText:
    Result := TTextWikiEntity.Create(Token.Text);
  ttString:
    Result := TTextWikiEntity.Create(Token.Text);
  ttBeginTemplate:
    Result := ParseTemplate(Token, EndTokens);
  ttComment:
    Result := TCommentWikiEntry.Create(Token.Text);
  ttOpenHyperLink:
    Result := ParseHyperLink(EndTokens);
  ttBeginTable:
    Result := ParseTable(Token, EndTokens);
  ttBar, ttEqualSign, ttMinus, ttLessThan, ttGreaterThan,
  ttBeginTableRow, ttTableCaption, ttEndTable, ttOpenCurlyBrace,
  ttCloseCurlyBrace, ttSlash,
  ttOpenBracket, ttCloseBracket, ttNewLine, ttTableCellSeparator:
    Result := TSeparatorWikiEntry.Create(Token.Text);
  else
    FMTDebugLn('+Unrecognized Token: %s %s', [
      TokenTypeToString(Token.TokenType), Token.Text]);
    raise EInvalidEntity.Create(Token);
  end;

  FMTDebugLn('Result: %s', [Result.ToXML], 0);
end;

function TWikiParser.ParseTag(Token: TToken; EndTokens: TTokens): TTagEntity;
var
  TagName: WideString;
  Current: TBaseWikiNode;
  EndTagToken: TToken;
  Parameters: TNodes;
  Param: TBaseWikiNode;

begin
  if IsSuffix(WideString('/>'), Token.Text) then
    Exit(TTagEntity.Create(Copy(Token.Text, 2, Length(Token.Text) - 3), nil));

  TagName := Token.Text;

  EndTokens.Add(MakeToken(nil, nil, ttGreaterThan));
  EndTokens.Add(MakeToken(nil, nil, ttEndTag));

  Parameters := TNodes.Create;
  try
    Param := ParseEntity(EndTokens);
    Parameters.Add(Param);

    Current := Param;
    while Current <> nil  do
    begin
      Current := Current.LastNode;
      Current.Next := ParseEntity(EndTokens);

    end;
    Result := TTagEntity.Create(TagName, Parameters);
    if Tokenizer.LastToken.TokenType = ttEndTag then
    begin
      Exit;

    end;

    EndTokens.Pop; EndTokens.Pop;

    EndTagToken := MakeToken(
      nil, nil, ttEndTag
    );
    EndTagToken.Text:= TagName + '>';
    EndTokens.Add(EndTagToken);

    Result.Next := ParseEntity(EndTokens);
    Current := Result;
    while Current.Next <> nil do
    begin
      Current := Current.Next.LastNode;
      Current.Next := ParseEntity(EndTokens);

    end;

    EndTokens.Pop;

  except
    on EInvalidEntity do
    begin
      FreeAndNil(Result);
      raise;

    end;
  end;
end;

function TWikiParser.ParseHyperLink(EndTokens: TTokens
  ): THyperLinkEntity;
var
  Current: TBaseWikiNode;
  Text, Link: TBaseWikiNode;
  Parameters: TNodes;

begin
  EndTokens.Add(MakeToken(nil, nil, ttCloseHyperLink));
  EndTokens.Add(MakeToken(nil, nil, ttBar));

  //Current := Result.LastNode;
  //Current.Next := ParseEntity(EndTokens);

  Current := ParseEntity(EndTokens);
  Text := Current;
  while Current <> nil do
  begin
    Current := Current.LastNode;
    Current.Next := ParseEntity(EndTokens);
    Current := Current.Next;

  end;

  if Tokenizer.LastToken.TokenType = ttCloseHyperLink then
  begin
    Result := THyperLinkEntity.Create(nil, Text, nil);

    EndTokens.Pop;
    EndTokens.Pop;
  end
  else if Tokenizer.LastToken.TokenType = ttBar then
  begin
    Parameters := TNodes.Create;
    Link := Text;

    while Tokenizer.LastToken.TokenType in [ttBar] do
    begin
      Current := Self.ParseEntity(EndTokens);
      Parameters.Add(Current);

      while Current <> nil do
      begin
        try
          Current.Next := Self.ParseEntity(EndTokens);

        except
          on EInvalidEntity do
          begin
            FreeAndNil(Result);
            raise;
          end;

        end;

        if Current.Next = nil then
          Break;

        Current := Current.Next.LastNode;

      end;

    end;

    Result := THyperLinkEntity.Create(Link, Text, nil);
    EndTokens.Pop;

  end;

end;

function TWikiParser.ParseTemplate(Token: TToken; EndTokens: TTokens
  ): TTemplate;
var
  Current: TBaseWikiNode;
  Name: TBaseWikiNode;
  Parameters: TNodes;

begin
  EndTokens.Add(MakeToken(nil, nil, ttEndTemplate));
  EndTokens.Add(MakeToken(nil, nil, ttBar));
  EndTokens.Add(MakeToken(nil, nil, ttTableCellSeparator));

  Parameters := nil;
  Current := Self.ParseEntity(EndTokens);
  Name := Current;

  while Current <> nil do
  begin
    try
      Current.Next := Self.ParseEntity(EndTokens);

    except
      on EInvalidEntity do
      begin
        FreeAndNil(Result);
        raise;
      end;

    end;

    if Current.Next = nil then
      Break;

    Current := Current.Next.LastNode;

  end;

  if Tokenizer.LastToken.TokenType in [ttBar, ttTableCellSeparator] then
  begin
    Parameters := TNodes.Create;

    while Tokenizer.LastToken.TokenType in [ttBar, ttTableCellSeparator] do
    begin
      Current := Self.ParseEntity(EndTokens);
      Parameters.Add(Current);

      while Current <> nil do
      begin
        try
          Current.Next := Self.ParseEntity(EndTokens);

        except
          on EInvalidEntity do
          begin
            FreeAndNil(Result);
            raise;
          end;

        end;

        if Current.Next = nil then
          Break;

        Current := Current.Next.LastNode;

      end;

    end;

  end;


  EndTokens.Pop;
  EndTokens.Pop;
  EndTokens.Pop;

  Result := TTemplate.Create(Name, Parameters);
end;

function TWikiParser.ParseTable(Token: TToken; EndTokens: TTokens): TTable;
var
  Current: TBaseWikiNode;

begin
  Result := TTable.Create;
  EndTokens.Add(MakeToken(nil, nil, ttEndTable));
  FMTDebugLn('In ParseTable', []);
  Current := Result.LastNode;
  while Current <> nil do
  begin
    Tokenizer.NextToken;

    case Tokenizer.LastToken.TokenType of
      ttTableCaption:
      begin
        EndTokens.Add(MakeToken(nil, nil, ttNewLine));
        Current.Next := ParseCaption(EndTokens);
        Current := Current.Next.LastNode;
      end;
      ttBeginTableRow:
      begin
        FmtFatalLn('Done', []);
        EndTokens.Add(MakeToken(nil, nil, ttNewLine));
        Current.Next := ParseCaption(EndTokens);
        while Current.Next <> nil do
        begin
          Current := Current.Next.LastNode;

        end;
        EndTokens.Pop;

      end
    end;

    Current := Current.Next.LastNode;

  end;

  EndTokens.Pop;

end;

function TWikiParser.ParseCaption(EndTokens: TTokens): TTextWikiEntity;
var
  Current: TBaseWikiNode;

begin
  Result := TTextWikiEntity.Create(WideString(''));

  Current := Result.LastNode;
  EndTokens.Add(MakeToken(nil, nil, ttNewLine));
  Current.Next := ParseEntity(EndTokens);
  while Current.Next <> nil do
  begin
    Current := Current.Next.LastNode;
    Current.Next := ParseEntity(EndTokens);

  end;
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
    Next := Self.ParseEntity(EndTokens);
    while Next <> nil do
    begin
      Result.Add(Next);
      Next := Self.ParseEntity(EndTokens);

    end;
    EndTokens.Free;
  except
    on e: EInvalidToken do
    begin
      EndTokens.Free;
      FreeAndNil(Result);
      raise
    end;
    on e: EInvalidEntity do
    begin
      EndTokens.Free;
      FreeAndNil(Result);
      raise
    end
  end;

end;

{ TWikiTokenizer }

function TWikiTokenizer.GetNextChar: WideChar;
begin
  Result := Current^;
  Inc(Current);

end;

function TWikiTokenizer.GetNextToken: TToken;
begin
  LastToken := _GetNextToken;
  Result := LastToken;
  // FMTDebugLn('GetNextToken: %s %s', [TokenTypeToString(Result.TokenType), Result.Text]);
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
          Result := MakeToken(Start + 1, Current - 1, ttStartTag)
        else if Status = 1 then
        begin
          if Current^ = '>' then
            Inc(Current)
          else
          begin
            SetLength(Result.Text, Current - Start + 1);
            Move(Start^, Result.Text[1], SizeOfWideChar * (Current - Start + 1));
            raise EInvalidToken.Create(AnsiString(Result.Text));

          end;

          Result := MakeToken(Start + 2, Current - 1, ttEndTag)
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
      if (GetNext(Current) = SingleQuote)
         and
         (GetNext(Current, 2) = SingleQuote) then
      begin
        Inc(Current, 3);
        Result := ScanTillToken(Current, SingleQuote + SingleQuote + SingleQuote, ttString);

      end
      else
      begin
        Inc(Current, 1);
        Result := ScanTillToken(Current, SingleQuote, ttString);

      end

    end;
    '"':
    begin
      Inc(Current, 1);
      Result := ScanTillToken(Current, '"', ttString);

    end;
    '=':
    begin
      Result := MaybeGroupSamePatternToken(Current, '=', ttEqualSign);

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
      else if 2 <= Length(Result.Text) then
      begin
        Result.TokenType := ttBeginTemplate;
      end;
    end;
    '}':
    begin
      Result := MaybeGroupSamePatternToken(Current, '}', ttEqualSign);
      if Length(Result.Text) = 2 then
      begin
        Result.TokenType := ttEndTemplate;
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

end.


unit WikiDocUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StringUnit, GenericCollectionUnit, WideStringUnit, WikiTypesUnits,
  Miscellaneous;

type

  { TBaseWikiNode }

  TBaseWikiNode = class(TObject)
  private
    FNext: TBaseWikiNode;
    FParent: TBaseWikiNode;
    function GetLastNode: TBaseWikiNode;
    function GetNext: TBaseWikiNode;
  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; virtual;
    procedure SetNext(NextNode: TBaseWikiNode);

    procedure DoExportText(Unigrams, Bigrams: TWideStringList); virtual;

  public
    property Next: TBaseWikiNode read GetNext write SetNext;
    property Parent: TBaseWikiNode read FParent write FParent;
    property LastNode: TBaseWikiNode read GetLastNode;

    constructor Create;
    function ToXML(constref Indent: AnsiString): AnsiString;
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList);

  end;

  { TNodes }

  TNodes = class(specialize TObjectCollection<TBaseWikiNode>)
  private
    function ToXML(Indent: AnsiString): AnsiString;
  end;


  { TBaseWikiNodeWithChildren }

  TBaseWikiNodeWithChildren = class(TBaseWikiNode)
  protected
    FChildren: TNodes;

    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property Children: TNodes read FChildren;

    constructor Create;
    destructor Destroy; override;
    procedure AddChild(Child: TBaseWikiNode);

  end;

  TPairPWideChar = specialize TPair<PWideChar, PWideChar>;

  { TTokens }

  TTokens = class(specialize TCollection<TPairPWideChar>)
  public
    function IsPrefix(constref SubStr: WideString): Boolean;
    function JoinStrings(constref Separator: WideString): WideString;
    procedure Join(tokens: TTokens);

    class function MakePair(TokenStart, TokenLast: PWideChar): TPairPWideChar;

  end;

  { TTextWikiEntity }

  TTextWikiEntity = class(TBaseWikiNodeWithChildren)
  private
    FContent: TTokens;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property Content: TTokens read FContent write FContent;

    constructor Create(TokenStart, TokenLast: PWideChar);
    destructor Destroy; override;

    procedure Flatten;
    function GetText: WideString;
  end;

  { TTextStyler }

  TTextStyler = class(TTextWikiEntity)
  private
    FStyle: TStyle;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;


  public
    property Style: TStyle read FStyle;

    constructor Create(_Style: TStyle);
    class function CreateStyler(constref Text: WideString): TTextStyler;
    destructor Destroy; override;

  end;

  { TItalicTextStyler }

  TItalicTextStyler = class(TTextStyler)
  public
    constructor Create;

  end;

  { TBoldTextStyler }

  TBoldTextStyler = class(TTextStyler)
  public
    constructor Create;

  end;


  { TItalicBoldTextStyler }

  TItalicBoldTextStyler = class(TTextStyler)
  public
    constructor Create;

  end;

  { TCommentWikiEntry }

  TCommentWikiEntry = class(TTextWikiEntity)
  protected
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    constructor Create(Start, Last: PWideChar);

  end;

  { TSeparatorWikiEntry }

  TSeparatorWikiEntry = class(TTextWikiEntity)
  protected
  public
    constructor Create(Start, Last: PWideChar);
    destructor Destroy; override;

  end;

  { TTagEntity }

  TTagEntity = class(TBaseWikiNodeWithChildren)
  private
    FParameters: TNodes;
    FTagName: WideString;

  protected
    function _ToXML(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property TagName: WideString read FTagName;
    property Parameters: TNodes read FParameters;

    constructor Create(constref _TagName: WideString; _Parameters: TNodes);
    destructor Destroy; override;

  end;

  { THyperLinkEntity }

  THyperLinkEntity = class(TBaseWikiNode)
  private
    FLink, FText: TBaseWikiNode;
    FParams: TNodes;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property Link: TBaseWikiNode read FLink;
    property Text: TBaseWikiNode read FText;
    property Params: TNodes read FParams;

    constructor Create(l, t: TBaseWikiNode; p: TNodes);
    destructor Destroy; override;



  end;

  { TTemplate }

  TTemplate = class(TBaseWikiNode)
  private
    FTemplateName: TBaseWikiNode;
    FParameters: TNodes;
    function GetTemplateName: WideString;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property TemplateName: WideString read GetTemplateName;

    constructor Create(NameNode: TBaseWikiNode; Params: TNodes);
    destructor Destroy; override;

  end;


  { THeadingSection }

  THeadingSection = class(TBaseWikiNodeWithChildren)
  private
    FNumber: Integer;
    FTitle: TBaseWikiNode;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property Number: Integer read FNumber;
    property Title: TBaseWikiNode read FTitle;


    constructor Create(_Number: Integer; _Title: TBaseWikiNode);
    destructor Destroy; override;

  end;

  { TTable }

  TTable = class(TBaseWikiNode)
  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;


  end;

  { TBulletedListEntity }

  TBulletedListEntity = class(TBaseWikiNodeWithChildren)

  end;

  { TNumberedListEntity }

  TNumberedListEntity = class(TBaseWikiNodeWithChildren)

  end;

  { TWideStringListPair }

  TWideStringListPair = specialize TPair<TWideStringList, TWideStringList>;

  function NewTWideStringListPair: TWideStringListPair;

  { TWikiPage }

type
  TWikiPage = class(TObject)
  private
    FContent: TNodes;
    FRawData: WideString;
    // FNodes: TNodes;
    FTitle, FNS, FRedirect, FID: WideString;
    procedure SetContent(AValue: TNodes);
    procedure SetRawData(const AValue: WideString);

  public
    // property Nodes: TNodes read FNodes write SetNodes;
    property Title: WideString read FTitle write FTitle;
    property NS: WideString read FNS write FNS;
    property ID: WideString read FID write FID;
    property Redirect: WideString read FRedirect write FRedirect;
    property Content: TNodes read FContent write SetContent;
    property RawData: WideString read FRawData write SetRawData;

    constructor Create;
    destructor Destroy; override;

    function ExportText: TWideStringListPair;
    function ToXML: AnsiString;

    function IsADisambiguationPage: Boolean;
  end;


implementation
uses
  ALoggerUnit;

const
  SingleQuote = #$27;
  WideStringSpace = WideString(' ');
  WideStringNewLine = WideString(#$0A);

var
  WideStrSplit4Extracts: WideString;

procedure ExtractUnigramsAndBigrams(
  constref Text: TWideStringList;
  Unigrams, Bigrams: TWideStringList);
var
  TextUnigrams: TWideStringList;
  i: Integer;
  Uni: WideString;


begin
  TextUnigrams := WideStrSplit(
    Text.JoinStrings(' '),
    WideStrSplit4Extracts,
    True);
  if TextUnigrams.IsEmpty then
  begin
    TextUnigrams.Free;
    Exit;

  end;

  i := 0;
  for Uni in TextUnigrams do
  begin
    if (Uni = WideStringSpace) or (Uni = WideStringNewLine) then
      continue;
    TextUnigrams[i] := Uni;
    Inc(i);
  end;

  TextUnigrams.Count := i;
  Unigrams.AddAnotherCollection(TextUnigrams);
  // Bigrams.Capacity := Bigrams.Capacity + TextUnigrams.Count - 1;

  for i := 0 to TextUnigrams.Count - 2 do
  begin
    Bigrams.Add(TextUnigrams[i] + ' ' + TextUnigrams[i + 1]);

  end;
  TextUnigrams.Free;

end;

procedure SplitTokens(Tokens: TTokens; constref Delimiters: WideString;
    Unigrams: TWideStringList);

  function IsADelimiter(constref Current: WideChar): Boolean; inline;
  var
    Delimiter: WideChar;

  begin
    for Delimiter in Delimiters do
      if Current = Delimiter then
        Exit(True);

    Result := False;
  end;

var
  Start, Current: PWideChar;
  Tmp: WideString;
  Token: TPairPWideChar;
  It: TTokens.TEnumerator;

begin
  Tmp := '';
  It := Tokens.GetEnumerator;

  while It.MoveNext do
  begin
    Token := It.Current;
    Start := Token.First;
    Current := Start;
    while Current <= Token.Second do
    begin
      if not IsADelimiter(Current^) then
      begin
        Inc(Current);
        Continue;

      end;

      if Start < Current then
      begin
        SetLength(Tmp, (Current - Start));
        Move(Start^, Tmp[1], SizeOf(WideChar) * (Current - Start));
        Unigrams.Add(Tmp);

      end;

      Start := Current + 1;
      Unigrams.Add(Current^);
      Inc(Current);

    end;

    if Start < Current then
    begin
      SetLength(Tmp, (Current - Start));
      Move(Start^, Tmp[1], SizeOf(WideChar) * (Current - Start));
      Unigrams.Add(Tmp);

    end;

  end;
  It.Free;

end;

procedure ExtractUnigramsAndBigrams(Tokens: TTokens; Unigrams, Bigrams: TWideStringList);
var
  BeforeUnigramCount, BeforeBigramCount: Integer;
  i: Integer;

begin
  if Tokens.Count = 0 then
    Exit;
  BeforeUnigramCount := Unigrams.Count;
  SplitTokens(Tokens, WideStrSplit4Extracts, Unigrams);
  if Unigrams.Count <= BeforeUnigramCount + 1 then
    Exit;

  BeforeBigramCount := Bigrams.Count;
  Bigrams.Count := BeforeBigramCount + (Unigrams.Count - BeforeUnigramCount - 1);
  for i := BeforeUnigramCount to Unigrams.Count - 2 do
  begin
    Bigrams[BeforeBigramCount + i - BeforeUnigramCount] := Unigrams[i] + ' ' + Unigrams[i + 1];
  end;

end;

{ HeadingSection }

function THeadingSection._ToXml(constref Indent: AnsiString): AnsiString;
var
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('%s<%s>', [Indent, ClassName]));
  Lines.Add(Format('%s  <TITLE>', [Indent]));
  Lines.Add(Self.Title.ToXML(Indent + '  '));
  Lines.Add(Format('%s  </TITLE>', [Indent]));
  Lines.Add(inherited _ToXml(Indent + '  '));
  Lines.Add(Format('%s</%s>', [Indent, ClassName]));

  Result := Lines.Text;
  Lines.Free;

end;

constructor THeadingSection.Create(_Number: Integer; _Title: TBaseWikiNode);
begin
  inherited Create;

  FNumber := _Number;
  FTitle := _Title;

end;

destructor THeadingSection.Destroy;
begin
  FTitle.Free;

  inherited Destroy;
end;

procedure THeadingSection.DoExportText(Unigrams, Bigrams: TWideStringList);
begin
  Title.ExportText(Unigrams, Bigrams);

end;

{ TTable }

function TTable._ToXml(constref Indent: AnsiString): AnsiString;
begin
  Result := '';
end;

function NewTWideStringListPair: TWideStringListPair;
begin
  Result.First := TWideStringList.Create;
  Result.Second := TWideStringList.Create;

end;


{ TBaseWikiNodeWithChildren }

function TBaseWikiNodeWithChildren._ToXml(constref Indent: AnsiString
  ): AnsiString;
var
  Child: TBaseWikiNode;
  Lines: TStringList;

begin
  if Children = nil then
    Exit('');

  Lines := TStringList.Create;
  for Child in Children do
    Lines.Add(Child.ToXML(Indent + '  '));

  Result := Lines.Text;
  Lines.Free;

end;

procedure TBaseWikiNodeWithChildren.DoExportText(Unigrams,
  Bigrams: TWideStringList);
var
  Child: TBaseWikiNode;
  CurrentUnigrams, CurrentBigrams: TWideStringList;
  LastUnigrams: WideString;

begin
  inherited DoExportText(Unigrams, Bigrams);

  if Children.Count = 0 then
    Exit;

  CurrentBigrams := TWideStringList.Create;
  CurrentUnigrams := TWideStringList.Create;
  LastUnigrams := '';

  for Child in Children do
  begin
    CurrentBigrams.Clear;
    CurrentUnigrams.Clear;

    Child.ExportText(CurrentUnigrams, CurrentBigrams);
    Unigrams.AddAnotherCollection(CurrentUnigrams);
    if (LastUnigrams <> '') and not CurrentUnigrams.IsEmpty then
    begin
      Bigrams.Add(LastUnigrams + WideString(' ') + CurrentUnigrams.First);

    end;
    Bigrams.AddAnotherCollection(CurrentBigrams);

    LastUnigrams := '';
    if not Unigrams.IsEmpty then
      LastUnigrams := Unigrams[0];

  end;
  CurrentBigrams.Free;
  CurrentUnigrams.Free;


end;

constructor TBaseWikiNodeWithChildren.Create;
begin
  inherited Create;

  FChildren := TNodes.Create;
end;

destructor TBaseWikiNodeWithChildren.Destroy;
begin
  FChildren.Free;

  inherited Destroy;
end;

{ TItalicBoldTextStyler }

constructor TItalicBoldTextStyler.Create;
begin
  inherited Create(tsItalicsBold);

end;

{ TBoldTextStyler }

constructor TBoldTextStyler.Create;
begin
  inherited Create(tsBold);

end;

{ TItalicTextStyler }

constructor TItalicTextStyler.Create;
begin
  inherited Create(tsItalics);

end;

{ TTextStyler }

function TTextStyler._ToXml(constref Indent: AnsiString): AnsiString;
var
  TagName: WideString;
  Child: TBaseWikiNode;
  Lines: TStringList;

begin
  WriteStr(TagName, Self.Style);

  Lines := TStringList.Create;
  Lines.Add(Format('<%s>', [WriteAsUTF8(TagName)]));

  for Child in FChildren do
    Lines.Add(Child.ToXML(Indent + '  '));
  Lines.Add(Format('</%s>', [TagName]));

  Result := Lines.Text;
  Lines.Free;

end;

const LineBreakWideString = WideString(sLineBreak);
const SingleQuoteWideString = WideString(SingleQuote);

constructor TTextStyler.Create(_Style: TStyle);
begin
  inherited Create(
    @LineBreakWideString[1],
    @LineBreakWideString[Length(LineBreakWideString)]
  );

  FStyle := _Style;
end;

class function TTextStyler.CreateStyler(constref Text: WideString
  ): TTextStyler;
var
  Child: TBaseWikiNode;

begin
  case Length(Text) of
  2: Exit(TItalicTextStyler.Create);
  3: Exit(TBoldTextStyler.Create);
  4:
    begin
      Child := TTextWikiEntity.Create(
        @SingleQuoteWideString[1],
        @SingleQuoteWideString[Length(SingleQuoteWideString)]
      );
      Result := TBoldTextStyler.Create;
      Result.Children.Add(Child);
    end;
  5:
    begin
      Result := TItalicBoldTextStyler.Create;
    end;
  6:
  begin
    Result := TItalicBoldTextStyler.Create;
    Result.Children.Add(
      TTextWikiEntity.Create(
        @SingleQuoteWideString[1],
        @SingleQuoteWideString[Length(SingleQuoteWideString)])
      );
  end
  else
    ALoggerUnit.GetLogger.FMTDebugLn('Invalid Style: Length(Text): %d ->%s', [Length(Text), Text]);
    Result := nil;

  end;
end;

destructor TTextStyler.Destroy;
begin

  inherited Destroy;
end;

procedure TTextStyler.DoExportText(Unigrams, Bigrams: TWideStringList);
begin
  inherited DoExportText(Unigrams, Bigrams);
end;

{ TTemplate }

function TTemplate.GetTemplateName: WideString;
var
  Current: TBaseWikiNode;

begin
  Result := (FTemplateName as TTextWikiEntity).GetText;

  for Current in (FTemplateName as TTextWikiEntity).Children do
  begin
    if not (Current is TTextWikiEntity) then
    begin
      ALoggerUnit.GetLogger.FMTDebugLn(
        'One of FTemplateName.Children is not TTextWikiEntity',
        []);
      Continue;

    end;
    Result += WideStringSpace;
    Result += (Current as TTextWikiEntity).GetText;

  end;

end;

function TTemplate._ToXml(constref Indent: AnsiString): AnsiString;
var
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('<%s>', [ClassName]));
  Lines.Add(Format('  <Name> %s </Name>', [FTemplateName.ToXML(Indent)]));
  Lines.Add(Format('  <Params> %s </Params>', [FParameters.ToXML(Indent)]));
  Lines.Add(Format('</%s>', [ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

var
  Goftavard: WideString;

procedure TTemplate.DoExportText(Unigrams, Bigrams: TWideStringList);
begin
  Exit;

end;

constructor TTemplate.Create(NameNode: TBaseWikiNode; Params: TNodes);
begin
  inherited Create;

  FTemplateName := NameNode;

  FParameters := Params;

end;

destructor TTemplate.Destroy;
begin
  FTemplateName.Free;
  FParameters.Free;

  inherited Destroy;
end;

{ TTagEntity }

function TTagEntity._ToXML(constref Indent: AnsiString): AnsiString;
var
  Lines: TStringList;
  Child: TBaseWikiNode;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('%s<%s>', [Indent, ClassName]));
  Lines.Add(Format('%s  <Name> %s </Name>', [Indent, FTagName]));
  Lines.Add(Format('%s  <Params>', [Indent]));
  Lines.Add(Format('%s    %s', [Indent, FParameters.ToXML(Indent)]));
  Lines.Add(Format('%s  </Params>', [Indent]));
  Lines.Add(Format('%s  <Children>', [Indent]));
  for Child in Self.Children do
    Lines.Add(Child.ToXML(Indent + '  '));
  Lines.Add(Format('%s  </Children>', [Indent]));
  Lines.Add(Format('%s</%s>', [Indent, ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

procedure TTagEntity.DoExportText(Unigrams, Bigrams: TWideStringList);
const
  ToBeProcessedTag: array of WideString = ();

var
  TName: WideString;

begin
  for TName in ToBeProcessedTag do
    if Self.TagName = TName then
    begin
      inherited DoExportText(Unigrams, Bigrams);
      Break;

    end;

end;

constructor TTagEntity.Create(constref _TagName: WideString; _Parameters: TNodes
  );
begin
  inherited Create;

  FTagName := _TagName;
  FParameters := _Parameters;

end;

destructor TTagEntity.Destroy;
begin
  FParameters.Free;

  inherited Destroy;
end;

{ TWikiPage }

procedure TWikiPage.SetContent(AValue: TNodes);
begin
  FContent.Free;
  FContent := AValue;
end;

procedure TWikiPage.SetRawData(const AValue: WideString);
  function DecodeXML: Boolean;
  var
    Target: PWideChar;
    Ch, Last: PWideChar;

  begin
    Ch := @FRawData[1];
    Last := Ch + Length(FRawData) - 1;
    Target := Ch;

    while Ch <= Last do
    begin
      if Ch^ <> '&' then
      begin
        Target^ := Ch^;
        Inc(Ch);
        Inc(Target);
        Continue;

      end;

      if HasPrefix(Ch, '&amp;') then
      begin
        Inc(Ch, 5);
        Target^ := '&';
        Inc(Target);

      end
      else if HasPrefix(Ch, '&lt;') then
      begin
        Inc(Ch, 4);
        Target^ := '<';
        Inc(Target);

      end
      else if HasPrefix(Ch, '&gt;') then
      begin
        Inc(Ch, 4);
        Target^ := '>';
        Inc(Target);

      end
      else if HasPrefix(Ch, '&apos;') then
      begin
        Inc(Ch, 6);
        Target^ := SingleQuote;
        Inc(Target);

      end
      else if HasPrefix(Ch, '&quot;') then
      begin
        Inc(Ch, 6);
        Target^ := '"';
        Inc(Target);

      end
      else
        ALoggerUnit.GetLogger.FmtFatalLn('Invalid Sequence: ', []);

    end;
    SetLength(FRawData, Target - @(FRawData[1]));

  end;

begin
  FRawData := AValue;
  DecodeXML;

end;

constructor TWikiPage.Create;
begin
  inherited Create;

end;

destructor TWikiPage.Destroy;
begin
  if Self = nil then
    Exit;

  FContent.Free;

  inherited Destroy;
end;

const
  TerminatorHeadings: array of AnsiString = (
  'جستارهای وابسته',
  'منابع',
  'پیوند به بیرون',
  'پانویس'
  );

function TWikiPage.ExportText: TWideStringListPair;

  function IsTerminatorHeadings(const Node: TBaseWikiNode): Boolean;
  var
    S: AnsiString;

  begin
    Result := True;

    if (Node is THeadingSection) and ((Node as THeadingSection).Number = 2) then
    begin
      if not ((Node as THeadingSection).Title is TTextWikiEntity) then
        Exit(False);

      for S in TerminatorHeadings do
        if WideStringUnit.WriteAsUTF8(
          ((Node as THeadingSection).Title as TTextWikiEntity).GetText) = S then
        begin
          Exit;
        end;
    end;

    Result := False;
  end;

var
  UCount, BCount: SizeInt;
  PrevNode, Node: TBaseWikiNode;
  PairForTitle: TWideStringListPair;
  NewBigram: WideString;

begin
  Result := NewTWideStringListPair;
  if Self = nil then
    Exit;

  PairForTitle := NewTWideStringListPair;

  if FContent <> nil then
  begin
    PrevNode := nil;
    for Node in FContent do
    begin
      if IsTerminatorHeadings(Node) then
        break;

      UCount := Result.First.Count;
      BCount := Result.Second.Count;

      Node.ExportText(
        Result.First,
        Result.Second);

      if Node is THeadingSection then
      begin
        PrevNode := Node;
        Continue;

      end;
      if (PrevNode is THeadingSection) and (UCount = Result.First.Count) then
      begin
        Continue;

      end;

      if (UCount <> 0) and (UCount < Result.First.Count) and
          not (PrevNode is THeadingSection) then
      begin
        NewBigram := Result.First[UCount - 1] + WideString(' ') + Result.First[UCount];
        if BCount < Result.Second.Count then
          Result.Second.Insert(BCount, NewBigram)
        else
          Result.Second.Add(NewBigram);

      end;
      PrevNode := Node;

      end;

    end;

  {
  if Title <> nil then
  begin
    ExtractUnigramsAndBigrams(
      Title.Content,
      PairForTitle.First,
      PairForTitle.Second);

  end;
  }

  Result.First.AddAnotherCollection(PairForTitle.First);
  Result.Second.AddAnotherCollection(PairForTitle.Second);

  PairForTitle.First.Free;
  PairForTitle.Second.Free;

end;

function TWikiPage.ToXML: AnsiString;
begin
  if Self = nil then
    Exit('<nil/>');

  Result := FContent.ToXML('');
end;

const
  AbhamzodaeeTemplate: AnsiString = 'ابهام‌زدایی';

function TWikiPage.IsADisambiguationPage: Boolean;

  function dfs(Node: TBaseWikiNode): Boolean;
  var
    Child: TBaseWikiNode;

  begin
    if Node is TTemplate then
      if WideStringUnit.WriteAsUTF8((
        Node as TTemplate).TemplateName) = AbhamzodaeeTemplate then
          Exit(True);

    if not (Node is TBaseWikiNodeWithChildren) then
      Exit(False);

    for Child in (Node as TBaseWikiNodeWithChildren).Children do
      if dfs(Child) then
        Exit(True);

    Result := False;

  end;

var
  Node: TBaseWikiNode;

begin
  if Self = nil then
    Exit(False);

  if Self.FContent = nil then
    Exit(False);

  for Node in FContent do
  begin
    if dfs(Node) then
      Exit(True);

  end;

  Result := False;
end;


{ TNodes }

function TNodes.ToXML(Indent: AnsiString): AnsiString;
var
  Node: TBaseWikiNode;
  Lines: TStringList;

begin
  if Self = nil then
    Exit('<TNodes/>');

  Lines := TStringList.Create;
  Lines.Add(Indent + '<TNodes>');

  for Node in Self do
  begin
    // FMTDebugLn('Node: %s', [Node.ClassName]);
    Lines.Add(Node.ToXML(Indent + '  '));
  end;

  Lines.Add(Indent + '</TNodes>');

  Result := Lines.Text;
  Lines.Free;

end;

{ THyperLinkEntity }

constructor THyperLinkEntity.Create(l, t: TBaseWikiNode; p: TNodes);
begin
  inherited Create;

  FLink := l;
  FText := t;
  FParams := p;
end;

destructor THyperLinkEntity.Destroy;
begin
  FLink.Free;
  FText.Free;
  FParams.Free;

  inherited Destroy;

end;

function THyperLinkEntity._ToXml(constref Indent: AnsiString): AnsiString;
var
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('%s<%s>', [Indent, ClassName]));
  if Link <> nil then
  begin
    Lines.Add(Format('%s  <Link>', [Indent]));
    Lines.Add(Format('%s    %s', [Indent, Link.ToXML((Indent + '  '))]));
    Lines.Add(Format('%s  </Link>', [Indent]));
  end;

  Lines.Add(Format('%s  <Text>', [Indent]));
  Lines.Add(Format('%s    %s', [Indent, Text.ToXML((Indent + '  '))]));
  Lines.Add(Format('%s  </Text>', [Indent]));
  Lines.Add(Format('%s</%s>', [Indent, ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

var Parvandeh: WideString;

procedure THyperLinkEntity.DoExportText(Unigrams, Bigrams: TWideStringList);
begin
  if (FLink <> nil) and(FLink is TTextWikiEntity) and
    (FLink as TTextWikiEntity).FContent.IsPrefix(Parvandeh) then
  begin
    Exit;
  end;

  if FText <> nil then
    FText.ExportText(Unigrams, Bigrams);

  inherited DoExportText(Unigrams, Bigrams);
end;

{ TSeparatorWikiEntry }

constructor TSeparatorWikiEntry.Create(Start, Last: PWideChar);
begin
  inherited Create(Start, Last);

end;

destructor TSeparatorWikiEntry.Destroy;
begin
  inherited Destroy;
end;

{ TCommentWikiEntry }

constructor TCommentWikiEntry.Create(Start, Last: PWideChar);
begin
  inherited Create(Start, Last);

end;

procedure TCommentWikiEntry.DoExportText(Unigrams, Bigrams: TWideStringList);
begin

end;

{ TTextWikiEntity }

function TTextWikiEntity.GetText: WideString;
var
  Child: TBaseWikiNode;

begin
  if Self = nil then
    Exit('');

  Result := FContent.JoinStrings(' ');
  for Child in Children do
    if Child is TTextWikiEntity then
      Result += ' ' + (Child as TTextWikiEntity).GetText;

end;


function TTextWikiEntity._ToXml(constref Indent: AnsiString): AnsiString;
var
  S: WideString;

begin
  if self = nil then
      Exit('');
  S := FContent.JoinStrings(' ');
  S := WideStringReplace(S, sLineBreak, WideString('[LINEBREAK]'), [rfReplaceAll]);
  S := WideStringReplace(S, '&', WideString('&amp;'), [rfReplaceAll]);
  S := WideStringReplace(S, '"', WideString('&quot;'), [rfReplaceAll]);

  Result := Format('<%s content="%s">' + sLineBreak, [ClassName, WriteAsUTF8(S)]);
  Result += inherited _ToXml(Indent + '  ');
  Result += Format(sLineBreak+'</%s>', [ClassName]);

end;

procedure TTextWikiEntity.Flatten;
var
  Child: TBaseWikiNode;
  TextChild: TTextWikiEntity;
  MyChildren: TNodes;

begin
  MyChildren := TNodes.Create;
  while 0 < Self.Children.Count do
  begin
    Child := Self.Children[0];

    if Child.ClassName = TTextWikiEntity.ClassName then
    begin
      TextChild := Child as TTextWikiEntity;
      Self.FContent.Join(TextChild.FContent);
      Self.Children.Delete(0);
      if TextChild.Children.Count <> 0 then
        MyChildren.AddAnotherCollection(TextChild.Children);

      TextChild.Children.Clear;
      TextChild.Free;

    end
    else
    begin
      Break;

    end;

  end;

  MyChildren.AddAnotherCollection(self.Children);
  Self.Children.Clear;
  Self.Children.Free;
  Self.FChildren := MyChildren;

end;

constructor TTextWikiEntity.Create(TokenStart, TokenLast: PWideChar);
begin
  inherited Create;

  FContent := TTokens.Create;
  if TokenStart <= TokenLast then
    FContent.Add(TTokens.MakePair(TokenStart, TokenLast));

end;

destructor TTextWikiEntity.Destroy;
begin
  FContent.Free;

  inherited Destroy;
end;

procedure TTextWikiEntity.DoExportText(Unigrams, Bigrams: TWideStringList);
var
  UCount, BCount: Integer;
  NewBigram: WideString;

begin
  ExtractUnigramsAndBigrams(Self.FContent, Unigrams, Bigrams);
  UCount := Unigrams.Count;
  BCount := Bigrams.Count;

  inherited DoExportText(Unigrams, Bigrams);


  if (UCount <> 0) and (UCount < Unigrams.Count) then
  begin
    NewBigram := Unigrams[UCount - 1] + WideString(' ') + Unigrams[UCount];
    if BCount < Bigrams.Count then
      Bigrams.Insert(BCount, NewBigram)
    else
      Bigrams.Add(NewBigram);

  end;

end;

procedure TBaseWikiNodeWithChildren.AddChild(Child: TBaseWikiNode);
begin
  self.Children.Add(Child);
end;

{ TTokens }

function TTokens.IsPrefix(constref SubStr: WideString): Boolean;
var
  Token: TPairPWideChar;
  Source, PCh: PWideChar;
  i: Integer;

begin
  Result := True;

  Source := @SubStr[1];
  i := 1;

  for Token in Self do
  begin
    Pch := Token.First;
    while PCh <= Token.Second do
    begin
      if PCh^ <> Source^ then
        Exit(False);

      Source^ := Pch^;
      Inc(Pch);
      Inc(i);
      if Length(SubStr) < i then
        Break;


    end;

  end;
end;

function TTokens.JoinStrings(constref Separator: WideString): WideString;
var
  Token: TPairPWideChar;
  Size: Integer;
  PCh, Target: PWideChar;

begin
  Result := '';

  Size := 0;
  for Token in Self do
    Size += Token.Second - Token.First;
  Inc(Size, 2 * Self.Count - 1);
  SetLength(Result, Size);
  Target := @Result[1];
  for Token in Self do
  begin
    Pch := Token.First;
    while PCh <= Token.Second do
    begin
      Target^ := Pch^;
      Inc(Target);
      Inc(Pch);

    end;
    Target^ := ' ';
    Inc(Target);

  end;
end;

procedure TTokens.Join(tokens: TTokens);
begin
  ALoggerUnit.GetLogger.FatalLn('NIY');

end;

class function TTokens.MakePair(TokenStart, TokenLast: PWideChar): TPairPWideChar;
begin
  Result := specialize MakePair<PWideChar, PWideChar>(TokenStart, TokenLast);
end;

{ TBaseWikiNode }

function TBaseWikiNode.GetLastNode: TBaseWikiNode;
begin
  if Self = nil then
    Exit(nil);

  Result := Self;

  while Result.Next <> nil do
    Result := Result.Next;

end;

function TBaseWikiNode.GetNext: TBaseWikiNode;
begin
  if Self = nil then
    Exit(nil);

  Result := FNext;
end;

function TBaseWikiNode._ToXml(constref Indent: AnsiString): AnsiString;
begin
  ALoggerUnit.FmtFatalLnIFFalse(False, '%s %s', [Indent, Self.ClassName]);
  Result := '<ClassName/>';

end;

var
  c: Integer;

procedure TBaseWikiNode.ExportText(Unigrams, Bigrams: TWideStringList);
var
  UCount, BCount: Integer;
  NewBigram: WideString;

begin
  Inc(c);
  if Self = nil then
    Exit;

  Self.DoExportText(Unigrams, Bigrams);

  UCount := Unigrams.Count;
  BCount := Bigrams.Count;
  if FNext <> nil then
    FNext.ExportText(Unigrams, Bigrams);

  if (UCount <> 0) and (UCount < Unigrams.Count) then
  begin
    NewBigram := Unigrams[UCount - 1] + WideString(' ') + Unigrams[UCount];
    if BCount < Bigrams.Count then
      Bigrams.Insert(BCount, NewBigram)
    else
      Bigrams.Add(NewBigram);

  end;

end;

procedure TBaseWikiNode.SetNext(NextNode: TBaseWikiNode);
begin
  if NextNode = nil then
    Exit;

  FNext := NextNode;
  NextNode.FParent := Self;

end;

procedure TBaseWikiNode.DoExportText(Unigrams, Bigrams: TWideStringList);
begin
  // Do nothing;
end;

constructor TBaseWikiNode.Create;
begin
  inherited Create;

  FNext := nil;

end;

function TBaseWikiNode.ToXML(constref Indent: AnsiString): AnsiString;
var
  n: TBaseWikiNode;

begin
  if Self = nil then
    Exit('<nil/>');

  Result := '';
  n := Self;
  while n <> nil do
  begin
    Result += n._ToXml(Indent + '  ') + sLineBreak;
    n := n.Next;

  end;

end;

destructor TBaseWikiNode.Destroy;
var
  Last: TBaseWikiNode;
  LastParent: TBaseWikiNode;

begin
  Last := Self;
  while Last.FNext <> nil do
  begin
    Last := Last.Next;
  end;


  while Last <> Self do
  begin
    Last.FNext := nil;
    LastParent := Last.Parent;
    Last.Free;
    Last := LastParent;

  end;
  Last.Next := nil;

  inherited Destroy;
end;

initialization
  WideStrSplit4Extracts := WideStringUnit.ReadWideStringFromString(
    ' .!?-_(),،»«؛');
  Goftavard := WideStringUnit.ReadWideStringFromString(
    'گفتاورد');
  Parvandeh := WideStringUnit.ReadWideStringFromString(
  'پرونده:');

end.


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

  public
    property Next: TBaseWikiNode read GetNext write SetNext;
    property Parent: TBaseWikiNode read FParent write FParent;
    property LastNode: TBaseWikiNode read GetLastNode;

    constructor Create;
    function ToXML(constref Indent: AnsiString): AnsiString;
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); virtual;
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
  public
    property Children: TNodes read FChildren;

    constructor Create;
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;

  end;

  { TTextWikiEntity }

  TTextWikiEntity = class(TBaseWikiNodeWithChildren)
  private
    FContent: WideString;
    function GetContent: WideString;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;

  public
    property Content: WideString read GetContent;

    constructor Create(constref Text: WideString);
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;
  end;

  { TTextStyler }

  TTextStyler = class(TBaseWikiNodeWithChildren)
  private
    FStyle: TStyle;

    function _ToXml(constref Indent: AnsiString): AnsiString; override;

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
  public
    constructor Create(constref Text: WideString);

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;

  end;

  { TSeparatorWikiEntry }

  TSeparatorWikiEntry = class(TTextWikiEntity)
  protected
  public
    constructor Create(constref Text: WideString);
    destructor Destroy; override;

  end;

  { TTagEntity }

  TTagEntity = class(TBaseWikiNodeWithChildren)
  private
    FParameters: TNodes;
    FTagName: WideString;

  protected
    function _ToXML(constref Indent: AnsiString): AnsiString; override;
  public
    property TagName: WideString read FTagName;
    property Parameters: TNodes read FParameters;

    constructor Create(constref _TagName: WideString; _Parameters: TNodes);
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;

  end;

  { THyperLinkEntity }

  THyperLinkEntity = class(TBaseWikiNode)
  private
    FLink, FText: TBaseWikiNode;
    FParams: TNodes;
    FParmas: TNodes;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
  public
    property Link: TBaseWikiNode read FLink;
    property Text: TBaseWikiNode read FText;
    property Params: TNodes read FParams;

    constructor Create(l, t: TBaseWikiNode; p: TNodes);
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;


  end;

  { TTemplate }

  TTemplate = class(TBaseWikiNode)
  private
    FTemplateName: TBaseWikiNode;
    FParameters: TNodes;
    function GetTemplateName: WideString;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;

  public
    property TemplateName: WideString read GetTemplateName;

    constructor Create(NameNode: TBaseWikiNode; Params: TNodes);
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;
  end;


  { THeadingSection }

  THeadingSection = class(TBaseWikiNodeWithChildren)
  private
    FNumber: Integer;
    FTitle: TTextWikiEntity;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;

  public
    property Number: Integer read FNumber;
    property Title: TTextWikiEntity read FTitle;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;

    constructor Create(_Number: Integer; _Title: TTextWikiEntity);
    destructor Destroy; override;

  end;

  { TTable }

  TTable = class(TBaseWikiNode)
  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;


  end;

  TBulletedListEntity = class(TBaseWikiNodeWithChildren)

  end;

  TNumberedListEntity = class(TBaseWikiNodeWithChildren)

  end;

  TWideStringListPair = specialize TPair<TWideStringList, TWideStringList>;
  { TWikiPage }

  TWikiPage = class(TObject)
  private
    FContent: TNodes;
    // FNodes: TNodes;
    FTitle, FNS, FID, FRedirect: TTextWikiEntity;
    function GetTitle: TTextWikiEntity;
    procedure SetContent(AValue: TNodes);
    procedure SetID(AValue: TTextWikiEntity);
    procedure SetNS(AValue: TTextWikiEntity);
    procedure SetRedirect(AValue: TTextWikiEntity);
    procedure SetTitle(AValue: TTextWikiEntity);

  public
    // property Nodes: TNodes read FNodes write SetNodes;
    property Title: TTextWikiEntity read GetTitle write SetTitle;
    property NS: TTextWikiEntity read FNS write SetNS;
    property ID: TTextWikiEntity read FID write SetID;
    property Redirect: TTextWikiEntity read FRedirect write SetRedirect;
    property Content: TNodes read FContent write SetContent;

    constructor Create;
    destructor Destroy; override;

    function ExportText: TWideStringListPair;
    function ToXML: AnsiString;
  end;


implementation
uses
  ALoggerUnit, QueueUnit;

const
  SingleQuote = #$27;
var
  WideStrSplit4Extracts: WideString;

procedure ExtractUnigramsAndBigrams(constref Text: WideString; Unigrams, Bigrams: TWideStringList);
var
  TextUnigrams: TWideStringList;
  i: Integer;

begin
  if Text = sLineBreak then
    Exit;
  TextUnigrams := WideStrSplit(Text, WideStrSplit4Extracts, True);
  Unigrams.AddAnotherCollection(TextUnigrams);
  for i := 0 to TextUnigrams.Count - 2 do
    Bigrams.Add(TextUnigrams[i] + ' ' + TextUnigrams[i + 1]);
  TextUnigrams.Free;

end;

{ HeadingSection }

function THeadingSection._ToXml(constref Indent: AnsiString): AnsiString;
var
  Lines: TWideStringList;

begin
  Lines := TWideStringList.Create;
  Lines.Add(Format('%s<%s>', [Indent, ClassName]));
  Lines.Add(Format('%s  <TITLE>', [Indent]));
  Lines.Add(Self.Title.ToXML(Indent + '  '));
  Lines.Add(Format('%s  </TITLE>', [Indent]));
  Lines.Add(inherited _ToXml(Indent + '  '));
  Lines.Add(Format('%s</%s>', [Indent, ClassName]));

  Result := Lines.JoinStrings;
  Lines.Free;

end;

constructor THeadingSection.Create(_Number: Integer; _Title: TTextWikiEntity);
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

procedure THeadingSection.ExportText(Unigrams, Bigrams: TWideStringList);
begin
  inherited ExportText(Unigrams, Bigrams);
end;

{ TTable }

function TTable._ToXml(constref Indent: AnsiString): AnsiString;
begin
  Result := '';
end;


{ TBaseWikiNodeWithChildren }

function TBaseWikiNodeWithChildren._ToXml(constref Indent: AnsiString
  ): AnsiString;
var
  Child: TBaseWikiNode;
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  for Child in Children do
    Lines.Add(Child.ToXML(Indent + '  '));

  Result := Lines.Text;
  Lines.Free;

end;

procedure TBaseWikiNodeWithChildren.ExportText(Unigrams,
  Bigrams: TWideStringList);
var
  Child: TBaseWikiNode;

begin
  for Child in Children do
    Child.ExportText(Unigrams, Bigrams);

  inherited ExportText(Unigrams, Bigrams);

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
  TagName: AnsiString;
  Child: TBaseWikiNode;
  Lines: TWideStringList;

begin
  WriteStr(TagName, Self.Style);

  Lines := TWideStringList.Create;
  Lines.Add(Format('<%s>', [TagName]));

  for Child in FChildren do
    Lines.Add(Child.ToXML(Indent + '  '));
  Lines.Add(Format('</%s>', [TagName]));

  Result := Lines.JoinStrings;

end;

constructor TTextStyler.Create(_Style: TStyle);
begin
  inherited Create;

  FStyle := _Style;
end;

class function TTextStyler.CreateStyler(constref Text: WideString): TTextStyler;
var
  Child: TBaseWikiNode;

begin
  case Length(Text) of
  2: Exit(TItalicTextStyler.Create);
  3: Exit(TBoldTextStyler.Create);
  4:
    begin
      Child := TTextWikiEntity.Create(SingleQuote);
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
      TTextWikiEntity.Create(SingleQuote));
  end
  else
    FmtFatalLn('Length(Text): %d', [Length(Text)]);

  end;
end;

destructor TTextStyler.Destroy;
begin

  inherited Destroy;
end;

{ TTemplate }

function TTemplate.GetTemplateName: WideString;
var
  Current: TBaseWikiNode;
  AllContents: TWideStringList;

begin
  Result := '';
  AllContents := TWideStringList.Create;

  AllContents.Add((FTemplateName as TTextWikiEntity).Content);
  for Current in (FTemplateName as TTextWikiEntity).Children do
  begin
    if not (Current is TTextWikiEntity) then
    begin
      FMTDebugLn('One of FTemplateName.Children is not TTextWikiEntity',  []);
      Continue;

    end;
    AllContents.Add((Current as TTextWikiEntity).Content);

  end;

  Result := AllContents.JoinStrings;
  AllContents.Free;

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

procedure TTemplate.ExportText(Unigrams, Bigrams: TWideStringList);
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

procedure TTagEntity.ExportText(Unigrams, Bigrams: TWideStringList);
begin
  if Self.TagName <> 'ref' then
    inherited ExportText(Unigrams, Bigrams);

  // Do nothing
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

function TWikiPage.GetTitle: TTextWikiEntity;
begin
  if Self = nil then
    Exit(nil);

  Result := FTitle;
end;

procedure TWikiPage.SetID(AValue: TTextWikiEntity);
begin
  FID.Free;
  FID := AValue;
end;

procedure TWikiPage.SetNS(AValue: TTextWikiEntity);
begin
  FNS.Free;
  FNS := AValue;
end;

procedure TWikiPage.SetRedirect(AValue: TTextWikiEntity);
begin
  FRedirect.Free;
  FRedirect := AValue;
end;

procedure TWikiPage.SetTitle(AValue: TTextWikiEntity);
begin
  FTitle.Free;
  FTitle := AValue;
end;

constructor TWikiPage.Create;
begin
  inherited Create;

end;

destructor TWikiPage.Destroy;
begin
  FTitle.Free;
  FID.Free;
  FNS.Free;
  FRedirect.Free;
  FContent.Free;

  inherited Destroy;
end;

function TWikiPage.ExportText: TWideStringListPair;
var
  Node: TBaseWikiNode;

begin
  Result.First := TWideStringList.Create;
  Result.Second := TWideStringList.Create;
  ExtractUnigramsAndBigrams(Self.Title.Content, Result.First, Result.Second);

  if FContent <> nil then
  begin
    for Node in FContent do
    begin
      Node.ExportText(Result.First, Result.Second);

    end;

  end;

end;

function TWikiPage.ToXML: AnsiString;
begin
  if Self = nil then
    Exit('<nil/>');

  Result := FContent.ToXML('');
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
  Lines.Add(Format('%s  <Link>', [Indent]));
  Lines.Add(Format('%s    %s', [Indent, Link.ToXML((Indent + '  '))]));
  Lines.Add(Format('%s  </Link>', [Indent]));
  Lines.Add(Format('%s  <Text>', [Indent]));
  Lines.Add(Format('%s    %s', [Indent, Text.ToXML((Indent + '  '))]));
  Lines.Add(Format('%s  </Text>', [Indent]));
  Lines.Add(Format('%s</%s>', [Indent, ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

var Parvandeh: WideString;

procedure THyperLinkEntity.ExportText(Unigrams, Bigrams: TWideStringList);
begin
  if (FLink <> nil) and(FLink is TTextWikiEntity) and
    IsPrefix(Parvandeh, (FLink as TTextWikiEntity).Content) then
  begin
    Exit;
  end;

  if FText <> nil then
    FText.ExportText(Unigrams, Bigrams);

  inherited ExportText(Unigrams, Bigrams);
end;

{ TSeparatorWikiEntry }

constructor TSeparatorWikiEntry.Create(constref Text: WideString);
begin
  inherited Create(Text);

end;

destructor TSeparatorWikiEntry.Destroy;
begin
  inherited Destroy;
end;

{ TCommentWikiEntry }

constructor TCommentWikiEntry.Create(constref Text: WideString);
begin
  inherited Create(Text);

end;

procedure TCommentWikiEntry.ExportText(Unigrams, Bigrams: TWideStringList);
begin

end;

{ TTextWikiEntity }

function TTextWikiEntity.GetContent: WideString;
begin
  if Self = nil then
    Exit('');

  Result := FContent;
end;

function TTextWikiEntity._ToXml(constref Indent: AnsiString): AnsiString;
begin
  if self = nil then
      Exit('');
  Result := Format('<%s>%s</%s>', [ClassName, WriteAsUTF8(FContent), ClassName]);
  Result += inherited _ToXml(Indent);

end;

constructor TTextWikiEntity.Create(constref Text: WideString);
begin
  inherited Create;

  FContent := Text;

end;

destructor TTextWikiEntity.Destroy;
begin
  inherited Destroy;
end;

procedure TTextWikiEntity.ExportText(Unigrams, Bigrams: TWideStringList);
begin
  ExtractUnigramsAndBigrams(Self.FContent, Unigrams, Bigrams);
  inherited ExportText(Unigrams, Bigrams);

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
  FmtFatalLn('%s %s', [Indent, Self.ClassName]);
  Result := '<ClassName/>';

end;

procedure TBaseWikiNode.ExportText(Unigrams, Bigrams: TWideStringList);
begin
  if FNext <> nil then
    FNext.ExportText(Unigrams, Bigrams);

end;

procedure TBaseWikiNode.SetNext(NextNode: TBaseWikiNode);
begin
  if NextNode = nil then
    Exit;

  FNext := NextNode;
  NextNode.FParent := Self;

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
  Count: Integer;
  Tmp: TWideStringList;

begin
  Last := Self;
  Count:= 0;
  while Last.FNext <> nil do
  begin
    Inc(Count);
    if Count mod 1000 = 0 then
      FMTDebugLn('Count: %d', [Count]);
    Last := Last.Next;
  end;

  if 6500 < Count then
  begin
    Tmp := TWideStringList.Create();
    FMTDebugLn('Self.ClassName: %s', [Self.ClassName]);
  end;
  Count := 0;
  while Last <> Self do
  begin
    Last.FNext := nil;
    LastParent := Last.Parent;
    Last.Free;
    Inc(Count);
    if Count mod 100 = 0 then
      FMTDebugLn('Count: %d', [Count]);
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


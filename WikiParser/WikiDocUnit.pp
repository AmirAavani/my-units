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
    function _ToXml: AnsiString; virtual;
    procedure SetNext(NextNode: TBaseWikiNode);

  public
    property Next: TBaseWikiNode read GetNext write SetNext;
    property Parent: TBaseWikiNode read FParent write FParent;
    property LastNode: TBaseWikiNode read GetLastNode;

    constructor Create;
    function ToXML: AnsiString;
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); virtual;
  end;

  { TNodes }

  TNodes = class(specialize TObjectCollection<TBaseWikiNode>)
  private
    function ToXML: AnsiString;
  end;


  { TBaseWikiNodeWithChildren }

  TBaseWikiNodeWithChildren = class(TBaseWikiNode)
  protected
    FChildren: TNodes;

    function _ToXml: AnsiString; override;
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
    function _ToXml: AnsiString; override;

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

    function _ToXml: AnsiString; override;

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
    function _ToXML: AnsiString; override;
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
    function _ToXml: AnsiString; override;
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

  protected
    function _ToXml: AnsiString; override;

  public
    constructor Create(NameNode: TBaseWikiNode; Params: TNodes);
    destructor Destroy; override;

    procedure ExportText(Unigrams, Bigrams: TWideStringList); override;
  end;

  { TTable }

  TTable = class(TBaseWikiNode)
  protected

  end;

  TWideStringListPair = specialize TPair<TWideStringList, TWideStringList>;
  { TWikiPage }

  TWikiPage = class(TObject)
  private
    FContent: TNodes;
    // FNodes: TNodes;
    FTitle, FNS, FID, FRedirect: TTextWikiEntity;
    procedure SetContent(AValue: TNodes);
    procedure SetID(AValue: TTextWikiEntity);
    procedure SetNS(AValue: TTextWikiEntity);
    procedure SetRedirect(AValue: TTextWikiEntity);
    procedure SetTitle(AValue: TTextWikiEntity);

  public
    // property Nodes: TNodes read FNodes write SetNodes;
    property Title: TTextWikiEntity read FTitle write SetTitle;
    property NS: TTextWikiEntity read FNS write SetNS;
    property ID: TTextWikiEntity read FID write SetID;
    property Redirect: TTextWikiEntity read FRedirect write SetRedirect;
    property Content: TNodes read FContent write SetContent;

    constructor Create;
    destructor Destroy; override;

    function ExportText: TWideStringListPair;
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


{ TBaseWikiNodeWithChildren }

function TBaseWikiNodeWithChildren._ToXml: AnsiString;
var
  Child: TBaseWikiNode;
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  for Child in Children do
    Lines.Add(Child.ToXML);

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

function TTextStyler._ToXml: AnsiString;
var
  TagName: AnsiString;
  Child: TBaseWikiNode;
  Lines: TWideStringList;

begin
  WriteStr(TagName, Self.Style);

  Lines := TWideStringList.Create;
  Lines.Add(Format('<%s>', [TagName]));

  for Child in FChildren do
    Lines.Add(Child.ToXML);
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

function TTemplate._ToXml: AnsiString;
var
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('<%s>', [ClassName]));
  Lines.Add(Format('  <Name> %s </Name>', [FTemplateName.ToXML]));
  Lines.Add(Format('  <Params> %s </Params>', [FParameters.ToXML]));
  Lines.Add(Format('</%s>', [ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

procedure TTemplate.ExportText(Unigrams, Bigrams: TWideStringList);
begin
  // Do nothing;
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

function TTagEntity._ToXML: AnsiString;
var
  Lines: TStringList;
  Child: TBaseWikiNode;
begin
  Lines := TStringList.Create;
  Lines.Add(Format('<%s>', [ClassName]));
  Lines.Add(Format('  <Name> %s </Name>', [FTagName]));
  Lines.Add(Format('  <Params> %s </Params>', [FParameters.ToXML]));
  Lines.Add(Format('  <Children>', []));
  for Child in Self.Children do
    Lines.Add(Child.ToXML);
  Lines.Add(Format('  </Children>', []));
  Lines.Add(Format('</%s>', [ClassName]));

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

  for Node in FContent do
    Node.ExportText(Result.First, Result.Second);

end;


{ TNodes }

function TNodes.ToXML: AnsiString;
var
  Node: TBaseWikiNode;
  Lines: TStringList;

begin
  if Self = nil then
    Exit('<TNodes/>');

  Lines := TStringList.Create;
  Lines.Add('<TNodes>');

  for Node in Self do
    Lines.Add(Node.ToXML);

  Lines.Add('</TNodes>');

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

function THyperLinkEntity._ToXml: AnsiString;
var
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('<%s>', [ClassName]));
  Lines.Add(Format('  <Link> %s </Link>', [Link.ToXML]));
  Lines.Add(Format('  <Text> %s </Text>', [Text.ToXML]));
  Lines.Add(Format('</%s>', [ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

const
  Parvandeh = 'پرونده:';

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

function TTextWikiEntity._ToXml: AnsiString;
begin
  if self = nil then
      Exit('');
  Result := Format('<%s>%s</%s>', [ClassName, WriteAsUTF8(FContent), ClassName]);
  Result += inherited _ToXml;

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

function TBaseWikiNode._ToXml: AnsiString;
begin
  FmtFatalLn('%s', [Self.ClassName]);
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

function TBaseWikiNode.ToXML: AnsiString;
var
  n: TBaseWikiNode;

begin
  if Self = nil then
    Exit('<nil/>');

  Result := '';
  n := Self;
  while n <> nil do
  begin
    Result += n._ToXml;
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
  WideStrSplit4Extracts := WideStringUnit.ReadWideStringFromString(' .!?-_(),،');

end.


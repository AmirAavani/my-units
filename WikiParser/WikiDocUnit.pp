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

  { TTextWikiEntity }

  TTextWikiEntity = class(TBaseWikiNodeWithChildren)
  private
    FContent: TWideStringList;
    function GetContent: WideString;

  protected
    function _ToXml(constref Indent: AnsiString): AnsiString; override;
    procedure DoExportText(Unigrams, Bigrams: TWideStringList); override;

  public
    property Content: WideString read GetContent;

    constructor Create(constref Text: WideString);
    destructor Destroy; override;

    procedure Flatten;
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
    constructor Create(constref Text: WideString);

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

  TBulletedListEntity = class(TBaseWikiNodeWithChildren)

  end;

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

procedure ExtractUnigramsAndBigrams(constref Text: TWideStringList; Unigrams, Bigrams: TWideStringList);
var
  TextUnigrams: TWideStringList;
  i: Integer;
  Uni: WideString;


begin
  TextUnigrams := WideStrSplit(
    Text.JoinStrings(' '),
    WideStrSplit4Extracts,
    True);
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

  for i := 0 to TextUnigrams.Count - 2 do
  begin
    Bigrams.Add(TextUnigrams[i] + ' ' + TextUnigrams[i + 1]);

  end;
  TextUnigrams.Free;

end;

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
      Bigrams.Add(LastUnigrams + WideString(' ') + CurrentUnigrams.First);
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

constructor TTextStyler.Create(_Style: TStyle);
begin
  inherited Create(sLineBreak);

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
  AllContents: TWideStringList;

begin
  Result := '';
  AllContents := TWideStringList.Create;

  AllContents.Add((FTemplateName as TTextWikiEntity).Content);
  for Current in (FTemplateName as TTextWikiEntity).Children do
  begin
    if not (Current is TTextWikiEntity) then
    begin
      ALoggerUnit.GetLogger.FMTDebugLn(
        'One of FTemplateName.Children is not TTextWikiEntity',
        []);
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
  if Self = nil then
    Exit;

  FTitle.Free;
  FID.Free;
  FNS.Free;
  FRedirect.Free;
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
          ((Node as THeadingSection).Title as TTextWikiEntity).Content) = S then
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
    IsPrefix(Parvandeh, (FLink as TTextWikiEntity).FContent.JoinStrings('')) then
  begin
    Exit;
  end;

  if FText <> nil then
    FText.ExportText(Unigrams, Bigrams);

  inherited DoExportText(Unigrams, Bigrams);
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

procedure TCommentWikiEntry.DoExportText(Unigrams, Bigrams: TWideStringList);
begin

end;

{ TTextWikiEntity }

function TTextWikiEntity.GetContent: WideString;
var
  Child: TBaseWikiNode;

begin
  if Self = nil then
    Exit('');

  Result := FContent.JoinStrings(' ');
  for Child in Children do
    if Child is TTextWikiEntity then
      Result += ' ' + (Child as TTextWikiEntity).GetContent;

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
      Self.FContent.AddAnotherCollection(TextChild.FContent);
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

constructor TTextWikiEntity.Create(constref Text: WideString);
begin
  inherited Create;

  FContent := TWideStringList.Create;
  if Length(Text) <> 0 then
    FContent.Add(Text);

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


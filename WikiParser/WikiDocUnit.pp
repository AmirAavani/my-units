unit WikiDocUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StringUnit, GenericCollectionUnit, WideStringUnit;

type

  { TBaseWikiNode }

  TBaseWikiNode = class(TObject)
  private
    FNext: TBaseWikiNode;
    function GetLastNode: TBaseWikiNode;
  protected
    function _ToXml: AnsiString; virtual;

  public
    property Next: TBaseWikiNode read FNext write FNext;
    property LastNode: TBaseWikiNode read GetLastNode;

    constructor Create;
    function ToXML: AnsiString;
    destructor Destroy; override;

    procedure ExportText(Texts: TWideStringList); virtual;
  end;

  { TNodes }

  TNodes = class(specialize TObjectCollection<TBaseWikiNode>)
  private
    procedure ExportText(Texts: TWideStringList);

    function ToXML: AnsiString;
  end;

  { TTextWikiEntity }

  TTextWikiEntity = class(TBaseWikiNode)
  private
    FContent: WideString;
    function GetContent: WideString;

  protected
    function _ToXml: AnsiString; override;

  public
    property Content: WideString read GetContent;

    constructor Create(constref Text: WideString);

    procedure ExportText(Texts: TWideStringList); override;
  end;

  { TCommentWikiEntry }

  TCommentWikiEntry = class(TTextWikiEntity)
  public
    constructor Create(constref Text: WideString);

    procedure ExportText(Texts: TWideStringList); override;
  end;

  { TSeparatorWikiEntry }

  TSeparatorWikiEntry = class(TTextWikiEntity)
  public
    constructor Create(constref Text: WideString);

    procedure ExportText(Texts: TWideStringList); override;
  end;

  { TTagEntity }

  TTagEntity = class(TBaseWikiNode)
  private
    FParameters: TNodes;
    FTagName: WideString;

  protected
    function _ToXML: AnsiString; override;

  public
    property TagName: WideString read FTagName;
    property Parameters: TNodes read FParameters;

    constructor Create(constref _TagName: WideString; _Parameters: TNodes);

    procedure ExportText(Texts: TWideStringList); override;
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

    procedure ExportText(Texts: TWideStringList); override;

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

    procedure ExportText(Texts: TWideStringList); override;
  end;

  { TTable }

  TTable = class(TBaseWikiNode)
  public
    procedure ExportText(Texts: TWideStringList); override;

  end;

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

    function ExportText: TWideStringList;
  end;


implementation
uses
  ALoggerUnit;

{ TTable }

procedure TTable.ExportText(Texts: TWideStringList);
begin

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

procedure TTemplate.ExportText(Texts: TWideStringList);
begin
  // Skip;

end;

{ TTagEntity }

function TTagEntity._ToXml: AnsiString;
var
  Lines: TStringList;

begin
  Lines := TStringList.Create;
  Lines.Add(Format('<%s>', [ClassName]));
  Lines.Add(Format('  <Name> %s </Name>', [FTagName]));
  Lines.Add(Format('  <Params> %s </Params>', [FParameters.ToXML]));
  Lines.Add(Format('</%s>', [ClassName]));

  Result := Lines.Text;
  Lines.Free

end;

constructor TTagEntity.Create(constref _TagName: WideString; _Parameters: TNodes
  );
begin
  inherited Create;

  FTagName := _TagName;
  FParameters := _Parameters;

end;

procedure TTagEntity.ExportText(Texts: TWideStringList);
begin

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

function TWikiPage.ExportText: TWideStringList;
begin
  Result := TWideStringList.Create;
  Result.Add(Self.Title.Content);
  Content.ExportText(Result);

end;


{ TNodes }

procedure TNodes.ExportText(Texts: TWideStringList);
var
  Node: TBaseWikiNode;

begin
  for Node in Self do
    Node.ExportText(Texts);

end;

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

  inherited Destroy;

end;

procedure THyperLinkEntity.ExportText(Texts: TWideStringList);
begin

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

{ TSeparatorWikiEntry }

constructor TSeparatorWikiEntry.Create(constref Text: WideString);
begin
  inherited Create(Text);

end;

procedure TSeparatorWikiEntry.ExportText(Texts: TWideStringList);
begin

end;

{ TCommentWikiEntry }

constructor TCommentWikiEntry.Create(constref Text: WideString);
begin
  inherited Create(Text);

end;

procedure TCommentWikiEntry.ExportText(Texts: TWideStringList);
begin
  Exit;

end;

{ TTextWikiEntity }

function TTextWikiEntity.GetContent: WideString;
begin
  if Self = nil then
    Exit('');

  Result := FContent;
end;

function TTextWikiEntity._ToXML: AnsiString;
begin
  Result:= Format('(%s -> %s)', [ClassName, WriteAsUTF8(FContent)]);

end;

constructor TTextWikiEntity.Create(constref Text: WideString);
begin
  inherited Create;

  FContent := Text;

end;

procedure TTextWikiEntity.ExportText(Texts: TWideStringList);
begin
  Texts.Add(FContent);

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

function TBaseWikiNode._ToXml: AnsiString;
begin
  FmtFatalLn('%s', [Self.ClassName]);
  Result := '<ClassName/>';

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
    Result += Format('%s', [n._ToXml]);
    n := n.Next;

  end;

end;

destructor TBaseWikiNode.Destroy;
begin
  if Self <> Self.FNext then
  begin
    Self.FNext.Free;

  end;

  inherited Destroy;
end;

procedure TBaseWikiNode.ExportText(Texts: TWideStringList);
begin
  FmtFatalLn('%s', [ClassName]);
end;

end.


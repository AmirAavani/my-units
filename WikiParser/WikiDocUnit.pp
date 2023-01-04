unit WikiDocUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StringUnit, GenericCollectionUnit, WideStringUnit, WikiTypesUnits;

type

  { TBaseWikiNode }

  TBaseWikiNode = class(TObject)
  private
    FNext: TBaseWikiNode;
    FParent: TBaseWikiNode;
    function GetLastNode: TBaseWikiNode;
  protected
    function _ToXml: AnsiString; virtual;
    procedure _ExportText(Texts: TWideStringList); virtual;
    procedure SetNext(NextNode: TBaseWikiNode);

  public
    property Next: TBaseWikiNode read FNext write SetNext;
    property Parent: TBaseWikiNode read FParent write FParent;
    property LastNode: TBaseWikiNode read GetLastNode;

    constructor Create;
    function ToXML: AnsiString;
    destructor Destroy; override;

    procedure ExportText(Texts: TWideStringList);
  end;

  { TNodes }

  TNodes = class(specialize TObjectCollection<TBaseWikiNode>)
  private
    procedure ExportText(Texts: TWideStringList);

    function ToXML: AnsiString;
  end;


  { TBaseWikiNodeWithChildren }

  TBaseWikiNodeWithChildren = class(TBaseWikiNode)
  protected
    FChildren: TNodes;

    function _ToXml: AnsiString; override;
    procedure _ExportText(Texts: TWideStringList); override;
  public
    property Children: TNodes read FChildren;

    constructor Create;
    destructor Destroy; override;

  end;

  { TTextWikiEntity }

  TTextWikiEntity = class(TBaseWikiNodeWithChildren)
  private
    FContent: WideString;
    function GetContent: WideString;

  protected
    function _ToXml: AnsiString; override;
    procedure _ExportText(Texts: TWideStringList); override;

  public
    property Content: WideString read GetContent;

    constructor Create(constref Text: WideString);
    destructor Destroy; override;

  end;

  { TTextStyler }

  TTextStyler = class(TBaseWikiNode)
  private
    FChildren: TNodes;
    FStyle: TStyle;

    function _ToXml: AnsiString; override;
    procedure _ExportText(Texts: TWideStringList); override;

  public
    property Style: TStyle read FStyle;
    property Children: TNodes read FChildren;

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
    procedure _ExportText(Texts: TWideStringList); override;

  public
    constructor Create(constref Text: WideString);

  end;

  { TSeparatorWikiEntry }

  TSeparatorWikiEntry = class(TTextWikiEntity)
  protected
    procedure _ExportText(Texts: TWideStringList); override;
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
    procedure _ExportText(Texts: TWideStringList); override;

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
    FParmas: TNodes;

  protected
    function _ToXml: AnsiString; override;
    procedure _ExportText(Texts: TWideStringList); override;

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

  protected
    function _ToXml: AnsiString; override;
    procedure _ExportText(Texts: TWideStringList); override;

  public
    constructor Create(NameNode: TBaseWikiNode; Params: TNodes);
    destructor Destroy; override;

  end;

  { TTable }

  TTable = class(TBaseWikiNode)
  protected

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

const
  SingleQuote = #$27;

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

procedure TBaseWikiNodeWithChildren._ExportText(Texts: TWideStringList);
var
  Child: TBaseWikiNode;

begin
  for Child in Children do
    Child.ExportText(Texts);

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

procedure TTextStyler._ExportText(Texts: TWideStringList);
var
  Child: TBaseWikiNode;

begin
  for Child in FChildren do
    Child.ExportText(Texts);

end;

constructor TTextStyler.Create(_Style: TStyle);
begin
  inherited Create;

  FChildren := TNodes.Create;
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
  FChildren.Free;

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

procedure TTemplate._ExportText(Texts: TWideStringList);
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
  FMTDebugLn('FTemplateName: %d', [UInt64(FTemplateName)]);
  FTemplateName.Free;
  FMTDebugLn('FParameters:', []);
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

procedure TTagEntity._ExportText(Texts: TWideStringList);
begin
  if Self.TagName <> 'ref' then
    inherited _ExportText(Texts);

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

procedure THyperLinkEntity._ExportText(Texts: TWideStringList);
begin
  //if FLink.ToXML ;
  if FText <> nil then
    FText.ExportText(Texts);

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

procedure TSeparatorWikiEntry._ExportText(Texts: TWideStringList);
begin

end;

{ TCommentWikiEntry }

constructor TCommentWikiEntry.Create(constref Text: WideString);
begin
  inherited Create(Text);

end;

procedure TCommentWikiEntry._ExportText(Texts: TWideStringList);
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

procedure TTextWikiEntity._ExportText(Texts: TWideStringList);
begin
  Texts.Add('{{');
  Texts.Add(FContent);

  inherited _ExportText(Texts);
  Texts.Add('}}');

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

procedure TBaseWikiNode._ExportText(Texts: TWideStringList);
begin
  FmtFatalLn('%s', [Self.ClassName]);

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

procedure TBaseWikiNode.ExportText(Texts: TWideStringList);
var
  n: TBaseWikiNode;
  Count: Integer;
begin
  n := Self;
  Count := 0;

  while n <> nil do
  begin
    Inc(count);
    if Count = 6472 then
    begin
      FMTDebugLn('Texts: %s', [Texts.JoinStrings]);
    end;
    n._ExportText(Texts);

    n := n.Next;
  end;
end;

end.


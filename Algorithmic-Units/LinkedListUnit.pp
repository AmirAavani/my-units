unit LinkedListUnit; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TData= TObject;
  
type

  { TLinkedListNode }

  generic TLinkedListNode<TData> = class(TObject)
  protected
    FData: TData;
    FNext: TLinkedListNode;

  public
    property Data: TData read FData;
    property Next: TLinkedListNode read FNext;
    
    constructor Create(d: TData);
    destructor Destroy; override;
    
    function Add(AData: TData): TLinkedListNode; inline;
    
  end;

  generic TLinkedList<TData> = class(TObject)
  public type
    TNode = specialize TLinkedListNode<TData>;

  private
    function GetRoot: TNode;
    function GetTail: TNode;

  protected
    BeforeRoot: TNode;
    FTail: TNode;

  public
    property Root: TNode read GetRoot;
    property Tail: TNode read GetTail;

    procedure AddData(d: TData); inline;

    constructor Create;
    destructor Destroy; override;

  end;

implementation

{ TLinkedListNode }

{constructor TLinkedListNode.Create;
begin
  inherited Create;
  
  FData := nil;
  FNext := nil;
  
end;
}

constructor TLinkedListNode.Create(d: TData);
begin
  inherited Create;
  
  FData := d;
  FNext := nil;
  
end;

destructor TLinkedListNode.Destroy;
begin
  FData.Free;
  // FNext.Free;
  
  inherited Destroy;
  
end;

function TLinkedListNode.Add(AData: TData): TLinkedListNode;
begin
  if FNext <> nil then
    Result := FNext.Add(AData)
  else
  begin
    FNext := TLinkedListNode.Create(AData);
    Result := FNext;

  end;
    
end;

{ TLinkedList }

function TLinkedList.GetRoot: TNode;
begin
  Result := BeforeRoot.Next;

end;

function TLinkedList.GetTail: TNode;
begin
  if FTail =  BeforeRoot then
    Exit(nil);

  Result := FTail;
end;

procedure TLinkedList.AddData(d: TData);
begin
  FTail.FNext := TNode.Create(d);
  FTail := FTail.Next;

end;

constructor TLinkedList.Create;
begin
  inherited;

  BeforeRoot := TNode.Create(nil);
  FTail := BeforeRoot;
end;

destructor TLinkedList.Destroy;
var
  Node, Next: TNode;

begin
  Node := Root;

  while Node <> nil do
  begin
    Next := Node.Next;
    Node.Free;

    Node := Next;
  end;

  inherited Destroy;
end;

end.


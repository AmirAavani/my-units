unit CompressedSuffixTreeUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  { TCompressedSuffixTree }

  generic TCompressedSuffixTree<TKey, TValue> = class(TObject)

  public type
    TKeys = specialize TList<TKey>;
    TKeyAtFn = function(Data: Pointer; Index: Integer): TKeys;


    { TNode }

    TNode = class(TObject)
    private type
      TNodeMap = specialize TAVLTreeMap<TKey, TObject>;
    private
      FChildren: TNodeMap;
      function GetChild(Key: TKey): TNode;

    public
      property Child[Key: TKey]: TNode read GetChild;

      constructor Create;
      destructor Destroy; override;

      function Insert(const Data: TKeys): TValue;
      function Insert(n: Integer; KeyAtFn: TKeyAtFn): TValue;

    end;

  protected
    Root: TNode;

  public
    constructor Create;
    destructor Destroy; override;


  end;

implementation

{ TCompressedSuffixTree }

constructor TCompressedSuffixTree.Create;
begin
  inherited;

  Root := TNode.Create;

end;

destructor TCompressedSuffixTree.Destroy;
var
  Current: TNode;

begin
  Root.Free;

  inherited Destroy;
end;

{ TCompressedSuffixTree.TNode }

function TCompressedSuffixTree.TNode.GetChild(Key: TKey): TNode;
var
  FindNode: TNodeMap.PNode;

begin
  if Self.FChildren = nil then
    Exit(nil);

  FindNode := Self.FChildren.Find(Key);
  if FindNode = nil then
    Exit(nil);

  Result := FindNode^.Value as TNode;

end;

constructor TCompressedSuffixTree.TNode.Create;
begin
  inherited;

  FChildren := nil;
end;

destructor TCompressedSuffixTree.TNode.Destroy;
begin
  Self.FChildren.Free;

  inherited Destroy;

end;

function TCompressedSuffixTree.TNode.Insert(const Data: TKeys): TValue;
begin

end;

function TCompressedSuffixTree.TNode.Insert(n: Integer; KeyAtFn: TKeyAtFn
  ): TValue;
begin

end;

end.


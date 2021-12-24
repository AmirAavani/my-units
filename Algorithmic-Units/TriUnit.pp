unit TriUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TTrie }

  generic TTrie<TKey, TValue>  = class(TObject)
  private
  protected type
    TKeyArray = array of TKey;
    TTrieNode = class;
    TCharNodeMap = specialize TFPGMap<TKey, TTrieNode>;

    { TTrieNode }

    TTrieNode = class(Tobject)
    private
      Children: TCharNodeMap;
      Value: TValue;

      procedure Add(Key: TKeyArray; Val: TValue; Index: Integer);
      function ValueOrDefault(Key: TKeyArray; DefValue: TValue; Index: Integer): TValue;
    public
      constructor Create;
      destructor Destroy; override;

    end;
  protected
    Root: TTrieNode;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Key: TKeyArray; Value: TValue);
    function ValueOrDefault(Key: TKeyArray; DefaultValue: TValue): TValue;

  end;

implementation

{ TTrie }

constructor TTrie.Create;
begin
  inherited;

  Root := TTrieNode.Create;
end;

destructor TTrie.Destroy;
begin
  Root.Free;

  inherited Destroy;
end;

procedure TTrie.Add(Key: TKeyArray; Value: TValue);
begin
  Root.Add(Key, Value, 0);

end;

function TTrie.ValueOrDefault(Key: TKeyArray; DefaultValue: TValue): TValue;
begin
  Result := Root.ValueOrDefault(Key, DefaultValue, 0);

end;

{ TTrie.TTrieNode }

procedure TTrie.TTrieNode.Add(Key: TKeyArray; Val: TValue; Index: Integer);
var
  Child: TTrieNode;

begin
  if Index = Length(Key) then
  begin
    Self.Value := Val;
    Exit;
  end;

  if Self.Children.TryGetData(Key[Index], Child) then
    Child.Add(Key, Val, Index + 1)
  else
  begin
    Child := TTrie.TTrieNode.Create;
    Self.Children.Add(Key[Index], Child);
    Child.Add(Key, Val, Index + 1);
  end;
end;

function TTrie.TTrieNode.ValueOrDefault(Key: TKeyArray; DefValue: TValue;
  Index: Integer): TValue;
var
  Child: TTrieNode;

begin
  if Index = Length(Key) then
    Exit(Self.Value);

  if Self.Children.TryGetData(Key[Index], Child) then
    Result := Child.ValueOrDefault(Key, DefValue, Index + 1)
  else
    Result := DefValue;
end;

constructor TTrie.TTrieNode.Create;
begin
  inherited Create;

  Children := TCharNodeMap.Create;

end;

destructor TTrie.TTrieNode.Destroy;
var
  i: Integer;
  Key: TKey;

begin
  for i := 0 to Children.Count - 1 do
  begin
    Key := Children.Keys[i];
    Children[Key].Free;

  end;
  Children.Free;

  inherited Destroy;
end;

end.


unit SegmentTree;
interface
uses
  fgl, Math, SysUtils;

type
  TIntList = specialize TFPGList<Integer>;

  TNode = class(TObject)
  protected
    FLeftMostIndex, FRightMostIndex: Integer;
    FLeftNode, FRightNode: TNode;
    function GetValue: Int64; virtual; abstract;

  public
    property LeftNode: TNode read FLeftNode;
    property RightNode: TNode read FRightNode;

    constructor Create;
    destructor Destroy; override;

    function GetLeftMostIndex: Integer;
    function GetRightMostIndex: Integer;

    function GetMax(const l, r: Integer): Int64;
  end;

  TLeafNode = class(TNode)
  private
    FValue: Int64;
    FIndex: Integer;
  protected
    function GetValue: Int64; override;

  public
    property Index: Integer read FIndex;

    constructor Create(const Ind: Integer; const Val: Int64);
    destructor Destroy; override;

  end;

  TNonLeafNode = class(TNode)
  private
    FValue: Int64;

  protected
    function GetValue: Int64; override;

  public

    constructor Create(const Left, Right: TNode);
    destructor Destroy; override;

  end;

implementation

constructor TNode.Create;
begin
  inherited;

  FLeftNode := nil;
  FRightNode := nil;

end;

destructor TNode.Destroy;
begin
  FLeftNode.Free;
  FRightNode.Free;

  inherited;

end;

function TNode.GetLeftMostIndex: Integer; 
begin
  Result := FLeftMostIndex;
end;

function TNode.GetRightMostIndex: Integer; 
begin
  Result := FRightMostIndex;
end;

function TNode.GetMax(const l, r: Integer): Int64;
begin
  Assert((GetLeftMostIndex <= l) and (r <= GetRightMostIndex), 'l:' + IntToStr(l) + ' r: ' + IntToStr(r) + ' Left:' + IntToStr(GetLeftMostIndex) + ' right:' + IntToStr(GetRightMostIndex));

  //WriteLn('1) l:', l, ' r:', r, ' L:', GetLeftMostIndex, ' R:', GetRightMostIndex);
  if (l = GetLeftMostIndex) and (r = GetRightMostIndex) then
    Exit(GetValue);

  //WriteLn('2) l:', l, ' r:', r, ' L:', GetLeftMostIndex, ' R:', GetRightMostIndex);
  if r <= FLeftNode.GetRightMostIndex then
    Exit(LeftNode.GetMax(l, r));
  //WriteLn('3) l:', l, ' r:', r, ' L:', GetLeftMostIndex, ' R:', GetRightMostIndex);
  if RightNode.GetLeftMostIndex <= l then
    Exit(RightNode.GetMax(l, r));
  //WriteLn('4) l:', l, ' r:', r, ' L:', GetLeftMostIndex, ' R:', GetRightMostIndex);
  Result := Max(LeftNode.GetMax(l, LeftNode.GetRightMostIndex), 
                RightNode.GetMax(RightNode.GetLeftMostIndex, r));
end;

constructor TLeafNode.Create(const Ind: Integer; const Val: Int64);
begin
  inherited Create;

  FIndex := Ind;
  FValue := Val;
  FLeftMostIndex := Ind;
  FRightMostIndex := Ind;
end;

destructor TLeafNode.Destroy;
begin
  inherited;

end;

function TLeafNode.GetValue: Int64; 
begin
  Result := FValue;
end;

constructor TNonLeafNode.Create(const Left, Right: TNode);
begin
  inherited Create;

  FLeftNode := Left;
  FRightNode := Right;
  FLeftMostIndex := Left.GetLeftMostIndex;
  FRightMostIndex := Right.GetRightMostIndex;
  FValue := -1;

end;

destructor TNonLeafNode.Destroy;
begin
  LeftNode.Free;
  RightNode.Free;

  inherited;
end;

function TNonLeafNode.GetValue: Int64;
begin
  if FValue <> -1 then
    Exit(FValue);

  FValue := Max(LeftNode.GetValue, RightNode.GetValue);
  Result := FValue;
end;

function BuildSegmentTree(const A: TIntList): TNode;

  function RecBuild(Left, Right: Integer): TNode;
  var
    Mid: Integer;
    LNode, RNode: TNode;
  begin
    if Left = Right then
      Exit(TLeafNode.Create(Left, A[Left]));

    Mid := (Left + Right) div 2;
    LNode := RecBuild(Left, Mid);
    RNode := RecBuild(Mid + 1, Right);

    Result := TNonLeafNode.Create(LNode, RNode);
  end;

begin
  Result := RecBuild(1, A.Count - 1);

end;

procedure Test;
var
  Ai: TIntList;
  x: Int64;
  Root: TNode;
  i, j, k, N: Integer;
  Ans: Int64;
begin
  Ai := TIntList.Create;
  Ai.Add(-1);
  N := 100;
  for i := 1 to N do
  begin
 x := Trunc(Random * 100000) + 1;
    Ai.Add(x);
  end;

  Root := BuildSegmentTree(Ai);
  for i := 1 to N do
    for j := i to N do
    begin
      Ans := Root.GetMax(i, j);
      //WriteLn(i, ' ', j, ' -> ', Ans);
      x := -1;
      for k := i to j do
      begin
        if Ans < Ai[k] then
        begin
          WriteLN('Error in ', i, ' to ', j, ' k =', k, ' ', 'Ai[k] = ', Ai[k]);
          Exit;
        end;
        x := Max(Ai[k], x);
      end;
      if x <> Ans then
      begin
        WriteLN('Error in ', i, ' to ', j, ' Max = ', x, ' Ans =', Ans);
        Exit;
      end;
    end;
end;

begin
  //Test;
end.

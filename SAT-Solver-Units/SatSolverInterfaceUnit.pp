unit SatSolverInterfaceUnit;

{$mode objfpc}{$H+}
{$Assertions on}
interface

uses
  Classes, SysUtils, GenericStackUnit, ClauseUnit, MyTypes;

type
  TArrayofInteger = array[0..2] of Integer;
  TVariablePolarity = (vpFalse= 0, vpTrue, vpNone);

  TSolverResult = (srError, srSAT, srUNSAT);

  TSatSolverType = (ssMiniSatSolver, ssCNFCollection);

  { TSATSolverInterface }

  TSATSolverInterface = class (TObject)
  private
    type
      TNoOfLiteralsInTopConstraint = record
        FalseCount: Integer;
        TrueCount: Integer;
      end;
      TClauseNoOfLiteralsPair = specialize TPairForBuiltInData<TClause, TNoOfLiteralsInTopConstraint>;
      TStackOfClauses = specialize TGenericStack<TClauseNoOfLiteralsPair>;
  private
    FClausesStack: TStackOfClauses;
    _FTopConstraint: TClause;
    FNoOfLiteralInTopConstraint: TNoOfLiteralsInTopConstraint;

    function GetNoOfLiteralInTopConstraint(gbValue: TGroundBool): Integer; inline;
    function GetTopConstarintSize: Integer; inline;
    function GetTopConstraint: TClause;

  protected
    FVarCount: Int64;
    FClauseCount: Int64;
    FSolverResult: TSolverResult;
    property Stack: TStackOfClauses read FClausesStack;


    function GetVarCount: Int64; virtual; 
    function GetClauseCount: Int64; virtual;
    function GetCNF: TClauseCollection; virtual; 

    procedure SyncInteractiveUPInfo; virtual;
    procedure SubmitContradiction; virtual;

  public
    property VarCount: Int64 read GetVarCount;
    property ClauseCount: Int64 read GetClauseCount;
    property TopConstraint: TClause read GetTopConstraint;// FTopConstraint;
    property TopConstarintSize: Integer read GetTopConstarintSize;
    property NoOfLiteralInTopConstraint[gbValue: TGroundBool]: Integer read GetNoOfLiteralInTopConstraint;
    property CNF: TClauseCollection read GetCNF;

    procedure AddComment(const Comment: AnsiString); virtual;
    // for debug... should be removed!!
    function CStack: TStackOfClauses;
    function GenerateNewVariable(VariablePolrity: TVariablePolarity; Decide: Boolean): Integer; virtual; abstract;
    function GetValue(v: Integer): TGroundBool; virtual;

    function BeginConstraint: TClause; //inline;
    procedure AbortConstraint; virtual; 
    procedure AddLiteral(Lit: TLiteral); virtual;
    procedure AddLiterals(Lits: array of const); virtual;
    procedure AddClause(AClause: TClause); inline;

    procedure SubmitExactlyOne(Literalts: array of TLiteral); virtual;
    procedure SubmitAndGate(p: TLiteral); virtual;
    procedure SubmitOrGate(p: TLiteral); virtual;
    procedure SubmitXOrGate(p: TLiteral); virtual;
    procedure SubmitITEGate(p: TLiteral); virtual;
    procedure SubmitEquivGate(p: TLiteral); virtual;
    procedure SubmitClause; virtual;

    function GenerateAndGate: TLiteral; virtual;
    function GenerateOrGate: TLiteral; virtual;
    function GenerateXOrGate: TLiteral; virtual;
    function GenerateITEGate: TLiteral; virtual;
    function GenerateEquiGate: TLiteral; virtual;

    function Solve: Boolean; virtual; abstract;
    function Solve(Literal: TLiteral): Boolean; virtual; abstract;
    procedure GetSolution(out Answer: AnsiString); virtual; abstract;
    function GetResult: TSolverResult;

    function GetLiteralValue(Lit: TLiteral): TGroundBool; inline;
    function GetValueInModel(v: Integer): TGroundBool; virtual;
    function GetLiteralValueInModel(Lit: TLiteral): TGroundBool; inline;
    procedure ImportModel(HighIndex: Integer; Model: TIntegerCollection); virtual;

    constructor Create;
    destructor Destroy; override;

    procedure ReportForcedVariables; virtual;

    procedure SetDecisionVar(Variable: Integer; SetFlag: Boolean); virtual; abstract;

  end;

function GetSatSolver: TSATSolverInterface; inline;
function ReNewSatSolver(const SatSolverType: AnsiString): TSATSolverInterface;
function PopBackSatSolver: TSATSolverInterface;
procedure Initialize;
procedure Finalize;

implementation
uses
  TSeitinVariableUnit,  MiniSatSolverInterfaceUnit, CNFCollectionUnit,
  ParameterManagerUnit, CNFStreamUnit;

type
  TSatSolverStack = specialize TGenericStack<TSATSolverInterface>;

var
  SatSolverStack: TSatSolverStack;
  SatSolver: TSATSolverInterface;

function GetSatSolver: TSATSolverInterface; inline;
begin
  Result := SatSolver;

end;

function ReNewSatSolver(const SatSolverType: AnsiString): TSATSolverInterface; inline;
begin
  SatSolverStack.Push(SatSolver);

  if UpperCase(SatSolverType)= UpperCase('CNFCollection') then
    SatSolver:= TCNFCollection.Create
  else if UpperCase(SatSolverType)= UpperCase('CNFStream') then
    SatSolver:= TCNFStream.Create(GetRunTimeParameterManager.GetValueByName('--OutputFilename'))
  else if UpperCase(SatSolverType)= UpperCase('InternalMiniSAT') then
    SatSolver:= TMiniSatSolverInterface.Create
  else
  begin
    WriteLn('Invalid SatSolveType!');
    Halt(1);

{  if GetRunTimeParameterManager.SATSolverType= ssMiniSatSolver then
     SatSolverInterface:= TMiniSatSolverInterface.Create
  else if GetRunTimeParameterManager.SATSolverType= ssCNFCollection then
      SatSolverInterface:= TCNFCollection.Create;
}
  end;

  Result:= SatSolver;

  Result.BeginConstraint;
  Result.AddLiteral(GetVariableManager.TrueLiteral);
  Result.SubmitClause;

end;

function PopBackSatSolver: TSATSolverInterface;
begin
  SatSolver.Free;

  SatSolver := SatSolverStack.Pop;

  Result := SatSolver;
end;

procedure Initialize;
begin
  if UpperCase(GetRunTimeParameterManager.GetValueByName('--SatSolverType'))= UpperCase('CNFCollection') then
  begin
    SatSolver:= TCNFCollection.Create;
   (SatSolver as TCNFCollection).OutputFilename := GetRunTimeParameterManager.ValueByName['--OutputFileName'];
  end
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--SatSolverType'))= UpperCase('CNFStream') then
    SatSolver:= TCNFStream.Create(GetRunTimeParameterManager.GetValueByName('--OutputFilename'))
  else if UpperCase(GetRunTimeParameterManager.GetValueByName('--SatSolverType'))= UpperCase('InternalMiniSAT') then
    SatSolver:= TMiniSatSolverInterface.Create
  else
  begin
    WriteLn('Invalid SatSolveType!');
    Halt(1);

{  if GetRunTimeParameterManager.SATSolverType= ssMiniSatSolver then
     SatSolverInterface:= TMiniSatSolverInterface.Create
  else if GetRunTimeParameterManager.SATSolverType= ssCNFCollection then
      SatSolverInterface:= TCNFCollection.Create;
}
  end;

end;

{ TSATSolverInterface }


procedure TSATSolverInterface.SyncInteractiveUPInfo;
begin
  raise Exception.Create('SyncInteractiveUPInfo');
 
end;

procedure TSATSolverInterface.SubmitContradiction;
begin
  BeginConstraint;
  AddLiteral(GetVariableManager.FalseLiteral);
  SubmitClause;

end;

function TSATSolverInterface.GetValue(v: Integer): TGroundBool;
begin
  Exit(gbUnknown);

end;

function TSATSolverInterface.GetLiteralValue(Lit: TLiteral): TGroundBool; inline;
begin
  if IsNegated(Lit) then
    Exit(TGroundBool(2- Ord(GetValue(GetVar(Lit)))))
  else
    Exit(GetValue(GetVar(Lit)));

end;

function TSATSolverInterface.GetValueInModel(v: Integer): TGroundBool;
begin
  raise Exception.Create('GetValueInModel');
  Exit(gbUnknown);

end;

function TSATSolverInterface.GetLiteralValueInModel(Lit: TLiteral): TGroundBool;
begin
  if IsNegated(Lit) then
    Exit(TGroundBool(2- Ord(GetValueInModel(GetVar(Lit)))))
  else
    Exit(GetValueInModel(GetVar(Lit)));


end;

procedure TSATSolverInterface.ImportModel(HighIndex: Integer; Model: TIntegerCollection);
var
  v: Integer;

begin
  Model.Count:= HighIndex+ 1;

  for v:= 0 to HighIndex do
    if GetValueInModel(v)= gbTrue then
      Model.Item[v]:= 1
    else
      Model.Item[v]:= -1;

end;

function TSATSolverInterface.GetNoOfLiteralInTopConstraint(gbValue: TGroundBool): Integer;
begin
  case gbValue of
    gbTrue: Exit(FNoOfLiteralInTopConstraint.TrueCount);
    gbFalse: Exit(FNoOfLiteralInTopConstraint.FalseCount);
    gbUnknown: Exit(TopConstraint.Count -
       FNoOfLiteralInTopConstraint.FalseCount -
       FNoOfLiteralInTopConstraint.TrueCount);
  end;

end;

function TSATSolverInterface.GetTopConstarintSize: Integer;
begin
  Result:= TopConstraint.Count;

end;

function TSATSolverInterface.GetTopConstraint: TClause;
begin
  assert(_FTopConstraint <> nil);
  Result := _FTopConstraint;
end;

function TSATSolverInterface.GetVarCount: Int64;
begin
  Result:= FVarCount;

end;

function TSATSolverInterface.GetClauseCount: Int64;
begin
  Result:= FClauseCount;

end;

function TSATSolverInterface.GetCNF: TClauseCollection;
begin
  Result:= nil;
  raise Exception.Create('GetCNF is not implemented here');

end;

procedure TSATSolverInterface.SubmitClause;
begin
  Inc(FClauseCount, 1);

  AbortConstraint;
  Inc(FClauseCount, 1);

end;

function TSATSolverInterface.GenerateAndGate: TLiteral;
var
  i: Integer;
  ActiveClause: TClause;

begin
  if 0 < NoOfLiteralInTopConstraint[gbFalse] then
  begin
    Result := GetVariableManager.FalseLiteral;
    AbortConstraint;
  end
  else if NoOfLiteralInTopConstraint[gbTrue] = TopConstraint.Count then
  begin
    Result := GetVariableManager.TrueLiteral;
    AbortConstraint;
  end
  else
  begin
    if TopConstraint.HasDuplicate then
    begin
      ActiveClause := TopConstraint;
      BeginConstraint;
      for i := 0 to ActiveClause.Count - 1 do
        if not TopConstraint.Exists(ActiveClause[i]) then
          AddLiteral(ActiveClause[i]);
      Result := Self.GenerateAndGate;
      AbortConstraint;
    end
    else if TopConstraint.Count = 0 then
    begin
      Result := GetVariableManager.TrueLiteral;
      AbortConstraint;
    end
    else if TopConstraint.Count = 1 then
    begin
      Result := TopConstraint[0];
      AbortConstraint;
    end
    else
    begin
      if (NoOfLiteralInTopConstraint[gbTrue] = 0) and
         (NoOfLiteralInTopConstraint[gbFalse] = 0) then
      begin
        Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
        Self.SubmitAndGate(Result);
      end
      else
      begin
        ActiveClause := TopConstraint;
        BeginConstraint;
        for i := 0 to ActiveClause.Count - 1 do
          if GetLiteralValue(ActiveClause[i]) = gbUnknown then
            AddLiteral(ActiveClause[i])
          else
            assert(GetLiteralValue(ActiveClause[i]) = gbTrue);

        Result := Self.GenerateAndGate;
        AbortConstraint;
      end;
    end;
  end;
end;

function TSATSolverInterface.GenerateOrGate: TLiteral;
var
  ActiveClause: TClause;
  i: Integer;

begin
  if 0 < NoOfLiteralInTopConstraint[gbTrue] then
  begin
    Result := GetVariableManager.TrueLiteral;
    AbortConstraint;
  end
  else if NoOfLiteralInTopConstraint[gbFalse] = TopConstraint.Count then
  begin
    Result := GetVariableManager.FalseLiteral;
    AbortConstraint;
  end
  else
  begin
    if TopConstraint.HasDuplicate then
    begin
      ActiveClause := TopConstraint;
      BeginConstraint;
      for i := 0 to ActiveClause.Count - 1 do
        if not TopConstraint.Exists(ActiveClause[i]) then
          AddLiteral(ActiveClause[i]);
      Result := Self.GenerateOrGate;
      AbortConstraint;
    end
    else if TopConstraint.Count = 0 then
    begin
      Result := GetVariableManager.TrueLiteral;
      AbortConstraint;
    end
    else if TopConstraint.Count = 1 then
    begin
      Result := TopConstraint[0];
      AbortConstraint;
    end
    else
    begin
      if (NoOfLiteralInTopConstraint[gbTrue] = 0) and
         (NoOfLiteralInTopConstraint[gbFalse] = 0) then
      begin
        Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
        Self.SubmitOrGate(Result);
      end
      else
      begin
        ActiveClause := TopConstraint;
        BeginConstraint;
        for i := 0 to ActiveClause.Count - 1 do
          if GetLiteralValue(ActiveClause[i]) = gbUnknown then
            AddLiteral(ActiveClause[i])
          else
            assert(GetLiteralValue(ActiveClause[i]) = gbFalse);

        Result := Self.GenerateOrGate;
        AbortConstraint;
      end;
    end;
  end;
end;

function TSATSolverInterface.GenerateXOrGate: TLiteral;
var
  ActiveClause: TClause;
  TrueCount: Integer;
  i: Integer;
  val: TGroundBool;
  HasValue: Boolean;
begin
  if TopConstraint.Count = 0 then
  begin
    Result := GetVariableManager.FalseLiteral;
    AbortConstraint;
  end
  else if TopConstraint.Count = 1 then
  begin
    Result := TopConstraint[0];
    AbortConstraint;
  end
  else if TopConstraint.Count= 2 then
  begin
    HasValue := True;
    if GetLiteralValue(TopConstraint.Items[0])= gbFalse then// False xor x => x
      Result := TopConstraint.Items[1]
    else if GetLiteralValue(TopConstraint.Items[0])= gbTrue then// True xor x => ~x
      Result := NegateLiteral(TopConstraint.Items[1])
    else if GetLiteralValue(TopConstraint.Items[1])= gbFalse then// False xor x => x
      Result := TopConstraint.Items[0]
    else if GetLiteralValue(TopConstraint.Items[1])= gbTrue then// True xor x => ~x
      Result := NegateLiteral(TopConstraint.Items[0])
    else
    begin
      HasValue := False;
      Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
      SubmitXOrGate(Result);
    end;

    if HasValue then
      AbortConstraint;
  end
  else
  begin
    ActiveClause := TopConstraint;
    TrueCount := NoOfLiteralInTopConstraint[gbTrue];

    BeginConstraint;
    HasValue := False;
    for i := 0 to ActiveClause.Count - 1 do
    begin
      val := GetLiteralValue(ActiveClause[i]);
      if val = gbUnknown then
        AddLiteral(ActiveClause[i])
      else
        HasValue:= True;
    end;

    if HasValue then
    begin
      Result:= Self.GenerateXOrGate;
      AbortConstraint;

      if Odd(TrueCount) then
        Result := NegateLiteral(Result);
    end
    else
    begin
      Result:= CreateLiteral(GetVariableManager.CreateNewVariable, False);
      SubmitXOrGate(Result);
      AbortConstraint;

      if Odd(TrueCount) then
        Result := NegateLiteral(Result);
    end;

  end;

end;

function TSATSolverInterface.GenerateITEGate: TLiteral;
var
  p, t, e: TLiteral;

begin
  Assert(TopConstraint.Count = 3);

  p := TopConstraint[0];
  t := TopConstraint[1];
  e := TopConstraint[2];

  if GetValue(p) = gbTrue then
    Exit(t)
  else if GetValue(p) = gbFalse then
    Exit(e)
  else
  begin
    Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);
    SubmitITEGate(Result);
  end;

end;

function TSATSolverInterface.GenerateEquiGate: TLiteral;
begin
  Result := CreateLiteral(GetVariableManager.CreateNewVariable, False);

  SubmitEquivGate(Result);
end;

procedure TSATSolverInterface.AddComment(const Comment: AnsiString);
begin

end;

function TSATSolverInterface.CStack: TStackOfClauses;
begin
  Result := FClausesStack;
end;

function TSATSolverInterface.BeginConstraint: TClause;
var
  Pair: TClauseNoOfLiteralsPair;

begin
  _FTopConstraint:= TClause.Create;//(30);
  FillChar(FNoOfLiteralInTopConstraint, SizeOf(FNoOfLiteralInTopConstraint), 0);
  Pair:= TClauseNoOfLiteralsPair.Create(_FTopConstraint, FNoOfLiteralInTopConstraint);
  Stack.Push(Pair);
  Result:= _FTopConstraint;

end;

procedure TSATSolverInterface.AbortConstraint;
var
  Pair: TClauseNoOfLiteralsPair;

begin
  if 0 < Stack.Count then
  begin
    Pair:= Stack.Pop;

    Pair.First.Free;
    _FTopConstraint := nil;
    Pair.Free;

  end
  else
    raise Exception.Create('Stack is empty!');

  if 0 < Stack.Count then
  begin
    Pair:= Stack.Top;
    _FTopConstraint:= Pair.First;
    FNoOfLiteralInTopConstraint:= Pair.Second;
  end;

  Dec(FClauseCount);

end;

procedure TSATSolverInterface.AddLiteral(Lit: TLiteral);
var
  v: Integer;
  LiteralValue: TGroundBool;

begin
  v := GetVar(Lit);

  while VarCount <= v do
    GenerateNewVariable(vpNone, True);

  TopConstraint.PushBack(Lit);

  LiteralValue := GetLiteralValue(Lit);

  case LiteralValue of
    gbTrue: Inc(FNoOfLiteralInTopConstraint.TrueCount);
    gbFalse: Inc(FNoOfLiteralInTopConstraint.FalseCount);
  end;

end;

procedure TSATSolverInterface.AddLiterals(Lits: array of const);
var
  i: Integer;
  l: TLiteral;
begin
  for i := 0 to High(Lits) do
  begin
    l := Lits[i].VInteger;
    AddLiteral(l);
  end;
end;

procedure TSATSolverInterface.AddClause(AClause: TClause); inline;
var
  i: Integer;

begin
  BeginConstraint;

  for i:= 0 to AClause.Count- 1 do
    AddLiteral(AClause.Items[i]);

  SubmitClause;

end;

procedure TSATSolverInterface.SubmitExactlyOne(Literalts: array of TLiteral);
var
  i, j: Integer;
begin
  BeginConstraint;

  for i := 0 to High(Literalts) do
  begin
    for j := i + 1 to High(Literalts) do
    begin
      BeginConstraint;
      AddLiteral(NegateLiteral(Literalts[i]));
      AddLiteral(NegateLiteral(Literalts[j]));
      SubmitClause;         //li -> ~lj
    end;
    AddLiteral(Literalts[i]);
  end;

  SubmitOrGate(GetVariableManager.TrueLiteral);

end;

procedure TSATSolverInterface.SubmitAndGate(p: TLiteral);
var
  pV: TGroundBool;
  i: Integer;
  ActiveClause: TClause;
  lit: TLiteral;

begin
//  AddComment('And ' + TopConstraint.ToString + ' = ' + LiteralToString(p));

  pV := GetLiteralValue(p);
  ActiveClause := TopConstraint;

  case Pv of
   gbFalse:
   begin
     if NoOfLiteralInTopConstraint[gbTrue] = TopConstraint.Count then // Contradictoin
     begin
       WriteLn('[SubmitAndGate]: UNchecked Path 0');
       SubmitContradiction;
       AbortConstraint;
       Exit;
     end
     else if NoOfLiteralInTopConstraint[gbFalse] <> 0 then
     begin
       WriteLn('[SubmitAndGate]: UNchecked Path 1');
       AbortConstraint;
       Exit;
     end;

     WriteLn('[SubmitAndGate]: UNchecked Path 2');
   //\lnot l_1\lor \lnor l_2 \lor \cdots \lor \lnot l_n
     BeginConstraint;
     for i:= 0 to ActiveClause.Count - 1 do
       AddLiteral(NegateLiteral(ActiveClause.Items[i]));
     SubmitClause;
   end;
   gbTrue:
   begin
     if NoOfLiteralInTopConstraint[gbTrue] = TopConstraint.Count then
     begin
       WriteLn('[SubmitAndGate]: UNchecked Path 3');
       AbortConstraint;
       Exit
     end
     else if NoOfLiteralInTopConstraint[gbFalse] <> 0 then
     begin
       WriteLn('[SubmitAndGate]: UNchecked Path 4');
       SubmitContradiction;
       AbortConstraint;
       Exit;
     end;

   //l_1\land l_2 \land \cdots \land \l_n
     for i:= 0 to ActiveClause.Count - 1 do
     begin
       BeginConstraint;
       AddLiteral(ActiveClause.Items[i]);
       SubmitClause;
     end;
     AbortConstraint;

   end;
   gbUnknown:
   begin
     if NoOfLiteralInTopConstraint[gbTrue] = TopConstraint.Count then
     begin
       WriteLn('[SubmitAndGate]: UNchecked Path 6');
       BeginConstraint;
       AddLiteral(p);
       SubmitClause;
     end
     else if NoOfLiteralInTopConstraint[gbFalse] <> 0 then
     begin
       WriteLn('[SubmitAndGate]: UNchecked Path 7');
       BeginConstraint;
       AddLiteral(NegateLiteral(p));
       SubmitClause;
     end
     else
     begin
       BeginConstraint;
       for i := 0 to ActiveClause.Count - 1 do
       begin
         BeginConstraint;

         AddLiteral(ActiveClause.Items[i]);
         AddLiteral(NegateLiteral(p));
         SubmitClause;

         lit  := ActiveClause.Item[i];
         AddLiteral(NegateLiteral(lit));

       end;
       AddLiteral(p);
       SubmitClause;

     end;

     AbortConstraint;
   end;

  end;

end;

function CompareLiteral(P1, P2: Pointer): Integer;
begin
  Exit(Integer(P1)- Integer(P2));

end;

procedure TSATSolverInterface.SubmitOrGate(p: TLiteral);
var
  pV: TGroundBool;
  i: Integer;
  ActiveClause: TClause;

begin
//  AddComment('Or ' + TopConstraint.ToString + ' = ' + LiteralToString(p));

// p <=> l_1 \lor l_2 \lor \cdots l_n;

  pv := GetValue(GetVar(p));
  ActiveClause := TopConstraint;
//  ActiveClause.Sort(@CompareLiteral);

  case Pv of
   gbFalse:
   begin
     if 0 < NoOfLiteralInTopConstraint[gbTrue] then//Contradiction
     begin
       WriteLn('[SubmitOrGate]: UNchecked Path 0');
       BeginConstraint;
       AddLiteral(p);
       SubmitClause;
       AbortConstraint;

       Exit;

     end;

     WriteLn('[SubmitOrGate]: UNchecked Path 1');
    //\lnot l_1\land \lnot l_2 \land \cdots \land \lnot \l_n
     BeginConstraint;
     for i := 0 to ActiveClause.Count- 1 do
       AddLiteral(NegateLiteral(ActiveClause.Items[i]));
     SubmitClause;

     AbortConstraint;
   end;

   gbTrue:
   begin
     if ActiveClause.Count = NoOfLiteralInTopConstraint[gbFalse] then//Contradiction
     begin
       WriteLn('[SubmitOrGate]: UNchecked Path 2');
       BeginConstraint;
       AddLiteral(NegateLiteral(p));
       SubmitClause;
       AbortConstraint;

       Exit;

     end;

     WriteLn('[SubmitOrGate]: UNchecked Path 3');
   //l_1\lor l_2 \lor \cdots \lor l_n
     SubmitClause;

   end;

   gbUnknown:
   begin
     if 0 < NoOfLiteralInTopConstraint[gbTrue] then
     begin
       WriteLn('[SubmitOrGate]: UNchecked Path 4');

       BeginConstraint;
       AddLiteral(p);
       SubmitClause;
       AbortConstraint;
       Exit;

     end;

     if ActiveClause.Count = NoOfLiteralInTopConstraint[gbFalse] then
     begin
       WriteLn('[SubmitOrGate]: UNchecked Path 5');
       BeginConstraint;
       AddLiteral(NegateLiteral(p));
       SubmitClause;
       AbortConstraint;
       Exit;

     end;

     BeginConstraint;
     for i  := 0 to ActiveClause.Count- 1 do
     begin
       BeginConstraint;
       AddLiteral(NegateLiteral(ActiveClause.Items[i]));
       AddLiteral(p);
       SubmitClause;

       AddLiteral(ActiveClause.Items[i]);
//       ActiveClause.Item[i]  := ActiveClause.Item[i];

     end;

     AddLiteral(NegateLiteral(p));
     SubmitClause;
     AbortConstraint;

   end;

  end;

end;

procedure TSATSolverInterface.SubmitXOrGate(p: TLiteral);
var
  i, j: Integer;
  ActiveClause: TClause;
  Count: Integer;

begin
//  AddComment('Xor ' + TopConstraint.ToString + ' = ' + LiteralToString(p));

// p <=> l_1 \lxor l_2 \lxor ... ln;
  {
  ~l1, l2, p
  l1, ~l2, p
  ~l1,~l2, ~p
  l1, l2, ~p
  }

  ActiveClause := TopConstraint;

  for i := 0 to(1 shl ActiveClause.Count)- 1 do
  begin

    BeginConstraint;
    Count := 0;
    for j := 0 to ActiveClause.Count- 1 do
      if  (i and(1 shl j))= 0 then
      begin
        Inc(Count);
        AddLiteral(NegateLiteral(ActiveClause.Items[j]));
      end
      else
        AddLiteral(ActiveClause.Items[j]);

    if Count mod 2 = 1 then
      AddLiteral(p)
    else
      AddLiteral(NegateLiteral(p));

    SubmitClause;

  end;

  AbortConstraint;
end;

procedure TSATSolverInterface.SubmitITEGate(p: TLiteral);
var
  ActiveClause: TClause;
  s, t, f: TLiteral;

begin
//  AddComment('ITE ' + TopConstraint.ToString + ' = ' + LiteralToString(p));

// p <=>(s \land t) \lor(\lnot s \land f);
  {
  ~s, ~t, p
  ~s, t, ~p
  s,~f, p
  s, f, ~p
  ~t, ~f, p
  t, f, ~p
  }

  ActiveClause := TopConstraint;
  Assert(ActiveClause.Count= 3);

//  ActiveClause.Sort(@CompareLiteral);

  s := ActiveClause.Items[0];
  t := ActiveClause.Items[1];
  f := ActiveClause.Items[2];

  BeginConstraint;
  AddLiteral(NegateLiteral(s));
  AddLiteral(NegateLiteral(t));
  AddLiteral(p);
  SubmitClause;

  BeginConstraint;
  AddLiteral(NegateLiteral(s));
  AddLiteral(t);
  AddLiteral(NegateLiteral(p));
  SubmitClause;

  BeginConstraint;
  AddLiteral(NegateLiteral(t));
  AddLiteral(NegateLiteral(f));
  AddLiteral(p);
  SubmitClause;

  BeginConstraint;
  AddLiteral(t);
  AddLiteral(f);
  AddLiteral(NegateLiteral(p));
  SubmitClause;

  BeginConstraint;
  AddLiteral(s);
  AddLiteral(NegateLiteral(f));
  AddLiteral(p);
  SubmitClause;

  BeginConstraint;
  AddLiteral(s);
  AddLiteral(f);
  AddLiteral(NegateLiteral(p));
  SubmitClause;

  AbortConstraint;

end;

procedure TSATSolverInterface.SubmitEquivGate(p: TLiteral);
var
  i: Integer;
  a, b: TLiteral;

begin
//  AddComment('Eq ' + TopConstraint.ToString + ' = ' + LiteralToString(p));

  Assert(TopConstraint.Count = 2);
  {p <=> a <-> b
  1) a \land b -> p    <-> ~a, ~b, p
  2) ~a \land b -> ~p  <->  a, ~b, ~p
  3) a \land ~b -> ~p  <-> ~a, b, ~p
  4) ~a \land ~b -> p  <-> a, b, p
  }
  a := TopConstraint.Items[0];
  b := TopConstraint.Items[1];

  for i := 0 to 3 do
  begin
    BeginConstraint;
    if (i and 1) = 0 then
      AddLiteral(NegateLiteral(a))
    else
      AddLiteral(a);
    if (i and 2) = 0 then
      AddLiteral(NegateLiteral(b))
    else
      AddLiteral(b);

    if i in [0, 3] then
      AddLiteral(p)
    else
      AddLiteral(NegateLiteral(p));

    SubmitClause;
  end;

  AbortConstraint;

end;

{
procedure TSATSolverInterface.SubmitFACarryGate(p: TLiteral);
var
  i, j: Integer;
  ActiveClause: TClause;

begin
  ActiveClause := TopConstraint;
  Assert(ActiveClause.Count= 3);

  for i := 0 to ActiveClause.Count - 1 do
    for j := i+ 1 to ActiveClause.Count - 1 do
    begin
      BeginConstraint;

      AddLiteral(NegateLiteral(ActiveClause.Item[i]));
      AddLiteral(NegateLiteral(ActiveClause.Item[j]));
      AddLiteral(p);

      SubmitClause;

    end;


  for i := 0 to ActiveClause.Count - 1 do
    for j := i+ 1 to ActiveClause.Count - 1 do
    begin
      BeginConstraint;

      AddLiteral(ActiveClause.Item[i]);
      AddLiteral(ActiveClause.Item[j]);
      AddLiteral(NegateLiteral(p));

      SubmitClause;

    end;
  AbortConstraint;

end;
}

function TSATSolverInterface.GetResult: TSolverResult;
begin
  Result := FSolverResult;

end;

constructor TSATSolverInterface.Create;
begin
  inherited Create;

  FClausesStack := TStackOfClauses.Create;
  _FTopConstraint := nil;
  FVarCount := 0;
  FClauseCount := 0;

end;

destructor TSATSolverInterface.Destroy;
begin
  if Stack.Count <> 0 then
    WriteLn('TSATSolverInterface.Destroy: CStack.Count =', CStack.Count);
  Assert(Stack.Count = 0);
  FClausesStack.Free;

  inherited Destroy;

end;

procedure TSATSolverInterface.ReportForcedVariables;
var
  i: Integer;

begin
  Write('Forced Variable Status:');

  for i := 1 to VarCount- 1 do
    if GetValue(i)<> gbUnknown then
    begin
      if GetValue(i)= gbTrue then
        Write('x', i, ' ')
      else
        Write('~x', i, ' ');

    end;
  WriteLn;

end;

procedure Finalize;
begin
  if SatSolverStack.Count <> 0 then
    WriteLn('Error in SatSolverStack');

  GetSatSolver.Free;

end;

initialization
  SatSolverStack := TSatSolverStack.Create;
  Initialize;

finalization
  Finalize;
  SatSolverStack.Free;
end.


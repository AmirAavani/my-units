unit MyTypes;

{$Mode objfpc}
interface
uses
  GenericCollectionUnit, Generics.Collections;

type
  PObject= ^TObject;
  TIntegerArray= array of Integer;
//  TBooleanCollection= specialize TGenericCollectionForBuiltInData<Boolean>;

  { TPair }

  generic TPair<Type1, Type2>= class (TObject)
  var public
    First: Type1;
    Second: Type2;

    constructor Create (F: Type1; S: Type2);
    destructor Destroy; override;

  end;

  { TPairForBuiltInData }

  generic TPairForBuiltInData<Type1, Type2>= class (TObject)
  var public
    First: Type1;
    Second: Type2;

    constructor Create (F: Type1; S: Type2);
    destructor Destroy; override;

  end;

  TIntegerCollection= specialize TList<Integer>;
  TBooleanCollection= specialize TList<Boolean>;

implementation

{ TPair }

constructor TPair.Create (F: Type1; S: Type2);
begin
  inherited Create;

  First:= F;
  Second:= S;

end;

destructor TPair.Destroy;
begin
  First.Free;
  Second.Free;

  inherited Destroy;
end;


{ TPairForBuiltInData }

constructor TPairForBuiltInData.Create (F: Type1; S: Type2);
begin
  inherited Create;

  First:= F;
  Second:= S;

end;

destructor TPairForBuiltInData.Destroy;
begin
  inherited Destroy;
end;

end.

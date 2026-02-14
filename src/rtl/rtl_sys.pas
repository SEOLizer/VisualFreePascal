{$mode objfpc}{$H+}
unit rtl_sys;

interface

type
  // Basis-Datentypen für Mini-LCL
  PtrInt = Integer;
  PtrUInt = Cardinal;
  
  // String-Typen
  TCaption = string;
  
  // Exception-Hierarchie (minimal)
  Exception = class(TObject)
  private
    FMessage: string;
  public
    constructor Create(const AMessage: string);
    property Message: string read FMessage write FMessage;
  end;
  
  EInvalidOperation = class(Exception);
  EComponentError = class(Exception);
  
  // Listen-Typen
  TList = class(TObject)
  private
    FItems: array of Pointer;
    FCount: Integer;
    FCapacity: Integer;
    procedure SetCapacity(ACapacity: Integer);
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; Value: Pointer);
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Delete(Index: Integer);
    procedure Clear;
    function IndexOf(Item: Pointer): Integer;
    property Count: Integer read FCount;
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
  end;

implementation

// Exception Implementation
constructor Exception.Create(const AMessage: string);
begin
  inherited Create;
  FMessage := AMessage;
end;

// TList Implementation
destructor TList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TList.SetCapacity(ACapacity: Integer);
begin
  if ACapacity <> FCapacity then
  begin
    SetLength(FItems, ACapacity);
    FCapacity := ACapacity;
    if FCount > FCapacity then
      FCount := FCapacity;
  end;
end;

function TList.GetItem(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EInvalidOperation.Create('List index out of bounds');
  Result := FItems[Index];
end;

procedure TList.SetItem(Index: Integer; Value: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EInvalidOperation.Create('List index out of bounds');
  FItems[Index] := Value;
end;

function TList.Add(Item: Pointer): Integer;
begin
  if FCount >= FCapacity then
    SetCapacity(FCapacity + 4 + (FCapacity div 4));
  
  FItems[FCount] := Item;
  Result := FCount;
  Inc(FCount);
end;

procedure TList.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EInvalidOperation.Create('List index out of bounds');
    
  for i := Index to FCount - 2 do
    FItems[i] := FItems[i + 1];
    
  Dec(FCount);
end;

procedure TList.Clear;
begin
  FCount := 0;
  SetCapacity(0);
end;

function TList.IndexOf(Item: Pointer): Integer;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    if FItems[i] = Item then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

{==============================================================================}
{ TObjectList - Liste die TObject-Instanzen besitzt                            }
{==============================================================================}

type
  TObjectList = class(TList)
  private
    FOwnsObjects: Boolean;
  public
    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; Value: TObject);
    function Add(Item: TObject): Integer;
    function Remove(Item: TObject): Integer;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

constructor TObjectList.Create(AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

destructor TObjectList.Destroy;
begin
  if FOwnsObjects then
    Clear;
  inherited Destroy;
end;

procedure TObjectList.Clear;
var
  i: Integer;
begin
  if FOwnsObjects then
    for i := 0 to Count - 1 do
      TObject(FItems[i]).Free;
  inherited Clear;
end;

procedure TObjectList.Delete(Index: Integer);
begin
  if FOwnsObjects and (Index >= 0) and (Index < FCount) then
    TObject(FItems[Index]).Free;
  inherited Delete(Index);
end;

function TObjectList.GetItem(Index: Integer): TObject;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := TObject(FItems[Index])
  else
    raise EInvalidOperation.Create('List index out of bounds');
end;

procedure TObjectList.SetItem(Index: Integer; Value: TObject);
begin
  if FOwnsObjects and (Index >= 0) and (Index < FCount) then
    TObject(FItems[Index]).Free;
  inherited SetItem(Index, Pointer(Value));
end;

function TObjectList.Add(Item: TObject): Integer;
begin
  Result := inherited Add(Pointer(Item));
end;

function TObjectList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Pointer(Item));
  if Result >= 0 then
    Delete(Result);
end;

end.
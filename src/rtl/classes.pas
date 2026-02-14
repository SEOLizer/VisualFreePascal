{$mode objfpc}{$H+}
unit classes;

interface

uses
  rtl_sys, events;

type
  // Operation-Typ für Notification
  TOperation = (opInsert, opRemove);

  // Forward-Deklarationen
  TComponent = class;
  TComponentClass = class of TComponent;
  
  // Komponenten-Liste
  TComponentList = class(TList)
  public
    function GetComponent(Index: Integer): TComponent;
    procedure SetComponent(Index: Integer; Value: TComponent);
    property Components[Index: Integer]: TComponent read GetComponent write SetComponent;
  end;
  
  // Persistent-Basisklasse (minimal)
  TPersistent = class(TObject)
  public
    // TODO: Assign, GetNamePath für erweiterte Funktionalität
  end;
  
  // Component-Basisklasse mit Owner/Components
  TComponent = class(TPersistent)
  private
    FOwner: TComponent;
    FComponents: TComponentList;
    FName: string;
    FTag: PtrInt;
    function GetComponent(Index: Integer): TComponent;
    function GetComponentCount: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;
    procedure SetName(const AName: string); virtual;
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    
    procedure InsertComponent(AComponent: TComponent);
    procedure RemoveComponent(AComponent: TComponent);
    function FindComponent(const AName: string): TComponent;
    
    property Owner: TComponent read FOwner;
    property Components[Index: Integer]: TComponent read GetComponent;
    property ComponentCount: Integer read GetComponentCount;
    property Name: string read FName write SetName;
    property Tag: PtrInt read FTag write FTag;
  end;

implementation

// TComponentList Implementation
function TComponentList.GetComponent(Index: Integer): TComponent;
begin
  Result := TComponent(inherited Items[Index]);
end;

procedure TComponentList.SetComponent(Index: Integer; Value: TComponent);
begin
  inherited Items[Index] := Value;
end;

// TComponent Implementation
constructor TComponent.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FComponents := TComponentList.Create;
  FName := '';
  FTag := 0;
  
  if Assigned(FOwner) then
    FOwner.InsertComponent(Self);
end;

destructor TComponent.Destroy;
var
  i: Integer;
begin
  // Entferne alle Child-Komponenten
  for i := FComponents.Count - 1 downto 0 do
    FComponents.Components[i].Free;
    
  FComponents.Free;
  
  // Entferne sich selbst vom Owner
  if Assigned(FOwner) then
    FOwner.RemoveComponent(Self);
    
  inherited Destroy;
end;

function TComponent.GetComponent(Index: Integer): TComponent;
begin
  Result := FComponents.Components[Index];
end;

function TComponent.GetComponentCount: Integer;
begin
  Result := FComponents.Count;
end;

procedure TComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Override in Subklassen für Notification-Handling
end;

procedure TComponent.SetName(const AName: string);
begin
  if FName <> AName then
  begin
    // TODO: Name-Validierung (eindeutig im Owner)
    FName := AName;
  end;
end;

procedure TComponent.InsertComponent(AComponent: TComponent);
begin
  if not Assigned(AComponent) then Exit;
  if FComponents.IndexOf(AComponent) >= 0 then Exit;
  
  FComponents.Add(AComponent);
  AComponent.FOwner := Self;
  Notification(AComponent, opInsert);
end;

procedure TComponent.RemoveComponent(AComponent: TComponent);
var
  Index: Integer;
begin
  Index := FComponents.IndexOf(AComponent);
  if Index >= 0 then
  begin
    Notification(AComponent, opRemove);
    AComponent.FOwner := nil;
    FComponents.Delete(Index);
  end;
end;

function TComponent.FindComponent(const AName: string): TComponent;
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    Result := Components[i];
    if Result.Name = AName then
      Exit;
  end;
  Result := nil;
end;

end.
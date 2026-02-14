{$mode objfpc}{$H+}
{$codepage utf8}
unit dfm_ast;

{==============================================================================}
{ DFM/LFM AST Datenmodell - FPC Standard Version                              }
{==============================================================================}

interface

uses
  SysUtils, Classes;  // Standard FPC Units

type
  TDfmValueKind = (dvkInteger, dvkFloat, dvkBoolean, dvkString, dvkIdentifier, dvkNil, dvkSet, dvkCollection, dvkBinary);

  TDfmValue = class
  private
    FKind: TDfmValueKind;
    FRawValue: string;
  public
    constructor Create(AKind: TDfmValueKind; const ARawValue: string);
    function AsInteger: Int64;
    function AsFloat: Double;
    function AsBoolean: Boolean;
    function AsString: string;
    property Kind: TDfmValueKind read FKind;
    property RawValue: string read FRawValue write FRawValue;
  end;

  TDfmNode = class
  private
    FInstanceName: string;
    FClassName: string;
    FProperties: TStringList;
    FChildren: TList;
  public
    constructor Create(const AInstanceName, AClassName: string);
    destructor Destroy; override;
    procedure AddChild(Child: TDfmNode);
    property InstanceName: string read FInstanceName write FInstanceName;
    property ClassName: string read FClassName write FClassName;
    property Properties: TStringList read FProperties;
    property Children: TList read FChildren;
  end;

  TDfmDocument = class
  private
    FRoot: TDfmNode;
  public
    constructor Create;
    destructor Destroy; override;
    property Root: TDfmNode read FRoot write FRoot;
  end;

implementation

{ TDfmValue }

constructor TDfmValue.Create(AKind: TDfmValueKind; const ARawValue: string);
begin
  inherited Create;
  FKind := AKind;
  FRawValue := ARawValue;
end;

function TDfmValue.AsInteger: Int64;
begin
  Result := StrToInt64(FRawValue);
end;

function TDfmValue.AsFloat: Double;
begin
  Result := StrToFloat(FRawValue);
end;

function TDfmValue.AsBoolean: Boolean;
begin
  Result := SameText(FRawValue, 'True');
end;

function TDfmValue.AsString: string;
begin
  Result := FRawValue;
end;

{ TDfmNode }

constructor TDfmNode.Create(const AInstanceName, AClassName: string);
begin
  inherited Create;
  FInstanceName := AInstanceName;
  FClassName := AClassName;
  FProperties := TStringList.Create;
  FChildren := TList.Create;
end;

destructor TDfmNode.Destroy;
var
  I: Integer;
begin
  for I := 0 to FChildren.Count - 1 do
    TDfmNode(FChildren[I]).Free;
  FChildren.Free;
  FProperties.Free;
  inherited Destroy;
end;

procedure TDfmNode.AddChild(Child: TDfmNode);
begin
  FChildren.Add(Child);
end;

{ TDfmDocument }

constructor TDfmDocument.Create;
begin
  inherited Create;
  FRoot := nil;
end;

destructor TDfmDocument.Destroy;
begin
  if Assigned(FRoot) then
    FRoot.Free;
  inherited Destroy;
end;

end.

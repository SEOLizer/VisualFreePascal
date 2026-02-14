{$mode objfpc}{$H+}
unit dfm_writer;

interface

uses
  SysUtils, Classes, dfm_ast;

type
  TDfmWriter = class
  private
    FOutput: TStrings;
    FIndent: string;
    FLevel: Integer;
    
    procedure AddLine(const AText: string);
    function GetIndent: string;
    procedure WriteNode(ANode: TDfmNode);
    procedure WriteValue(AValue: TDfmValue);
  public
    constructor Create;
    destructor Destroy; override;
    function WriteDocument(ADoc: TDfmDocument): string;
  end;

implementation

constructor TDfmWriter.Create;
begin
  FOutput := TStringList.Create;
  FIndent := '  ';
  FLevel := 0;
end;

destructor TDfmWriter.Destroy;
begin
  FOutput.Free;
  inherited;
end;

function TDfmWriter.GetIndent: string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to FLevel do
    Result := Result + FIndent;
end;

procedure TDfmWriter.AddLine(const AText: string);
begin
  FOutput.Add(GetIndent + AText);
end;

procedure TDfmWriter.WriteValue(AValue: TDfmValue);
var
  S: string;
begin
  case AValue.Kind of
    dvkInteger, dvkFloat, dvkIdentifier:
      S := AValue.RawValue;
    dvkBoolean:
      if AValue.AsBoolean then S := 'True' else S := 'False';
    dvkString:
      S := '''' + AValue.AsString + '''';
    dvkNil:
      S := 'nil';
    else
      S := AValue.RawValue;
  end;
  FOutput[FOutput.Count - 1] := FOutput[FOutput.Count - 1] + S;
end;

procedure TDfmWriter.WriteNode(ANode: TDfmNode);
var
  I: Integer;
  PropName: string;
  PropValue: TDfmValue;
  Child: TDfmNode;
begin
  // object Name: ClassName
  AddLine('object ' + ANode.InstanceName + ': ' + ANode.ClassName);
  Inc(FLevel);
  
  // Properties
  for I := 0 to ANode.Properties.Count - 1 do
  begin
    PropName := ANode.Properties[I];
    PropValue := TDfmValue(ANode.Properties.Objects[I]);
    AddLine(PropName + ' = ');
    WriteValue(PropValue);
  end;
  
  // Kinder rekursiv
  for I := 0 to ANode.Children.Count - 1 do
  begin
    Child := TDfmNode(ANode.Children[I]);
    if Assigned(Child) then
      WriteNode(Child);
  end;
  
  Dec(FLevel);
  AddLine('end');
end;

function TDfmWriter.WriteDocument(ADoc: TDfmDocument): string;
begin
  FOutput.Clear;
  FLevel := 0;
  
  if Assigned(ADoc.Root) then
    WriteNode(ADoc.Root);
  
  Result := FOutput.Text;
end;

end.
